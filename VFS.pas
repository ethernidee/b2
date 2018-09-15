unit VFS;
{
DESCRIPTION:  Virtual File System
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)

TODO: Log all redirected api address, ex. kernel32 => api-ms...
}

(***)  interface  (***)
uses
  Windows, SysUtils, Math, MMSystem,
  Utils, Crypto, Lists, DataLib, StrLib, StrUtils, Files, Log, TypeWrappers,
  PatchApi, Core, Ini, WinUtils, DlgMes (* DELETEME *);

(*
  Redirects calls to:
    CreateDirectoryA,
    CreateFileA,
    DeleteFileA,
    FindClose,
    FindFirstFileA,
    FindNextFileA,
    GetCurrentDirectoryA,
    GetFileAttributesA,
    GetFullPathNameA,
    GetPrivateProfileStringA,
    LoadCursorFromFileA,
    LoadLibraryA,
    PlaySoundA,
    RemoveDirectoryA,
    SetCurrentDirectoryA
*)

(* IMPORT *)
type
  TDict    = DataLib.TDict;
  TObjDict = DataLib.TObjDict;
  TString  = TypeWrappers.TString;
  TList    = Lists.TList;

const
  CMDLINE_ARG_MODLIST = 'modlist';

  OVERWRITE_EXISTING      = true;
  DONT_OVERWRITE_EXISTING = false;

type
  TSearchList = class
    {O} FileList: {O} Lists.TStringList {OF Windows.PWin32FindData};
        FileInd:  integer;

    constructor Create;
    destructor  Destroy; override;
  end; // .class TSearchList

  TDirListingSortType = (SORT_FIFO, SORT_LIFO);


var
{O} ModList: Lists.TStringList;

(*
  CachedPaths, case RedirectedPath of
    '[Full path to file in Mods]' => file exists, no search is necessary
    ''                            => file does not exist in Mods, no search is necessary
    nil                           => no information, search is necessary
*)
{O} CachedPaths:   {O} TDict {OF RelPath: STRING => RedirectedPath: TString};
{O} SearchHandles: {O} TObjDict {OF hSearch: INTEGER => Value: TSearchList};

  // The value is used for finding free seacrh handle for FindFirstFileA function
  hSearch: integer = 1;

  CachedPathsCritSection: Windows.TRTLCriticalSection;
  FileSearchCritSection:  Windows.TRTLCriticalSection;
  FileSearchInProgress:   boolean = false;
  CurrDirCritSection:     Windows.TRTLCriticalSection;
  VfsCritSection:         Windows.TRTLCriticalSection;

  NativeGetFileAttributes:  function (FilePath: pchar): integer; stdcall;
  NativeGetFileAttributesW: function (FilePath: PWideChar): integer; stdcall;

  Kernel32Handle: integer;
  User32Handle:   integer;

  GamePath:   string;
  ModsDir:    string;
  CurrentDir: string;

  DebugOpt: boolean;

(*
  @param aModList List of mod directories from the highest priority mod to the lowest one, each mod directory
                  must be an absolute path.
*)
procedure Init ({U} aModList: Lists.TStringList);

implementation

const
  MAX_SEARCH_HANDLE = 1000;
  VFS_EXTRA_DEBUG   = false;

  AUTO_PRIORITY                = MAXLONGINT div 2;
  INITIAL_OVERWRITING_PRIORITY = AUTO_PRIORITY + 1;
  INITIAL_ADDING_PRIORITY      = AUTO_PRIORITY - 1;

type
  TVfsItem = class
   public
    (* Name in original case *)
    Name: WideString;

    (* Name in lower case, used for wildcard mask matching *)
    SearchName: WideString;

    (* Absolute path to real file/folder location without trailing slash *)
    RealPath: WideString;

    (* The higher priority, the ealier item will appear in directory listing after virtual directories sorting *)
    Priority: integer;

    (* List of directory child items or nil *)
    {On} Children: {U} TList {OF TVfsItem};

    (* Up to 32 attribute flags *)
    Attrs: integer;

    (* Full file info *)
    Info: Windows.TWin32FindDataW;

    function IsDir (): boolean;

    destructor Destroy; override;
  end; // .class TVfsItem

  TFileInfo = class
   public
    Data: TWin32FindDataW;

    constructor Create ({n} Data: Windows.PWin32FindDataW = nil);
  end;

var
(* Map of DLL handle => API name => Real api address *)
{O} DllRealApiAddrs: {O} TObjDict {OF TDict};

{O} VfsItems: {O} TDict {OF TVfsItem};

  VfsHooksInstalled: boolean = false;
  VfsIsRunning:      boolean = false;
  OverwritingPriority: integer = INITIAL_OVERWRITING_PRIORITY;
  AddingPriority:      integer = INITIAL_ADDING_PRIORITY;

constructor TSearchList.Create;
begin
  Self.FileList := Lists.NewStrList
  (
    Utils.OWNS_ITEMS,
    not Utils.ITEMS_ARE_OBJECTS,
    Utils.NO_TYPEGUARD,
    Utils.ALLOW_NIL
  );

  Self.FileList.CaseInsensitive := true;
  Self.FileList.ForbidDuplicates := true;
end; // .constructor TSearchList.Create

destructor TSearchList.Destroy;
begin
  SysUtils.FreeAndNil(Self.FileList);
end;

function TVfsItem.IsDir: boolean;
begin
  result := (Self.Info.dwFileAttributes and Windows.FILE_ATTRIBUTE_DIRECTORY) <> 0;
end;

destructor TVfsItem.Destroy;
begin
  SysUtils.FreeAndNil(Self.Children);
end;

constructor TFileInfo.Create ({n} Data: Windows.PWin32FindDataW = nil);
begin
  if Data <> nil then begin
    Self.Data := Data^;
  end;
end;

procedure FindOutRealSystemApiAddrs (const DllHandles: array of integer);
const
  PE_SIGNATURE_LEN = 4;

type
  PImageImportDirectory = ^TImageImportDirectory;
  TImageImportDirectory = packed record
    RvaImportLookupTable  : integer;
    TimeDateStamp         : integer;
    ForwarderChain        : integer;
    RvaModuleName         : integer;
    RvaImportAddressTable : integer;
  end;

  PHintName = ^THintName;
  THintName = packed record
    Hint: word;
    Name: array [0..MAXLONGINT - 5] of char;
  end;

var
  ImportDirInfo     : PImageDataDirectory;
  ImportDir         : PImageImportDirectory;
  ImportLookupTable : Utils.PEndlessIntArr;
  ImportAddrTable   : Utils.PEndlessIntArr;
  DllApiRedirs      : {U} TDict {OF POINTER};
  DllHandle         : integer;
  i, j              : integer;

begin
  ImportDirInfo     := nil;
  ImportDir         := nil;
  ImportLookupTable := nil;
  ImportAddrTable   := nil;
  DllApiRedirs      := nil;
  // * * * * * //
  for i := 0 to high(DllHandles) do begin
    DllHandle     := DllHandles[i];
    ImportDirInfo := @PImageOptionalHeader(DllHandle + PImageDosHeader(DllHandle)._lfanew + PE_SIGNATURE_LEN + sizeof(TImageFileHeader)).DataDirectory[1];
    DllApiRedirs  := DllRealApiAddrs[Ptr(DllHandle)];

    if DllApiRedirs = nil then begin
      DllApiRedirs                    := DataLib.NewDict(NOT Utils.OWNS_ITEMS, DataLib.CASE_SENSITIVE);
      DllRealApiAddrs[Ptr(DllHandle)] := DllApiRedirs;
    end;

    // Found valid import directory in Win32 PE
    if ((ImportDirInfo.Size > 0) and (ImportDirInfo.VirtualAddress <> 0)) then begin
      ImportDir         := pointer(DllHandle + integer(ImportDirInfo.VirtualAddress));

      while ImportDir.RvaImportLookupTable <> 0 do begin
        ImportLookupTable := pointer(DllHandle + ImportDir.RvaImportLookupTable);
        ImportAddrTable   := pointer(DllHandle + ImportDir.RvaImportAddressTable);
        //DlgMes.Msg(pchar(DllHandle + ImportDir.RvaModuleName));

        j := 0;

        while (j >= 0) and (ImportLookupTable[j] <> 0) do begin
          if ImportLookupTable[j] > 0 then begin
            DllApiRedirs[pchar(@PHintName(DllHandle + ImportLookupTable[j]).Name)] := Ptr(ImportAddrTable[j]);
            //s := s + #13#10 + Format('%s.%s: %p', [pchar(DllHandle + ImportDir.RvaModuleName), pchar(@PHintName(DllHandle + ImportLookupTable[j]).Name), Ptr(ImportAddrTable[j])]);
            //if pchar(@PHintName(DllHandle + ImportLookupTable[j]).Name) = 'IsValidLocale' then DlgMes.Msg(pchar(@PHintName(DllHandle + ImportLookupTable[j]).Name));
          end;

          Inc(j);
        end; // .while

        Inc(ImportDir);
      end; // .while
    end; // .if
  end; // .for

  //Files.WriteFileContents(s, 'D:\__LOG.txt');
  //(Format('%p', [ TDict( DllRealApiAddrs[Ptr(DllHandles[0])] ) ['CreateFileA'] ]));
end; // .procedure FindOutRealSystemApiAddrs

function GetRealAddress (CodeOrRedirStub: pointer): {n} pointer;
begin
  {!} Assert(CodeOrRedirStub <> nil);

  // JMP DWORD [PTR]
  if pword(CodeOrRedirStub)^ = $FF25 then begin
    result := ppointer(integer(CodeOrRedirStub) + 2)^;
  // JMP SHORT REL [INT8 OFS]
  end else if pbyte(CodeOrRedirStub)^ = $EB then begin
    result := pointer(integer(CodeOrRedirStub) + 2 + pshortint(integer(CodeOrRedirStub) + 1)^);
  // JMP REL [INT32 OFS]
  end else if pbyte(CodeOrRedirStub)^ = $E9 then begin
    result := pointer(integer(CodeOrRedirStub) + 5 + pinteger(integer(CodeOrRedirStub) + 1)^);
  // Regular code
  end else begin
    result := CodeOrRedirStub;
  end; // .else
end; // .function GetRealAddress

function GetRealProcAddress (DllHandle: integer; const ProcName: string): {n} pointer;
var
{Un} DllApiRedirs: {U} TDict {OF pointer};

begin
  DllApiRedirs := DllRealApiAddrs[Ptr(DllHandle)];
  result       := nil;
  // * * * * * //

  if DllApiRedirs <> nil then begin
    result := DllApiRedirs[ProcName];
  end;

  if result = nil then begin
    result := Windows.GetProcAddress(DllHandle, pchar(ProcName));
  end;

  if result <> nil then begin
    result := GetRealAddress(result);
  end;
end; // .function GetRealProcAddress

function CompareBinStringsW (const Str1, Str2: WideString): integer;
var
  Str1Ptr: PWideChar;
  Str2Ptr: PWideChar;
  Char1:   WideChar;
  Char2:   WideChar;

begin
  Str1Ptr := PWideChar(Str1);
  Str2Ptr := PWideChar(Str2);
  Char1   := #0;
  Char2   := #0;
  result  := 0;

  while true do begin
    Char1 := Str1Ptr^;
    Char2 := Str2Ptr^;

    if Char1 = Char2 then begin
      if Char1 = #0 then begin
        exit;
      end;

      Inc(Str1Ptr);
      Inc(Str2Ptr);
    end else begin
      break;
    end;
  end;

  // Characters differ, fix up each one if they're both in or above the surrogate range, then compare them
  if (ord(Char1) >= $D800) and (ord(Char2) >= $D800) then begin
    if ord(Char1) >= $E000 then begin
      Char1 := WideChar(ord(Char1) - $800);
    end else begin
      Char1 := WideChar(ord(Char1) + $2000);
    end;

    if ord(Char2) >= $E000 then begin
      Char2 := WideChar(ord(Char2) - $800);
    end else begin
      Char2 := WideChar(ord(Char2) + $2000);
    end;
  end;

  // Now both characters are in code point order
  result := ord(Char1) - ord(Char2);
end; // .function CompareBinStringsW

(*
  Returns expanded unicode path, preserving trailing delimiter, or original path on error.
*)
function ExpandPath (const Path: WideString): WideString;
var
  BufLen:         integer;
  NumCharsCopied: integer;
  FileNameAddr:   PWideChar;

begin
  result := '';

  if Path <> '' then begin
    BufLen         := 0;
    NumCharsCopied := Windows.GetFullPathNameW(PWideChar(Path), 0, nil, FileNameAddr);

    while NumCharsCopied > BufLen do begin
      BufLen         := NumCharsCopied;
      SetLength(result, BufLen - 1);
      NumCharsCopied := Windows.GetFullPathNameW(PWideChar(Path), BufLen, PWideChar(result), FileNameAddr);
    end;

    if NumCharsCopied <= 0 then begin
      result := Path;
    end else begin
      SetLength(result, NumCharsCopied);
    end;
  end; // .if
end; // .function ExpandPath

(*
  Returns expanded path without trailing delimiter. Optionally returns flag, whether path had trailing delim or not.
*)
function NormalizePath (const Path: WideString; {n} HadTrailingDelim: pboolean = nil): WideString;
var
  ExpandedPath: WideString;

begin
  ExpandedPath := ExpandPath(Path);
  result := StrLib.ExcludeTrailingDelimW(ExpandedPath);

  if HadTrailingDelim <> nil  then begin
    HadTrailingDelim^ := Length(result) <> Length(ExpandedPath);
  end;
end;

(*
  Expands path, onverts it to lower case, trims trailing delimiter and packs in ANSI string as buffer.
  @param AbsPath Normalized absolute path
*)
function PackPath (const AbsPath: WideString): string;
var
  ProcessedPath: WideString;

begin
  result := '';

  if AbsPath <> '' then begin
    ProcessedPath := StrLib.WideLowerCase(AbsPath);
    SetLength(result, Length(ProcessedPath) * sizeof(ProcessedPath[1]) div sizeof(result[1]));
    Utils.CopyMem(Length(result) * sizeof(result[1]), PWideChar(ProcessedPath), PChar(result));
  end;
end;

function UnpackPath (const PackedPath: string): WideString;
begin
  result := '';

  if PackedPath <> '' then begin
    SetLength(result, Length(PackedPath) * sizeof(PackedPath[1]) div sizeof(result[1]));
    Utils.CopyMem(Length(result) * sizeof(result[1]), pchar(PackedPath), PWideChar(result));
  end;
end;

(*
  Returns Win32FindDataW record for single found file/directory.
*)
function GetFileInfo (const FilePath: WideString; {OUT} var Res: Windows.TWin32FindDataW): boolean;
var
  hSearch:  cardinal;
  FileInfo: TWin32FindDataW;

begin
  hSearch := FindFirstFileW(PWideChar(FilePath), FileInfo);
  result  := hSearch <> INVALID_HANDLE_VALUE;

  if result then begin
    Res := FileInfo;
    Windows.FindClose(hSearch);
  end;
end; // .function GetFileInfo

function FileExistsW (const FilePath: WideString): boolean;
begin
  result := NativeGetFileAttributesW(PWideChar(FilePath)) <> -1;
end;

function CompareVfsItemsByPriorityDescAndNameAsc (Item1, Item2: integer): integer;
begin
  result := TVfsItem(Item2).Priority - TVfsItem(Item1).Priority;

  if result = 0 then begin
    result := CompareBinStringsW(TVfsItem(Item1).Name, TVfsItem(Item2).Name);
  end;
end;

function CompareVfsItemsByPriorityAscAndNameAsc (Item1, Item2: integer): integer;
begin
  result := TVfsItem(Item1).Priority - TVfsItem(Item2).Priority;

  if result = 0 then begin
    result := CompareBinStringsW(TVfsItem(Item1).Name, TVfsItem(Item2).Name);
  end;
end;

procedure SortVfsListing ({U} List: DataLib.TList {OF TVfsItem}; SortType: TDirListingSortType);
begin
  if SortType = SORT_FIFO then begin
    List.CustomSort(CompareVfsItemsByPriorityDescAndNameAsc);
  end else begin
    List.CustomSort(CompareVfsItemsByPriorityAscAndNameAsc);
  end;
end;

procedure SortVfsDirListings (SortType: TDirListingSortType);
var
{Un} Children: DataLib.TList {OF TVfsItem};

begin
  Children := nil;
  // * * * * * //
  with DataLib.IterateDict(VfsItems) do begin
    while IterNext() do begin
      Children := TVfsItem(IterValue).Children;

      if (Children <> nil) and (Children.Count > 1) then begin
        SortVfsListing(Children, SortType);
      end;
    end;
  end;
end; // .procedure SortVfsDirListings

procedure RebuildVfsItemsTree;
var
{Un} DirVfsItem: TVfsItem;
     AbsDirPath: WideString;

begin
  DirVfsItem := nil;
  // * * * * * //
  with DataLib.IterateDict(VfsItems) do begin
    while IterNext() do begin
      AbsDirPath := StrLib.ExtractDirPathW(UnpackPath(IterKey));
      DirVfsItem := VfsItems[PackPath(AbsDirPath)];

      if DirVfsItem <> nil then begin
        DirVfsItem.Children.Add(IterValue);
      end;
    end;
  end;
end; // .procedure RebuildVfsItemsTree

procedure ClearRedirections;
begin
  VfsItems.Clear();
end;

function FindVfsItem (const AbsPath: WideString): {Un} TVfsItem;
begin
  result := VfsItems[PackPath(AbsPath)];
end;

function IsRedirected (const AbsPath: WideString): boolean;
begin
  result := FindVfsItem(AbsPath) <> nil;
end;

(*
  Redirects single file/directory path (not including directory contents). Target must exist for success.
*)
function RedirectFile (const AbsVirtPath, AbsRealPath: WideString; {n} FileInfoPtr: Windows.PWin32FindDataW; OverwriteExisting: boolean; Priority: integer): {Un} TVfsItem;
var
{Un} VfsItem:        TVfsItem;
     PackedVirtPath: string;
     IsNewItem:      boolean;
     FileInfo:       Windows.TWin32FindDataW;
     Success:        boolean;

begin
  VfsItem := nil;
  result  := nil;
  // * * * * * //
  PackedVirtPath := PackPath(AbsVirtPath);
  VfsItem        := VfsItems[PackedVirtPath];
  IsNewItem      := VfsItem = nil;
  Success        := true;

  if IsNewItem or OverwriteExisting then begin
    if FileInfoPtr = nil then begin
      Success := GetFileInfo(AbsRealPath, FileInfo);
    end;

    if Success then begin
      if IsNewItem then begin
        VfsItem                  := TVfsItem.Create();
        VfsItems[PackedVirtPath] := VfsItem;
      end;

      if FileInfoPtr <> nil then begin
        VfsItem.Info := FileInfoPtr^;
      end else begin
        VfsItem.Info := FileInfo;
      end;
      
      VfsItem.Name       := VfsItem.Info.cFileName;
      VfsItem.SearchName := StrLib.WideLowerCase(VfsItem.Name);
      VfsItem.RealPath   := AbsRealPath;
      VfsItem.Priority   := Priority;
      VfsItem.Attrs      := 0;
    end; // .if
  end; // .if

  if Success then begin
    result := VfsItem;
  end;
end; // .function RedirectFile

function IsDir (const AbsPath: WideChar): boolean;
begin
  result := (NativeGetFileAttributesW(PWideChar(AbsPath)) and Windows.FILE_ATTRIBUTE_DIRECTORY) = Windows.FILE_ATTRIBUTE_DIRECTORY;
end;

(*
  Maps real directory contents to virtual path. Target must exist for success.
*)
function _MapDir (const AbsVirtPath, AbsRealPath: WideString; {n} FileInfoPtr: Windows.PWin32FindDataW; OverwriteExisting: boolean; Priority: integer): {Un} TVfsItem;
var
{O}  Subdirs:        {O} TList {OF TFileInfo};
{U}  SubdirInfo:     TFileInfo;
{Un} DirVfsItem:     TVfsItem;
     Success:        boolean;
     hSearch:        cardinal;
     FileInfo:       Windows.TWin32FindDataW;
     VirtPathPrefix: WideString;
     RealPathPrefix: WideString;
     i:              integer;

begin
  DirVfsItem := nil;
  Subdirs    := DataLib.NewList(Utils.OWNS_ITEMS);
  SubdirInfo := nil;
  result     := nil;
  // * * * * * //
  if Priority = AUTO_PRIORITY then begin
    if OverwriteExisting then begin
      Priority := OverwritingPriority;
      Inc(OverwritingPriority);
    end else begin
      Priority := AddingPriority;
      Dec(AddingPriority);
    end;
  end;
  
  DirVfsItem := RedirectFile(AbsVirtPath, AbsRealPath, FileInfoPtr, OverwriteExisting, Priority);
  Success    := DirVfsItem <> nil;

  if Success then begin
    hSearch := Windows.FindFirstFileW(PWideChar(AbsRealPath + '\*'), FileInfo);

    if hSearch <> Windows.INVALID_HANDLE_VALUE then begin
      VirtPathPrefix := AbsVirtPath + '\';
      RealPathPrefix := AbsRealPath + '\';

      if DirVfsItem.Children = nil then begin
        DirVfsItem.Children := DataLib.NewList(not Utils.OWNS_ITEMS);
      end;

      repeat
        if (FileInfo.dwFileAttributes and Windows.FILE_ATTRIBUTE_DIRECTORY) = Windows.FILE_ATTRIBUTE_DIRECTORY then begin         
          // Exclude '.' and '..'
          if (pinteger(@FileInfo.cFileName)^ <> $0000002E) and (pinteger(@FileInfo.cFileName)^ <> $002E002E) then begin
            Subdirs.Add(TFileInfo.Create(@FileInfo));
          end;
        end else begin
          RedirectFile(VirtPathPrefix + FileInfo.cFileName, RealPathPrefix + FileInfo.cFileName, @FileInfo, OverwriteExisting, Priority);
        end;
      until not Windows.FindNextFileW(hSearch, FileInfo);

      Windows.FindClose(hSearch);
    end;

    for i := 0 to Subdirs.Count - 1 do begin
      SubdirInfo := TFileInfo(Subdirs[i]);
      _MapDir(VirtPathPrefix + SubdirInfo.Data.cFileName, RealPathPrefix + SubdirInfo.Data.cFileName, @SubdirInfo.Data, OverwriteExisting, Priority);
    end;
  end; // .if

  if Success then begin
    result := DirVfsItem;
  end;
  // * * * * * //
  SysUtils.FreeAndNil(Subdirs);
end; // .function _MapDir

(*
  Returns real path for vfs item by its absolute virtual path or empty string. Optionally returns file info structure.
*)
function GetVfsItemRealPath (const AbsVirtPath: WideString; {n} FileInfo: Windows.PWin32FindDataW = nil): WideString;
var
{n} VfsItem: TVfsItem;

begin
  VfsItem := nil;
  // * * * * * //
  result := '';
  
  {!} Windows.EnterCriticalSection(VfsCritSection);

  VfsItem := VfsItems[PackPath(AbsVirtPath)];

  if VfsItem <> nil then begin
    result := VfsItem.RealPath;

    if FileInfo <> nil then begin
      FileInfo^ := VfsItem.Info;
    end;
  end;

  {!} Windows.LeaveCriticalSection(VfsCritSection);
end; // .function GetVfsItemRealPath

function IsRelativePath (const Path: string): boolean;
var
  DesignatorPos: integer;

begin
  result := not StrLib.FindChar(':', Path, DesignatorPos) and
            not StrUtils.AnsiStartsStr('\\', Path);
end; // .function IsRelativePath

function FileExists (const FilePath: string): boolean;
begin
  result := NativeGetFileAttributes(pchar(FilePath)) <> -1;
end;

function DirExists (const FilePath: string): boolean;
var
  Attrs: integer;

begin
  Attrs := NativeGetFileAttributes(pchar(FilePath));
  result := (Attrs <> - 1) and ((Attrs and Windows.FILE_ATTRIBUTE_DIRECTORY) <> 0);
end; // .function DirExists

function FindVFSPath (const RelativePath: string; out RedirectedPath: string): boolean;
var
{U} RedirectedPathValue:  TString;
    NumMods:              integer;
    i:                    integer;

begin
  RedirectedPathValue := nil;
  // * * * * * //
  {!} Windows.EnterCriticalSection(CachedPathsCritSection);

  result := false;

  if DebugOpt then begin
    Log.Write('VFS', 'FindVFSPath', 'Original: ' + RelativePath);
  end; // .if

  if CachedPaths.GetExistingValue(RelativePath, pointer(RedirectedPathValue)) then begin
    result := RedirectedPathValue.Value <> '';

    if result then begin
      RedirectedPath := RedirectedPathValue.Value;
    end; // .if
  end else begin
    NumMods := ModList.Count;
    i := 0;

    while (i < NumMods) and not result do begin
      RedirectedPath := StrLib.Concat([ModList[i], '\', RelativePath]);
      result         := FileExists(RedirectedPath);

      Inc(i);
    end; // .while

    if result then begin
      CachedPaths[RelativePath] := TString.Create(RedirectedPath);
    end else begin
      CachedPaths[RelativePath] := TString.Create('');
    end; // .else
  end; // .else

  if DebugOpt then begin
    if result then begin
      Log.Write('VFS', 'FindVFSPath', 'Redirected: ' + RedirectedPath);
    end else begin
      Log.Write('VFS', 'FindVFSPath', 'result: NOT_FOUND');
    end; // .else
  end; // .if

  {!} Windows.LeaveCriticalSection(CachedPathsCritSection);
end; // .function FindVFSPath

function IsInGameDir (const FullPath: string): boolean;
  function GetCharAt (const Str: string; Pos: integer): char; inline;
  begin
    if (Pos >= 1) and (Pos <= length(Str)) then begin
      result := Str[Pos];
    end else begin
      result := #0;
    end;
  end;

begin
  result := ((Length(FullPath) - Length(GamePath)) > 1) and
            StrUtils.AnsiStartsText(GamePath, FullPath) and
            (FullPath[Length(GamePath) + 1] = '\')      and
            (not StrUtils.AnsiStartsText(ModsDir, FullPath) or not (GetCharAt(FullPath, length(ModsDir) + 1) in [#0, '\']));

  if DebugOpt then begin
    if result then begin
      Log.Write('VFS', 'IsInGameDir', FullPath + '  =>  YES');
    end else begin
      Log.Write('VFS', 'IsInGameDir', FullPath + '  =>  NO');
    end;
  end;
end; // .function IsInGameDir

function GameRelativePath (const FullPath: string): string;
begin
  // Copy rest of path right after "\" character
  result := System.Copy(FullPath, Length(GamePath) + sizeof('\') + 1);
end; // .function GameRelativePath

procedure MyScanDir (const MaskedPath: string; SearchList: TSearchList);
var
{U} FoundData: Windows.PWin32FindData;

begin
  {!} Assert(SearchList <> nil);

  with Files.Locate(MaskedPath, Files.FILES_AND_DIRS) do begin
    while FindNext do begin
      if SearchList.FileList.Items[FoundName] = nil then begin
        New(FoundData);
        FoundData^ := FoundRec.Rec.FindData;
        SearchList.FileList.AddObj(FoundName, FoundData);
      end; // .if
    end; // .while
  end; // .with 
end; // .procedure MyScanDir

function MyFindFirstFile (const MaskedPath: string; IsInternalSearch: boolean; out ResHandle: integer): boolean;
var
{O} SearchList:   TSearchList;
    RelativePath: string;
    i:            integer;

begin
  SearchList := TSearchList.Create;
  // * * * * * //
  if IsInternalSearch then begin
    RelativePath := GameRelativePath(MaskedPath);

    if VFS_EXTRA_DEBUG then begin
      Log.Write('VFS', 'MyFindFirstFile', 'RelativePath: ' + RelativePath);
    end; // .if

    for i := 0 to ModList.Count - 1 do begin
      if VFS_EXTRA_DEBUG then begin
        Log.Write('VFS', 'MyFindFirstFile', 'TestPath: ' + ModList[i] + '\' + RelativePath);
      end; // .if

      MyScanDir(ModList[i] + '\' + RelativePath, SearchList);
    end; // .for
  end; // .if

  MyScanDir(MaskedPath, SearchList);
  result := SearchList.FileList.Count > 0;

  if result then begin 
    hSearch := 1;

    while (hSearch < MAX_SEARCH_HANDLE) and (SearchHandles[Ptr(hSearch)] <> nil) do begin
      Inc(hSearch);
    end; // .while

    {!} Assert(hSearch < MAX_SEARCH_HANDLE); 
    ResHandle := hSearch;
    SearchHandles[Ptr(ResHandle)] := SearchList; SearchList :=  nil;
  end; // .if
  // * * * * * //
  SysUtils.FreeAndNil(SearchList);
end; // .function MyFindFirstFile

function MyFindNextFile (SearchHandle: integer; out ResData: Windows.PWin32FindData): boolean;
var
{U} SearchList: TSearchList;

begin
  {!} Assert(ResData = nil);
  SearchList := SearchHandles[Ptr(SearchHandle)];
  // * * * * * //
  result := (SearchList <> nil) and ((SearchList.FileInd + 1) < SearchList.FileList.Count);

  if result then begin
    Inc(SearchList.FileInd);
    ResData := SearchList.FileList.Values[SearchList.FileInd];
  end; // .if
end; // .function MyFindNextFile

function Hook_CreateFileW (Hook: PatchApi.THiHook; lpFileName: PWideChar;
                           dwDesiredAccess, dwShareMode: DWORD;
                           lpSecurityAttributes: PSecurityAttributes;
                           dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                           hTemplateFile: THandle): THandle; stdcall;
var
  UseRedirection:   boolean;
  RedirectedPath:   WideString;
  HadTrailingDelim: boolean;
  OrigPathA:        string;
  RedirectedPathA:  string;

begin
  UseRedirection := ((dwCreationDisposition and Windows.OPEN_EXISTING) = Windows.OPEN_EXISTING) and ((dwDesiredAccess and Windows.GENERIC_WRITE) <> Windows.GENERIC_WRITE);

  if UseRedirection then begin
    RedirectedPath := GetVfsItemRealPath(NormalizePath(lpFileName, @HadTrailingDelim));
    UseRedirection := RedirectedPath <> '';
  end;

  if DebugOpt then begin
    StrLib.PWideCharToAnsi(lpFileName, OrigPathA);

    if UseRedirection then begin
      StrLib.PWideCharToAnsi(PWideChar(RedirectedPath), RedirectedPathA);
      Log.Write('VFS', 'CreateFileW', 'Redirected "' + OrigPathA + '" => "' + RedirectedPathA + '"');
    end else begin
      Log.Write('VFS', 'CreateFileW', '"' + OrigPathA + '"');
    end;
  end;

  if UseRedirection then begin
    if HadTrailingDelim then begin
      RedirectedPath := RedirectedPath + '\';
    end;

    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [PWideChar(RedirectedPath), dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile]);
  end else begin
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile]);
  end;
end; // .function Hook_CreateFileW

function Hook_CreateFileA (Hook: PatchApi.THiHook; lpFileName: PAnsiChar;
                           dwDesiredAccess, dwShareMode: DWORD;
                           lpSecurityAttributes: PSecurityAttributes;
                           dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                           hTemplateFile: THandle): THandle; stdcall;
begin
  result := Windows.CreateFileW(PWideChar(WideString(String(lpFileName))), dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
end; // .function Hook_CreateFileA

function Hook_OpenFile (Hook: PatchApi.THiHook; const lpFileName: LPCSTR; var lpReOpenBuff: TOFStruct; uStyle: UINT): THandle; stdcall;
const
  NOT_SUPPORTED_FLAGS = Windows.OF_CREATE or Windows.OF_DELETE or Windows.OF_READWRITE or Windows.OF_WRITE;

var
  UseRedirection:   boolean;
  RedirectedPath:   WideString;
  HadTrailingDelim: boolean;
  RedirectedPathA:  string;

begin
  UseRedirection := (uStyle and NOT_SUPPORTED_FLAGS) = 0;

  if UseRedirection then begin
    RedirectedPath := GetVfsItemRealPath(NormalizePath(String(lpFileName), @HadTrailingDelim));
    UseRedirection := (RedirectedPath <> '') and StrLib.PWideCharToAnsi(PWideChar(RedirectedPath), RedirectedPathA, StrLib.FAIL_ON_ERROR);

    if UseRedirection and HadTrailingDelim then begin
      RedirectedPathA := RedirectedPathA + '\';
    end;
  end;

  if DebugOpt then begin
    if UseRedirection then begin
      Log.Write('VFS', 'OpenFile', 'Redirected "' + lpFileName + '" => "' + RedirectedPathA + '"');
    end else begin
      Log.Write('VFS', 'OpenFile', '"' + lpFileName + '"');
    end;
  end;

  if UseRedirection then begin
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [pchar(RedirectedPathA), @lpReOpenBuff, uStyle]);
  end else begin
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [lpFileName, @lpReOpenBuff, uStyle]);
  end;
end; // .function Hook_OpenFile

function Hook_GetFileAttributesW (Hook: PatchApi.THiHook; lpFileName: PWideChar): DWORD; stdcall;
var
  UseRedirection:   boolean;
  RedirectedPath:   WideString;
  HadTrailingDelim: boolean;
  OrigPathA:        string;
  RedirectedPathA:  string;

begin
  RedirectedPath := GetVfsItemRealPath(NormalizePath(lpFileName, @HadTrailingDelim));
  UseRedirection := RedirectedPath <> '';

  if DebugOpt then begin
    StrLib.PWideCharToAnsi(lpFileName, OrigPathA);

    if UseRedirection then begin
      StrLib.PWideCharToAnsi(PWideChar(RedirectedPath), RedirectedPathA);
      Log.Write('VFS', 'GetFileAttributesW', 'Redirected "' + OrigPathA + '" => "' + RedirectedPathA + '"');
    end else begin
      Log.Write('VFS', 'GetFileAttributesW', '"' + OrigPathA + '"');
    end;
  end;

  if UseRedirection then begin
    if HadTrailingDelim then begin
      RedirectedPath := RedirectedPath + '\';
    end;

    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [PWideChar(RedirectedPath)]);
  end else begin
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [lpFileName]);
  end;
end; // .function Hook_GetFileAttributesW

function Hook_GetFileAttributesA (Hook: PatchApi.THiHook; lpFileName: pchar): DWORD; stdcall;
begin
  result := Windows.GetFileAttributesW(PWideChar(WideString(String(lpFileName))));
end;

function Hook_LoadCursorFromFileA (Hook: PatchApi.THiHook; lpFileName: PAnsiChar): DWORD; stdcall;
var
  FilePath:           string;
  RedirectedFilePath: string;
  FinalFilePath:      string;

begin
  FilePath := lpFileName;

  if DebugOpt then begin
    Log.Write('VFS', 'LoadCursorFromFileA', 'Original: ' + FilePath);
  end; // .if

  FilePath := SysUtils.ExpandFileName(FilePath);
  FinalFilePath := FilePath;

  if IsInGameDir(FilePath) and
     FindVFSPath(GameRelativePath(FilePath), RedirectedFilePath)
  then begin
    FinalFilePath := RedirectedFilePath;
  end; // .if

  if DebugOpt then begin
    Log.Write('VFS', 'LoadCursorFromFileA', 'Redirected: ' + FinalFilePath);
  end; // .if

  result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [pchar(FinalFilePath)]);
end; // .function Hook_LoadCursorFromFileA

function Hook_LoadLibraryW (Hook: PatchApi.THiHook; lpLibFileName: PWideChar): HMODULE; stdcall;
var
  LibPath:          WideString;
  CharPos:          integer;
  UseRedirection:   boolean;
  RedirectedPath:   WideString;
  HadTrailingDot:   boolean;
  HadTrailingDelim: boolean;
  OrigPathA:        string;
  RedirectedPathA:  string;

begin
  LibPath        := lpLibFileName;
  HadTrailingDot := false;
  result         := 0;

  if StrLib.FindCharW('.', StrLib.ExtractFileNameW(LibPath), CharPos) then begin
    // Remove trailing dot, if any
    if LibPath[Length(LibPath)] = '.' then begin
      SetLength(LibPath, Length(LibPath) - 1);
      HadTrailingDot := true;
    end;
  end else begin
    // No extension, append '.dll'
    LibPath := LibPath + '.dll';
  end;

  // For relative path or unsupported path type prepend exe directory, not current directory
  if not StrLib.FindCharW(':', LibPath, CharPos) then begin
    LibPath := StrLib.ExtractDirPathW(WinUtils.GetExePath()) + '\' + LibPath;
  end;
  
  RedirectedPath := GetVfsItemRealPath(NormalizePath(LibPath, @HadTrailingDelim));
  UseRedirection := RedirectedPath <> '';

  if UseRedirection then begin
    if HadTrailingDot then begin
      RedirectedPath := RedirectedPath + '.';
    end;

    if HadTrailingDelim then begin
      RedirectedPath := RedirectedPath + '\';
    end;

    result         := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [PWideChar(RedirectedPath)]);
    UseRedirection := result <> 0;
    
    if DebugOpt then begin
      StrLib.PWideCharToAnsi(lpLibFileName, OrigPathA);
      StrLib.PWideCharToAnsi(PWideChar(RedirectedPath), RedirectedPathA);

      if UseRedirection then begin
        Log.Write('VFS', 'LoadLibraryW', 'Redirected "' + OrigPathA + '" => "' + RedirectedPathA + '"');
      end else begin
        Log.Write('VFS', 'LoadLibraryW', 'Redirected library was not loaded: "' + OrigPathA + '" => "' + RedirectedPathA + '"');
      end;
    end;    
  end; // .if

  if not UseRedirection then begin
    if DebugOpt then begin
      StrLib.PWideCharToAnsi(lpLibFileName, OrigPathA);
      Log.Write('VFS', 'LoadLibraryW', '"' + OrigPathA + '"');
    end;

    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [lpLibFileName]);
  end;
end; // .function Hook_LoadLibraryW

function Hook_LoadLibraryA (Hook: PatchApi.THiHook; lpLibFileName: PAnsiChar): HMODULE; stdcall;
begin
  result := Windows.LoadLibraryW(PWideChar(WideString(String(lpLibFileName))));
end;

function Hook_CreateDirectoryA (Hook: PatchApi.THiHook; lpPathName: PAnsiChar;
                                lpSecurityAttributes: PSecurityAttributes): BOOL; stdcall;
var
  DirPath:         string;
  ExpandedDirPath: string;

begin
  DirPath := lpPathName;

  if DebugOpt then begin
    Log.Write('VFS', 'CreateDirectoryA', 'Original: ' + DirPath);
  end; // .if

  ExpandedDirPath := SysUtils.ExpandFileName(DirPath);

  if DebugOpt then begin
    Log.Write('VFS', 'CreateDirectoryA', 'Expanded: ' + ExpandedDirPath);
  end; // .if

  result := BOOL(PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc,
                               [pchar(ExpandedDirPath), lpSecurityAttributes]));
end; // .function Hook_CreateDirectoryA

function Hook_RemoveDirectoryA (Hook: PatchApi.THiHook; lpPathName: PAnsiChar): BOOL; stdcall;
var
  DirPath:         string;
  ExpandedDirPath: string;

begin
  DirPath := lpPathName;

  if DebugOpt then begin
    Log.Write('VFS', 'RemoveDirectoryA', 'Original: ' + DirPath);
  end; // .if

  ExpandedDirPath := SysUtils.ExpandFileName(DirPath);

  if DebugOpt then begin
    Log.Write('VFS', 'RemoveDirectoryA', 'Expanded: ' + ExpandedDirPath);
  end; // .if

  result := BOOL(PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc,[pchar(ExpandedDirPath)]));
end; // .function Hook_RemoveDirectoryA

function Hook_DeleteFileA (Hook: PatchApi.THiHook; lpFileName: PAnsiChar): BOOL; stdcall;
var
  FilePath:         string;
  ExpandedFilePath: string;

begin
  FilePath := lpFileName;

  if DebugOpt then begin
    Log.Write('VFS', 'DeleteFileA', 'Original: ' + FilePath);
  end; // .if

  ExpandedFilePath := SysUtils.ExpandFileName(FilePath);

  if DebugOpt then begin
    Log.Write('VFS', 'DeleteFileA', 'Expanded: ' + ExpandedFilePath);
  end; // .if

  result := BOOL(PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [pchar(ExpandedFilePath)]));
end; // .function Hook_DeleteFileA

function Hook_FindFirstFileW (Hook: PatchApi.THiHook; lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindDataA): THandle; stdcall;
var
  FilePath:  string;
  FoundPath: string;
  ResHandle: integer;

begin
  {!} Windows.EnterCriticalSection(FileSearchCritSection);

  if FileSearchInProgress then begin
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [lpFileName, @lpFindFileData]);
  end else begin
    FileSearchInProgress := true;
    FilePath             := lpFileName;

    if DebugOpt then begin
      Log.Write('VFS', 'FindFirstFileA', 'Original: ' + FilePath);
    end;

    FilePath := SysUtils.ExpandFileName(FilePath);

    if DebugOpt then begin
      Log.Write('VFS', 'FindFirstFileA', 'MaskedPath: ' + FilePath);
    end;

    if MyFindFirstFile(FilePath, IsInGameDir(FilePath), ResHandle) then begin
      result         := ResHandle;
      lpFindFileData := Windows.PWin32FindData(TSearchList(SearchHandles[Ptr(ResHandle)])
                                               .FileList.Values[0])^;
      Windows.SetLastError(Windows.ERROR_SUCCESS);

      if DebugOpt then begin
        FoundPath := lpFindFileData.cFileName;
        Log.Write('VFS', 'FindFirstFileA', StrLib.Concat(['Handle: ', SysUtils.IntToStr(ResHandle),
                                                          #13#10, 'result: ', FoundPath]));
      end;
    end else begin
      result := Windows.INVALID_HANDLE_VALUE;
      Windows.SetLastError(Windows.ERROR_NO_MORE_FILES);

      if DebugOpt then begin
        Log.Write('VFS', 'FindFirstFileA', 'Error: ERROR_NO_MORE_FILES');
      end;
    end; // .else

    FileSearchInProgress := false;
  end; // .else

  {!} Windows.LeaveCriticalSection(FileSearchCritSection);
end; // .function Hook_FindFirstFileW

function Hook_FindFirstFileA (Hook: PatchApi.THiHook; lpFileName: pchar; var lpFindFileData: TWIN32FindDataA): THandle; stdcall;
const
  MAX_FILENAME_LEN     = Windows.MAX_PATH - 1;
  MAX_ALT_FILENAME_LEN = 14 - 1;

var
  FileInfo:    Windows.TWin32FindDataW;
  FileName:    string;
  AltFileName: string;

begin
  result := Windows.FindFirstFileW(lpFileName, FileInfo);

  if result <> Windows.INVALID_HANDLE_VALUE then begin
    lpFindFileData.dwFileAttributes := FileInfo.dwFileAttributes;
    lpFindFileData.ftCreationTime   := FileInfo.ftCreationTime;
    lpFindFileData.ftLastAccessTime := FileInfo.ftLastAccessTime;
    lpFindFileData.ftLastWriteTime  := FileInfo.ftLastWriteTime;
    lpFindFileData.nFileSizeHigh    := FileInfo.nFileSizeHigh;
    lpFindFileData.nFileSizeLow     := FileInfo.nFileSizeLow;
    lpFindFileData.dwReserved0      := FileInfo.dwReserved0;
    lpFindFileData.dwReserved1      := FileInfo.dwReserved1;

    StrLib.PWideCharToAnsi(WideString(FileInfo.cFileName), FileName);
    StrLib.PWideCharToAnsi(WideString(FileInfo.cAlternateFileName), AltFileName);

    if Length(FileName) > MAX_FILENAME_LEN then begin
      SetLength(FileName, MAX_FILENAME_LEN);
    end;
    
    if Length(AltFileName) > MAX_ALT_FILENAME_LEN then begin
      SetLength(AltFileName, MAX_ALT_FILENAME_LEN);
    end;
    
    Utils.CopyMem(Length(FileName) + 1, pchar(FileName), @lpFindFileData.cFileName);
    Utils.CopyMem(Length(AltFileName) + 1, pchar(AltFileName), @lpFindFileData.cAlternateFileName);
  end; // .if
end; // .function Hook_FindFirstFileA

function Hook_FindNextFileA (Hook: PatchApi.THiHook; hFindFile: THandle; var lpFindFileData: TWIN32FindDataA): BOOL; stdcall;
var
{U} FoundData: Windows.PWin32FindData;
    FoundPath: string;

begin
  {!} Windows.EnterCriticalSection(FileSearchCritSection);

  if FileSearchInProgress then begin
    result := BOOL(PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc,
                   [hFindFile, @lpFindFileData]));
  end else begin
    if DebugOpt then begin
      Log.Write('VFS', 'FindNextFileA', 'Handle: ' + SysUtils.IntToStr(hFindFile))
    end; // .if

    FoundData := nil;
    result := MyFindNextFile(hFindFile, FoundData);

    if result then begin
      lpFindFileData := FoundData^;
      Windows.SetLastError(Windows.ERROR_SUCCESS);

      if DebugOpt then begin
        FoundPath := FoundData.cFileName;
        Log.Write('VFS', 'FindNextFileA', 'Result: ' + FoundPath)
      end; // .if
    end else begin
      Windows.SetLastError(Windows.ERROR_NO_MORE_FILES);

      if DebugOpt then begin
        Log.Write('VFS', 'FindNextFileA', 'Error: ERROR_NO_MORE_FILES')
      end; // .if
    end; // .else
  end; // .else

  {!} Windows.LeaveCriticalSection(FileSearchCritSection);
end; // .function Hook_FindNextFileA

function Hook_FindNextFileW (Hook: PatchApi.THiHook; hFindFile: THandle; var lpFindFileData: TWIN32FindDataW): BOOL; stdcall;
var
  FindDataA: TWIN32FindDataA;
  FoundPath: string;

begin
  {!} Windows.EnterCriticalSection(FileSearchCritSection);

  if FileSearchInProgress then begin
    result := BOOL(PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [hFindFile, @lpFindFileData]));
  end else begin
    if DebugOpt then begin
      Log.Write('VFS', 'FindNextFileW', 'Handle: ' + SysUtils.IntToStr(hFindFile));
    end;

    result := Windows.FindNextFileA(hFindFile, FindDataA);

    if not result then begin
      Log.Write('VFS', 'FindNextFileW', 'Error: ERROR_NO_MORE_FILES');
    end else begin
      lpFindFileData.dwFileAttributes := FindDataA.dwFileAttributes;
      lpFindFileData.ftCreationTime   := FindDataA.ftCreationTime;
      lpFindFileData.ftLastAccessTime := FindDataA.ftLastAccessTime;
      lpFindFileData.ftLastWriteTime  := FindDataA.ftLastWriteTime;
      lpFindFileData.nFileSizeHigh    := FindDataA.nFileSizeHigh;
      lpFindFileData.nFileSizeLow     := FindDataA.nFileSizeLow;
      lpFindFileData.dwReserved0      := FindDataA.dwReserved0;
      lpFindFileData.dwReserved1      := FindDataA.dwReserved1;
      StrLib.WideStringToBuf(WideString(FindDataA.cFileName),          @lpFindFileData.cFileName);
      StrLib.WideStringToBuf(WideString(FindDataA.cAlternateFileName), @lpFindFileData.cAlternateFileName);

      if DebugOpt then begin
        FoundPath := FindDataA.cFileName;
        Log.Write('VFS', 'FindNextFileW', 'Result: ' + FoundPath);
      end;
    end; // .else
  end; // .else

  {!} Windows.LeaveCriticalSection(FileSearchCritSection);
end; // .function Hook_FindNextFileW

function Hook_FindClose (Hook: PatchApi.THiHook; hFindFile: THandle): BOOL; stdcall;
begin
  {!} Windows.EnterCriticalSection(FileSearchCritSection);

  if FileSearchInProgress or (hFindFile < 1) or (hFindFile >= MAX_SEARCH_HANDLE) then begin
    {!} Windows.LeaveCriticalSection(FileSearchCritSection);

    result := BOOL(PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [hFindFile]));
  end else begin
    if DebugOpt then begin
      Log.Write('VFS', 'FindClose', 'Handle: ' + SysUtils.IntToStr(hFindFile))
    end; // .if

    result := SearchHandles[Ptr(hFindFile)] <> nil;

    if result then begin
      SearchHandles.DeleteItem(Ptr(hFindFile));
      Windows.SetLastError(Windows.ERROR_SUCCESS);

      if DebugOpt then begin
        Log.Write('VFS', 'FindClose', 'result: ERROR_SUCCESS');
      end; // .if
    end else begin
      Windows.SetLastError(Windows.ERROR_INVALID_HANDLE);

      if DebugOpt then begin
        Log.Write('VFS', 'FindClose', 'result: ERROR_INVALID_HANDLE');
      end; // .if
    end; // .else

    {!} Windows.LeaveCriticalSection(FileSearchCritSection);
  end; // .else
end; // .function Hook_FindClose

function Hook_GetPrivateProfileStringA (Hook: PatchApi.THiHook;
                                        lpAppName, lpKeyName, lpDefault: PAnsiChar;
                                        lpReturnedString: PAnsiChar; nSize: DWORD;
                                        lpFileName: PAnsiChar): DWORD; stdcall;
var
  FilePath:           string;
  RedirectedFilePath: string;
  FinalFilePath:      string;

begin
  FilePath := lpFileName;

  if DebugOpt then begin
    Log.Write('VFS', 'GetPrivateProfileStringA', 'Original: ' + FilePath);
  end; // .if

  // if ini is not found in current directory, we should preserve its original
  // unexpanded form in order kernel to search for ini in system directories
  FinalFilePath := FilePath;
  FilePath := SysUtils.ExpandFileName(FilePath);

  if IsInGameDir(FilePath) then begin
    if FindVFSPath(GameRelativePath(FilePath), RedirectedFilePath) then begin
      FinalFilePath := RedirectedFilePath;
    end else if FileExists(FilePath) then begin
      FinalFilePath := FilePath;
    end; // .ELSEIF
  end; // .if

  if DebugOpt then begin
    Log.Write('VFS', 'GetPrivateProfileStringA', 'Redirected: ' + FinalFilePath);
  end; // .if

  result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc,
                          [lpAppName, lpKeyName, lpDefault, lpReturnedString, nSize,
                           pchar(FinalFilePath)]);
end; // .function Hook_GetPrivateProfileStringA

function Hook_PlaySoundA (Hook: PatchApi.THiHook; pszSound: PAnsiChar; hmod: HMODULE;
                          fdwSound: DWORD): BOOL; stdcall;

const
  SND_NOT_FILE = MMSystem.SND_ALIAS or MMSystem.SND_RESOURCE;

var
  FilePath:           string;
  RedirectedFilePath: string;
  FinalFilePath:      string;

begin
  FilePath := pszSound;

  if DebugOpt then begin
    Log.Write('VFS', 'PlaySoundA', 'Original: ' + FilePath);
  end; // .if

  // if sound is not found in current directory, we should preserve its original
  // unexpanded form in order kernel to search for it in system directories
  FinalFilePath := FilePath;
  FilePath      := SysUtils.ExpandFileName(FilePath);

  // NULL name means stop playing any sound
  if (FinalFilePath <> '') and ((fdwSound and SND_NOT_FILE) = 0) and IsInGameDir(FilePath)
  then begin
    if FindVFSPath(GameRelativePath(FilePath), RedirectedFilePath) then begin
      FinalFilePath := RedirectedFilePath;
    end else if FileExists(FilePath) then begin
      FinalFilePath := FilePath;
    end; // .ELSEIF
  end; // .if

  if DebugOpt then begin
    Log.Write('VFS', 'PlaySoundA', 'Redirected: ' + FinalFilePath);
  end; // .if

  result := BOOL(PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc,
                               [pchar(FinalFilePath), hmod, fdwSound]));
end; // .function Hook_PlaySoundA

function Hook_GetCurrentDirectoryA (Hook: PatchApi.THiHook; nBufferLength: DWORD;
                                    lpBuffer: PAnsiChar): DWORD; stdcall;
var
  FixedCurrDir: string;

begin
  {!} Windows.EnterCriticalSection(CurrDirCritSection);
  FixedCurrDir := CurrentDir;
  {!} Windows.LeaveCriticalSection(CurrDirCritSection);

  result := ORD(Utils.IsValidBuf(lpBuffer, nBufferLength));

  if result <> 0 then begin
    if FixedCurrDir[Length(FixedCurrDir)] = ':' then begin
      FixedCurrDir := FixedCurrDir + '\';
    end; // .if

    result := Length(FixedCurrDir) + 1;

    if (integer(nBufferLength) - 1) >= Length(FixedCurrDir) then begin
      Utils.CopyMem(Length(FixedCurrDir) + 1, pchar(FixedCurrDir), lpBuffer);
    end; // .if

    Windows.SetLastError(Windows.ERROR_SUCCESS);

    if DebugOpt then begin
      Log.Write('VFS', 'GetCurrentDirectoryA', 'Result: ' + FixedCurrDir);
    end; // .if
  end else begin
    Windows.SetLastError(Windows.ERROR_NOT_ENOUGH_MEMORY);

    if DebugOpt then begin
      Log.Write('VFS', 'GetCurrentDirectoryA', 'Error: ERROR_NOT_ENOUGH_MEMORY');
    end; // .if
  end; // .else
end; // .function Hook_GetCurrentDirectoryA

function Hook_SetCurrentDirectoryA (Hook: PatchApi.THiHook; lpPathName: PAnsiChar): BOOL; stdcall;
var
  DirPath:            string;
  RedirectedFilePath: string;
  DirPathLen:         integer;

begin
  DirPath := lpPathName;

  if DebugOpt then begin
    Log.Write('VFS', 'SetCurrentDirectoryA', 'Original: ' + DirPath);
  end;

  DirPath    := SysUtils.ExpandFileName(DirPath);
  DirPathLen := Length(DirPath);

  while (DirPathLen > 0) and (DirPath[DirPathLen] = '\') do begin
    Dec(DirPathLen);
  end;

  SetLength(DirPath, DirPathLen);
  result := DirPath <> '';

  if result then begin
    result := DirExists(DirPath) or (IsInGameDir(DirPath) and
                                     FindVFSPath(GameRelativePath(DirPath),
                                                 RedirectedFilePath) and
                                     DirExists(RedirectedFilePath));

    if result then begin
      {!} Windows.EnterCriticalSection(CurrDirCritSection);
      CurrentDir := DirPath;
      {!} Windows.LeaveCriticalSection(CurrDirCritSection);

      Windows.SetLastError(Windows.ERROR_SUCCESS);
    end;
  end; // .if

  if not result then begin
    Windows.SetLastError(Windows.ERROR_FILE_NOT_FOUND);
  end;

  if DebugOpt then begin
    if result then begin
      Log.Write('VFS', 'SetCurrentDirectoryA', 'Result: ' + DirPath);
    end else begin
      Log.Write('VFS', 'SetCurrentDirectoryA', 'Error: ERROR_FILE_NOT_FOUND');
    end;
  end;
end; // .function Hook_SetCurrentDirectoryA

function MapDir (const VirtPath, RealPath: WideString; OverwriteExisting: boolean; Flags: integer = 0): boolean;
begin
  result := _MapDir(NormalizePath(VirtPath), NormalizePath(RealPath), nil, OverwriteExisting, AUTO_PRIORITY) <> nil;
end;

procedure ResetVfs;
begin
  {!} Windows.EnterCriticalSection(VfsCritSection);

  if VfsIsRunning then begin
    VfsItems.Clear();
    VfsIsRunning := false;
  end;

  {!} Windows.LeaveCriticalSection(VfsCritSection);
end;

procedure InstallVfsHooks;
var
{U} SetProcessDEPPolicyAddr: pointer;
    Kernel32Handle: integer;
    User32Handle:   integer;

begin
  if not VfsHooksInstalled then begin
    Kernel32Handle := Windows.GetModuleHandle('kernel32.dll');
    User32Handle   := Windows.GetModuleHandle('user32.dll');
    FindOutRealSystemApiAddrs([Kernel32Handle, User32Handle]);

    (* Trying to turn off DEP *)
    SetProcessDEPPolicyAddr := Windows.GetProcAddress(Kernel32Handle, 'SetProcessDEPPolicy');

    if SetProcessDEPPolicyAddr <> nil then begin
      if PatchApi.Call(PatchApi.STDCALL_, SetProcessDEPPolicyAddr, [0]) <> 0 then begin
        Log.Write('VFS', 'SetProcessDEPPolicy', 'DEP was turned off');
      end else begin
        Log.Write('VFS', 'SetProcessDEPPolicy', 'Failed to turn DEP off');
      end; // .else
    end; // .if

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetFileAttributesW hook');
    NativeGetFileAttributes := pointer(Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'GetFileAttributesW'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_GetFileAttributesW,
    ).GetDefaultFunc());

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetFileAttributesA hook');
    NativeGetFileAttributes := pointer(Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'GetFileAttributesA'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_GetFileAttributesA,
    ).GetDefaultFunc());

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing CreateFileW hook');
    Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'CreateFileW'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_CreateFileW,
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing CreateFileA hook');
    Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'CreateFileA'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_CreateFileA,
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing OpenFile hook');
    Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'OpenFile'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_OpenFile,
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing LoadLibraryW hook');
    Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'LoadLibraryW'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_LoadLibraryW,
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing LoadLibraryA hook');
    Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'LoadLibraryA'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_LoadLibraryA,
    );

    VfsHooksInstalled := true;
  end; // .if
end; // .procedure InstallVfsHooks

procedure RunVfs (DirListingOrder: TDirListingSortType);
begin
  {!} Windows.EnterCriticalSection(VfsCritSection);

  if not VfsIsRunning then begin
    RebuildVfsItemsTree();
    SortVfsDirListings(DirListingOrder);
    InstallVfsHooks();
    VfsIsRunning := true;
  end;

  {!} Windows.LeaveCriticalSection(VfsCritSection);
end;

procedure AssertHandler (const Mes, FileName: string; LineNumber: integer; Address: pointer);
var
  CrashMes: string;

begin
  CrashMes := StrLib.BuildStr
  (
    'Assert violation in file "~FileName~" on line ~Line~.'#13#10'Error at address: $~Address~.'#13#10'Message: "~Message~"',
    [
      'FileName', FileName,
      'Line',     SysUtils.IntToStr(LineNumber),
      'Address',  SysUtils.Format('%x', [integer(Address)]),
      'Message',  Mes
    ],
    '~'
  );
  Log.Write('Core', 'AssertHandler', CrashMes);
  Core.FatalError(CrashMes);
end; // .procedure AssertHandler

procedure InitModList ({U} aModList: Lists.TStringList);
var
  i: integer;

begin
  {!} Assert(aModList <> nil);
  // * * * * * //
  FreeAndNil(ModList);
  ModList                 := Lists.NewSimpleStrList();
  ModList.CaseInsensitive := true;
  ModList.SetCapacity(aModList.Capacity);

  for i := 0 to aModList.Count - 1 do begin
    ModList.Add(aModList[i]);
  end;

  Log.Write('VFS', 'InitModList', #13#10 + ModList.ToText(#13#10));
end;

procedure Init ({U} aModList: Lists.TStringList);
var
{U} SetProcessDEPPolicyAddr: pointer;

begin
  {!} Assert(aModList <> nil);
  SetProcessDEPPolicyAddr := nil;
  // * * * * * //
  InitModList(aModList);
  //BuildVfsSnapshot();

  Kernel32Handle := Windows.GetModuleHandle('kernel32.dll');
  User32Handle   := Windows.GetModuleHandle('user32.dll');

  DllRealApiAddrs := DataLib.NewObjDict(Utils.OWNS_ITEMS);
  FindOutRealSystemApiAddrs([Kernel32Handle, User32Handle]);

  (* Trying to turn off DEP *)
  SetProcessDEPPolicyAddr := Windows.GetProcAddress(Kernel32Handle, 'SetProcessDEPPolicy');

  if SetProcessDEPPolicyAddr <> nil then begin
    if PatchApi.Call(PatchApi.STDCALL_, SetProcessDEPPolicyAddr, [0]) <> 0 then begin
      Log.Write('VFS', 'Init', 'DEP was turned off');
    end else begin
      Log.Write('VFS', 'Init', 'Failed to turn DEP off');
    end; // .else
  end; // .if

  // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetFullPathNameA hook');
  // Core.p.WriteHiHook
  // (
  //   Windows.GetProcAddress(Kernel32Handle, 'GetFullPathNameA'),
  //   PatchApi.SPLICE_,
  //   PatchApi.EXTENDED_,
  //   PatchApi.STDCALL_,
  //   @Hook_GetFullPathNameA,
  // );

  // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetFullPathNameW hook');
  // Core.p.WriteHiHook
  // (
  //   Windows.GetProcAddress(Kernel32Handle, 'GetFullPathNameW'),
  //   PatchApi.SPLICE_,
  //   PatchApi.EXTENDED_,
  //   PatchApi.STDCALL_,
  //   @Hook_GetFullPathNameW,
  // );

  // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing CreateFileA hook');
  // Core.p.WriteHiHook
  // (
  //   GetRealProcAddress(Kernel32Handle, 'CreateFileA'),
  //   PatchApi.SPLICE_,
  //   PatchApi.EXTENDED_,
  //   PatchApi.STDCALL_,
  //   @Hook_CreateFileA,
  // );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing CreateFileW hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'CreateFileW'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_CreateFileW,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing OpenFile hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'OpenFile'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_OpenFile,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetFileAttributesA hook');
  NativeGetFileAttributes := pointer(Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'GetFileAttributesA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_GetFileAttributesA,
  ).GetDefaultFunc());

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing LoadLibraryA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'LoadLibraryA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_LoadLibraryA,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetPrivateProfileStringA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'GetPrivateProfileStringA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_GetPrivateProfileStringA,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindFirstFileA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'FindFirstFileA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_FindFirstFileA,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindFirstFileW hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'FindFirstFileW'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_FindFirstFileW,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindNextFileA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'FindNextFileA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_FindNextFileA,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindNextFileW hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'FindNextFileW'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_FindNextFileW,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindClose hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'FindClose'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_FindClose,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing LoadCursorFromFileA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(User32Handle, 'LoadCursorFromFileA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_LoadCursorFromFileA,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing PlaySoundA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Windows.LoadLibrary('winmm.dll'), 'PlaySoundA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_PlaySoundA,
  );

  // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetCurrentDirectoryA hook');
  // Core.p.WriteHiHook
  // (
  //   GetRealProcAddress(Kernel32Handle, 'GetCurrentDirectoryA'),
  //   PatchApi.SPLICE_,
  //   PatchApi.EXTENDED_,
  //   PatchApi.STDCALL_,
  //   @Hook_GetCurrentDirectoryA,
  // );

  // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing SetCurrentDirectoryA hook');
  // Core.p.WriteHiHook
  // (
  //   GetRealProcAddress(Kernel32Handle, 'SetCurrentDirectoryA'),
  //   PatchApi.SPLICE_,
  //   PatchApi.EXTENDED_,
  //   PatchApi.STDCALL_,
  //   @Hook_SetCurrentDirectoryA,
  // );
end; // .procedure Init

function String2Hex(const Buffer: Ansistring): string;
 var
   n: Integer;
 begin
   Result := '';
   for n := 1 to Length(Buffer) do
     Result := LowerCase(Result + IntToHex(Ord(Buffer[n]), 2));
 end;

 var
 FileInfo: TWin32FindDataW;
 i: integer;
 s: string;

begin
  Windows.InitializeCriticalSection(CachedPathsCritSection);
  Windows.InitializeCriticalSection(FileSearchCritSection);
  Windows.InitializeCriticalSection(CurrDirCritSection);
  Windows.InitializeCriticalSection(VfsCritSection);

  AssertErrorProc := AssertHandler;

  ModList       := Lists.NewSimpleStrList;
  VfsItems      := DataLib.NewDict(Utils.OWNS_ITEMS, DataLib.CASE_SENSITIVE);
  CachedPaths   := DataLib.NewDict(Utils.OWNS_ITEMS, DataLib.CASE_INSENSITIVE);
  SearchHandles := DataLib.NewObjDict(Utils.OWNS_ITEMS);

  DllRealApiAddrs := DataLib.NewObjDict(Utils.OWNS_ITEMS);

  ResetVfs;
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Phoenix', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\WoG', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\WoG_Native_Dialogs', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Quick Savings', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Secondary Skills Scrolling', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Fast Battle Animation', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Yona', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Dlg_ExpaMon', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Dev', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\BattleHeroes', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Vallex Portraits', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Morn battlefields', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Big Spellbook', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\New Music Pack', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\WoG Rus', DONT_OVERWRITE_EXISTING);
  RunVfs(SORT_FIFO);

  if SysUtils.FileExists('D:\Heroes 3\Data\s\38 wog - first money.erm') then begin
    Files.ReadFileContents('D:\Heroes 3\Data\s\38 wog - first money.erm', s);
  end;
  
  Msg(s);

  s := '';

  with (TVfsItem(VfsItems[PackPath('D:\Heroes 3\Data')]).Children) do begin
    for i := 0 to Count - 1 do begin
      s := s + TVfsItem(Items[i]).RealPath + ' ' + inttostr(TVfsItem(Items[i]).Priority) + #13#10;
    end;
  end;

  Msg(s);

  //Msg(Utils.IfThen(MatchW('tests24523', 't?s*23'), 'Match', 'Not Match'));

  //RedirectFile('D:/heroes 3/h3ERA.exe', 'D:\soft\games\heroes3\era\h3era.exe', OVERWRITE_EXISTING);

  Core.KillThisProcess();
end.
