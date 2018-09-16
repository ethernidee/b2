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
  PatchApi, Core, Ini, WinUtils, Concur, DlgMes;

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

  TDirListing = class
   protected
    {O} FileList: {O} DataLib.TList {OF TFileInfo};
        FileInd:  integer;

   public
    constructor Create;
    destructor  Destroy; override;

    procedure AddItem ({U} FileInfo: Windows.PWin32FindDataW);
    function  GetNextItem: {n} Windows.PWin32FindDataW;
  end; // .class TDirListing

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
{O} SearchHandles: {O} TObjDict {OF hSearch: INTEGER => Value: TDirListing};

  // The value is used for finding free seacrh handle for FindFirstFileA function
  hSearch: integer = 1;

  CachedPathsCritSection: Windows.TRTLCriticalSection;
  FileSearchCritSection:  Concur.TCritSection;
  FileSearchInProgress:   boolean = false;
  CurrDirCritSection:     Windows.TRTLCriticalSection;
  VfsCritSection:         Concur.TCritSection;

  NativeGetFileAttributesW: function (FilePath: PWideChar): integer; stdcall;
  NativeCreateFileW:        function (lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD; lpSecurityAttributes: PSecurityAttributes;
                                      dwCreationDisposition, dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle; stdcall;
  NativeOpenFile:           function (const lpFileName: LPCSTR; var lpReOpenBuff: TOFStruct; uStyle: UINT): THandle; stdcall;
  NativeLoadLibraryW:       function (lpLibFileName: PWideChar): HMODULE; stdcall;
  NativeFindFirstFileExW:   function (lpFileName: PWideChar; fInfoLevelId: TFindexInfoLevels; var lpFindFileData: TWin32FindDataW;
                                      fSearchOp: TFindexSearchOps; lpSearchFilter: Pointer; dwAdditionalFlags: DWORD): THandle; stdcall;
  NativeFindNextFileW:      function (hFindFile: THandle; var lpFindFileData: TWIN32FindDataW): BOOL; stdcall;
  NativeFindClose:          function (hFindFile: THandle): BOOL; stdcall;

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


(***)  implementation  (***)


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
  SearchAutoHandle:    integer = 1;

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

constructor TDirListing.Create;
begin
  Self.FileList  := DataLib.NewList(Utils.OWNS_ITEMS);
  Self.FileInd := 0;
end;

destructor TDirListing.Destroy;
begin
  SysUtils.FreeAndNil(Self.FileList);
end;

procedure TDirListing.AddItem ({U} FileInfo: Windows.PWin32FindDataW);
var
{O} Item: TFileInfo;

begin
  {!} Assert(FileInfo <> nil);
  // * * * * * //
  Item := TFileInfo.Create(FileInfo);
  Self.FileList.Add(Item); Item := nil;
  // * * * * * //
  SysUtils.FreeAndNil(Item);
end;

function TDirListing.GetNextItem: {n} Windows.PWin32FindDataW;
begin
  result := nil;
  // * * * * * //
  if Self.FileInd < Self.FileList.Count then begin
    result := Self.FileList[Self.FileInd];
    Inc(Self.FileInd);
  end;
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

function CompareWideChars (Str1Ptr, Str2Ptr: PWideChar): integer;
var
  Char1: WideChar;
  Char2: WideChar;

begin
  {!} Assert(Str1Ptr <> nil);
  {!} Assert(Str2Ptr <> nil);
  // * * * * * //
  Char1  := #0;
  Char2  := #0;
  result := 0;

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
end; // .function CompareWideChars

function CompareBinStringsW (const Str1, Str2: WideString): integer;
begin
  result := CompareWideChars(PWideChar(Str1), PWideChar(Str2));
end;

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
  Packs WideString bytes into AnsiString buffer.
*)
function WideStrToKey (const Str: WideString): string;
begin
  result := '';

  if Str <> '' then begin
    SetLength(result, Length(Str) * sizeof(Str[1]) div sizeof(result[1]));
    Utils.CopyMem(Length(result) * sizeof(result[1]), PWideChar(Str), PChar(result));
  end;
end;

(*
  Packs lower cased WideString bytes into AnsiString buffer.
*)
function WideStrToCaselessKey (const Str: WideString): string;
var
  ProcessedPath: WideString;

begin
  result := '';

  if Str <> '' then begin
    ProcessedPath := StrLib.WideLowerCase(Str);
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

function _CompareVfsItemsByPriorityDescAndNameAsc (Item1, Item2: integer): integer;
begin
  result := TVfsItem(Item2).Priority - TVfsItem(Item1).Priority;

  if result = 0 then begin
    result := CompareBinStringsW(TVfsItem(Item1).SearchName, TVfsItem(Item2).SearchName);

    if result = 0 then begin
      result := CompareBinStringsW(TVfsItem(Item1).Name, TVfsItem(Item2).Name);
    end;
  end;
end;

function _CompareVfsItemsByPriorityAscAndNameAsc (Item1, Item2: integer): integer;
begin
  result := TVfsItem(Item1).Priority - TVfsItem(Item2).Priority;

  if result = 0 then begin
    result := CompareBinStringsW(TVfsItem(Item1).SearchName, TVfsItem(Item2).SearchName);

    if result = 0 then begin
      result := CompareBinStringsW(TVfsItem(Item1).Name, TVfsItem(Item2).Name);
    end;
  end;
end;

procedure SortVfsListing ({U} List: DataLib.TList {OF TVfsItem}; SortType: TDirListingSortType);
begin
  if SortType = SORT_FIFO then begin
    List.CustomSort(_CompareVfsItemsByPriorityDescAndNameAsc);
  end else begin
    List.CustomSort(_CompareVfsItemsByPriorityAscAndNameAsc);
  end;
end;

function _CompareNativeItemsByAndNameAsc (Item1, Item2: integer): integer;
begin
  result := CompareWideChars(PWideChar(TVfsItem(Item1).SearchName), PWideChar(TVfsItem(Item2).SearchName));

  if result = 0 then begin
    result := CompareWideChars(@TVfsItem(Item1).Info.cFileName, @TVfsItem(Item2).Info.cFileName);
  end;
end;

procedure SortNativeDirListing ({U} List: DataLib.TList {OF TVfsItem});
begin
  List.CustomSort(_CompareNativeItemsByAndNameAsc);
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
      DirVfsItem := VfsItems[WideStrToCaselessKey(AbsDirPath)];

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
  result := VfsItems[WideStrToCaselessKey(AbsPath)];
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
  PackedVirtPath := WideStrToCaselessKey(AbsVirtPath);
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

  with VfsCritSection do begin
    Enter;

    if VfsIsRunning then begin
      VfsItem := VfsItems[WideStrToCaselessKey(AbsVirtPath)];

      if VfsItem <> nil then begin
        result := VfsItem.RealPath;

        if FileInfo <> nil then begin
          FileInfo^ := VfsItem.Info;
        end;
      end;
    end; // .if

    Leave;
  end; // .with
end; // .function GetVfsItemRealPath

function AllocSearchHandle ({U} var {OUT} DirListing: TDirListing): integer;
var
  SearchStartHandle: integer;

begin
  SearchStartHandle := SearchAutoHandle;
  result            := Windows.INVALID_HANDLE_VALUE;

  while (SearchAutoHandle <= MAX_SEARCH_HANDLE) and (SearchHandles[Ptr(SearchAutoHandle)] <> nil) do begin
    Inc(SearchAutoHandle);
  end;

  if SearchAutoHandle <= MAX_SEARCH_HANDLE then begin
    result := SearchAutoHandle;
  end else begin
    SearchAutoHandle := 1;

    while (SearchAutoHandle < SearchStartHandle) and (SearchHandles[Ptr(SearchAutoHandle)] <> nil) do begin
      Inc(SearchAutoHandle);
    end;

    if SearchAutoHandle < SearchStartHandle then begin
      result := SearchAutoHandle;
    end;
  end; // .else

  if result <> Windows.INVALID_HANDLE_VALUE then begin
    DirListing := TDirListing.Create();
    SearchHandles[Ptr(result)] := DirListing;
  end;
end; // .function AllocSearchHandle

function ReleaseSearchHandle (hSearch: integer): boolean;
begin
  result := SearchHandles.DeleteItem(Ptr(hSearch));
end;

function GetSearchData (SearchHandle: integer): {n} TDirListing;
begin
  result := TDirListing(SearchHandles[Ptr(SearchHandle)]);
end;

procedure RealScanDir (const FullPath: WideString; Exclude: TDict {OF not NIL}; DirListing: TDirListing);
var
{O} Items:     {O} TList {OF TVfsItem};
{O} Item:      {O} TVfsItem;
    hSearch:   THandle;
    i:         integer;

begin
  Items := DataLib.NewList(Utils.OWNS_ITEMS);
  Item  := TVfsItem.Create;
  // * * * * * //
  hSearch := NativeFindFirstFileExW(PWideChar(FullPath), Windows.FindExInfoStandard, Item.Info, Windows.FindExSearchNameMatch, nil, 0);

  if hSearch <> Windows.INVALID_HANDLE_VALUE then begin
    repeat
      Item.SearchName := StrLib.WideLowerCase(Item.Info.cFileName);

      if Exclude[WideStrToKey(Item.SearchName)] = nil then begin
        Items.Add(Item); Item := nil;
        Item := TVfsItem.Create;
      end;
    until not NativeFindNextFileW(hSearch, Item.Info);

    NativeFindClose(hSearch);
  end;

  SortNativeDirListing(Items);

  for i := 0 to Items.Count - 1 do begin
    DirListing.AddItem(@Item.Info);
  end;
  // * * * * * //
  SysUtils.FreeAndNil(Items);
  SysUtils.FreeAndNil(Item);
end; // .procedure RealScanDir

procedure ConvertWin32FindDataToAnsi (WideData: Windows.PWin32FindDataW; AnsiData: Windows.PWin32FindDataA);
const
  MAX_FILENAME_LEN     = Windows.MAX_PATH - 1;
  MAX_ALT_FILENAME_LEN = 14 - 1;

var
  FileName:    string;
  AltFileName: string;

begin
  {!} Assert(WideData <> nil);
  {!} Assert(AnsiData <> nil);
  // * * * * * //
  AnsiData.dwFileAttributes := WideData.dwFileAttributes;
  AnsiData.ftCreationTime   := WideData.ftCreationTime;
  AnsiData.ftLastAccessTime := WideData.ftLastAccessTime;
  AnsiData.ftLastWriteTime  := WideData.ftLastWriteTime;
  AnsiData.nFileSizeHigh    := WideData.nFileSizeHigh;
  AnsiData.nFileSizeLow     := WideData.nFileSizeLow;
  AnsiData.dwReserved0      := WideData.dwReserved0;
  AnsiData.dwReserved1      := WideData.dwReserved1;

  StrLib.PWideCharToAnsi(@WideData.cFileName, FileName);
  StrLib.PWideCharToAnsi(@WideData.cAlternateFileName, AltFileName);

  if Length(FileName) > MAX_FILENAME_LEN then begin
    SetLength(FileName, MAX_FILENAME_LEN);
  end;
  
  if Length(AltFileName) > MAX_ALT_FILENAME_LEN then begin
    SetLength(AltFileName, MAX_ALT_FILENAME_LEN);
  end;
  
  Utils.CopyMem(Length(FileName) + 1, pchar(FileName), @AnsiData.cFileName);
  Utils.CopyMem(Length(AltFileName) + 1, pchar(AltFileName), @AnsiData.cAlternateFileName);
end; // .procedure ConvertWin32FindDataToAnsi

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

function Hook_FindFirstFileExW (Hook: PatchApi.THiHook; lpFileName: PWideChar; fInfoLevelId: TFindexInfoLevels; var lpFindFileData: TWin32FindDataW;
                                fSearchOp: TFindexSearchOps; lpSearchFilter: Pointer; dwAdditionalFlags: DWORD): THandle; stdcall;
var
{Un} DirVfsItem:       TVfsItem;
{O}  AddedVfsItems:    DataLib.TDict {OF 1};
{U}  ItemFromVfs:      TVfsItem;
{U}  DirListing:       TDirListing;
{U}  FirstResult:      Windows.PWin32FindDataW;
     IsStandardSearch: boolean;
     FullPath:         WideString;
     VirtDirPath:      WideString;
     SearchPattern:    WideString;
     OrigPathA:        string;
     FirstResultNameA: string;
     i:                integer;

begin
  DirVfsItem    := nil;
  AddedVfsItems := nil;
  ItemFromVfs   := nil;
  DirListing    := nil;
  FirstResult   := nil;
  // * * * * * //
  with FileSearchCritSection do begin
    Enter;

    IsStandardSearch := (fInfoLevelId = Windows.FindExInfoStandard) and (fSearchOp = Windows.FindExSearchNameMatch) and (lpSearchFilter = nil) and (dwAdditionalFlags = 0);

    if FileSearchInProgress or not IsStandardSearch then begin
      result := NativeFindFirstFileExW(lpFileName, fInfoLevelId, lpFindFileData, fSearchOp, lpSearchFilter, dwAdditionalFlags);
    end else begin
      FullPath := ExpandPath(lpFileName);

      if DebugOpt then begin
        StrLib.PWideCharToAnsi(lpFileName, OrigPathA);
      end;

      // Disallow empty or '\'-ending paths
      if (FullPath = '') or (FullPath[Length(FullPath)] = '\') then begin
        if DebugOpt then begin
          Log.Write('VFS', 'FindFirstFileExW', 'Path: "' + OrigPathA + '". Result: INVALID_HANDLE_VALUE');
        end;

        Windows.SetLastError(Windows.ERROR_INVALID_PARAMETER);
        result := Windows.INVALID_HANDLE_VALUE;
      end
      // Non-empty path, dividable into directory path and pattern
      else begin
        AddedVfsItems := DataLib.NewDict(not Utils.OWNS_ITEMS, DataLib.CASE_SENSITIVE);
        result        := AllocSearchHandle(DirListing);

        if result <> Windows.INVALID_HANDLE_VALUE then begin
          with VfsCritSection do begin
            Enter;

            DirVfsItem := VfsItems[NormalizePath(FullPath)];

            // Request for directory itself, record is found in VFS
            if DirVfsItem <> nil then begin
              DirListing.AddItem(@DirVfsItem.Info);
            end
            // Request with parent directory and files pattern
            else begin
              VirtDirPath   := StrLib.ExtractDirPathW(FullPath);
              SearchPattern := StrLib.WideLowerCase(StrLib.ExtractFileNameW(FullPath));
              DirVfsItem    := VfsItems[NormalizePath(VirtDirPath)];

              // Found VFS directory, copy its items, that match pattern and remember copied names
              if (DirVfsItem <> nil) and (DirVfsItem.Children <> nil) then begin
                for i := 0 to DirVfsItem.Children.Count - 1 do begin
                  ItemFromVfs := TVfsItem(DirVfsItem.Children[i]);

                  if StrLib.MatchW(ItemFromVfs.SearchName, SearchPattern) then begin
                    AddedVfsItems[WideStrToCaselessKey(ItemFromVfs.SearchName)] := Ptr(1);
                    DirListing.AddItem(@ItemFromVfs.Info);
                  end;
                end;
              end;

              FileSearchInProgress := true;

              // Scan real directory
              RealScanDir(FullPath, AddedVfsItems, DirListing);

              FileSearchInProgress := false;
            end; // .else

            Leave;
          end; // .with VfsCritSection

          FirstResult := DirListing.GetNextItem();

          if FirstResult = nil then begin
            Windows.SetLastError(Windows.ERROR_NO_MORE_FILES);
            ReleaseSearchHandle(result);
            result := Windows.INVALID_HANDLE_VALUE;
          end else begin
            lpFindFileData := FirstResult^;
          end;
        end; // .if
      end; // .else

      if DebugOpt then begin
        if result = Windows.INVALID_HANDLE_VALUE then begin
          Log.Write('VFS', 'FindFirstFileExW', 'Path: "' + OrigPathA + '". Result: INVALID_HANDLE_VALUE');
        end else begin
          StrLib.PWideCharToAnsi(lpFindFileData.cFileName, FirstResultNameA);
          Log.Write('VFS', 'FindFirstFileExW', 'Path: "' + OrigPathA + '". Result: ' + SysUtils.IntToStr(integer(result)) + '. Found: "' + FirstResultNameA + '"');
        end;
      end;
    end; // .else

    Leave;
  end; // .with FileSearchCritSection
end; // .function Hook_FindFirstFileExW

function Hook_FindFirstFileExA (Hook: PatchApi.THiHook; lpFileName: PAnsiChar; fInfoLevelId: TFindexInfoLevels; var lpFindFileData: TWIN32FindDataA;
                                fSearchOp: TFindexSearchOps; lpSearchFilter: Pointer; dwAdditionalFlags: DWORD): THandle; stdcall;
var
  FileInfo: Windows.TWin32FindDataW;

begin
  result := THandle(Windows.FindFirstFileExW(PWideChar(WideString(String(lpFileName))), fInfoLevelId, @FileInfo, fSearchOp, lpSearchFilter, dwAdditionalFlags));

  if result <> Windows.INVALID_HANDLE_VALUE then begin
    ConvertWin32FindDataToAnsi(@FileInfo, @lpFindFileData);
  end;
end;

function Hook_FindFirstFileW (Hook: PatchApi.THiHook; lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
begin
  result := THandle(Windows.FindFirstFileExW(lpFileName, Windows.FindExInfoStandard, @lpFindFileData, Windows.FindExSearchNameMatch, nil, 0));
end;

function Hook_FindFirstFileA (Hook: PatchApi.THiHook; lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindDataA): THandle; stdcall;
var
  FileInfo: Windows.TWin32FindDataW;

begin
  result := THandle(Windows.FindFirstFileExW(PWideChar(WideString(String(lpFileName))), Windows.FindExInfoStandard, @FileInfo, Windows.FindExSearchNameMatch, nil, 0));

  if result <> Windows.INVALID_HANDLE_VALUE then begin
    ConvertWin32FindDataToAnsi(@FileInfo, @lpFindFileData);
  end;
end;

function Hook_FindNextFileW (Hook: PatchApi.THiHook; hFindFile: THandle; var lpFindFileData: TWIN32FindDataW): BOOL; stdcall;
var
{Un} DirListing: TDirListing;
{Un} FileInfo:   Windows.PWin32FindDataW;
     FileNameA:  string;
     Status:     integer;
     ErrorName:  string;

begin
  DirListing := nil;
  FileInfo   := nil;
  // * * * * * //
  with FileSearchCritSection do begin
    Enter;

    // Real search or real/invalid handle (pointer)
    if FileSearchInProgress or (hFindFile <= 0) or (hFindFile > MAX_SEARCH_HANDLE) then begin
      result := NativeFindNextFileW(hFindFile, lpFindFileData);
    end
    // Virtual search handle
    else begin
      result     := false;
      Status     := Windows.ERROR_SUCCESS;
      DirListing := GetSearchData(integer(hFindFile));

      if DirListing = nil then begin
        Windows.SetLastError(Windows.ERROR_INVALID_HANDLE);
        Status := ERROR_INVALID_HANDLE;
      end else begin
        FileInfo := DirListing.GetNextItem();

        if FileInfo = nil then begin
          Windows.SetLastError(Windows.ERROR_NO_MORE_FILES);
          Status := Windows.ERROR_NO_MORE_FILES;
        end else begin
          Windows.SetLastError(Windows.ERROR_SUCCESS);
          lpFindFileData := FileInfo^;
          result         := true;
        end;
      end; // .else

      if DebugOpt then begin
        if result then begin
          StrLib.PWideCharToAnsi(@FileInfo.cFileName, FileNameA);
          Log.Write('VFS', 'FindNextFileW', SysUtils.Format('Handle: %x. Result: "%s"', [integer(hFindFile), FileNameA]));
        end else begin
          case Status of
            Windows.ERROR_INVALID_HANDLE: ErrorName := 'ERROR_INVALID_HANDLE';
            Windows.ERROR_NO_MORE_FILES:  ErrorName := 'ERROR_NO_MORE_FILES';
          else
            ErrorName := 'UNKNOWN_ERROR';
          end;
          
          Log.Write('VFS', 'FindNextFileW', SysUtils.Format('Handle: %x. Result: %s', [integer(hFindFile), ErrorName]));
        end; // .else
      end; // .if
    end; // .else

    Leave;
  end; // .with FileSearchCritSection
end; // .function Hook_FindNextFileW

function Hook_FindNextFileA (Hook: PatchApi.THiHook; hFindFile: THandle; var lpFindFileData: TWIN32FindDataA): BOOL; stdcall;
var
  FileInfo: Windows.TWin32FindDataW;

begin
  result := Windows.FindNextFileW(hFindFile, FileInfo);

  if result then begin
    ConvertWin32FindDataToAnsi(@FileInfo, @lpFindFileData);
  end;
end;

function Hook_FindClose (Hook: PatchApi.THiHook; hFindFile: THandle): BOOL; stdcall;
begin
  with FileSearchCritSection do begin
    Enter;

    // Native search or native/invalid handle
    if FileSearchInProgress or (hFindFile <= 0) or (hFindFile > MAX_SEARCH_HANDLE) then begin
      result := NativeFindClose(hFindFile);
    end
    // Closing VFS search handle
    else begin
      result := ReleaseSearchHandle(integer(hFindFile));
      Windows.SetLastError(Utils.IfThen(result, Windows.ERROR_SUCCESS, Windows.ERROR_INVALID_HANDLE));

      if DebugOpt then begin
        Log.Write('VFS', 'FindClose', Format('Handle: . Result: ', [integer(hFindFile), Utils.IfThen(result, 'ERROR_SUCCESS', 'ERROR_INVALID_HANDLE')]));
      end;
    end;

    Leave;
  end; // .with FileSearchCritSection;
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
  if (FinalFilePath <> '') and ((fdwSound and SND_NOT_FILE) = 0)
  then begin
    
  end; // .if

  if DebugOpt then begin
    Log.Write('VFS', 'PlaySoundA', 'Redirected: ' + FinalFilePath);
  end; // .if

  result := BOOL(PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc,
                               [pchar(FinalFilePath), hmod, fdwSound]));
end; // .function Hook_PlaySoundA

function MapDir (const VirtPath, RealPath: WideString; OverwriteExisting: boolean; Flags: integer = 0): boolean;
begin
  result := _MapDir(NormalizePath(VirtPath), NormalizePath(RealPath), nil, OverwriteExisting, AUTO_PRIORITY) <> nil;
end;

procedure ResetVfs;
begin
  with VfsCritSection do begin
    Enter;

    if VfsIsRunning then begin
      VfsItems.Clear();
      VfsIsRunning := false;
    end;

    Leave;
  end;
end; // .procedure ResetVfs

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
    NativeGetFileAttributesW := pointer(Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'GetFileAttributesW'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_GetFileAttributesW,
    ).GetDefaultFunc());

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetFileAttributesA hook');
    Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'GetFileAttributesA'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_GetFileAttributesA,
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing CreateFileW hook');
    NativeCreateFileW := pointer(Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'CreateFileW'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_CreateFileW,
    ).GetDefaultFunc());

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
    NativeOpenFile := pointer(Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'OpenFile'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_OpenFile,
    ).GetDefaultFunc());

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing LoadLibraryW hook');
    NativeLoadLibraryW := pointer(Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'LoadLibraryW'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_LoadLibraryW,
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

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindFirstFileExW hook');
    NativeFindFirstFileExW := pointer(Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'FindFirstFileExW'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_FindFirstFileExW,
    ).GetDefaultFunc());

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindFirstFileExA hook');
    Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'FindFirstFileA'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_FindFirstFileExA,
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

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindFirstFileA hook');
    Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'FindFirstFileA'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_FindFirstFileA,
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindNextFileW hook');
    NativeFindNextFileW := pointer(Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'FindNextFileW'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_FindNextFileW,
    ).GetDefaultFunc());

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindNextFileA hook');
    Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'FindNextFileA'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_FindNextFileA,
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindClose hook');
    NativeFindClose := pointer(Core.p.WriteHiHook
    (
      GetRealProcAddress(Kernel32Handle, 'FindClose'),
      PatchApi.SPLICE_,
      PatchApi.EXTENDED_,
      PatchApi.STDCALL_,
      @Hook_FindClose,
    ).GetDefaultFunc());

    VfsHooksInstalled := true;
  end; // .if
end; // .procedure InstallVfsHooks

procedure RunVfs (DirListingOrder: TDirListingSortType);
begin
  with VfsCritSection do begin
    Enter;

    if not VfsIsRunning then begin
      RebuildVfsItemsTree();
      SortVfsDirListings(DirListingOrder);
      InstallVfsHooks();
      VfsIsRunning := true;
    end;

    Leave;
  end; // .with VfsCritSection
end; // .procedure RunVfs

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

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetPrivateProfileStringA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'GetPrivateProfileStringA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_GetPrivateProfileStringA,
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
  FileSearchCritSection.Init;
  VfsCritSection.Init;

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

  with (TVfsItem(VfsItems[WideStrToCaselessKey('D:\Heroes 3\Data')]).Children) do begin
    for i := 0 to Count - 1 do begin
      s := s + TVfsItem(Items[i]).RealPath + ' ' + inttostr(TVfsItem(Items[i]).Priority) + #13#10;
    end;
  end;

  Msg(s);

  //Msg(Utils.IfThen(MatchW('tests24523', 't?s*23'), 'Match', 'Not Match'));

  //RedirectFile('D:/heroes 3/h3ERA.exe', 'D:\soft\games\heroes3\era\h3era.exe', OVERWRITE_EXISTING);

  Core.KillThisProcess();
end.
