unit VFS;
(*
  DESCRIPTION:  Virtual File System
  AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)

  Allows to virtually 'unpack' contents of many directories to existing or non-existing paths.
  Example: map D:\Mods\A and D:\Mods\B to C:\Game.
  Example: map D:\Archive\2018 to D:\NonExistingReallyDir
  Real files/subdirs are not overwritten, but the effect is the same as real directory to directory (deep) copying.

  WinApi users WILL BE able to:
  - open mapped files for reading/writing/truncating.
  - query mapped file attributes by path/name.
  - list mapped directories (contents will be combined/overlapped).
  - change current directory to virtual directory, that does not actually exist on hard drive.
  - call most system api like LoadLibrary and PlaySound with virtual paths.

  WinApi users WILL NOT be able to:
  - open mapped files in OPEN_OR_CREATE mode.
  - open mapped files/dirs for deletion/renaming.

  Known limitations:
  - it's not possible to map directories to drive roots (C:, D:, etc).
  - short DOS names are not generated and listed for virtual items (see FindFirstFile).
  - Unique 16 byte object IDs are returned as 0 on listing '.' and '..' directory items.

  TODO!!!!!!!!!!!!!!!!
  !!! Recheck file info structures sizes and structure of all types: https://msdn.microsoft.com/en-us/library/cc232070.aspx

  Intercepted WinApi functions:
  - LoadLibraryW (for debuggers support)
  - LoadLibraryA (for debuggers support)
  - FindFirstFileExW
  - FindFirstFileA
  - FindFirstFileW
  - FindFirstFileA
  - FindNextFileW
  - FindNextFileA
  - FindClose
  - SetCurrentDirectoryW
  - SetCurrentDirectoryA
  - NtQueryAttributesFile
  - NtQueryFullAttributesFile
  - NtOpenFile
  - NtCreateFile

  Possible VFS item attributes to implement in the future:
  - DONT_SHOW_IN_LISTINGS. Virtual file/directory will not be shown in directory listings (only accessible by direct path).
  - IGNORE_REAL_LISTING.   During directory scan, real files will be ignored.
*)

(***)  interface  (***)
uses
  Windows, SysUtils, Math,
  Utils, WinNative, Alg, Lists, DataLib, StrLib, Files, Log, TypeWrappers,
  PatchApi, Core, Ini, WinUtils, Concur, PatchForge, hde32, VfsPatching, {ApiJack, }DlgMes,
  {DELETEME}FilesEx;

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

type
  (* Import *)
  TDict    = DataLib.TDict;
  TObjDict = DataLib.TObjDict;
  TString  = TypeWrappers.TString;
  TList    = Lists.TList;

const
  CMDLINE_ARG_MODLIST = 'modlist';

  OVERWRITE_EXISTING      = true;
  DONT_OVERWRITE_EXISTING = false;

type
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
  CurrDirCritSection:     Concur.TCritSection;
  VfsCritSection:         Concur.TCritSection;

  NativeGetFileAttributesW:        function (FilePath: PWideChar): integer; stdcall;
  NativeCreateFileW:               function (lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD; lpSecurityAttributes: PSecurityAttributes;
                                             dwCreationDisposition, dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle; stdcall;
  NativeOpenFile:                  function (const lpFileName: LPCSTR; var lpReOpenBuff: TOFStruct; uStyle: UINT): THandle; stdcall;
  NativeLoadLibraryW:              function (lpLibFileName: PWideChar): HMODULE; stdcall;
  NativeFindFirstFileExW:          function (lpFileName: PWideChar; fInfoLevelId: TFindexInfoLevels; var lpFindFileData: TWin32FindDataW;
                                             fSearchOp: TFindexSearchOps; lpSearchFilter: Pointer; dwAdditionalFlags: DWORD): THandle; stdcall;
  NativeFindNextFileW:             function (hFindFile: THandle; var lpFindFileData: TWIN32FindDataW): BOOL; stdcall;
  NativeFindClose:                 function (hFindFile: THandle): BOOL; stdcall;
  NativeSetCurrentDirectoryW:      function (lpPathName: LPWSTR): BOOL; stdcall;
  NativeNtOpenFile:                WinNative.TNtOpenFile;
  NativeNtCreateFile:              WinNative.TNtCreateFile;
  NativeNtClose:                   WinNative.TNtClose;
  NativeNtQueryDirectoryFile:      WinNative.TNtQueryDirectoryFile;
  NativeNtQueryAttributesFile:     WinNative.TNtQueryAttributesFile;
  NativeNtQueryFullAttributesFile: WinNative.TNtQueryFullAttributesFile;
  NativeNtDeleteFile:              WinNative.TNtDeleteFile;

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
  MAX_FILENAME_SIZE               = WinNative.MAX_FILENAME_LEN * sizeof(WideChar);
  DRIVE_CHAR_INDEX_IN_NT_ABS_PATH = 5; // \??\D:
  MAX_SEARCH_HANDLE               = 1000;
  VFS_EXTRA_DEBUG                 = false; // FIXME? Any need?

  AUTO_PRIORITY                = MAXLONGINT div 2;
  INITIAL_OVERWRITING_PRIORITY = AUTO_PRIORITY + 1;
  INITIAL_ADDING_PRIORITY      = AUTO_PRIORITY - 1;

type
  PNativeFileInfo = ^TNativeFileInfo;
  TNativeFileInfo = record
    Base:     WinNative.FILE_ID_BOTH_DIR_INFORMATION;
    FileName: WideString;

    procedure SetFileName (const NewFileName: WideString);
    function  CopyFileNameToBuf ({ni} Buf: pbyte; BufSize: integer): boolean;
  end;

  TFileInfo = class
   public
    Data: TNativeFileInfo;

    constructor Create ({n} Data: PNativeFileInfo = nil);
  end;

  // FIXME: DEPRECATED, NOTUSED
  TSearchList = class
    {O} FileList: {O} Lists.TStringList {OF Windows.PWin32FindData};
        FileInd:  integer;

    constructor Create;
    destructor  Destroy; override;
  end; // .class TSearchList

  TDirListing = class
   private
    {O} fFileList: {O} DataLib.TList {OF TFileInfo};
        fFileInd:  integer;

    function GetCount: integer;

   public
    constructor Create;
    destructor  Destroy; override;

    function  IsEnd: boolean;
    procedure AddItem ({U} FileInfo: PNativeFileInfo; const FileName: WideString = ''; const InsertBefore: integer = High(integer));
    function  GetNextItem ({OUT} var Res: TFileInfo): boolean;
    procedure Rewind;

    (* Always seeks as close as possible *)
    function Seek (SeekInd: integer): boolean;
    function SeekRel (RelInd: integer): boolean;

    property FileInd: integer read fFileInd;
    property Count: integer read GetCount;
  end; // .class TDirListing

  TVfsItem = class
   private
    function  GetName: WideString; inline;
    procedure SetName (const NewName: WideString); inline;

   public
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
    Info: TNativeFileInfo;

    function IsDir (): boolean;

    destructor Destroy; override;

    (* Name in original case. Automatically sets/converts SearchName, Info.FileName,  Info.Base.FileNameLength *)
    property Name: WideString read GetName write SetName;
  end; // .class TVfsItem

  (* Meta-information for file handle *)
  TOpenedFile = class
   public
    (* Handle for either virtual or real path *)
    hFile: Windows.THandle;

    (* Formal or virtual path to file *)
    AbsPath: WideString;

    (* Directory listing (both real and virtual children). Created on the fly on FillDirListing call *)
    {On} DirListing: TDirListing;

    constructor Create (hFile: Windows.THandle; const AbsPath: WideString);
    destructor Destroy; override;

    (* Makes complete directory listing, including real and virtual items. Does nothing if listing already exists.
       Can be called only for redirected items. Returns true on non-nil listing. *)
    procedure FillDirListing (const Mask: WideString);
  end;

  ISysDirScanner = interface
    function IterNext ({OUT} var FileName: WideString; {n} FileInfo: WinNative.PFILE_ID_BOTH_DIR_INFORMATION = nil): boolean;
  end;

  TSysDirScanner = class (Utils.TManagedObject, ISysDirScanner)
   protected const
     BUF_SIZE = (sizeof(WinNative.FILE_ID_BOTH_DIR_INFORMATION) + MAX_FILENAME_SIZE) * 10;

   protected
    fOwnsDirHandle: boolean;
    fDirHandle:     Windows.THandle;
    fMask:          WideString;
    fMaskU:         WinNative.UNICODE_STRING;
    fIsStart:       boolean;
    fIsEnd:         boolean;
    fBufPos:        integer;
    fBuf:           array [0..BUF_SIZE - 1] of byte;

   public
    constructor Create (const hDir: Windows.THandle; const Mask: WideString); overload;
    constructor Create (const DirPath, Mask: WideString); overload;
    destructor Destroy; override;
    
    function IterNext ({OUT} var FileName: WideString; {n} FileInfo: WinNative.PFILE_ID_BOTH_DIR_INFORMATION = nil): boolean;
  end; // .class TSysDirScanner

// Will it become zero during finalization?
threadvar
  DisableVfsForThisThread: boolean;

var
(* Map of DLL handle => API name => Real api address *)
{O} DllRealApiAddrs: {O} TObjDict {OF TDict};

{O} VfsItems:    {O} TDict {OF TVfsItem};
{O} OpenedFiles: {O} TObjDict {OF TOpenedFile};

  VfsHooksInstalled:   boolean = false;
  VfsIsRunning:        boolean = false;
  OverwritingPriority: integer = INITIAL_OVERWRITING_PRIORITY;
  AddingPriority:      integer = INITIAL_ADDING_PRIORITY;
  SearchAutoHandle:    integer = 1;

  CurrDirIsVirtual: boolean = false;
  CurrDirVirtPath:  WideString;
  CurrDirRealLoweredPath:  WideString;

procedure TNativeFileInfo.SetFileName (const NewFileName: WideString);
begin
  Self.FileName            := NewFileName;
  Self.Base.FileNameLength := Length(NewFileName) * sizeof(WideChar);
end;

function TNativeFileInfo.CopyFileNameToBuf ({ni} Buf: pbyte; BufSize: integer): boolean;
begin
  {!} Assert(Utils.IsValidBuf(Buf, BufSize));
  result := integer(Self.Base.FileNameLength) <= BufSize;

  if BufSize > 0 then begin
    Utils.CopyMem(Self.Base.FileNameLength, PWideChar(Self.FileName), Buf);
  end;
end;

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
  Self.fFileList := DataLib.NewList(Utils.OWNS_ITEMS);
  Self.fFileInd  := 0;
end;

destructor TDirListing.Destroy;
begin
  SysUtils.FreeAndNil(Self.fFileList);
end;

procedure TDirListing.AddItem (FileInfo: PNativeFileInfo; const FileName: WideString = ''; const InsertBefore: integer = High(integer));
var
{O} Item: TFileInfo;

begin
  {!} Assert(FileInfo <> nil);
  // * * * * * //
  Item := TFileInfo.Create(FileInfo);

  if FileName <> '' then begin
    Item.Data.SetFileName(FileName);
  end;

  if InsertBefore >= Self.fFileList.Count then begin
    Self.fFileList.Add(Item); Item := nil;
  end else begin
    Self.fFileList.Insert(Item, InsertBefore); Item := nil;
  end;  
  // * * * * * //
  SysUtils.FreeAndNil(Item);
end; // .procedure TDirListing.AddItem

function TDirListing.GetCount: integer;
begin
  result := Self.fFileList.Count;
end;

function TDirListing.IsEnd: boolean;
begin
  result := Self.fFileInd >= Self.fFileList.Count;
end;

function TDirListing.GetNextItem ({OUT} var Res: TFileInfo): boolean;
begin
  result := Self.fFileInd < Self.fFileList.Count;

  if result then begin
    Res := TFileInfo(Self.fFileList[Self.fFileInd]);
    Inc(Self.fFileInd);
  end;
end;

procedure TDirListing.Rewind;
begin
  Self.fFileInd := 0;
end;

function TDirListing.Seek (SeekInd: integer): boolean;
begin
  Self.fFileInd := Alg.ToRange(SeekInd, 0, Self.fFileList.Count - 1);
  result        := Self.fFileInd = SeekInd;
end;

function TDirListing.SeekRel (RelInd: integer): boolean;
begin
  result := Self.Seek(Self.fFileInd + RelInd);    
end;

function TVfsItem.IsDir: boolean;
begin
  result := (Self.Info.Base.FileAttributes and Windows.FILE_ATTRIBUTE_DIRECTORY) <> 0;
end;

function TVfsItem.GetName: WideString;
begin
  result := Self.Info.FileName;
end;

procedure TVfsItem.SetName (const NewName: WideString);
begin
  Self.Info.SetFileName(NewName);
  Self.SearchName := StrLib.WideLowerCase(NewName);
end;  

destructor TVfsItem.Destroy;
begin
  SysUtils.FreeAndNil(Self.Children);
end;

constructor TFileInfo.Create ({n} Data: PNativeFileInfo = nil);
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
        end;

        Inc(ImportDir);
      end; // .while
    end; // .if
  end; // .for

  //Files.WriteFileContents(s, 'D:\__LOG.txt');
  //(Format('%p', [ TDict( DllRealApiAddrs[Ptr(DllHandles[0])] ) ['CreateFileA'] ]));
end; // .procedure FindOutRealSystemApiAddrs

(* Returns real code address, bypassing possibly nested simple redirection stubs like JMP [...] or JMP XXX. *)
function GetRealAddress (CodeOrRedirStub: pointer): {n} pointer;
const
 MAX_DEPTH = 100;

var
  Depth: integer;

begin
  {!} Assert(CodeOrRedirStub <> nil);
  result := CodeOrRedirStub;
  Depth  := 0;

  while Depth < MAX_DEPTH do begin
    // JMP DWORD [PTR]
    if pword(result)^ = PatchForge.OPCODE_JMP_PTR_CONST32 then begin
      result := ppointer(integer(result) + sizeof(word))^;
    // JXX SHORT CONST8
    end else if PatchForge.IsShortJumpConst8Opcode(pbyte(result)^) then begin
      result := pointer(integer(result) + sizeof(byte) + pshortint(integer(result) + sizeof(byte))^);
    // JMP NEAR CONST32
    end else if pbyte(result)^ = PatchForge.OPCODE_JMP_CONST32 then begin
      result := pointer(integer(result) + sizeof(PatchForge.TJumpCall32Rec) + pinteger(integer(result) + sizeof(byte))^);
    // JXX (conditional) NEAR CONST32
    end else if PatchForge.IsNearJumpConst32Opcode(pword(result)^) then begin
      result := pointer(integer(result) + sizeof(word) + sizeof(integer) + pinteger(integer(result) + sizeof(word))^);
    // Regular code
    end else begin
      break;
    end; // .else

    Inc(Depth);
  end; // .while
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

function IsDir (const Path: WideString): boolean;
var
  ItemAttrs: integer;

begin
  ItemAttrs := Windows.GetFileAttributesW(PWideChar(Path));
  result    := (ItemAttrs <> WinNative.INVALID_FILE_ATTRIBUTES) and Utils.HasFlag(ItemAttrs, Windows.FILE_ATTRIBUTE_DIRECTORY);
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

(* Returns expanded path without trailing delimiter. Optionally returns flag, whether path had trailing delim or not. *)
function NormalizePath (const Path: WideString; {n} HadTrailingDelim: pboolean = nil): WideString;
begin
  result := StrLib.ExcludeTrailingDelimW(ExpandPath(Path), HadTrailingDelim);
end;

(* Returns absolute expanded path without trailing delimiter and with nt path prefix '\??\' (unless
   path already begins with '\' character). Optionally returns flag, whether path had trailing delim or not. *)
function ToNtAbsPath (const Path: WideString; {n} HadTrailingDelim: pboolean = nil): WideString;
begin
  result := NormalizePath(Path, HadTrailingDelim);

  if (result <> '') and (result[1] <> '\') then begin
    result := '\??\' + result;
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

(* Return true if path is valid absolute path to root drive like '\??\X:' with any/zero number of trailing slashes *)
function IsNtRootDriveAbsPath (const Path: WideString): boolean;
const
  MIN_VALID_LEN = Length('\??\X:');

var
  i: integer;

begin
  result := (Length(Path) >= MIN_VALID_LEN) and (Path[1] = '\') and (Path[2] = '?') and (Path[3] = '?') and (Path[4] = '\') and (ord(Path[5]) < 256) and (char(Path[5]) in ['A'..'Z']) and (Path[6] = ':');

  if result then begin
    for i := MIN_VALID_LEN + 1 to Length(Path) do begin
      if Path[i] <> '\' then begin
        result := false;
        exit;
      end;
    end;
  end;
end; // .function IsNtRootDriveAbsPath

function StripNtAbsPathPrefix (const Path: WideString): WideString;
begin
  result := Path;

  if (Length(Path) >= 4) and (Path[1] = '\') and (Path[2] = '?') and (Path[3] = '?') and (Path[4] = '\') then begin
    result := Copy(Path, 4 + 1);
  end;
end;

function SaveAndRet (Res: integer; out ResCopy): integer;
begin
  integer(ResCopy) := Res;
  result           := Res;
end;

type
  TSysOpenFileMode = (OPEN_AS_ANY = 0, OPEN_AS_FILE = WinNative.FILE_NON_DIRECTORY_FILE, OPEN_AS_DIR = WinNative.FILE_DIRECTORY_FILE);

(* Opens file/directory using absolute NT path and returns success flag *)
function SysOpenFile (const NtAbsPath: WideString; {OUT} var Res: Windows.THandle; const OpenMode: TSysOpenFileMode = OPEN_AS_ANY): boolean;
var
  FilePathU:     WinNative.UNICODE_STRING;
  hFile:         Windows.THandle;
  ObjAttrs:      WinNative.OBJECT_ATTRIBUTES;
  IoStatusBlock: WinNative.IO_STATUS_BLOCK;

begin
  FilePathU.AssignExistingStr(NtAbsPath);
  ObjAttrs.Init(@FilePathU);
  
  result := WinNative.NtOpenFile(@hFile, GENERIC_READ or SYNCHRONIZE, @ObjAttrs, @IoStatusBlock, FILE_SHARE_READ or FILE_SHARE_WRITE, ord(OpenMode) or FILE_SYNCHRONOUS_IO_NONALERT) = WinNative.STATUS_SUCCESS;

  if result then begin
    Res := hFile;
  end;
end; // .function SysOpenFile

(* Returns TNativeFileInfo record for single file/directory. Short names in the result are always empty. *)
function GetFileInfo (const FilePath: WideString; {OUT} var Res: TNativeFileInfo): boolean;
const
  BUF_SIZE = sizeof(WinNative.FILE_ALL_INFORMATION) + MAX_FILENAME_SIZE;

var
{U} FileAllInfo:   WinNative.PFILE_ALL_INFORMATION;
    NtAbsPath:     WideString;
    hFile:         Windows.THandle;
    Buf:           array [0..BUF_SIZE - 1] of byte;
    IoStatusBlock: WinNative.IO_STATUS_BLOCK;

begin
  FileAllInfo := @Buf;
  // * * * * * //
  NtAbsPath := ToNtAbsPath(FilePath); 
  result    := SysOpenFile(NtAbsPath, hFile);

  if not result then begin
    exit;
  end;

  result := WinNative.NtQueryInformationFile(hFile, @IoStatusBlock, FileAllInfo, BUF_SIZE, ord(WinNative.FileAllInformation)) = WinNative.STATUS_SUCCESS;

  if result then begin
    Res.Base.FileIndex       := 0;
    Res.Base.CreationTime    := FileAllInfo.BasicInformation.CreationTime;
    Res.Base.LastAccessTime  := FileAllInfo.BasicInformation.LastAccessTime;
    Res.Base.LastWriteTime   := FileAllInfo.BasicInformation.LastWriteTime;
    Res.Base.ChangeTime      := FileAllInfo.BasicInformation.ChangeTime;
    Res.Base.FileAttributes  := FileAllInfo.BasicInformation.FileAttributes;
    Res.Base.EndOfFile       := FileAllInfo.StandardInformation.EndOfFile;
    Res.Base.AllocationSize  := FileAllInfo.StandardInformation.AllocationSize;
    Res.Base.EaSize          := FileAllInfo.EaInformation.EaSize;
    Res.Base.ShortNameLength := 0;
    Res.Base.ShortName[0]    := #0;
    Res.Base.FileNameLength  := FileAllInfo.NameInformation.FileNameLength;
    Res.Base.FileId.LowPart  := 0;
    Res.Base.FileId.HighPart := 0;
    Res.SetFileName(StrLib.ExtractFileNameW(StrLib.WideStringFromBuf(@FileAllInfo.NameInformation.FileName, Min(integer(IoStatusBlock.Information) - sizeof(FileAllInfo^), FileAllInfo.NameInformation.FileNameLength) div sizeof(WideChar))));
  end else if IsNtRootDriveAbsPath(NtAbsPath) and (SaveAndRet(Windows.GetFileAttributesW(PWideChar(StripNtAbsPathPrefix(NtAbsPath))), FileAllInfo.BasicInformation.FileAttributes) <> integer(Windows.INVALID_HANDLE_VALUE)) then begin
    // Return fake info for root drive
    result := true;
    FillChar(Res.Base, sizeof(Res.Base), 0);
    Res.Base.FileAttributes := FileAllInfo.BasicInformation.FileAttributes;
    Res.SetFileName(NtAbsPath[DRIVE_CHAR_INDEX_IN_NT_ABS_PATH] + WideString(':\'#0));
  end; // .elseif

  WinNative.NtClose(hFile);
end; // .function GetFileInfo

constructor TSysDirScanner.Create (const hDir: Windows.THandle; const Mask: WideString);
begin
  Self.fOwnsDirHandle := false;
  Self.fDirHandle     := hDir;
  Self.fMask          := StrLib.WideLowerCase(Mask);
  Self.fMaskU.AssignExistingStr(Self.fMask);
  Self.fIsStart       := true;
  Self.fIsEnd         := false;
  Self.fBufPos        := 0;
end;

constructor TSysDirScanner.Create (const DirPath, Mask: WideString);
var
  hDir: Windows.THandle;

begin
  hDir := Windows.INVALID_HANDLE_VALUE;
  SysOpenFile(ToNtAbsPath(DirPath), hDir, OPEN_AS_DIR);

  Self.Create(hDir, Mask);

  if hDir <> Windows.INVALID_HANDLE_VALUE then begin
    Self.fOwnsDirHandle := true;
  end else begin
    Self.fIsEnd := true;
  end;
end; // .constructor TSysDirScanner.Create

destructor TSysDirScanner.Destroy;
begin
  if Self.fOwnsDirHandle then begin
    WinNative.NtClose(Self.fDirHandle);
  end;
end;

function TSysDirScanner.IterNext ({OUT} var FileName: WideString; {n} FileInfo: WinNative.PFILE_ID_BOTH_DIR_INFORMATION = nil): boolean;
const
  MULTIPLE_ENTRIES = false;

var
{n} FileInfoInBuf: WinNative.PFILE_ID_BOTH_DIR_INFORMATION;
    IoStatusBlock: WinNative.IO_STATUS_BLOCK;
    FileNameLen:   integer;
    Status:        integer;

begin
  FileInfoInBuf := nil;
  // * * * * * //
  result := not Self.fIsEnd and (Self.fDirHandle <> Windows.INVALID_HANDLE_VALUE);

  if not result then begin
    exit;
  end;

  if not Self.fIsStart and (Self.fBufPos < Self.BUF_SIZE) then begin
    FileInfoInBuf := @Self.fBuf[Self.fBufPos];
    FileNameLen   := Min(FileInfoInBuf.FileNameLength, Self.BUF_SIZE - Self.fBufPos) div sizeof(WideChar);
    FileName      := StrLib.WideStringFromBuf(@FileInfoInBuf.FileName, FileNameLen);

    if FileInfo <> nil then begin
      FileInfo^               := FileInfoInBuf^;
      FileInfo.FileNameLength := FileNameLen * sizeof(WideChar);
    end;

    //VarDump(['Read entry: ', FileName]);

    Self.fBufPos := Utils.IfThen(FileInfoInBuf.NextEntryOffset <> 0, Self.fBufPos + integer(FileInfoInBuf.NextEntryOffset), Self.BUF_SIZE);
  end else begin
    Self.fBufPos  := 0;
    Status        := WinNative.NtQueryDirectoryFile(Self.fDirHandle, 0, nil, nil, @IoStatusBlock, @Self.fBuf, Self.BUF_SIZE, ord(WinNative.FileIdBothDirectoryInformation), MULTIPLE_ENTRIES, @Self.fMaskU, Self.fIsStart);
    result        := (Status = WinNative.STATUS_SUCCESS) and (integer(IoStatusBlock.Information) <> 0);
    //VarDump([Format('Called NtQueryDirectoryFile. Status: %x. Io.Information: %d', [Status, int(IoStatusBlock.Information)])]);
    Self.fIsStart := false;

    if result then begin
      result := Self.IterNext(FileName, FileInfo);
    end else begin
      Self.fIsEnd := true;
    end;
  end; // .else
end; // .function TSysDirScanner.IterNext

function SysScanDir (const hDir: Windows.THandle; const Mask: WideString): ISysDirScanner; overload;
begin
  result := TSysDirScanner.Create(hDir, Mask);
end;

function SysScanDir (const DirPath, Mask: WideString): ISysDirScanner; overload;
begin
  result := TSysDirScanner.Create(DirPath, Mask);
end;

procedure SortNativeDirListing ({U} List: DataLib.TList {OF TVfsItem}); forward;
function GetVfsDirInfo (const AbsVirtPath, Mask: WideString; {OUT} var DirInfo: TNativeFileInfo; DirListing: TDirListing): boolean; forward;

procedure GetDirectoryListing (const SearchPath, FileMask: WideString; Exclude: TDict {OF not NIL}; DirListing: TDirListing);
var
{O} Items: {O} TList {OF TVfsItem};
{O} Item:  {O} TVfsItem;
    i:     integer;

begin
  {!} Assert(Exclude <> nil);
  {!} Assert(DirListing <> nil);
  // * * * * * //
  Items := DataLib.NewList(Utils.OWNS_ITEMS);
  Item  := TVfsItem.Create;
  // * * * * * //
  with SysScanDir(SearchPath, FileMask) do begin
    while IterNext(Item.Info.FileName, @Item.Info.Base) do begin
      // Update all name dependencies
      Item.Name := Item.Info.FileName;
      
      if Exclude[WideStrToCaselessKey(Item.Name)] = nil then begin
        Items.Add(Item); Item := nil;
        Item := TVfsItem.Create;
      end;
    end;
  end;

  SortNativeDirListing(Items);

  for i := 0 to Items.Count - 1 do begin
    DirListing.AddItem(@TVfsItem(Items[i]).Info);
  end;
  // * * * * * //
  SysUtils.FreeAndNil(Items);
  SysUtils.FreeAndNil(Item);
end; // .procedure GetDirectoryListing

constructor TOpenedFile.Create (hFile: Windows.THandle; const AbsPath: WideString);
begin
  Self.hFile   := hFile;
  Self.AbsPath := AbsPath;
end;

destructor TOpenedFile.Destroy;
begin
  FreeAndNil(Self.DirListing);
end;

procedure TOpenedFile.FillDirListing (const Mask: WideString);
var
{On} ExcludedItems:               {U} TDict {OF not nil};
     PrevDisableVfsForThisThread: boolean;
     VfsItemFound:                boolean;
     NumVfsChildren:              integer;
     DirInfo:                     TNativeFileInfo;
     ParentDirInfo:               TNativeFileInfo;
     DirItem:                     TFileInfo;
     i:                           integer;

begin
  ExcludedItems := nil;
  // * * * * * //
  if Self.DirListing <> nil then begin
    exit;
  end;

  Self.DirListing := TDirListing.Create;
  VfsItemFound    := GetVfsDirInfo(Self.AbsPath, Mask, DirInfo, Self.DirListing);
  ExcludedItems   := DataLib.NewDict(not Utils.OWNS_ITEMS, DataLib.CASE_SENSITIVE);

  if VfsItemFound then begin
    while DirListing.GetNextItem(DirItem) do begin
      ExcludedItems[WideStrToCaselessKey(DirItem.Data.FileName)] := Ptr(1);
    end;

    Self.DirListing.Rewind;
  end;

  // Add real items
  NumVfsChildren              := Self.DirListing.Count;
  PrevDisableVfsForThisThread := DisableVfsForThisThread;
  DisableVfsForThisThread     := true;
  GetDirectoryListing(Self.AbsPath, Mask, ExcludedItems, Self.DirListing);
  DisableVfsForThisThread     := PrevDisableVfsForThisThread;

  // No real items added, maybe there is a need to add '.' and/or '..' manually
  if VfsItemFound and (Self.DirListing.Count = NumVfsChildren) then begin
    if StrLib.MatchW('..', Mask) and GetFileInfo(Self.AbsPath + '\..', ParentDirInfo) then begin
      Self.DirListing.AddItem(@ParentDirInfo, '..');
    end;

    if StrLib.MatchW('.', Mask) then begin
      Self.DirListing.AddItem(@DirInfo, '.');
    end;
  end;
  // * * * * * //
  SysUtils.FreeAndNil(ExcludedItems);
end; // .procedure TOpenedFile.FillDirListing

function FileExistsW (const FilePath: WideString): boolean;
begin
  result := integer(Windows.GetFileAttributesW(PWideChar(FilePath))) <> -1;
end;

function DirExistsW (const DirPath: WideString): boolean;
var
  FileAttrs: integer;

begin
  FileAttrs := Windows.GetFileAttributesW(PWideChar(DirPath));
  result    := (FileAttrs <> -1) and Utils.HasFlag(Windows.FILE_ATTRIBUTE_DIRECTORY, FileAttrs);
end;

function _CompareVfsItemsByPriorityDescAndNameAsc (Item1, Item2: integer): integer;
begin
  result := TVfsItem(Item2).Priority - TVfsItem(Item1).Priority;

  if result = 0 then begin
    result := StrLib.CompareBinStringsW(TVfsItem(Item1).SearchName, TVfsItem(Item2).SearchName);
  end;
end;

function _CompareVfsItemsByPriorityAscAndNameAsc (Item1, Item2: integer): integer;
begin
  result := TVfsItem(Item1).Priority - TVfsItem(Item2).Priority;

  if result = 0 then begin
    result := StrLib.CompareBinStringsW(TVfsItem(Item1).SearchName, TVfsItem(Item2).SearchName);
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
    result := CompareWideChars(@TVfsItem(Item1).Info.FileName, @TVfsItem(Item2).Info.FileName);
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

procedure CopyFileInfoWithoutNames (var Src, Dest: WinNative.FILE_ID_BOTH_DIR_INFORMATION);
begin
  Dest.FileIndex      := 0;
  Dest.CreationTime   := Src.CreationTime;
  Dest.LastAccessTime := Src.LastAccessTime;
  Dest.LastWriteTime  := Src.LastWriteTime;
  Dest.ChangeTime     := Src.ChangeTime;
  Dest.EndOfFile      := Src.EndOfFile;
  Dest.AllocationSize := Src.AllocationSize;
  Dest.FileAttributes := Src.FileAttributes;
  Dest.EaSize         := Src.EaSize;
end;

(*
  Redirects single file/directory path (not including directory contents). Target must exist for success.
*)
function RedirectFile (const AbsVirtPath, AbsRealPath: WideString; {n} FileInfoPtr: WinNative.PFILE_ID_BOTH_DIR_INFORMATION; OverwriteExisting: boolean; Priority: integer): {Un} TVfsItem;
const
  WIDE_NULL_CHAR_LEN = 1;

var
{Un} VfsItem:        TVfsItem;
     PackedVirtPath: string;
     IsNewItem:      boolean;
     FileInfo:       TNativeFileInfo;
     FileNameSize:   integer;
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
        VfsItem                           := TVfsItem.Create();
        VfsItems[PackedVirtPath]          := VfsItem;
        VfsItem.Name                      := StrLib.ExtractFileNameW(AbsVirtPath);
        VfsItem.SearchName                := StrLib.WideLowerCase(VfsItem.Name);
        VfsItem.Info.Base.ShortNameLength := 0;
        VfsItem.Info.Base.ShortName[0]    := #0;
      end;

      if FileInfoPtr <> nil then begin
        CopyFileInfoWithoutNames(FileInfoPtr^, VfsItem.Info.Base);
      end else begin
        CopyFileInfoWithoutNames(FileInfo.Base, VfsItem.Info.Base);
      end;
 
      VfsItem.RealPath := AbsRealPath;
      VfsItem.Priority := Priority;
      VfsItem.Attrs    := 0;
    end; // .if
  end; // .if

  if Success then begin
    result := VfsItem;
  end;
end; // .function RedirectFile

// function IsDir (const AbsPath: WideChar): boolean;
// begin
//   result := (NativeGetFileAttributesW(PWideChar(AbsPath)) and Windows.FILE_ATTRIBUTE_DIRECTORY) = Windows.FILE_ATTRIBUTE_DIRECTORY;
// end;

(*
  Maps real directory contents to virtual path. Target must exist for success.
*)
function _MapDir (const AbsVirtPath, AbsRealPath: WideString; {n} FileInfoPtr: WinNative.PFILE_ID_BOTH_DIR_INFORMATION; OverwriteExisting: boolean; Priority: integer): {Un} TVfsItem;
var
{O}  Subdirs:        {O} TList {OF TFileInfo};
{U}  SubdirInfo:     TFileInfo;
{Un} DirVfsItem:     TVfsItem;
     Success:        boolean;
     hSearch:        cardinal;
     FileInfo:       TNativeFileInfo;
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
    VirtPathPrefix := AbsVirtPath + '\';
    RealPathPrefix := AbsRealPath + '\';

    if DirVfsItem.Children = nil then begin
      DirVfsItem.Children := DataLib.NewList(not Utils.OWNS_ITEMS);
    end;

    with SysScanDir(AbsRealPath, '*') do begin
      while IterNext(FileInfo.FileName, @FileInfo.Base) do begin
        if Utils.HasFlag(FileInfo.Base.FileAttributes, Windows.FILE_ATTRIBUTE_DIRECTORY) then begin         
          if (FileInfo.FileName <> '.') and (FileInfo.FileName <> '..') then begin
            Subdirs.Add(TFileInfo.Create(@FileInfo));
          end;
        end else begin
          RedirectFile(VirtPathPrefix + FileInfo.FileName, RealPathPrefix + FileInfo.FileName, @FileInfo, OverwriteExisting, Priority);
        end;
      end;
    end;

    for i := 0 to Subdirs.Count - 1 do begin
      SubdirInfo := TFileInfo(Subdirs[i]);
      _MapDir(VirtPathPrefix + SubdirInfo.Data.FileName, RealPathPrefix + SubdirInfo.Data.FileName, @SubdirInfo.Data, OverwriteExisting, Priority);
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
function GetVfsItemRealPath (const AbsVirtPath: WideString; {n} FileInfo: PNativeFileInfo = nil): WideString;
var
{n} VfsItem: TVfsItem;

begin
  VfsItem := nil;
  // * * * * * //
  result := '';

  if not DisableVfsForThisThread then begin
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
  end; // .if
end; // .function GetVfsItemRealPath

function IsVfsOn: boolean;
begin
  result := false;

  if not DisableVfsForThisThread then begin
    with VfsCritSection do begin
      Enter;
      result := VfsIsRunning;
      Leave;
    end;
  end;
end;

function GetVfsDirInfo (const AbsVirtPath, Mask: WideString; {OUT} var DirInfo: TNativeFileInfo; DirListing: TDirListing): boolean;
var
{n} VfsItem:        TVfsItem;
    NormalizedMask: WideString;
    i:              integer;

begin
  {!} Assert(DirListing <> nil);
  result := false;
  // * * * * * //
  if not DisableVfsForThisThread then begin
    with VfsCritSection do begin
      Enter;

      if VfsIsRunning then begin
        VfsItem := VfsItems[WideStrToCaselessKey(AbsVirtPath)];

        if (VfsItem <> nil) and VfsItem.IsDir then begin
          result  := true;
          DirInfo := VfsItem.Info;

          if VfsItem.Children <> nil then begin
            NormalizedMask := StrLib.WideLowerCase(Mask);

            for i := 0 to VfsItem.Children.Count - 1 do begin
              if StrLib.MatchW(TVfsItem(VfsItem.Children[i]).SearchName, NormalizedMask) then begin
                DirListing.AddItem(@TVfsItem(VfsItem.Children[i]).Info);
              end;
            end;
          end; // .if
        end; // .if
      end; // .if

      Leave;
    end; // .with
  end; // .if
end; // .function GetVfsDirInfo

function AllocSearchHandle ({U} var {OUT} DirListing: TDirListing): THandle;
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
    DirListing                 := TDirListing.Create();
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

function Hook_LoadLibraryW (OrigFunc: pointer; lpLibFileName: PWideChar): HMODULE; stdcall;
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
  if WinNative.PathIsRelativeW(PWideChar(LibPath)) then begin
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

    result         := NativeLoadLibraryW(PWideChar(RedirectedPath));
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

    result := NativeLoadLibraryW(lpLibFileName);
  end;
end; // .function Hook_LoadLibraryW

function Hook_LoadLibraryA (OrigFunc: pointer; lpLibFileName: PAnsiChar): HMODULE; stdcall;
begin
  result := Windows.LoadLibraryW(PWideChar(WideString(String(lpLibFileName))));
end;

function _FindFirstFileExW (var LastError: integer; lpFileName: PWideChar; fInfoLevelId: TFindexInfoLevels; var lpFindFileData: TWin32FindDataW;
                            fSearchOp: TFindexSearchOps; lpSearchFilter: Pointer; dwAdditionalFlags: DWORD): THandle;
var
{Un} DirVfsItem:       TVfsItem;
{O}  AddedVfsItems:    DataLib.TDict {OF 1};
{U}  ItemFromVfs:      TVfsItem;
{U}  DirListing:       TDirListing;
{U}  FirstResult:      Windows.PWin32FindDataW;
     PrevDisableVfsForThisThread: boolean;
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

  // WHRE IS REACTION FOR VFS BEING OFF???

  with FileSearchCritSection do begin
    Enter;

    // {TODO} Optimize check, move it ouside critical section
    IsStandardSearch := (fInfoLevelId = Windows.FindExInfoStandard) and (fSearchOp = Windows.FindExSearchNameMatch) and (lpSearchFilter = nil) and (dwAdditionalFlags = 0);

    if FileSearchInProgress or not IsStandardSearch then begin
      result    := NativeFindFirstFileExW(lpFileName, fInfoLevelId, lpFindFileData, fSearchOp, lpSearchFilter, dwAdditionalFlags);
      LastError := Windows.GetLastError();
    end else begin
      LastError := Windows.ERROR_INVALID_HANDLE;
      FullPath  := ExpandPath(lpFileName);

      if DebugOpt then begin
        StrLib.PWideCharToAnsi(lpFileName, OrigPathA);
      end;

      // Disallow empty or '\'-ending paths
      if (FullPath = '') or (FullPath[Length(FullPath)] = '\') then begin
        if DebugOpt then begin
          Log.Write('VFS', 'FindFirstFileExW', 'Path: "' + OrigPathA + '". Result: ERROR_INVALID_PARAMETER');
        end;

        LastError := Windows.ERROR_INVALID_PARAMETER;
        result    := Windows.INVALID_HANDLE_VALUE;
      end
      // Non-empty path, dividable into directory path and pattern
      else begin
        AddedVfsItems := DataLib.NewDict(not Utils.OWNS_ITEMS, DataLib.CASE_SENSITIVE);
        result        := AllocSearchHandle(DirListing);

        if result = Windows.INVALID_HANDLE_VALUE then begin
          LastError := Windows.ERROR_TOO_MANY_OPEN_FILES;
        end else begin
          with VfsCritSection do begin
            Enter;

            DirVfsItem := VfsItems[WideStrToCaselessKey(FullPath)];

            // Request for file/directory itself, record is found in VFS
            if DirVfsItem <> nil then begin
              DirListing.AddItem(@DirVfsItem.Info);
            end
            // Request with parent directory and files pattern
            else begin
              VirtDirPath   := StrLib.ExtractDirPathW(FullPath);
              SearchPattern := StrLib.WideLowerCase(StrLib.ExtractFileNameW(FullPath));
              DirVfsItem    := VfsItems[WideStrToCaselessKey(NormalizePath(VirtDirPath))];

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
              PrevDisableVfsForThisThread := DisableVfsForThisThread;
              DisableVfsForThisThread         := true; // what's about previous state ????????????????????????????????????????????????????

              if DirVfsItem <> nil then begin
                //VarDump(['Running real scan dir over: ', lpFileName]);
              end;

              // Scan real directory
              // GetDirectoryListing(lpFileName, AddedVfsItems, DirListing);

              DisableVfsForThisThread         := PrevDisableVfsForThisThread;
              FileSearchInProgress := false;
            end; // .else

            Leave;
          end; // .with VfsCritSection

          //FirstResult := DirListing.GetNextItem();

          if FirstResult = nil then begin
            if (DirVfsItem <> nil) or IsDir(VirtDirPath) then begin
              LastError := Windows.ERROR_NO_MORE_FILES;
            end else begin
              LastError := Windows.ERROR_PATH_NOT_FOUND;
            end;
            
            ReleaseSearchHandle(result);
            result := Windows.INVALID_HANDLE_VALUE;
          end else begin
            LastError      := Windows.ERROR_SUCCESS;
            lpFindFileData := FirstResult^;
          end;
        end; // .else
      end; // .else

      if DebugOpt then begin
        if result = Windows.INVALID_HANDLE_VALUE then begin
          Log.Write('VFS', 'FindFirstFileExW', 'Path: "' + OrigPathA + '". Result: INVALID_HANDLE_VALUE. Expanded: ' + FullPath + '. Current Directory: ' + GetCurrentDir());
        end else begin
          StrLib.PWideCharToAnsi(lpFindFileData.cFileName, FirstResultNameA);
          Log.Write('VFS', 'FindFirstFileExW', 'Path: "' + OrigPathA + '". Result: ' + SysUtils.IntToStr(integer(result)) + '. Found: "' + FirstResultNameA + '"');
        end;
      end;
    end; // .else

    Leave;
  end; // .with FileSearchCritSection
  // * * * * * //
  SysUtils.FreeAndNil(AddedVfsItems);
end; // .function _FindFirstFileExW

function _FindFirstFileExA (var LastError: integer; lpFileName: PAnsiChar; fInfoLevelId: TFindexInfoLevels; var lpFindFileData: Windows.TWIN32FindDataA;
                            fSearchOp: TFindexSearchOps; lpSearchFilter: Pointer; dwAdditionalFlags: DWORD): THandle;
var
  FileInfo: Windows.TWin32FindDataW;

begin
  result := _FindFirstFileExW(LastError, PWideChar(WideString(String(lpFileName))), fInfoLevelId, FileInfo, fSearchOp, lpSearchFilter, dwAdditionalFlags);

  if result <> Windows.INVALID_HANDLE_VALUE then begin
    ConvertWin32FindDataToAnsi(@FileInfo, @lpFindFileData);
  end;
end;

function _FindFirstFileW (var LastError: integer; lpFileName: PWideChar; var lpFindFileData: Windows.TWIN32FindDataW): THandle;
var
  FileInfo: Windows.TWin32FindDataW;

begin
  result := _FindFirstFileExW(LastError, lpFileName, Windows.FindExInfoStandard, FileInfo, Windows.FindExSearchNameMatch, nil, 0);

  if result <> Windows.INVALID_HANDLE_VALUE then begin
    ConvertWin32FindDataToAnsi(@FileInfo, @lpFindFileData);
  end;
end;

function _FindFirstFileA (var LastError: integer; lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindDataA): THandle;
var
  FileInfo: Windows.TWin32FindDataW;

begin
  result := _FindFirstFileExW(LastError, PWideChar(WideString(String(lpFileName))), Windows.FindExInfoStandard, FileInfo, Windows.FindExSearchNameMatch, nil, 0);

  if result <> Windows.INVALID_HANDLE_VALUE then begin
    ConvertWin32FindDataToAnsi(@FileInfo, @lpFindFileData);
  end;
end;

function Hook_FindFirstFileExW (OrigFunc: pointer; lpFileName: PWideChar; fInfoLevelId: TFindexInfoLevels; var lpFindFileData: TWin32FindDataW;
                                fSearchOp: TFindexSearchOps; lpSearchFilter: Pointer; dwAdditionalFlags: DWORD): THandle; stdcall;
var
  LastError: integer;

begin
  result := _FindFirstFileExW(LastError, lpFileName, fInfoLevelId, lpFindFileData, fSearchOp, lpSearchFilter, dwAdditionalFlags);
  Windows.SetLastError(LastError);
end;

function Hook_FindFirstFileExA (OrigFunc: pointer; lpFileName: PAnsiChar; fInfoLevelId: TFindexInfoLevels; var lpFindFileData: TWIN32FindDataA;
                                fSearchOp: TFindexSearchOps; lpSearchFilter: Pointer; dwAdditionalFlags: DWORD): THandle; stdcall;
var
  LastError: integer;

begin
  result := _FindFirstFileExA(LastError, lpFileName, fInfoLevelId, lpFindFileData, fSearchOp, lpSearchFilter, dwAdditionalFlags);
  Windows.SetLastError(LastError);
end;

function Hook_FindFirstFileW (OrigFunc: pointer; lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
var
  LastError: integer;

begin
  result := _FindFirstFileExW(LastError, lpFileName, Windows.FindExInfoStandard, lpFindFileData, Windows.FindExSearchNameMatch, nil, 0);
  Windows.SetLastError(LastError);
end;

function Hook_FindFirstFileA (OrigFunc: pointer; lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindDataA): THandle; stdcall;
var
  LastError: integer;

begin
  result := _FindFirstFileA(LastError, lpFileName, lpFindFileData);
  Windows.SetLastError(LastError);
end;

function Hook_FindNextFileW (OrigFunc: pointer; hFindFile: THandle; var lpFindFileData: TWIN32FindDataW): BOOL; stdcall;
var
{Un} DirListing: TDirListing;
{Un} FileInfo:   Windows.PWin32FindDataW;
     LastError:  integer;
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
    if FileSearchInProgress or (integer(hFindFile) <= 0) or (hFindFile > MAX_SEARCH_HANDLE) then begin
      result := NativeFindNextFileW(hFindFile, lpFindFileData);
    end
    // Virtual search handle
    else begin
      result     := false;
      Status     := Windows.ERROR_SUCCESS;
      DirListing := GetSearchData(integer(hFindFile));

      if DirListing = nil then begin
        LastError := Windows.ERROR_INVALID_HANDLE;
        Status    := ERROR_INVALID_HANDLE;
      end else begin
        //FileInfo := DirListing.GetNextItem();

        if FileInfo = nil then begin
          LastError := Windows.ERROR_NO_MORE_FILES;
          Status    := Windows.ERROR_NO_MORE_FILES;
        end else begin
          LastError      := Windows.ERROR_SUCCESS;
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

      Windows.SetLastError(LastError);
    end; // .else

    Leave;
  end; // .with FileSearchCritSection
end; // .function Hook_FindNextFileW

function Hook_FindNextFileA (OrigFunc: pointer; hFindFile: THandle; var lpFindFileData: TWIN32FindDataA): BOOL; stdcall;
var
  FileInfo: Windows.TWin32FindDataW;

begin
  result := Windows.FindNextFileW(hFindFile, FileInfo);

  if result then begin
    ConvertWin32FindDataToAnsi(@FileInfo, @lpFindFileData);
  end;
end;

function Hook_FindClose (OrigFunc: pointer; hFindFile: THandle): BOOL; stdcall;
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
        Log.Write('VFS', 'FindClose', Format('Handle: %d. Result: %s', [integer(hFindFile), Utils.IfThen(result, 'ERROR_SUCCESS', 'ERROR_INVALID_HANDLE')]));
      end;
    end;

    Leave;
  end; // .with FileSearchCritSection;
end; // .function Hook_FindClose

function GetFilePathByHandle (hFile: THandle): WideString;
const
  BUFSIZE_PREFIX_SIZE = 4;

var
  Buf:             WideString;
  BufSize:         integer;
  BufSizeRequired: integer;
  IoStatusBlock:   WinNative.IO_STATUS_BLOCK;
  NumChars:        integer;

begin
  result := '';
  Buf    := '';
  
  SetLength(Buf, Windows.MAX_PATH + BUFSIZE_PREFIX_SIZE div 2);
  BufSize := Length(Buf) * sizeof(Buf[1]);

  if WinNative.NtQueryInformationFile(hFile, @IoStatusBlock, PWideChar(Buf), BufSize, ord(WinNative.FileNameInformation)) <> WinNative.STATUS_SUCCESS then begin
    exit;
  end;

  BufSizeRequired := pinteger(Buf)^ + BUFSIZE_PREFIX_SIZE;

  while BufSizeRequired > BufSize do begin
    BufSize := BufSizeRequired;
    SetLength(Buf, BufSizeRequired div sizeof(Buf[1]));
    
    if WinNative.NtQueryInformationFile(hFile, @IoStatusBlock, PWideChar(Buf), BufSize, ord(WinNative.FileNameInformation)) <> WinNative.STATUS_SUCCESS then begin
      exit;
    end;

    BufSizeRequired := pinteger(Buf)^ + BUFSIZE_PREFIX_SIZE;
  end;

  if BufSizeRequired > BUFSIZE_PREFIX_SIZE then begin
    NumChars := (BufSizeRequired - BUFSIZE_PREFIX_SIZE) div sizeof(Buf[1]);
    SetLength(result, NumChars);
    Utils.CopyMem(NumChars * sizeof(Buf[1]), Utils.PtrOfs(PWideChar(Buf), BUFSIZE_PREFIX_SIZE), @result[1]);
  end;
end; // .function GetFilePathByHandle

(* Returns single absolute path, not dependant on RootDirectory member. '\??\' prefix is always removed, \\.\ and \\?\ paths remain not touched. *)
// FIXME GetFilePathByHandle does not return DRIVE prefix!!! C:\x.txt => \x.txt
//
function GetFileObjectPath (ObjectAttributes: POBJECT_ATTRIBUTES): WideString;
var
  FilePath: WideString;
  DirPath:  WideString;

begin
  FilePath := ObjectAttributes.ObjectName.ToWideStr();

  if FilePath = '' then begin
    result := '';
  end else begin
    if FilePath[1] = '\' then begin
      FilePath := StripNtAbsPathPrefix(FilePath);
    end;

    if ObjectAttributes.RootDirectory <> 0 then begin
      DirPath := NormalizePath(GetFilePathByHandle(ObjectAttributes.RootDirectory));

      with CurrDirCritSection do begin
        Enter;

        if (DirPath <> '') and CurrDirIsVirtual and (Length(DirPath) = Length(CurrDirRealLoweredPath)) and
           (StrLib.CompareWideChars(PWideChar(StrLib.WideLowerCase(DirPath)), PWideChar(CurrDirRealLoweredPath)) = 0)
        then begin
          DirPath := CurrDirVirtPath;
        end;

        Leave;
      end;
      
      result := ExpandPath(DirPath + '\' + FilePath);
    end else begin
      result := FilePath;
    end; // .else
  end; // .else
end; // .function GetFileObjectPath

function Hook_NtQueryAttributesFile (OrigFunc: WinNative.TNtQueryAttributesFile; ObjectAttributes: POBJECT_ATTRIBUTES; FileInformation: PFILE_BASIC_INFORMATION): NTSTATUS; stdcall;
var
  ExpandedPath:     WideString;
  RedirectedPath:   WideString;
  ReplacedObjAttrs: WinNative.TObjectAttributes;
  FileInfo:         Windows.TWin32FindDataW;
  HadTrailingDelim: boolean;

begin
  if DebugOpt then begin
    Log.Write('VFS', 'NtQueryAttributesFile', Format('Dir: %d. Path: "%s"', [ObjectAttributes.RootDirectory, ObjectAttributes.ObjectName.ToWideStr()]));
  end;

  ReplacedObjAttrs        := ObjectAttributes^;
  ReplacedObjAttrs.Length := sizeof(ReplacedObjAttrs);
  ExpandedPath            := GetFileObjectPath(ObjectAttributes);
  RedirectedPath          := '';

  if ExpandedPath <> '' then begin
    RedirectedPath := GetVfsItemRealPath(StrLib.ExcludeTrailingDelimW(ExpandedPath, @HadTrailingDelim), @FileInfo);
  end;

  // Return cached VFS file info with fake ChangeTime = LastWriteTime
  if RedirectedPath <> '' then begin
//    if not HadTrailingDelim or ((FileInfo.FileAttributes and Windows.FILE_ATTRIBUTE_DIRECTORY) = Windows.FILE_ATTRIBUTE_DIRECTORY) then begin
//      FileInformation.CreationTime   := WinNative.LARGE_INTEGER(FileInfo.ftCreationTime);
//      FileInformation.LastAccessTime := WinNative.LARGE_INTEGER(FileInfo.ftLastAccessTime);
//      FileInformation.LastWriteTime  := WinNative.LARGE_INTEGER(FileInfo.ftLastWriteTime);
//      FileInformation.ChangeTime     := WinNative.LARGE_INTEGER(FileInfo.ftLastWriteTime);
//      FileInformation.FileAttributes := FileInfo.FileAttributes;
//      result                         := WinNative.STATUS_SUCCESS;
//    end else begin
//      result := WinNative.STATUS_NO_SUCH_FILE;
//    end;
  end
  // Query file with real path
  else begin
    RedirectedPath := ExpandedPath;

    if (RedirectedPath <> '') and (RedirectedPath[1] <> '\') then begin
      RedirectedPath := '\??\' + RedirectedPath;
    end;

    ReplacedObjAttrs.RootDirectory := 0;
    ReplacedObjAttrs.Attributes    := ReplacedObjAttrs.Attributes or WinNative.OBJ_CASE_INSENSITIVE;
    ReplacedObjAttrs.ObjectName.AssignExistingStr(RedirectedPath);
    
    result := OrigFunc(@ReplacedObjAttrs, FileInformation);
  end; // .else

  if DebugOpt then begin
    Log.Write('VFS', 'NtQueryAttributesFile', Format('Result: %x. Attrs: 0x%x. Path: "%s" => "%s"', [result, FileInformation.FileAttributes, string(ExpandedPath), string(RedirectedPath)]));
  end;
end; // .function Hook_NtQueryAttributesFile

function Hook_NtQueryFullAttributesFile (OrigFunc: WinNative.TNtQueryFullAttributesFile; ObjectAttributes: POBJECT_ATTRIBUTES; FileInformation: PFILE_NETWORK_OPEN_INFORMATION): NTSTATUS; stdcall;
var
  ExpandedPath:     WideString;
  RedirectedPath:   WideString;
  ReplacedObjAttrs: WinNative.TObjectAttributes;
  HadTrailingDelim: boolean;

begin
  if DebugOpt then begin
    Log.Write('VFS', 'NtQueryFullAttributesFile', Format('Dir: %d. Path: "%s"', [ObjectAttributes.RootDirectory, ObjectAttributes.ObjectName.ToWideStr()]));
  end;

  ReplacedObjAttrs        := ObjectAttributes^;
  ReplacedObjAttrs.Length := sizeof(ReplacedObjAttrs);
  ExpandedPath            := GetFileObjectPath(ObjectAttributes);
  RedirectedPath          := '';

  if ExpandedPath <> '' then begin
    RedirectedPath := GetVfsItemRealPath(StrLib.ExcludeTrailingDelimW(ExpandedPath, @HadTrailingDelim));
  end;

  if RedirectedPath = '' then begin
    RedirectedPath := ExpandedPath;
  end else if HadTrailingDelim then begin
    RedirectedPath := RedirectedPath + '\';
  end;
    
  if (RedirectedPath <> '') and (RedirectedPath[1] <> '\') then begin
    RedirectedPath := '\??\' + RedirectedPath;
  end;

  ReplacedObjAttrs.RootDirectory := 0;
  ReplacedObjAttrs.Attributes    := ReplacedObjAttrs.Attributes or WinNative.OBJ_CASE_INSENSITIVE;
  ReplacedObjAttrs.ObjectName.AssignExistingStr(RedirectedPath);
  
  result := OrigFunc(@ReplacedObjAttrs, FileInformation);

  if DebugOpt then begin
    Log.Write('VFS', 'NtQueryFullAttributesFile', Format('Result: %x. Attrs: 0x%x. Path: "%s" => "%s"', [result, FileInformation.FileAttributes, string(ExpandedPath), string(RedirectedPath)]));
  end;
end; // .Hook_NtQueryFullAttributesFile

function Hook_NtOpenFile (OrigFunc: WinNative.TNtOpenFile; FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES;
                          IoStatusBlock: PIO_STATUS_BLOCK; ShareAccess: ULONG; OpenOptions: ULONG): NTSTATUS; stdcall;
begin
  if DebugOpt then begin
    Log.Write('VFS', 'NtOpenFile', ObjectAttributes.ObjectName.ToWideStr());
  end;

  result := WinNative.NtCreateFile(FileHandle, DesiredAccess, ObjectAttributes, IoStatusBlock, nil, 0, ShareAccess, WinNative.FILE_OPEN, OpenOptions, nil, 0);
end;

function Hook_NtCreateFile (OrigFunc: WinNative.TNtCreateFile; FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK;
                            AllocationSize: PLARGE_INTEGER; FileAttributes: ULONG; ShareAccess: ULONG; CreateDisposition: ULONG; CreateOptions: ULONG; EaBuffer: PVOID; EaLength: ULONG): NTSTATUS; stdcall;
var
  ExpandedPath:     WideString;
  RedirectedPath:   WideString;
  ReplacedObjAttrs: WinNative.TObjectAttributes;
  HadTrailingDelim: boolean;

  FileInfo: Windows.TWin32FindDataW;

begin
  if DebugOpt then begin
    Log.Write('VFS', 'NtCreateFile', ObjectAttributes.ObjectName.ToWideStr());
  end;

  ReplacedObjAttrs        := ObjectAttributes^;
  ReplacedObjAttrs.Length := sizeof(ReplacedObjAttrs);
  ExpandedPath            := GetFileObjectPath(ObjectAttributes);
  RedirectedPath          := '';

  if (ExpandedPath <> '') and ((DesiredAccess and WinNative.DELETE) = 0) and (CreateDisposition = WinNative.FILE_OPEN) then begin
    RedirectedPath := GetVfsItemRealPath(StrLib.ExcludeTrailingDelimW(ExpandedPath, @HadTrailingDelim), @FileInfo);
  end;

  if RedirectedPath = '' then begin
    RedirectedPath := ExpandedPath;
  end else if HadTrailingDelim then begin
    RedirectedPath := RedirectedPath + '\';
  end;

  if (RedirectedPath <> '') and (RedirectedPath[1] <> '\') then begin
    RedirectedPath := '\??\' + RedirectedPath;
  end;

  ReplacedObjAttrs.RootDirectory := 0;
  ReplacedObjAttrs.Attributes    := ReplacedObjAttrs.Attributes or WinNative.OBJ_CASE_INSENSITIVE;
  ReplacedObjAttrs.ObjectName.AssignExistingStr(RedirectedPath);

  result := OrigFunc(FileHandle, DesiredAccess, @ReplacedObjAttrs, IoStatusBlock, AllocationSize, FileAttributes, ShareAccess, CreateDisposition, CreateOptions, EaBuffer, EaLength);

  if (result = WinNative.STATUS_SUCCESS) and Utils.HasFlag(WinNative.FILE_SYNCHRONOUS_IO_NONALERT, CreateOptions) and Utils.HasFlag(WinNative.SYNCHRONIZE, DesiredAccess) then begin
    OpenedFiles[Ptr(FileHandle^)] := TOpenedFile.Create(FileHandle^, ExpandedPath);
  end;

  if DebugOpt then begin
    if ExpandedPath <> StripNtAbsPathPrefix(RedirectedPath) then begin
      Log.Write('VFS', 'NtCreateFile', Format('Access: 0x%x. Handle: %x. Status: %x. Redirected "%s" => "%s"', [DesiredAccess, FileHandle^, result, StrLib.WideToAnsiSubstitute(ExpandedPath), StrLib.WideToAnsiSubstitute(StripNtAbsPathPrefix(RedirectedPath))]));
    end else begin
      Log.Write('VFS', 'NtCreateFile', Format('Access: 0x%x. Handle: %x. Status: %x. Path: "%s"', [DesiredAccess, FileHandle^, result, StrLib.WideToAnsiSubstitute(ExpandedPath)]));
    end;
  end;
end; // .function Hook_NtCreateFile

function Hook_NtClose (OrigFunc: WinNative.TNtClose; hData: HANDLE): NTSTATUS; stdcall;
begin
  if DebugOpt then begin
    Log.Write('VFS', 'NtClose', Format('Handle: %x', [integer(hData)]));
  end;

  OpenedFiles.DeleteItem(pointer(hData));

  result := OrigFunc(hData);

  if DebugOpt then begin
    Log.Write('VFS', 'NtClose', Format('Status: %x', [integer(result)]));
  end;
end; // .function Hook_NtClose

function IsSupportedFileInformationClass (FileInformationClass: integer): boolean;
begin
  result := (FileInformationClass <= High(byte)) and (FILE_INFORMATION_CLASS(byte(FileInformationClass)) in [FileBothDirectoryInformation, FileDirectoryInformation, FileFullDirectoryInformation, FileIdBothDirectoryInformation, FileIdFullDirectoryInformation, FileNamesInformation]);
end;

type
  TFileInfoConvertResult  = (TOO_SMALL_BUF, COPIED_ALL, TRUNCATED_NAME);
  TTruncatedNamesStrategy = (DONT_TRUNCATE_NAMES, TRUNCATE_NAMES);

function ConvertFileInfoStruct (SrcInfo: PNativeFileInfo; TargetFormat: FILE_INFORMATION_CLASS; {n} Buf: pointer; BufSize: integer; TruncatedNamesStrategy: TTruncatedNamesStrategy;
                                {OUT} var BytesWritten: integer): TFileInfoConvertResult;
var
{n} FileNameBuf:     pointer;
    FileNameBufSize: integer;
    StructBaseSize:  integer;
    StructFullSize:  integer;

begin
  {!} Assert(SrcInfo <> nil);
  {!} Assert(IsSupportedFileInformationClass(ord(TargetFormat)), Format('Unsupported file information class: %d', [ord(TargetFormat)]));
  FileNameBuf := nil;
  // * * * * * //
  BytesWritten   := 0;
  StructBaseSize := WinNative.GetFileInformationClassSize(TargetFormat);
  StructFullSize := StructBaseSize + SrcInfo.Base.FileNameLength;

  if (Buf = nil) or (BufSize < StructBaseSize)  then begin
    result := TOO_SMALL_BUF;
    exit;
  end;

  result := COPIED_ALL;

  if BufSize < StructFullSize then begin
    result := TRUNCATED_NAME;

    if TruncatedNamesStrategy = DONT_TRUNCATE_NAMES then begin
      exit;
    end;
  end;

  case TargetFormat of
    FileNamesInformation: PFILE_NAMES_INFORMATION(Buf).FileNameLength := SrcInfo.Base.FileNameLength;
   
    FileBothDirectoryInformation, FileDirectoryInformation, FileFullDirectoryInformation, FileIdBothDirectoryInformation, FileIdFullDirectoryInformation: begin
      Utils.CopyMem(StructBaseSize, @SrcInfo.Base, Buf);
    end;
  else
    {!} Assert(IsSupportedFileInformationClass(ord(TargetFormat)), Format('Unexpected unsupported file information class: %d', [ord(TargetFormat)]));
  end;

  FileNameBufSize := Min(BufSize - StructBaseSize, SrcInfo.Base.FileNameLength) and not $00000001;
  FileNameBuf     := Utils.PtrOfs(Buf, StructBaseSize);

  Utils.CopyMem(FileNameBufSize, PWideChar(SrcInfo.FileName), FileNameBuf);

  BytesWritten := StructBaseSize + FileNameBufSize;
end; // .function ConvertFileInfoStruct

function Hook_NtQueryDirectoryFile (OrigFunc: WinNative.TNtQueryDirectoryFile; FileHandle: HANDLE; Event: HANDLE; ApcRoutine: pointer; ApcContext: PVOID; Io: PIO_STATUS_BLOCK; Buffer: PVOID;
                                    BufLength: ULONG; InfoClass: integer (* FILE_INFORMATION_CLASS *); SingleEntry: BOOLEAN; Mask: PUNICODE_STRING; RestartScan: BOOLEAN): NTSTATUS; stdcall;
const
  ENTRIES_ALIGNMENT = 8;

type
  PPrevEntry = ^TPrevEntry;
  TPrevEntry = packed record
    NextEntryOffset: ULONG;
    FileIndex:       ULONG;
  end;

var
{Un} OpenedFile:             TOpenedFile;
{Un} FileInfo:               TFileInfo;
{n}  BufCurret:              pointer;
{n}  PrevEntry:              PPrevEntry;
     BufSize:                integer;
     BufSizeLeft:            integer;
     BytesWritten:           integer;
     IsFirstEntry:           boolean;
     Proceed:                boolean;
     TruncatedNamesStrategy: TTruncatedNamesStrategy;
     StructConvertResult:    TFileInfoConvertResult;

begin
  OpenedFile := TOpenedFile(OpenedFiles[pointer(FileHandle)]);
  FileInfo   := nil;
  BufCurret  := nil;
  PrevEntry  := nil;
  // * * * * * //
  if DebugOpt then begin
    Log.Write('VFS', 'NtQueryDirectoryFile', Format('Handle: %x. InfoClass: %s', [integer(FileHandle), WinNative.FileInformationClassToStr(InfoClass)]));
  end;

  if (OpenedFile = nil) or (Event <> 0) or (ApcRoutine <> nil) or (ApcContext <> nil) or not IsVfsOn() then begin
    result := OrigFunc(FileHandle, Event, ApcRoutine, ApcContext, Io, Buffer, BufLength, InfoClass, SingleEntry, Mask, RestartScan);
  end else begin
    int(Io.Information) := 0;
    result              := STATUS_SUCCESS;

    if RestartScan then begin
      SysUtils.FreeAndNil(OpenedFile.DirListing);
    end;

    OpenedFile.FillDirListing(Mask.ToWideStr());

    Proceed := (Buffer <> nil) and (BufLength > 0);

    // Validate buffer
    if not Proceed then begin
      result := STATUS_INVALID_BUFFER_SIZE;
    end else begin
      BufSize := Utils.IfThen(integer(BufLength) > 0, integer(BufLength), High(integer));
    end;

    // Validate information class
    if Proceed then begin
      Proceed := IsSupportedFileInformationClass(InfoClass);

      if not Proceed then begin
        result := STATUS_INVALID_INFO_CLASS;
      end;
    end;

    // Signal of scanning end, if necessary
    if Proceed then begin
      Proceed := not OpenedFile.DirListing.IsEnd;

      if not Proceed then begin
        result := STATUS_NO_MORE_FILES;
      end;
    end;

    // Scan directory
    if Proceed then begin
      BufCurret    := Buffer;
      BytesWritten := 1;

      while (BytesWritten > 0) and OpenedFile.DirListing.GetNextItem(FileInfo) do begin
        // Align next record to 8-bytes boundary from Buffer start
        BufCurret   := pointer(int(Buffer) + Alg.IntRoundToBoundary(int(Io.Information), ENTRIES_ALIGNMENT));
        BufSizeLeft := BufSize - (int(BufCurret) - int(Buffer));

        IsFirstEntry := OpenedFile.DirListing.FileInd = 1;

        if IsFirstEntry then begin
          TruncatedNamesStrategy := TRUNCATE_NAMES;
        end else begin
          TruncatedNamesStrategy := DONT_TRUNCATE_NAMES;
        end;

        StructConvertResult := ConvertFileInfoStruct(@FileInfo.Data, FILE_INFORMATION_CLASS(byte(InfoClass)), BufCurret, BufSizeLeft, TruncatedNamesStrategy, BytesWritten);
        //VarDump(['Converted struct to buf offset:', int(BufCurret) - int(Buffer), 'Written:', BytesWritten, 'Result:', ord(StructConvertResult)]);

        with PFILE_ID_BOTH_DIR_INFORMATION(BufCurret)^ do begin
          NextEntryOffset := 0;
          FileIndex       := 0;
        end;

        if StructConvertResult = TOO_SMALL_BUF then begin
          OpenedFile.DirListing.SeekRel(-1);

          if IsFirstEntry then begin
            result := STATUS_BUFFER_TOO_SMALL;
          end;
        end else if StructConvertResult = TRUNCATED_NAME then begin
          if IsFirstEntry then begin
            result := STATUS_BUFFER_OVERFLOW;
            Inc(int(Io.Information), BytesWritten);
          end else begin
            OpenedFile.DirListing.SeekRel(-1);
          end;
        end else if StructConvertResult = COPIED_ALL then begin
          if PrevEntry <> nil then begin
            int(Io.Information) := int(BufCurret) - int(Buffer) + BytesWritten;
          end else begin
            int(Io.Information) := BytesWritten;
          end;
        end; // .else

        if (BytesWritten > 0) and (PrevEntry <> nil) then begin
          PrevEntry.NextEntryOffset := cardinal(int(BufCurret) - int(PrevEntry));
        end;

        PrevEntry := BufCurret;

        //Msg(Format('Written: %d. Total: %d', [BytesWritten, int(Io.Information)]));

        if SingleEntry then begin
          BytesWritten := 0;
        end;
      end; // .while
    end; // .if    

    Io.Status.Status := result;
  end; // .else

  if DebugOpt then begin
    Log.Write('VFS', 'NtQueryDirectoryFile', Format('Status: %x. Written: %d bytes', [integer(result), integer(Io.Information)]));
  end;
end; // .function Hook_NtQueryDirectoryFile

function Hook_SetCurrentDirectoryW (OrigFunc: pointer; lpPathName: PWideChar): LONGBOOL; stdcall;
var
  UseRedirection: boolean;
  DirPath:        WideString;
  RedirectedPath: WideString;
  OrigPathA:      string;
  CurrDir:        WinNative.PCURDIR;
  PrevDisableVfsForThisThread: boolean;
  //DirInfo:        Windows.TWin32FindDataW;

begin
  // * * * * * //
  DirPath := NormalizePath(lpPathName);
  //VarDump(['Set current dir', DirPath]);

  //VarDump(['Set current dir to: ', lpPathName, 'Current is by handle: ', ExpandPath(GetFilePathByHandle(GetTeb().Peb.ProcessParameters.CurrentDirectory.Handle))]);

  with CurrDirCritSection do begin
    Enter;

    PrevDisableVfsForThisThread := DisableVfsForThisThread;
    DisableVfsForThisThread     := true;

    result := NativeSetCurrentDirectoryW(PWideChar(DirPath));

    DisableVfsForThisThread := PrevDisableVfsForThisThread;

    // Changed to really existing directory
    if result then begin
      CurrDirIsVirtual := false;
    end else begin
      // Any need in DirInfo???
      RedirectedPath := GetVfsItemRealPath(DirPath);

      if RedirectedPath <> '' then begin
        // Maybe we should disable VFS for this call also?
        //
        result := NativeSetCurrentDirectoryW(PWideChar(DirPath));

        if result then begin
          CurrDirIsVirtual       := true;
          CurrDirRealLoweredPath := StrLib.WideLowerCase(RedirectedPath);
          CurrDirVirtPath        := DirPath;
        end;
      end;
    end;

    Leave;
  end; // .with CurrDirCritSection
  
  //VarDump(['@New current dir: ', GetCurrentDir(), '@New real dir path:', CurrDirRealLoweredPath, 'Expanding test.abc', ExpandPath('test.abc')]);

  // if not result then begin
  //   DirPath := NormalizePath(lpPathName);

  //   if DirPath <> '' then begin
  //     RedirectedPath := GetVfsItemRealPath(DirPath);
  //     VarDump(['Redirected path: ', RedirectedPath]);
  //     UseRedirection := RedirectedPath <> '';

  //     if UseRedirection then begin
  //       DirPath := DirPath + '\';

  //       RtlAcquirePebLock;
  //       CurrDir := @GetTeb().Peb.ProcessParameters.CurrentDirectory;
  //       CurrDir.DosPath.Release;
  //       CurrDir.DosPath.AssignNewStr(DirPath);
  //       RtlReleasePebLock;

  //       result := true;
  //     end;
  //   end; // .if
  // end; // .if
  // DirPath := WideStringFromBuf(ObjectAttributes.ObjectName.Buffer, ObjectAttributes.ObjectName.GetLength());

  // if DebugOpt then begin
  //   StrLib.PWideCharToAnsi(PWideChar(DirPath), OrigPathA);
  // end;

  // // Disallow empty or '\'-ending paths
  // if (FullPath = '') then begin
  //   if DebugOpt then begin
  //     Log.Write('VFS', 'NtOpenFile', 'Path: "' + OrigPathA + '". Result: ERROR_INVALID_PARAMETER');
  //   end;

  //   Windows.SetLastError(Windows.ERROR_INVALID_PARAMETER);
  //   result := Windows.INVALID_HANDLE_VALUE;
  // end
  //asm int 3 end;

  //VarDump([GetCurrentDir()]);
end; // .function Hook_SetCurrentDirectoryW

function Hook_SetCurrentDirectoryA (OrigFunc: pointer; lpPathName: pchar): LONGBOOL; stdcall;
begin
  result := Windows.SetCurrentDirectoryW(PWideChar(WideString(String(lpPathName))));
end;

function MapDir (const VirtPath, RealPath: WideString; OverwriteExisting: boolean; Flags: integer = 0): boolean;
begin
  result := _MapDir(NormalizePath(VirtPath), NormalizePath(RealPath), nil, OverwriteExisting, AUTO_PRIORITY) <> nil;
end;

procedure ResetVfs;
begin
  with VfsCritSection do begin
    Enter;

    if VfsIsRunning then begin
      OpenedFiles.Clear();
      VfsItems.Clear();
      VfsIsRunning := false;
    end;

    Leave;
  end;
end; // .procedure ResetVfs

procedure InstallVfsHooks;
var
{n} SetProcessDEPPolicyAddr: pointer;
    hDll:           Windows.THandle;
    Kernel32Handle: integer;
    User32Handle:   integer;
    NtdllHandle:    integer;

    s: string; // FIXME DELETEME
    Disasm: TDisasm;

begin
  if not VfsHooksInstalled then begin
  (* WHERE ARE VFS LOCKS???????????????? *)
    // Ensure, that Memory manager is thread safe
    System.IsMultiThread := true;

    // Ensure, that library with VFS hooks installed is never unloaded
    if System.IsLibrary then begin
      WinNative.GetModuleHandleExW(WinNative.GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS or WinNative.GET_MODULE_HANDLE_EX_FLAG_PIN, @VFS.GetRealAddress, hDll);
    end;

    Kernel32Handle := Windows.GetModuleHandle('kernel32.dll');
    User32Handle   := Windows.GetModuleHandle('user32.dll');
    NtdllHandle    := Windows.GetModuleHandle('ntdll.dll');
    FindOutRealSystemApiAddrs([Kernel32Handle, User32Handle]);

    //s := #$F0#$36#$3E#$3E#$3E#$2E#$66#$67#$90;
    //Disasm.Disassemble(pointer(s));
    //VarDump([Disasm.Len, Disasm.Opcode, Disasm.OpcodeSize, Disasm.PrefixedOpcodeSize]);
    //VarDump([TFuncCodeSizeDetector.Create().GetCodeSize(Ptr($401000))]);


    // with TPatchHelper.Init(TPatchMaker.Create) do begin
    //   WriteCode(@PatchForge.JumpTypeToNear, TMinCodeSizeDetector.Create(56), FIX_CODE_MAKE_MOVABLE);
    //   SetLength(s, Size);
    //   ApplyPatch(pointer(s));
    //   asm mov eax, s; int 3; end;
    // end;

    (* Trying to turn off DEP *)
    SetProcessDEPPolicyAddr := Windows.GetProcAddress(Kernel32Handle, 'SetProcessDEPPolicy');

    if SetProcessDEPPolicyAddr <> nil then begin
      if PatchApi.Call(PatchApi.STDCALL_, SetProcessDEPPolicyAddr, [0]) <> 0 then begin
        Log.Write('VFS', 'SetProcessDEPPolicy', 'DEP was turned off');
      end else begin
        Log.Write('VFS', 'SetProcessDEPPolicy', 'Failed to turn DEP off');
      end;
    end;

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetFileAttributesW hook');
    // NativeGetFileAttributesW := pointer(Core.p.WriteHiHook
    // (
    //   GetRealProcAddress(Kernel32Handle, 'GetFileAttributesW'),
    //   PatchApi.SPLICE_,
    //   PatchApi.EXTENDED_,
    //   PatchApi.STDCALL_,
    //   @Hook_GetFileAttributesW,
    // ).GetDefaultFunc());

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetFileAttributesA hook');
    // Core.p.WriteHiHook
    // (
    //   GetRealProcAddress(Kernel32Handle, 'GetFileAttributesA'),
    //   PatchApi.SPLICE_,
    //   PatchApi.EXTENDED_,
    //   PatchApi.STDCALL_,
    //   @Hook_GetFileAttributesA,
    // );

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing CreateFileW hook');
    // NativeCreateFileW := pointer(Core.p.WriteHiHook
    // (
    //   GetRealProcAddress(Kernel32Handle, 'CreateFileW'),
    //   PatchApi.SPLICE_,
    //   PatchApi.EXTENDED_,
    //   PatchApi.STDCALL_,
    //   @Hook_CreateFileW,
    // ).GetDefaultFunc());

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing CreateFileA hook');
    // Core.p.WriteHiHook
    // (
    //   GetRealProcAddress(Kernel32Handle, 'CreateFileA'),
    //   PatchApi.SPLICE_,
    //   PatchApi.EXTENDED_,
    //   PatchApi.STDCALL_,
    //   @Hook_CreateFileA,
    // );

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing OpenFile hook');
    // NativeOpenFile := pointer(Core.p.WriteHiHook
    // (
    //   GetRealProcAddress(Kernel32Handle, 'OpenFile'),
    //   PatchApi.SPLICE_,
    //   PatchApi.EXTENDED_,
    //   PatchApi.STDCALL_,
    //   @Hook_OpenFile,
    // ).GetDefaultFunc());
    // 

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing LoadLibraryW hook');
    // NativeLoadLibraryW := pointer(Core.p.WriteHiHook
    // (
    //   GetRealProcAddress(Kernel32Handle, 'LoadLibraryW'),
    //   PatchApi.SPLICE_,
    //   PatchApi.EXTENDED_,
    //   PatchApi.STDCALL_,
    //   @Hook_LoadLibraryW,
    // ).GetDefaultFunc());

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing LoadLibraryA hook');
    // Core.p.WriteHiHook
    // (
    //   GetRealProcAddress(Kernel32Handle, 'LoadLibraryA'),
    //   PatchApi.SPLICE_,
    //   PatchApi.EXTENDED_,
    //   PatchApi.STDCALL_,
    //   @Hook_LoadLibraryA,
    // );

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindFirstFileExW hook');
    // NativeFindFirstFileExW := VfsPatching.SpliceWinApi
    // (
    //   GetRealProcAddress(Kernel32Handle, 'FindFirstFileExW'),
    //   @Hook_FindFirstFileExW
    // );

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindFirstFileExA hook');
    // VfsPatching.SpliceWinApi
    // (
    //   GetRealProcAddress(Kernel32Handle, 'FindFirstFileExA'),
    //   @Hook_FindFirstFileExA
    // );

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindFirstFileW hook');
    // VfsPatching.SpliceWinApi
    // (
    //   GetRealProcAddress(Kernel32Handle, 'FindFirstFileW'),
    //   @Hook_FindFirstFileW
    // );

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindFirstFileA hook');
    // VfsPatching.SpliceWinApi
    // (
    //   GetRealProcAddress(Kernel32Handle, 'FindFirstFileA'),
    //   @Hook_FindFirstFileA
    // );

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindNextFileW hook');
    // NativeFindNextFileW := VfsPatching.SpliceWinApi
    // (
    //   GetRealProcAddress(Kernel32Handle, 'FindNextFileW'),
    //   @Hook_FindNextFileW
    // );

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindNextFileA hook');
    // VfsPatching.SpliceWinApi
    // (
    //   GetRealProcAddress(Kernel32Handle, 'FindNextFileA'),
    //   @Hook_FindNextFileA
    // );

    // if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing FindClose hook');
    // NativeFindClose := VfsPatching.SpliceWinApi
    // (
    //   GetRealProcAddress(Kernel32Handle, 'FindClose'),
    //   @Hook_FindClose
    // );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing SetCurrentDirectoryW hook');
    NativeSetCurrentDirectoryW := VfsPatching.SpliceWinApi
    (
      GetRealProcAddress(Kernel32Handle, 'SetCurrentDirectoryW'),
      @Hook_SetCurrentDirectoryW
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing SetCurrentDirectoryA hook');
    VfsPatching.SpliceWinApi
    (
      GetRealProcAddress(Kernel32Handle, 'SetCurrentDirectoryA'),
      @Hook_SetCurrentDirectoryA
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing NtQueryAttributesFile hook');
    NativeNtQueryAttributesFile := VfsPatching.SpliceWinApi
    (
      GetRealProcAddress(NtdllHandle, 'NtQueryAttributesFile'),
      @Hook_NtQueryAttributesFile
    );

    //asm int 3; end;

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing NtQueryFullAttributesFile hook');
    NativeNtQueryFullAttributesFile := VfsPatching.SpliceWinApi
    (
      GetRealProcAddress(NtdllHandle, 'NtQueryFullAttributesFile'),
      @Hook_NtQueryFullAttributesFile
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing NtOpenFile hook');
    NativeNtOpenFile := VfsPatching.SpliceWinApi
    (
      GetRealProcAddress(NtdllHandle, 'NtOpenFile'),
      @Hook_NtOpenFile
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing NtCreateFile hook');
    NativeNtCreateFile := VfsPatching.SpliceWinApi
    (
      GetRealProcAddress(NtdllHandle, 'NtCreateFile'),
      @Hook_NtCreateFile
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing NtClose hook');
    NativeNtClose := VfsPatching.SpliceWinApi
    (
      GetRealProcAddress(NtdllHandle, 'NtClose'),
      @Hook_NtClose
    );

    if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing NtQueryDirectoryFile hook');
    NativeNtQueryDirectoryFile := VfsPatching.SpliceWinApi
    (
      GetRealProcAddress(NtdllHandle, 'NtQueryDirectoryFile'),
      @Hook_NtQueryDirectoryFile
    );

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
s: string;
attr: OBJECT_ATTRIBUTES;
ustr:UNICODE_STRING;
hDir: THandle;
io: IO_STATUS_BLOCK;
FileInfo: TNativeFileInfo;
w: TWin32FindDataW;
FileName: WideString;
fifo: TFileInfo;
Listing: TDirListing;
Exclude: TDict;
f: TOpenedFile;

begin
  {!} Assert(aModList <> nil);
  // * * * * * //
  InitModList(aModList);
  //BuildVfsSnapshot();
  DebugOpt := true; // FIXME!!!

  //VarDump([GetFileList('D:\Heroes 3\*', FILES_AND_DIRS).ToText(#13#10)]);
  ResetVfs;
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Phoenix', DONT_OVERWRITE_EXISTING);
  //MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\BattleHeroes', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Morn battlefields', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Big Spellbook', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Vallex Portraits', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\New Music Pack', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Dlg_ExpaMon', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Yona', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Fast Battle Animation', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\WoG_Native_Dialogs', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Quick Savings', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Secondary Skills Scrolling', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\WoG Rus', DONT_OVERWRITE_EXISTING);
  MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\WoG', DONT_OVERWRITE_EXISTING);

  // {!} Assert(VfsItems[WideStrToCaselessKey(NormalizePath('D:\Heroes 3\Data\s\29 wog - henchmen.erm'))] <> nil);
  // VarDump([TVfsItem(VfsItems[WideStrToCaselessKey(NormalizePath('D:\Heroes 3\Data\s\29 wog - henchmen.erm'))]).Info.Base.EndOfFile.LowPart]);
  // halt(0);
  //MapDir('D:\Heroes 3', 'D:\Heroes 3\Mods\Dev', DONT_OVERWRITE_EXISTING);
  RunVfs(SORT_FIFO);

  with SysScanDir('D:\Heroes 3\Data', '*') do begin
    while IterNext(FileName, @FileInfo.Base) do begin
      VarDump([FileName + ': ' + IntToStr(FileInfo.Base.EndOfFile.LowPart)]);
    end;
  end;

  halt(0);

  // if SysOpenFile(ToNtAbsPath(), hDir) then begin
  //   NtClose(hDir);
  //   halt(0);
  //   // f := TOpenedFile.Create(hDir, 'D:\Heroes 3\Data');
  //   // f.FillDirListing('*');
  //   // {!} Assert(f.DirListing <> nil);

  //   // while f.DirListing.GetNextItem(fifo) do begin
  //   //   VarDump([fifo.Data.FileName + ' ' + IntToStr(fifo.Data.Base.EndOfFile.LowPart)]);
  //   // end;
  // end;

  //
  // DO NOT LIST . and .. for ROOT DRIVES in NtQueryDirectoryFile
  //

  //VarDump([GetFileList('D:\Heroes 3\*', FILES_AND_DIRS).ToText(#13#10)]);
  //halt(0);
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
 tof: WIN32_FILE_ATTRIBUTE_DATA;
  hDll: Windows.THandle;

initialization
  VfsCritSection.Init;
  FileSearchCritSection.Init;
  CurrDirCritSection.Init;

  AssertErrorProc := AssertHandler;

  ModList       := Lists.NewSimpleStrList;
  VfsItems      := DataLib.NewDict(Utils.OWNS_ITEMS, DataLib.CASE_SENSITIVE);
  OpenedFiles   := DataLib.NewObjDict(Utils.OWNS_ITEMS);
  CachedPaths   := DataLib.NewDict(Utils.OWNS_ITEMS, DataLib.CASE_INSENSITIVE);
  SearchHandles := DataLib.NewObjDict(Utils.OWNS_ITEMS);

  DllRealApiAddrs := DataLib.NewObjDict(Utils.OWNS_ITEMS);



  // s := '';
  
  // with Files.Locate('D:\Heroes 3\Data\s\*.erm', FILES_AND_DIRS) do begin
  //   while (FindNext()) do begin
  //     s := s + GetFoundName + ' ' + inttostr(GetFoundRec().Rec.Size) + #13#10;
  //   end;
  // end;

  // Msg(s);

  // if SysUtils.FileExists('D:\Heroes 3\Data\s\38 wog - first money.erm') then begin
  //   Files.ReadFileContents('D:\Heroes 3\Data\s\38 wog - first money.erm', s);
  // end;
  
  // Msg(s);

  // s := '';

  // with (TVfsItem(VfsItems[WideStrToCaselessKey('D:\Heroes 3\Data\s')]).Children) do begin
  //   for i := 0 to Count - 1 do begin
  //     s := s + TVfsItem(Items[i]).RealPath + ' ' + inttostr(TVfsItem(Items[i]).Priority) + #13#10;
  //   end;
  // end;

  // Msg(s);

  //Msg(Utils.IfThen(MatchW('tests24523', 't?s*23'), 'Match', 'Not Match'));

  //RedirectFile('D:/heroes 3/h3ERA.exe', 'D:\soft\games\heroes3\era\h3era.exe', OVERWRITE_EXISTING);

  //Core.KillThisProcess();
end.
