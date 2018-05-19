unit VFS;
{
DESCRIPTION:  Virtual File System
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)

TODO: Log all redirected api address, ex. kernel32 => api-ms...
}

(***)  interface  (***)
uses
  Windows, SysUtils, Math, MMSystem,
  Utils, Crypto, Lists, DataLib, StrLib, StrUtils, Files, Log, TypeWrappers, CmdApp,
  PatchApi, Core, Ini, Concur, DlgMes (* DELETEME *);

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


const
  MODS_DIR                 = 'Mods';
  CMDLINE_ARG_MODLIST      = 'modlist';
  DEFAULT_MODLIST_FILEPATH = MODS_DIR + '\list.txt';

type
  TSearchList = class
    {O} FileList: {O} Lists.TStringList {OF Windows.PWin32FindData};
        FileInd:  integer;

    constructor Create;
    destructor  Destroy; override;
  end; // .class TSearchList


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
  
  NativeGetFileAttributes: function (FilePath: pchar): integer; stdcall;
  
  Kernel32Handle: integer;
  User32Handle:   integer;

  GamePath:   string;
  ModsDir:    string;
  CurrentDir: string;
  
  DebugOpt: boolean;

procedure Init;

implementation

const
  MAX_SEARCH_HANDLE = 1000;
  VFS_EXTRA_DEBUG   = false;

var
  SetProcessDEPPolicyAddr: pointer;

// Map of DLL handle => API name => Real api address
{O} DllRealApiAddrs: {O} TObjDict {OF TDict};
  
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
end; // .destructor TSearchList.Destroy

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
  s: string; // DELETEME

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

function IsRelativePath (const Path: string): boolean;
var
  DesignatorPos: integer;

begin
  result := not StrLib.FindChar(':', Path, DesignatorPos) and
            not StrUtils.AnsiStartsStr('\\', Path);
end; // .function IsRelativePath

procedure MakeModList;
var
{O} FileList:         Lists.TStringList;
    ModListFilePath:  string;
    ModListText:      string;
    ModName:          string;
    ModPath:          string;
    ModInd:           integer;
    i:                integer;
   
begin
  FileList := Lists.NewSimpleStrList;
  // * * * * * //
  ModList.CaseInsensitive := true;
  ModListFilePath := CmdApp.GetArg(CMDLINE_ARG_MODLIST);

  if ModListFilePath = '' then begin
    ModListFilePath := DEFAULT_MODLIST_FILEPATH;
  end; // .if
  
  if Files.ReadFileContents(ModListFilePath, ModListText) then begin
    FileList.LoadFromText(ModListText, #13#10);
    
    for i := FileList.Count - 1 downto 0 do begin
      ModName := SysUtils.ExcludeTrailingBackslash(
                  SysUtils.ExtractFileName(
                   SysUtils.Trim(FileList[i])));

      if ModName <> '' then begin
        ModPath := SysUtils.ExpandFileName
        (
          StrLib.Concat([GamePath, '\' + MODS_DIR + '\', ModName])
        );

        if not ModList.Find(ModPath, ModInd) and Files.DirExists(ModPath) then begin
          ModList.Add(ModPath);
        end; // .if
      end; // .if
    end; // .for
  end; // .if
  
  if DebugOpt then begin
    Log.Write('VFS', 'MakeModList', 'mod list:'#13#10 + ModList.ToText(#13#10));
  end; // .if
  // * * * * * //
  SysUtils.FreeAndNil(FileList);
end; // .procedure MakeModList

function FileExists (const FilePath: string): boolean;
begin
  result := NativeGetFileAttributes(pchar(FilePath)) <> -1;
end; // .function FileExists

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

function Hook_GetFullPathNameA (Hook: PatchApi.THiHook; lpFileName: pchar;
                                nBufferLength: integer; lpBuffer: pchar;
                                lpFilePart: pointer): integer; stdcall;
var
  FilePath: string;
  ApiRes:   string;

begin
  FilePath := lpFileName;

  if DebugOpt then begin
    Log.Write('VFS', 'GetFullPathNameA', 'Original: ' + FilePath);
  end;
  
  if IsRelativePath(FilePath) then begin
    {!} Windows.EnterCriticalSection(CurrDirCritSection);
    FilePath := StrLib.Concat([CurrentDir, '\', FilePath]);
    {!} Windows.LeaveCriticalSection(CurrDirCritSection);
  end;

  result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc,
                          [pchar(FilePath), nBufferLength, lpBuffer, lpFilePart]);

  if DebugOpt then begin
    if (nBufferLength > 0) and (result > 0) then begin
      System.SetString(ApiRes, lpBuffer, result);
      Log.Write('VFS', 'GetFullPathNameA', 'Result: ' + ApiRes);
    end else begin
      Log.Write('VFS', 'GetFullPathNameW', 'Required buf length: ' + IntToStr(result));
    end;
  end;
end; // .function Hook_GetFullPathNameA

function Hook_GetFullPathNameW (Hook: PatchApi.THiHook; lpFileName: PWideChar;
                                nBufferLength: integer; lpBuffer: PWideChar;
                                lpFilePart: pointer): integer; stdcall;
var
  FilePath: string;

begin
  if not StrLib.PWideCharToAnsi(lpFileName, FilePath, StrLib.FAIL_ON_ERROR) then begin
    StrLib.PWideCharToAnsi(lpFileName, FilePath);
    Log.Write('VFS', 'GetFullPathNameW', 'Unicode conversion failed. ~Original: ' + FilePath);
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [lpFileName, nBufferLength, lpBuffer, lpFilePart]);
  end else begin
    if DebugOpt then begin
      Log.Write('VFS', 'GetFullPathNameW', 'Original: ' + FilePath);
    end;
    
    if IsRelativePath(FilePath) then begin
      {!} Windows.EnterCriticalSection(CurrDirCritSection);
      FilePath := StrLib.Concat([CurrentDir, '\', FilePath]);
      {!} Windows.LeaveCriticalSection(CurrDirCritSection);
    end;
    
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [PWideChar(WideString(FilePath)), nBufferLength, lpBuffer, lpFilePart]);
    
    if DebugOpt then begin
      if (nBufferLength > 0) and (result > 0) then begin
        Log.Write('VFS', 'GetFullPathNameW', 'Result: ' + WideStringFromBuf(lpBuffer, result));
      end else begin
        Log.Write('VFS', 'GetFullPathNameW', 'Required buf length: ' + IntToStr(result));
      end;
    end;
  end; // .else
end; // .function Hook_GetFullPathNameW

function Hook_CreateFileA (Hook: PatchApi.THiHook; lpFileName: PAnsiChar;
                           dwDesiredAccess, dwShareMode: DWORD;
                           lpSecurityAttributes: PSecurityAttributes;
                           dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                           hTemplateFile: THandle): THandle; stdcall;
var
  FilePath:           string;
  RedirectedFilePath: string;
  FinalFilePath:      string;
  CreationFlags:      integer;

begin
  FilePath := lpFileName;
  
  if DebugOpt then begin
    Log.Write('VFS', 'CreateFileA', 'Original: ' + FilePath);
  end; // .if
  
  FilePath      :=  SysUtils.ExpandFileName(FilePath);
  CreationFlags :=  dwCreationDisposition;
  FinalFilePath :=  FilePath;

  if
    IsInGameDir(FilePath) and
    (
      ((CreationFlags and Windows.OPEN_EXISTING)     = Windows.OPEN_EXISTING) or
      ((CreationFlags and Windows.TRUNCATE_EXISTING) = Windows.TRUNCATE_EXISTING)
    ) and
    FindVFSPath(GameRelativePath(FilePath), RedirectedFilePath)
  then begin
    FinalFilePath := RedirectedFilePath;
  end; // .if
  
  if DebugOpt then begin
    Log.Write('VFS', 'CreateFileA', 'Redirected: ' + FinalFilePath);
  end; // .if
  
  result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc,
                          [pchar(FinalFilePath), dwDesiredAccess, dwShareMode,
                           lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
                           hTemplateFile]);
end; // .function Hook_CreateFileA

function Hook_CreateFileW (Hook: PatchApi.THiHook; lpFileName: PWideChar;
                           dwDesiredAccess, dwShareMode: DWORD;
                           lpSecurityAttributes: PSecurityAttributes;
                           dwCreationDisposition, dwFlagsAndAttributes: DWORD;
                           hTemplateFile: THandle): THandle; stdcall;
var
  FilePath:           string;
  RedirectedFilePath: string;
  FinalFilePath:      string;
  CreationFlags:      integer;

begin
  if not StrLib.PWideCharToAnsi(lpFileName, FilePath, StrLib.FAIL_ON_ERROR) then begin
    StrLib.PWideCharToAnsi(lpFileName, FilePath);
    Log.Write('VFS', 'CreateFileW', 'Unicode conversion failed. ~Original: ' + FilePath);
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile]);
  end else begin   
    if DebugOpt then begin
      Log.Write('VFS', 'CreateFileW', 'Original: ' + FilePath);
    end;
    
    FilePath      :=  SysUtils.ExpandFileName(FilePath);
    CreationFlags :=  dwCreationDisposition;
    FinalFilePath :=  FilePath;

    if
      IsInGameDir(FilePath) and
      (
        ((CreationFlags and Windows.OPEN_EXISTING)     = Windows.OPEN_EXISTING) or
        ((CreationFlags and Windows.TRUNCATE_EXISTING) = Windows.TRUNCATE_EXISTING)
      ) and
      FindVFSPath(GameRelativePath(FilePath), RedirectedFilePath)
    then begin
      FinalFilePath := RedirectedFilePath;
    end;
    
    if DebugOpt then begin
      Log.Write('VFS', 'CreateFileW', 'Redirected: ' + FinalFilePath);
    end;
    
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [PWideChar(WideString(FinalFilePath)), dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile]);
  end; // .else
end; // .function Hook_CreateFileW

function Hook_OpenFile (Hook: PatchApi.THiHook; const lpFileName: LPCSTR; var lpReOpenBuff: TOFStruct; uStyle: UINT): THandle; stdcall;
const
  NOT_SUPPORTED_FLAGS = Windows.OF_CREATE or Windows.OF_DELETE or Windows.OF_READWRITE or Windows.OF_WRITE;

var
  FilePath:           string;
  RedirectedFilePath: string;
  FinalFilePath:      string;
  Flags:              integer;

begin
  Flags    := uStyle;
  FilePath := lpFileName;

  if (Flags and Windows.OF_REOPEN) <> 0 then begin
    Flags    := Flags and not Windows.OF_REOPEN;
    FilePath := lpReOpenBuff.szPathName;
  end;
  
  if DebugOpt then begin
    Log.Write('VFS', 'OpenFile', 'Original: ' + FilePath);
  end; // .if
  
  FilePath      := SysUtils.ExpandFileName(FilePath);
  FinalFilePath := FilePath;

  if
    IsInGameDir(FilePath) and ((Flags and NOT_SUPPORTED_FLAGS) = 0) and
    FindVFSPath(GameRelativePath(FilePath), RedirectedFilePath)
  then begin
    FinalFilePath := RedirectedFilePath;
  end; // .if
  
  if DebugOpt then begin
    Log.Write('VFS', 'OpenFile', 'Redirected: ' + FinalFilePath);
  end; // .if
  
  result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [pchar(FinalFilePath), @lpReOpenBuff, Flags]);
end; // .function Hook_OpenFile

function Hook_GetFileAttributesA (Hook: PatchApi.THiHook; lpFileName: pchar): DWORD; stdcall;
var
  FilePath:           string;
  RedirectedFilePath: string;
  FinalFilePath:      string;

begin
  FilePath := lpFileName;
  
  if DebugOpt then begin
    Log.Write('VFS', 'GetFileAttributesA', 'Original: ' + FilePath);
  end; // .if
  
  FilePath      := SysUtils.ExpandFileName(FilePath);
  FinalFilePath := FilePath;

  if IsInGameDir(FilePath) and
     FindVFSPath(GameRelativePath(FilePath), RedirectedFilePath)
  then begin
    FinalFilePath := RedirectedFilePath;
  end; // .if

  if DebugOpt then begin
    Log.Write('VFS', 'GetFileAttributesA', 'Redirected: ' + FinalFilePath);
  end; // .if
  
  result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [pchar(FinalFilePath)]);
end; // .function Hook_GetFileAttributesA

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

function Hook_LoadLibraryA (Hook: PatchApi.THiHook; lpLibFileName: PAnsiChar): HMODULE; stdcall;
var
  FilePath:           string;
  RedirectedFilePath: string;
  FinalFilePath:      string;

begin
  FilePath := lpLibFileName;
  
  if DebugOpt then begin
    Log.Write('VFS', 'LoadLibraryA', 'Original: ' + FilePath);
  end; // .if

  // if dll is not found in current directory, we should preserve its original
  // unexpanded form in order kernel to search for dll in system directories 
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
    Log.Write('VFS', 'LoadLibraryA', 'Redirected: ' + FinalFilePath);
  end; // .if
  
  result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [pchar(FinalFilePath)]);
end; // .function Hook_LoadLibraryA

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

function Hook_FindFirstFileA (Hook: PatchApi.THiHook; lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindDataA): THandle; stdcall;
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
end; // .function Hook_FindFirstFileA

function Hook_FindFirstFileW (Hook: PatchApi.THiHook; lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle; stdcall;
var
  FindDataA: TWIN32FindDataA;
  FilePath:  string;
  FoundPath: string;

begin
  {!} Windows.EnterCriticalSection(FileSearchCritSection);

  if FileSearchInProgress then begin
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [lpFileName, @lpFindFileData]);
  end else if not StrLib.PWideCharToAnsi(lpFileName, FilePath, StrLib.FAIL_ON_ERROR) then begin
    StrLib.PWideCharToAnsi(lpFileName, FilePath);
    Log.Write('VFS', 'FindFirstFileW', 'Unicode conversion failed. ~Original: ' + FilePath);
    result := PatchApi.Call(PatchApi.STDCALL_, Hook.GetDefaultFunc, [lpFileName, @lpFindFileData]);
  end else begin
    if DebugOpt then begin
      Log.Write('VFS', 'FindFirstFileW', 'Original: ' + FilePath);
    end;

    result := Windows.FindFirstFileA(pchar(FilePath), FindDataA);

    if result = Windows.INVALID_HANDLE_VALUE then begin
      Log.Write('VFS', 'FindFirstFileW', 'Error: ERROR_NO_MORE_FILES');
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
        Log.Write('VFS', 'FindFirstFileW', StrLib.Concat(['Handle: ', SysUtils.IntToStr(result), #13#10, 'result: ', FoundPath]));
      end;
    end; // .else
  end; // .else
  
  {!} Windows.LeaveCriticalSection(FileSearchCritSection);
end; // .function Hook_FindFirstFileW

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

procedure Init;
begin
  GamePath   := SysUtils.ExtractFileDir(ParamStr(0));
  ModsDir    := GamePath + '\' + MODS_DIR;
  CurrentDir := GamePath;
  Windows.SetCurrentDirectory(pchar(GamePath));

  MakeModList;

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

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetFullPathNameW hook');
  Core.p.WriteHiHook
  (
    Windows.GetProcAddress(Kernel32Handle, 'GetFullPathNameW'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_GetFullPathNameW,
  );

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
  
  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing CreateDirectoryA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'CreateDirectoryA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_CreateDirectoryA,
  );

  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing RemoveDirectoryA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'RemoveDirectoryA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_RemoveDirectoryA,
  );
  
  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing DeleteFileA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'DeleteFileA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_DeleteFileA,
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
  
  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing GetCurrentDirectoryA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'GetCurrentDirectoryA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_GetCurrentDirectoryA,
  );
  
  if DebugOpt then Log.Write('VFS', 'InstallHook', 'Installing SetCurrentDirectoryA hook');
  Core.p.WriteHiHook
  (
    GetRealProcAddress(Kernel32Handle, 'SetCurrentDirectoryA'),
    PatchApi.SPLICE_,
    PatchApi.EXTENDED_,
    PatchApi.STDCALL_,
    @Hook_SetCurrentDirectoryA,
  );
end; // .procedure Init

begin
  Windows.InitializeCriticalSection(CachedPathsCritSection);
  Windows.InitializeCriticalSection(FileSearchCritSection);
  Windows.InitializeCriticalSection(CurrDirCritSection);
  
  AssertErrorProc := AssertHandler;

  ModList       := Lists.NewSimpleStrList;
  CachedPaths   := DataLib.NewDict(Utils.OWNS_ITEMS, DataLib.CASE_INSENSITIVE);
  SearchHandles := DataLib.NewObjDict(Utils.OWNS_ITEMS);
end.
