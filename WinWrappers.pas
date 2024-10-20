unit WinWrappers;
{
DESCRIPTION:  Correct wrappers for many Windows/SysUtils/... functions
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Windows, SysUtils;

const
  INVALID_HANDLE  = -1;


function  FileCreate (const FilePath: string; (* i *) out hFile: integer): boolean;
function  FileOpen (const FilePath: string; OpenMode: integer; out hFile: integer): boolean;
function  GetFileSize (hFile: integer; out FileSizeL, FileSizeH: integer): boolean;
function  FileRead (hFile: integer; var Buffer; StrictCount: integer): boolean;
function  GetModuleHandle (const ModuleName: string; out hModule: integer): boolean;
function  FindResource (hModule: integer; const ResName: string; ResType: pchar; out hResource: integer): boolean;
function  LoadResource (hModule, hResource: integer; out hMem: integer): boolean;
function  LockResource (hMem: integer; out ResData: pointer): boolean;
function  SizeOfResource (hResource, hInstance: integer; out ResSize: integer): boolean;
function  FindFirstFile (const Path: string; out hSearch: integer; out FindData: Windows.TWin32FindData): boolean;
function  FindNextFile (hSearch: integer; var FindData: Windows.TWin32FindData): boolean;
function  GetModuleFileName (hMod: HMODULE): string;


(***) implementation (***)


function FileCreate (const FilePath: string; (* i *) out hFile: integer): boolean;
begin
  hFile   :=  SysUtils.FileCreate(FilePath);
  result  :=  hFile <> INVALID_HANDLE;
end;

function FileOpen (const FilePath: string; OpenMode: integer; (* i *) out hFile: integer): boolean;
begin
  hFile   :=  SysUtils.FileOpen(FilePath, OpenMode);
  result  :=  hFile <> INVALID_HANDLE;
end;

function GetFileSize (hFile: integer; out FileSizeL, FileSizeH: integer): boolean;
begin
  FileSizeL :=  Windows.GetFileSize(hFile, @FileSizeH);
  result    :=  FileSizeL <> -1;
end;

function FileRead (hFile: integer; var Buffer; StrictCount: integer): boolean;
begin
  result  :=  SysUtils.FileRead(hFile, Buffer, StrictCount) = StrictCount;
end;

function GetModuleHandle (const ModuleName: string; out hModule: integer): boolean;
begin
  hModule :=  Windows.GetModuleHandle(pchar(ModuleName));
  result  :=  hModule <> 0;
end;

function FindResource (hModule: integer; const ResName: string; ResType: pchar; out hResource: integer): boolean;
begin
  hResource :=  Windows.FindResource(hModule, pchar(ResName), ResType);
  result    :=  hResource <> 0;
end;

function LoadResource (hModule, hResource: integer; out hMem: integer): boolean;
begin
  hMem    :=  Windows.LoadResource(hModule, hResource);
  result  :=  hMem <> 0;
end;

function LockResource (hMem: integer; out ResData: pointer): boolean;
begin
  {!} Assert(ResData = nil);
  ResData :=  Windows.LockResource(hMem);
  result  :=  ResData <> nil;
end;

function SizeOfResource (hResource, hInstance: integer; out ResSize: integer): boolean;
begin
  ResSize :=  Windows.SizeOfResource(hResource, hInstance);
  result  :=  ResSize <> 0;
end;

function FindFirstFile (const Path: string; out hSearch: integer; out FindData: Windows.TWin32FindData): boolean;
begin
  hSearch :=  Windows.FindFirstFile(pchar(Path), FindData);
  result  :=  hSearch <> INVALID_HANDLE;
end;

function FindNextFile (hSearch: integer; var FindData: Windows.TWin32FindData): boolean;
begin
  result  :=  Windows.FindNextFile(hSearch, FindData);
end;

function GetModuleFileName (hMod: HMODULE): string;
const
  INITIAL_BUF_SIZE = 1000;

begin
  SetLength(result, INITIAL_BUF_SIZE);
  SetLength(result, Windows.GetModuleFileName(hMod, @result[1], Length(result)));

  if (Length(result) > INITIAL_BUF_SIZE) and
     (Windows.GetModuleFileName(hMod, @result[1], Length(result)) <> cardinal(Length(result)))
  then begin
    result := '';
  end;
end;

end.
