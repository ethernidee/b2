unit WinWrappers;
{
DESCRIPTION:  Correct wrappers for many Windows/SysUtils/... functions
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Windows, SysUtils;

const
  INVALID_HANDLE  = -1;


function  FileCreate (const FilePath: string; (* i *) out hFile: INTEGER): BOOLEAN;
function  FileOpen (const FilePath: string; OpenMode: INTEGER; out hFile: INTEGER): BOOLEAN;
function  GetFileSize (hFile: INTEGER; out FileSizeL, FileSizeH: INTEGER): BOOLEAN;
function  FileRead (hFile: INTEGER; var Buffer; StrictCount: INTEGER): BOOLEAN;
function  GetModuleHandle (const ModuleName: string; out hModule: INTEGER): BOOLEAN;
function  FindResource (hModule: INTEGER; const ResName: string; ResType: PCHAR; out hResource: INTEGER): BOOLEAN;
function  LoadResource (hModule, hResource: INTEGER; out hMem: INTEGER): BOOLEAN;
function  LockResource (hMem: INTEGER; out ResData: POINTER): BOOLEAN;
function  SizeOfResource (hResource, hInstance: INTEGER; out ResSize: INTEGER): BOOLEAN;
function  FindFirstFile (const Path: string; out hSearch: INTEGER; out FindData: Windows.TWin32FindData): BOOLEAN;
function  FindNextFile (hSearch: INTEGER; var FindData: Windows.TWin32FindData): BOOLEAN;


(***) implementation (***)


function FileCreate (const FilePath: string; (* i *) out hFile: INTEGER): BOOLEAN;
begin
  hFile   :=  SysUtils.FileCreate(FilePath);
  result  :=  hFile <> INVALID_HANDLE;
end; // .function FileCreate

function FileOpen (const FilePath: string; OpenMode: INTEGER; (* i *) out hFile: INTEGER): BOOLEAN;
begin
  hFile   :=  SysUtils.FileOpen(FilePath, OpenMode);
  result  :=  hFile <> INVALID_HANDLE;
end; // .function FileOpen

function GetFileSize (hFile: INTEGER; out FileSizeL, FileSizeH: INTEGER): BOOLEAN;
begin
  FileSizeL :=  Windows.GetFileSize(hFile, @FileSizeH);
  result    :=  FileSizeL <> -1;
end; // .function GetFileSize

function FileRead (hFile: INTEGER; var Buffer; StrictCount: INTEGER): BOOLEAN;
begin
  result  :=  SysUtils.FileRead(hFile, Buffer, StrictCount) = StrictCount;
end; // .function FileRead

function GetModuleHandle (const ModuleName: string; out hModule: INTEGER): BOOLEAN;
begin
  hModule :=  Windows.GetModuleHandle(PCHAR(ModuleName));
  result  :=  hModule <> 0;
end; // .function GetModuleHandle

function FindResource (hModule: INTEGER; const ResName: string; ResType: PCHAR; out hResource: INTEGER): BOOLEAN;
begin
  hResource :=  Windows.FindResource(hModule, PCHAR(ResName), ResType);
  result    :=  hResource <> 0;
end; // .function FindResource

function LoadResource (hModule, hResource: INTEGER; out hMem: INTEGER): BOOLEAN;
begin
  hMem    :=  Windows.LoadResource(hModule, hResource);
  result  :=  hMem <> 0;
end; // .function LoadResource

function LockResource (hMem: INTEGER; out ResData: POINTER): BOOLEAN;
begin
  {!} Assert(ResData = nil);
  ResData :=  Windows.LockResource(hMem);
  result  :=  ResData <> nil;
end; // .function LockResource

function SizeOfResource (hResource, hInstance: INTEGER; out ResSize: INTEGER): BOOLEAN;
begin
  ResSize :=  Windows.SizeOfResource(hResource, hInstance);
  result  :=  ResSize <> 0;
end; // .function SizeOfResource

function FindFirstFile (const Path: string; out hSearch: INTEGER; out FindData: Windows.TWin32FindData): BOOLEAN;
begin
  hSearch :=  Windows.FindFirstFile(PCHAR(Path), FindData);
  result  :=  hSearch <> INVALID_HANDLE;
end; // .function FindFirstFile

function FindNextFile (hSearch: INTEGER; var FindData: Windows.TWin32FindData): BOOLEAN;
begin
  result  :=  Windows.FindNextFile(hSearch, FindData);
end; // .function FindNextFile

end.
