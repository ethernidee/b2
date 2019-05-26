unit WinUtils;

interface
uses
  Windows;

const
  OWNS_BUF    = true;
  HIDE_CURSOR = true;

  WAIT_ALL           = true;
  WAIT_SINGLE_OBJECT = not WAIT_ALL;

type  
  PBgraPix = ^TBgraPix;
  TBgraPix = packed record
    case boolean of
      false: (Value: integer);
      true:
      (
        Blue:  byte;
        Green: byte;
        Red:   byte;
        Alpha: byte;
      );
  end; // .record TBgraPix
  
  PBgraBuf = ^TBgraBuf;
  TBgraBuf = array [0..MAXLONGINT div sizeof(TBgraPix) - 1] of TBgraPix;

  TRawImage = class
   protected
         fWidth:   integer;
         fHeight:  integer;
    {?n} fBuf:     PBgraBuf;
         fOwnsBuf: boolean;

   public
    constructor Create({?} aBuf: PBgraBuf; aOwnsBuf: boolean; aWidth, aHeight: integer);
    destructor  Destroy; override;

    property Buf:    {?} PBgraBuf read fBuf;
    property Width:  integer read fWidth;
    property Height: integer read fHeight;
  end; // .class TRawImage

  TWaitResult = (WR_WAITED, WR_ABANDONED_MUTEX, WR_TIMEOUT, WR_FAILED, WR_UNKNOWN);

function IsValidHandle (Handle: THandle): boolean; inline;
function TakeScreenshot (hWnd: THandle; HideCursor: boolean; out Res: TRawImage): boolean;
function GetExePath: WideString;
function GetCurrentDirW: WideString;
function SetCurrentDirW (const DirPath: WideString): boolean;
function GetLongPathNameW (lpszShortPath, lpszLongPath: PWideChar; cchBuffer: integer): integer; stdcall; external 'kernel32.dll';
function GetLongPathW (const FilePath: WideString; Success: pboolean = nil): WideString;
function WaitForObjects (Objects: array of THandle; out ResObject: THandle; TimeoutMsec: integer = integer(INFINITE); WaitAll: boolean = false): TWaitResult;

(* Returns UTC time in msec since Jan 1, 1970 *)
function GetMicroTime: Int64;

(* Returns path to system directory or empty string on error *)
function GetSysDirW: WideString;


(***)  implementation  (***)


uses Utils, Concur;

var
  ExePathW: WideString = '';
  ExeDirW:  WideString = '';
  StaticCritSection: Concur.TCritSection;


function IsValidHandle (Handle: THandle): boolean; inline;
begin
  result := (Handle <> 0) and (Handle <> INVALID_HANDLE_VALUE);
end;

constructor TRawImage.Create ({?} aBuf: PBgraBuf; aOwnsBuf: boolean; aWidth, aHeight: integer);
begin
  {!} Assert(aWidth >= 0);
  {!} Assert(aHeight >= 0);
  {!} Assert(Utils.IsValidBuf(aBuf, aWidth and aHeight));
  fBuf     := aBuf;
  fOwnsBuf := aOwnsBuf;
  fWidth   := aWidth;
  fHeight  := aHeight;
end; // .constructor TRawImage.Create

destructor TRawImage.Destroy;
begin
  if fOwnsBuf and (fBuf <> nil) then begin
    FreeMem(fBuf); fBuf := nil;
  end;
end; // .destructor TRawImage.Destroy

function TakeScreenshot (hWnd: THandle; HideCursor: boolean; out Res: TRawImage): boolean;
const
  BITS_PER_PIXEL  = 32;
  BYTES_PER_PIXEL = BITS_PER_PIXEL div 8;

var
{U} Buf:                 PBgraBuf;
    BufSize:             integer;
    WindowDeviceContext: HDC;
    ImageDeviceContext:  HDC;
    WindowInfo:          TWindowInfo;
    ScreenWidth:         integer;
    ScreenHeight:        integer;
    VisibleRect:         TRect;
    VisibleRectWidth:    integer;
    VisibleRectHeight:   integer;
    ImageHandle:         HBITMAP;
    BitmapInfo:          TBitmapInfo;
    OldImage:            HGDIOBJ;

  function IsWndVisible: boolean;
  begin
    result := (WindowInfo.rcWindow.Right  >= 0) and (WindowInfo.rcWindow.Left < ScreenWidth) and
              (WindowInfo.rcWindow.Bottom >= 0) and (WindowInfo.rcWindow.Top  < ScreenHeight)
  end;

  procedure GetVisibleRect;
  begin
    if WindowInfo.rcWindow.Left < 0 then begin
      VisibleRect.Left := Abs(WindowInfo.rcWindow.Left);
    end else begin
      VisibleRect.Left := 0;
    end;

    VisibleRect.Right := WindowInfo.rcWindow.Right - WindowInfo.rcWindow.Left;

    if WindowInfo.rcWindow.Right >= ScreenWidth then begin
      VisibleRect.Right := VisibleRect.Right + ScreenWidth - 1 - WindowInfo.rcWindow.Right;
    end;

    if WindowInfo.rcWindow.Top < 0 then begin
      VisibleRect.Top := Abs(WindowInfo.rcWindow.Top);
    end else begin
      VisibleRect.Top := 0;
    end;

    VisibleRect.Bottom := WindowInfo.rcWindow.Bottom - WindowInfo.rcWindow.Top;

    if WindowInfo.rcWindow.Bottom >= ScreenHeight then begin
      VisibleRect.Bottom := VisibleRect.Bottom + ScreenHeight - 1 - WindowInfo.rcWindow.Bottom;
    end;
  end; // .procedure GetVisibleRect

  procedure DrawCursor;
  var
    hCursor:    HICON;
    CursorInfo: TCursorInfo;
    IconInfo:   TIconInfo;

  begin
    if not HideCursor then begin
      CursorInfo.cbSize := sizeof(CursorInfo);
      GetCursorInfo(CursorInfo);

      if CursorInfo.flags = CURSOR_SHOWING then begin
        hCursor := CopyIcon(CursorInfo.hCursor);

        if hCursor <> 0 then begin
          GetIconInfo(hCursor, IconInfo);
          DrawIcon(ImageDeviceContext,
                   CursorInfo.ptScreenPos.x - WindowInfo.rcWindow.Left - VisibleRect.Left
                     - integer(IconInfo.xHotspot),
                   CursorInfo.ptScreenPos.y - WindowInfo.rcWindow.Top - VisibleRect.Top
                     - integer(IconInfo.yHotspot),
                   CursorInfo.hCursor);
          DestroyIcon(hCursor);
        end;
      end; // .if
    end; // .if
  end; // .procedure DrawCursor

begin
  {!} Assert(Res = nil);
  Buf := nil;
  // * * * * * //
  WindowDeviceContext := GetWindowDC(hWnd);
  result              := (WindowDeviceContext <> 0) and GetWindowInfo(hWnd, WindowInfo);

  if result then begin
    ScreenWidth  := GetDeviceCaps(WindowDeviceContext, HORZRES);
    ScreenHeight := GetDeviceCaps(WindowDeviceContext, VERTRES);
    result       := IsWndVisible;

    if result then begin
      GetVisibleRect;
      VisibleRectWidth  := VisibleRect.Right  - VisibleRect.Left + 1;
      VisibleRectHeight := VisibleRect.Bottom - VisibleRect.Top  + 1;
      BufSize           := VisibleRectWidth * VisibleRectHeight * BYTES_PER_PIXEL;
      GetMem(Buf, BufSize);
      Res := TRawImage.Create(Buf, OWNS_BUF, VisibleRectWidth, VisibleRectHeight);

      BitmapInfo.bmiHeader.biSize         := sizeof(BITMAPINFOHEADER);
      BitmapInfo.bmiHeader.biWidth        := VisibleRectWidth;
      BitmapInfo.bmiHeader.biHeight       := -VisibleRectHeight; // Top to bottom line scan
      BitmapInfo.bmiHeader.biPlanes       := 1;
      BitmapInfo.bmiHeader.biBitCount     := BITS_PER_PIXEL;
      BitmapInfo.bmiHeader.biCompression  := BI_RGB;
      BitmapInfo.bmiHeader.biSizeImage    := BufSize;
      BitmapInfo.bmiHeader.biClrUsed      := 0;
      BitmapInfo.bmiHeader.biClrImportant := 0;

      ImageDeviceContext := CreateCompatibleDC(WindowDeviceContext);
      ImageHandle        := CreateCompatibleBitmap(WindowDeviceContext, VisibleRectWidth,
                                                   VisibleRectHeight);
      
      OldImage := SelectObject(ImageDeviceContext, ImageHandle);
      BitBlt(ImageDeviceContext, 0, 0, VisibleRectWidth, VisibleRectHeight, WindowDeviceContext,
             VisibleRect.Left, VisibleRect.Top, SRCCOPY);
      ReleaseDC(hWnd, WindowDeviceContext); WindowDeviceContext := 0;
      DrawCursor;

      SelectObject(ImageDeviceContext, OldImage);
      GetDIBits(ImageDeviceContext, ImageHandle, 0, VisibleRectHeight, Buf, BitmapInfo,
                DIB_RGB_COLORS);

      DeleteObject(ImageHandle);
      DeleteDC(ImageDeviceContext);
    end; // .if
  end; // .if
  // * * * * * //
  if WindowDeviceContext <> 0 then begin
    ReleaseDC(hWnd, WindowDeviceContext);
  end;
end; // .function TakeScreenshot

function GetExePath: WideString;
const
  MAX_UNICODE_PATH_LEN = 32768;

var
  NumCharsCopied: integer;

begin
  with StaticCritSection do begin
    Enter;

    if ExePathW = '' then begin
      SetLength(ExePathW, MAX_UNICODE_PATH_LEN - 1);
      NumCharsCopied := Windows.GetModuleFileNameW(Windows.GetModuleHandle(nil), PWideChar(ExePathW), MAX_UNICODE_PATH_LEN);

      if NumCharsCopied > 0 then begin
        SetLength(ExePathW, NumCharsCopied);
      end else begin
        ExePathW := '';
      end;
    end;

    result := ExePathW;

    Leave;
  end; // .with
end; // .function GetExePath

function GetCurrentDirW: WideString;
var
  Buf:    array [0..32767 - 1] of WideChar;
  ResLen: integer;

begin
  result := '';
  ResLen := Windows.GetCurrentDirectoryW(sizeof(Buf), @Buf);

  if ResLen > 0 then begin
    SetLength(result, ResLen);
    Utils.CopyMem(ResLen * sizeof(WideChar), @Buf, PWideChar(result));
  end;
end;

function SetCurrentDirW (const DirPath: WideString): boolean;
begin
  result := Windows.SetCurrentDirectoryW(PWideChar(DirPath));
end;

(* Returns path as is on failure *)
function GetLongPathW (const FilePath: WideString; Success: pboolean = nil): WideString;
var
  Buf:       Utils.TArrayOfWideChar;
  ResLen:    integer;
  IsSuccess: boolean;

begin
  SetLength(Buf, 1000);
  ResLen := GetLongPathNameW(PWideChar(FilePath), pointer(Buf), Length(Buf) + 1);

  if ResLen > Length(Buf) + 1 then begin
    SetLength(Buf, ResLen - 1);
    ResLen := GetLongPathNameW(PWideChar(FilePath), pointer(Buf), Length(Buf) + 1);
  end;

  IsSuccess := (ResLen > 0) and (ResLen <= Length(Buf));

  if Success <> nil then begin
    Success^ := IsSuccess;
  end;

  if IsSuccess then begin
    SetLength(result, ResLen);
    Utils.CopyMem(ResLen * sizeof(WideChar), pointer(Buf), pointer(result));
  end else begin
    result := FilePath;
  end;
end; // .function GetLongPathW

function WaitForObjects (Objects: array of THandle; out ResObject: THandle; TimeoutMsec: integer = integer(INFINITE); WaitAll: boolean = false): TWaitResult;
var
  NumObjects: integer;
  WaitRes:    cardinal;
  i:          integer;

begin
  ResObject  := INVALID_HANDLE_VALUE;
  result     := WR_FAILED;
  i          := 0;
  NumObjects := 0;
  
  while (i < Length(Objects)) do begin
    if IsValidHandle(Objects[i]) then begin
      Objects[NumObjects] := Objects[i];
      Inc(NumObjects);
    end;

    Inc(i);
  end;

  if NumObjects > 0 then begin
    if NumObjects = 1 then begin
      WaitRes := WaitForSingleObject(Objects[0], TimeoutMsec);
    end else begin
      WaitRes := WaitForMultipleObjects(NumObjects, @Objects, WaitAll, TimeoutMsec);
    end;    

    if WaitRes < cardinal(NumObjects) then begin
      result   := WR_WAITED;
      ResObject := Objects[WaitRes - WAIT_OBJECT_0];
    end else if (WaitRes >= WAIT_ABANDONED_0) and (WaitRes <= WAIT_ABANDONED_0 + cardinal(NumObjects) - 1) then begin
      result    := WR_ABANDONED_MUTEX;
      ResObject := Objects[WaitRes - WAIT_ABANDONED_0];
    end else if WaitRes = WAIT_TIMEOUT then begin
      result := WR_TIMEOUT;
    end else if WaitRes = WAIT_FAILED then begin
      result := WR_FAILED;
    end else begin
      result := WR_UNKNOWN;
    end;
  end; // .if
end; // .function WaitForObjects

function GetMicroTime: Int64;
var
  Time: Windows.TFileTime;

begin
  Windows.GetSystemTimeAsFileTime(Time);
  result := Int64(Time.dwLowDateTime) + (Int64(Time.dwHighDateTime) shl 32) + Int64(116444736000000000);
end;

function GetSysDirW: WideString;
var
  PathLen: integer;

begin
  result  := '';
  PathLen := Windows.GetSystemDirectoryW(@PathLen, 0);

  if PathLen > 1 then begin
    SetLength(result, PathLen - 1);
    PathLen := Windows.GetSystemDirectoryW(pointer(result), PathLen);
    
    if PathLen <> Length(result) then begin
      result := '';
    end;
  end;
end; // .function GetSysDirW

initialization
  StaticCritSection.Init;
finalization
  StaticCritSection.Delete;
end.