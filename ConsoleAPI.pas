unit ConsoleAPI;
{!INFO
NAME = 'Console API'
VERSION = '1.0'
AUTHOR = 'Berserker'
}

interface
uses Windows, SysUtils;

type
  PWin32Cell = ^TWin32Cell;
  TWin32Cell = packed record
    Ch:     WORD;
    Attr:   WORD;
  end;

  TMenu = record
    Count: INTEGER;
    Start: PAnsiString;
    Interval: INTEGER;
  end; // .record TMenu
  
  PMenu = ^TMenu;

  TConsoleBuffer = record
    Width, Height: INTEGER;
  end; // .record TConsoleBuffer

  TCodePage = (cp1251, cp866);

  TConsole = class
    {*} protected {*}
    fWidth: INTEGER;
    fHeight: INTEGER;
    fBufWidth: INTEGER;
    fBufHeight: INTEGER;
    fTitle: string;
    fColor: BYTE;
    fBack: BYTE;
    TIR: TInputRecord;
    {*} public {*}
    { FIELDS }
    hIn, hOut: INTEGER;
    hWnd: INTEGER;
    CodePage: TCodePage;
    { PROCEDURES }
    procedure SetWindowSize(NewWidth, NewHeight: INTEGER);
    procedure SetWidth(NewWidth: INTEGER);
    procedure SetHeight(NewHeight: INTEGER);
    procedure SetBufferSize(NewWidth, NewHeight: INTEGER);
    procedure SetBufferWidth(NewWidth: INTEGER);
    procedure SetBufferHeight(NewHeight: INTEGER);
    function  GetTitle: string;
    procedure SetTitle(NewTitle: string);
    procedure SetColors(NewColor, NewBack: BYTE);
    procedure SetColor(NewColor: BYTE);
    procedure SetBack(NewBack: BYTE);
    procedure Clear;
    procedure GotoXY(x,y: INTEGER);
    procedure GotoX(x: INTEGER);
    procedure GotoY(y: INTEGER);
    function  WhereX: INTEGER;
    function  WhereY: INTEGER;
    procedure SetWindowPos(x,y: INTEGER);
    procedure MoveWindow(dx, dy: INTEGER);
    procedure Center;
    procedure WriteC(Attr: BYTE; Txt: string);
    procedure Print(Txt: string);
    procedure SetCodePage(NewCodePage: INTEGER);
    function ReadKey: CHAR;
    function ReadCode: CHAR;
    function Read: string;
    procedure HideArea(x1, y1, x2, y2: INTEGER; col, back: BYTE);
    function Menu(TitleCol, TitleBack, ItemCol, ItemBack, ChosenCol, ChosenBack,
                          FooterCol, FooterBack: BYTE;
                          const Title: string; const Footer: string; Items: PMenu): INTEGER;
    constructor Create(Title: string; WinWidth, WinHeight, BufWidth, BufHeight: INTEGER);
    { PROPERTIES }
    property Width: INTEGER READ fWidth WRITE SetWidth;
    property Height: INTEGER READ fHeight WRITE SetHeight;
    property BufWidth: INTEGER READ fBufWidth WRITE SetBufferWidth;
    property BufHeight: INTEGER READ fBufHeight WRITE SetBufferHeight;
    property Title: string READ GetTitle WRITE SetTitle;
    property Color: BYTE READ fColor WRITE SetColor;
    property Back: BYTE READ fBack WRITE SetBack;
    property CurX: INTEGER READ WhereX WRITE GotoX;
    property CurY: INTEGER READ WhereY WRITE GotoY;
  end; // .class TConsole

implementation

function PackColors(col, back: BYTE): BYTE;inline;
begin
  result:=(back shl 4) or col;
end; // .function PackColors

procedure TConsole.SetWindowSize(NewWidth, NewHeight: INTEGER);
var
  sr: TSmallRect;

begin
  fWidth:=NewWidth;
  fHeight:=NewHeight;
  sr.Left:=0;
  sr.Top:=0;
  sr.Right:=NewWidth-1;
  sr.Bottom:=NewHeight-1;
  SetConsoleWindowInfo(hOut, TRUE, sr);
end; // .procedure TConsole.SetWindowSize

procedure TConsole.SetWidth(NewWidth: INTEGER);
begin
  fWidth:=NewWidth;
  SetWindowSize(NewWidth, Height);
end; // .procedure TConsole.SetWidth

procedure TConsole.SetHeight(NewHeight: INTEGER);
begin
  fHeight:=NewHeight;
  SetWindowSize(Width, NewHeight);
end; // .procedure TConsole.SetHeight

procedure TConsole.SetBufferSize(NewWidth, NewHeight: INTEGER);
var
  tc: TCoord;

begin
  fBufWidth:=NewWidth;
  fBufHeight:=NewHeight;
  tc.x:=NewWidth;
  tc.y:=NewHeight;
  SetConsoleScreenBufferSize(hOut, tc);
end; // .procedure TConsole.SetBufferSize

procedure TConsole.SetBufferWidth(NewWidth: INTEGER);
begin
  fBufWidth:=NewWidth;
  SetBufferSize(NewWidth, BufHeight);
end; // .procedure TConsole.SetBufferWidth

procedure TConsole.SetBufferHeight(NewHeight: INTEGER);
begin
  fBufHeight:=NewHeight;
  SetBufferSize(BufWidth, NewHeight);
end; // .procedure TConsole.SetBufferHeight

function TConsole.GetTitle: string;
var
  NewTitle: string;
  C: CHAR;

begin
  NewTitle:=fTitle;
  if CodePage=cp1251 then begin
    C:=NewTitle[1];
    NewTitle[1]:=C;
    OEMToCharBuff(@NewTitle[1], @NewTitle[1], Length(NewTitle));
  end; // .if
  result:=NewTitle;
end; // .procedure TConsole.GetTitle

procedure TConsole.SetTitle(NewTitle: string);
begin
  fTitle:=NewTitle;
  if CodePage=cp1251 then CharToOEMBuff(@NewTitle[1], @NewTitle[1], Length(NewTitle));
  Windows.SetConsoleTitle(@fTitle[1]);
end; // .procedure TConsole.SetTitle

procedure TConsole.SetColors(NewColor, NewBack: BYTE);
begin
  fColor:=NewColor;
  fBack:=NewBack;
  SetConsoleTextAttribute(hOut, PackColors(NewColor, NewBack));
end; // .procedure TConsole.SetColors

procedure TConsole.SetColor(NewColor: BYTE);
begin
  fColor:=NewColor;
  SetColors(NewColor, Back);
end; // .procedure TConsole.SetColor

procedure TConsole.SetBack(NewBack: BYTE);
begin
  fBack:=NewBack;
  SetColors(Color, NewBack);
end; // .procedure TConsole.SetBack

procedure TConsole.Clear;
var
  sr: TSmallRect;
  c: TWin32Cell;
  cr: TCoord;

begin
  sr.Left:=0;
  sr.Top:=0;
  sr.Right:=BufWidth-1;
  sr.Bottom:=BufHeight-1;
  c.Ch:=32;
  c.Attr:=PackColors(Color, Back);
  cr.x:=0;
  cr.y:=BufHeight;
  ScrollConsoleScreenBuffer(hOut, sr, @sr, cr, TCharInfo(c));
  GotoXY(0,0);
end; // .procedure TConsole.Clear

procedure TConsole.GotoXY(x, y: INTEGER);
var
  pos: TCoord;

begin
  pos.x:=x;
  pos.y:=y;
  SetConsoleCursorPosition(hOut, pos);
end; // .procedure TConsole.GotoXY

procedure TConsole.GotoX(x: INTEGER);
begin
  GotoXY(x, CurY);
end; // .procedure TConsole.GotoX

procedure TConsole.GotoY(y: INTEGER);
begin
  GotoXY(CurX, y);
end; // .procedure TConsole.GotoY

function TConsole.WhereX: INTEGER;
var
  info: TConsoleScreenBufferInfo;

begin
  GetConsoleScreenBufferInfo(hOut, info);
  result:=info.dwCursorPosition.x;
end; // .function TConsole.WhereX

function TConsole.WhereY: INTEGER;
var
  info: TConsoleScreenBufferInfo;

begin
  GetConsoleScreenBufferInfo(hOut, info);
  result:=info.dwCursorPosition.y;
end; // .function TConsole.WhereY

function GetConsoleWindow: INTEGER; external 'kernel32.dll' NAME 'GetConsoleWindow';

procedure TConsole.SetWindowPos(x, y: INTEGER);
var
  r: TRect;

begin
  GetWindowRect(hWnd, r);
  Windows.MoveWindow(hWnd, x, y, r.Right-r.Left+1, r.Bottom-r.Top+1, TRUE);
end; // .procedure TConsole.SetWindowPos

procedure TConsole.MoveWindow(dx, dy: INTEGER);
var
  r: TRect;

begin
  GetWindowRect(hWnd, r);
  Windows.MoveWindow(hWnd, r.Left+dx, r.Top+dy, r.Right-r.Left+1, r.Bottom-r.Top+1, TRUE);
end; // .procedure TConsole.MoveWindow

procedure TConsole.Center;
var
  r: TRect;
  w, h, x, y: INTEGER;

begin
  GetWindowRect(hWnd, r);
  w:=r.Right-r.Left+1;
  h:=r.Bottom-r.Top+1;
  GetWindowRect(GetDesktopWindow, r);
  x:=(r.Right-r.Left-w) div 2;
  y:=(r.Bottom-r.Top-h) div 2;
  if y<0 then y:=0;
  if (x>0) then Windows.MoveWindow(hWnd, x, y, w, h, TRUE);
end; // .procedure TConsole.Center

procedure TConsole.WriteC(Attr: BYTE; Txt: string);
var
  col: BYTE;

begin
  col:=Color;
  SetColors(Attr and $0F, (Attr and $F0) shr 4);
  if CodePage=cp1251 then begin
    CharToOEMBuff(@Txt[1], @Txt[1], Length(Txt));
    Write(Txt);
    OEMToCharBuff(@Txt[1], @Txt[1], Length(Txt));
  end // .if 
  else begin
    Write(Txt);
  end; // .else 
  Color:=col;
end; // .procedure TConsole.WriteC

procedure TConsole.Print(Txt: string);
begin
  if CodePage=cp1251 then begin
    CharToOEMBuff(@Txt[1], @Txt[1], Length(Txt));
    Write(Txt);
    OEMToCharBuff(@Txt[1], @Txt[1], Length(Txt));
  end // .if 
  else begin
    Write(Txt);
  end; // .else 
end; // .procedure TConsole.Print

procedure TConsole.SetCodePage(NewCodePage: INTEGER);
begin
  CodePage:=TCodePage(NewCodePage);
end; // .procedure SetCodePage

function TConsole.ReadKey: CHAR;
var
  Temp: INTEGER;
  
begin
  repeat
    Windows.ReadConsoleInput(hIn, TIR, 1, DWORD(Temp));
  until ((TIR.EventType and 1)=1) and TIR.Event.KeyEvent.bKeyDown;
  result:=TIR.Event.KeyEvent.AsciiChar;
end; // .function ReadKey

function TConsole.ReadCode: CHAR;
begin
  result:=CHAR(BYTE(TIR.Event.KeyEvent.wVirtualScanCode));
end; // .function ReadCode

function TConsole.Read: string;
var
  s: string;
  temp: INTEGER;

begin
  SetLength(s, 256);
  Windows.ReadFile(hIn, s[1], 256, DWORD(temp), nil);
  SetLength(s, temp);
  result:=s;
end; // .function Read

procedure TConsole.HideArea(x1, y1, x2, y2: INTEGER; col, back: BYTE);
var
  tc: TCoord;
  temp: INTEGER;
  
begin
  tc.x:=x1;
  tc.y:=y1;
  FillConsoleOutputAttribute(hOut, PackColors(col, back), (y2-y1+1)*(Width)-x1, tc, DWORD(temp));
end; // .procedure HideArea

function TConsole.Menu(TitleCol, TitleBack, ItemCol, ItemBack, ChosenCol, ChosenBack, FooterCol, FooterBack: BYTE;
                      const Title: string; const Footer: string; Items: PMenu): INTEGER;

const
  C_ERROR_TOO_LARGE = -2;
  C_ESC = -1;

var
  Count: INTEGER; // Кол-во элементов меню
  X, Y: INTEGER; // Верхняя граница первого элемента меню
  index: INTEGER;
  bExit: BOOLEAN;
  i: INTEGER;
  C: CHAR;
  Chosen: INTEGER;
  P: POINTER;
  Interval: INTEGER;
  
begin
  // Определяем, а вместится ли наше меню в экран, если нет, то не отображаем
  Count:=Items^.Count;
  P:=Items^.Start;
  Interval:=Items^.Interval;
  if (Height-CurY-Count-2)<0 then begin
    result:=C_ERROR_TOO_LARGE; exit;
  end; // .if 
  X:=CurX;
  WriteC(PackColors(TitleCol, TitleBack), Title); GotoXY(X, CurY+1);
  Y:=CurY;
  index:=0;
  bExit:=FALSE; 
  while not bExit do begin
    // Цикл отрисовки
    GotoXY(X, Y);
    for i:=0 to Count-1 do begin
      if i=index then begin
        WriteC(PackColors(ChosenCol, ChosenBack), PAnsiString(INTEGER(P)+i*Interval)^); GotoXY(X, Y+i+1);
      end // .if 
      else begin
        WriteC(PackColors(ItemCol, ItemBack), PAnsiString(INTEGER(P)+i*Interval)^); GotoXY(X, Y+i+1);
      end; // .else 
    end; // .for - конец цикла отрисовки
    WriteC(PackColors(FooterCol, FooterBack), Footer);
    // Цикл чтения клавиатуры
    while TRUE do begin
      C:=ReadKey;
      case C of 
        #13:
          begin
            Chosen:=index;
            bExit:=TRUE;
            BREAK;
          end;
        #27:
          begin
            Chosen:=-1;
            bExit:=TRUE;
            BREAK;
          end;
        #0:
          begin
            C:=ReadCode;
            case C of 
              #72:
                begin
                  DEC(index);
                  if index<0 then begin
                    index:=Count-1;
                  end; // .if 
                  BREAK;
                end;
              #80:
                begin
                  INC(index);
                  if index=Count then begin
                    index:=0;
                  end; // .if 
                  BREAK;
                end;
            end; // .case 
          end;
      end; // .case C
    end; // .while 
  end; // .while
  // Очищаем экран от меню и восстанавливаем положение курсора
  DEC(Y);
  HideArea(X, Y, Width-1, Y+Count+1, Self.Back, Self.Back);
  GotoXY(X, Y);
  result:=Chosen;
end; // .function Menu

constructor TConsole.Create(Title: string; WinWidth, WinHeight, BufWidth, BufHeight: INTEGER);
begin
  AllocConsole;
  hWnd:=GetConsoleWindow;
  hIn:=GetStdHandle(STD_INPUT_HANDLE);
  hOut:=GetStdHandle(STD_OUTPUT_HANDLE);
  PInteger(@Input)^:=hIn;
  PInteger(@Output)^:=hOut;
  CodePage:=cp1251;
  Self.Title:=Title;
  SetBufferSize(BufWidth, BufHeight);
  SetWindowSize(WinWidth, WinHeight);
  SetColors(15, 0);
  Windows.ShowWindow(hWnd, SW_NORMAL);
end; // .constructor TConsole.Create

begin
end.
