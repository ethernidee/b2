unit TextScan;
{
DESCRIPTION:  Provides high-level access to solid text in string
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Math, Utils;

type
  (* IMPORT *)
  TCharSet  = Utils.TCharSet;

  TTextScanner = class
    (***) protected (***)
      fTextBuf:         string;
      fPos:             INTEGER;  // Start from 1
      fLineN:           INTEGER;  // Start from 1
      fEndOfLineMarker: CHAR;
      fLineStartPos:    INTEGER;  // Position of EndOfLineMarker; LinePos = Pos - LineStartPos
      fTextBufLen:      INTEGER;
      fEndOfText:       BOOLEAN;
    
    (***) public (***)
      property  TextBuf:          string READ fTextBuf;
      property  Pos:              INTEGER READ fPos;          // Start from 1
      property  LineN:            INTEGER READ fLineN;        // Start from 1
      property  EndOfLineMarker:  CHAR READ fEndOfLineMarker;
      property  LineStartPos:     INTEGER READ fLineStartPos; // Position of EndOfLineMarker; LinePos = Pos - LineStartPos
      property  TextBufLen:       INTEGER READ fTextBufLen;
      property  EndOfText:        BOOLEAN READ fEndOfText;
      property  Eot:              BOOLEAN READ fEndOfText;
    
      constructor Create;
      function  IsValidPos (CheckPos: INTEGER): BOOLEAN;
      (* Returns #0 for EndOfText *)
      function  c: CHAR;
      function  GetCurrChar (out Res: CHAR): BOOLEAN;
      function  ReadChar (out Res: CHAR): BOOLEAN;
      function  GetCharAtPos (TargetPos: INTEGER; out Res: CHAR): BOOLEAN;
      function  GetCharAtRelPos (RelPos: INTEGER; out Res: CHAR): BOOLEAN;
      function  CharAtPos (TargetPos: INTEGER): CHAR;
      function  CharAtPosRel (RelPos: INTEGER): CHAR;
      function  GetSubstrAtPos (TargetPos, SubstrLen: INTEGER): string;
      function  GetSubstrAtRelPos (RelPos, SubstrLen: INTEGER): string;
      function  GotoNextChar: BOOLEAN;
      function  GotoPrevChar: BOOLEAN;
      function  GotoPos (TargetPos: INTEGER): BOOLEAN;
      function  GotoRelPos (RelPos: INTEGER): BOOLEAN;
      function  GotoNextLine: BOOLEAN;
      function  GotoPrevLine: BOOLEAN;
      function  GotoLine (TargetLine: INTEGER): BOOLEAN;
      function  GotoRelLine (RelLineN: INTEGER): BOOLEAN;
      function  SkipChars (Ch: CHAR): BOOLEAN;
      function  SkipCharset (const Charset: TCharSet): BOOLEAN;
      function  FindChar (Ch: CHAR): BOOLEAN;
      function  FindCharset (const Charset: TCharSet): BOOLEAN;
      function  ReadToken (const TokenCharset: TCharSet; out Token: string): BOOLEAN;
      function  ReadTokenTillDelim (const DelimCharset: TCharSet; out Token: string): BOOLEAN;
      procedure Connect (const TextBuf: string; EndOfLineMarker: CHAR);
      
      property Chars[CharPos: INTEGER]: CHAR READ CharAtPos; default;
      property CharsRel[RelCharPos: INTEGER]: CHAR READ CharAtPosRel;
  end; // .class TTextScanner


(***)  implementation  (***)


constructor TTextScanner.Create;
const
  EMPTY_TEXT          = '';
  END_OF_LINE_MARKER  = #10;

begin
  Self.Connect(EMPTY_TEXT, END_OF_LINE_MARKER);
end; // .constructor TTextScanner.Create

function TTextScanner.IsValidPos (CheckPos: INTEGER): BOOLEAN;
begin
  result  :=  Math.InRange(CheckPos, 1, Self.TextBufLen + 1);
end; // .function TTextScanner.IsValidPos

function TTextScanner.c: CHAR;
begin
  if Self.EndOfText then begin
    result := #0;
  end // .if
  else begin
    result := Self.TextBuf[Self.Pos];
  end; // .else
end; // .function TTextScanner.c

function TTextScanner.GetCurrChar (out Res: CHAR): BOOLEAN;
begin
  result  :=  not Self.EndOfText;
  
  if result then begin
    Res :=  Self.TextBuf[Self.Pos];
  end; // .if
end; // .function TTextScanner.GetCurrChar

function TTextScanner.ReadChar (out Res: CHAR): BOOLEAN;
begin
  result  :=  Self.GetCurrChar(Res);
  Self.GotoNextChar;
end; // .function TTextScanner.ReadChar

function TTextScanner.GetCharAtPos (TargetPos: INTEGER; out Res: CHAR): BOOLEAN;
begin
  result  :=  Self.IsValidPos(TargetPos) and (TargetPos <= Self.TextBufLen);
  if result then begin
    Res :=  Self.TextBuf[TargetPos];
  end; // .if
end; // .function TTextScanner.GetCharAtPos

function TTextScanner.GetCharAtRelPos (RelPos: INTEGER; out Res: CHAR): BOOLEAN;
begin
  result  :=  Self.GetCharAtPos(Self.Pos + RelPos, Res);
end; // .function TTextScanner.GetCharAtRelPos

function TTextScanner.CharAtPos (TargetPos: INTEGER): CHAR;
begin
  if not GetCharAtPos(TargetPos, result) then begin
    result := #0;
  end; // .if
end; // .function TTextScanner.CharAtPos.CharAtPos

function TTextScanner.CharAtPosRel (RelPos: INTEGER): CHAR;
begin
  result := CharAtPos(Self.Pos + RelPos);
end; // .function TTextScanner.CharAtPosRel

function TTextScanner.GetSubstrAtPos (TargetPos, SubstrLen: INTEGER): string;
var
  StartPos: INTEGER;
  EndPos:   INTEGER;
  
begin
  {!} Assert(SubstrLen >= 0);
  StartPos  :=  Math.EnsureRange(TargetPos, 1, Self.TextBufLen + 1);
  EndPos    :=  Math.EnsureRange(TargetPos + SubstrLen, 1, Self.TextBufLen + 1);
  result    :=  Copy(Self.TextBuf, StartPos, EndPos - StartPos);
end; // .function TTextScanner.GetSubstrAtPos

function TTextScanner.GetSubstrAtRelPos (RelPos, SubstrLen: INTEGER): string;
begin
  result  :=  Self.GetSubstrAtPos(Self.Pos + RelPos, SubstrLen);
end; // .function TTextScanner.GetSubstrAtRelPos

function TTextScanner.GotoNextChar: BOOLEAN;
begin
  result  :=  not Self.EndOfText;
  if result then begin
    if Self.TextBuf[Self.Pos] = Self.EndOfLineMarker then begin
      Self.fLineStartPos  :=  Self.Pos;
      INC(Self.fLineN);
    end; // .if
    INC(Self.fPos);
    if Self.Pos > Self.TextBufLen then begin
      Self.fEndOfText :=  TRUE;
      result          :=  FALSE;
    end; // .if
  end; // .if
end; // .function TTextScanner.GotoNextChar

function TTextScanner.GotoPrevChar: BOOLEAN;
var
  i: INTEGER;

begin
  result  :=  Self.Pos > 1;
  if result then begin
    DEC(Self.fPos);
    if Self.TextBuf[Self.Pos] = Self.EndOfLineMarker then begin
      DEC(Self.fLineN);
      i :=  Self.Pos - 1;
      while (i >= 1) and (Self.TextBuf[i] <> Self.EndOfLineMarker) do begin
        DEC(i);
      end; // .while
      Self.fLineStartPos  :=  i;
    end; // .if
    Self.fEndOfText :=  FALSE;
  end; // .if
end; // .function TTextScanner.GotoPrevChar

function TTextScanner.GotoPos (TargetPos: INTEGER): BOOLEAN;
var
  NumSteps: INTEGER;
  i:        INTEGER;
  
begin
  result  :=  Self.IsValidPos(TargetPos);
  if result then begin
    NumSteps  :=  ABS(TargetPos - Self.Pos);
    if TargetPos >= Self.Pos then begin
      for i:=1 to NumSteps do begin
        Self.GotoNextChar;
      end; // .for
    end // .if
    else begin
      for i:=1 to NumSteps do begin
        Self.GotoPrevChar;
      end; // .for
    end; // .else
  end; // .if
end; // .function TTextScanner.GotoPos

function TTextScanner.GotoRelPos (RelPos: INTEGER): BOOLEAN;
begin
  result  :=  Self.GotoPos(Self.Pos + RelPos);
end; // .function TTextScanner.GotoRelPos

function TTextScanner.GotoNextLine: BOOLEAN;
var
  OrigLineN: INTEGER;

begin
  OrigLineN :=  Self.LineN;
  while (Self.LineN = OrigLineN) and Self.GotoNextChar do begin end;
  result  :=  Self.LineN > OrigLineN;
end; // .function TTextScanner.GotoNextLine

function TTextScanner.GotoPrevLine: BOOLEAN;
var
  OrigLineN: INTEGER;

begin
  OrigLineN :=  Self.LineN;
  while (Self.LineN = OrigLineN) and Self.GotoPrevChar do begin end;
  result  :=  Self.LineN < OrigLineN;
  if result then begin
    Self.GotoPos(Self.LineStartPos + 1);
  end; // .if
end; // .function TTextScanner.GotoPrevLine

function TTextScanner.GotoLine (TargetLine: INTEGER): BOOLEAN;
begin
  if TargetLine > Self.LineN then begin
    while (Self.LineN <> TargetLine) and Self.GotoNextLine do begin end;
  end // .if
  else begin
    while (Self.LineN <> TargetLine) and Self.GotoPrevLine do begin end;
  end; // .else
  result  :=  Self.LineN = TargetLine;
end; // .function TTextScanner.GotoLine

function TTextScanner.GotoRelLine (RelLineN: INTEGER): BOOLEAN;
begin
  result  :=  Self.GotoLine(Self.LineN + RelLineN);
end; // .function TTextScanner.GotoRelLine 

function TTextScanner.SkipChars (Ch: CHAR): BOOLEAN;
begin
  result  :=  not Self.EndOfText;
  if result then begin
    while (Self.TextBuf[Self.Pos] = Ch) and Self.GotoNextChar do begin end;
    result  :=  not Self.EndOfText;
  end; // .if
end; // .function TTextScanner.SkipChars

function TTextScanner.SkipCharset (const Charset: TCharSet): BOOLEAN;
begin
  result  :=  not Self.EndOfText;
  if result then begin
    while (Self.TextBuf[Self.Pos] in Charset) and Self.GotoNextChar do begin end;
    result  :=  not Self.EndOfText;
  end; // .if
end; // .function TTextScanner.SkipCharset

function TTextScanner.FindChar (Ch: CHAR): BOOLEAN;
begin
  result  :=  not Self.EndOfText;
  if result then begin
    while (Self.TextBuf[Self.Pos] <> Ch) and Self.GotoNextChar do begin end;
    result  :=  not Self.EndOfText;
  end; // .if
end; // .function TTextScanner.FindChar

function TTextScanner.FindCharset (const Charset: TCharSet): BOOLEAN;
begin
  result  :=  not Self.EndOfText;
  if result then begin
    while not (Self.TextBuf[Self.Pos] in Charset) and Self.GotoNextChar do begin end;
    result  :=  not Self.EndOfText;
  end; // .if
end; // .function TTextScanner.FindCharset

function TTextScanner.ReadToken (const TokenCharset: TCharSet; out Token: string): BOOLEAN;
var
  StartPos: INTEGER;

begin
  result  :=  not Self.EndOfText;
  if result then begin
    StartPos  :=  Self.Pos;
    Self.SkipCharset(TokenCharset);
    Token :=  Copy(Self.TextBuf, StartPos, Self.Pos - StartPos);
  end; // .if
end; // .function TTextScanner.ReadToken

function TTextScanner.ReadTokenTillDelim (const DelimCharset: TCharSet; out Token: string): BOOLEAN;
var
  StartPos: INTEGER;

begin
  result  :=  not Self.EndOfText;
  if result then begin
    StartPos  :=  Self.Pos;
    Self.FindCharset(DelimCharset);
    Token :=  Copy(Self.TextBuf, StartPos, Self.Pos - StartPos);
  end; // .if
end; // .function TTextScanner.ReadTokenTillDelim

procedure TTextScanner.Connect (const TextBuf: string; EndOfLineMarker: CHAR);
const
  MIN_STR_POS = 1;

begin
  Self.fTextBuf         :=  TextBuf;
  Self.fTextBufLen      :=  LENGTH(TextBuf);
  Self.fPos             :=  MIN_STR_POS;
  Self.fLineN           :=  1;
  Self.fLineStartPos    :=  MIN_STR_POS - 1;
  Self.fEndOfLineMarker :=  EndOfLineMarker;
  Self.fEndOfText       :=  Self.TextBufLen = 0;
end; // .procedure Connect

end.
