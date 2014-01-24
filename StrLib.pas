unit StrLib;
{
DESCRIPTION:  Strings processing
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Windows, Math, SysUtils, Classes, StrUtils, Utils;

const
  (* ExplodeEx *)
  INCLUDE_DELIM = TRUE;
  LIMIT_TOKENS  = TRUE;


type
  (* IMPORT *)
  TArrayOfString  = Utils.TArrayOfString;
  
  PListItem = ^TListItem;
  TListItem = record
          Data:     array of CHAR;
          DataSize: INTEGER;
    {On}  NextItem: PListItem;
  end; // .record TListItem
  
  TStrBuilder = class
    (***) protected (***)
    const
      MIN_BLOCK_SIZE  = 65536;
      
    var
      {On}  fRootItem:  PListItem;
      {Un}  fCurrItem:  PListItem;
            fSize:      INTEGER;
          
    (***) public (***)
      destructor  Destroy; override;
      procedure Append (const Str: string);
      procedure AppendBuf (BufSize: INTEGER; {n} Buf: POINTER);
      function  BuildStr: string;
      procedure Clear;
      
      property  Size: INTEGER READ fSize;
  end; // .class TStrBuilder


function  InStrBounds (Pos: INTEGER; const Str: string): BOOLEAN;
function  BytesToAnsiString (PBytes: PBYTE; NumBytes: INTEGER): AnsiString;
function  BytesToWideString (PBytes: PBYTE; NumBytes: INTEGER): WideString;
function  FindCharEx
(
        Ch:       CHAR;
  const Str:      string;
        StartPos: INTEGER;
  out   CharPos:  INTEGER
): BOOLEAN;

function  ReverseFindCharEx
(
        Ch:       CHAR;
  const Str:      string;
        StartPos: INTEGER;
  out   CharPos:  INTEGER
): BOOLEAN;

function  FindChar (Ch: CHAR; const Str: string; out CharPos: INTEGER): BOOLEAN;
function  ReverseFindChar (Ch: CHAR; const Str: string; out CharPos: INTEGER): BOOLEAN;

function  FindCharsetEx
(
        Charset:  Utils.TCharSet;
  const Str:      string;
        StartPos: INTEGER;
  out   CharPos:  INTEGER
): BOOLEAN;

function  FindCharset (Charset: Utils.TCharSet; const Str: string; out CharPos: INTEGER): BOOLEAN;
function  FindSubstrEx (const Substr, Str: string; StartPos: INTEGER; out SubstrPos: INTEGER): BOOLEAN;
function  FindSubstr (const Substr, Str: string; out SubstrPos: INTEGER): BOOLEAN;
{
f('') => NIL
f(Str, '') => [Str]
}
function  ExplodeEx (const Str, Delim: string; InclDelim: BOOLEAN; LimitTokens: BOOLEAN; MaxTokens: INTEGER): TArrayOfString;
function  Explode (const Str: string; const Delim: string): TArrayOfString;
function  Join (const Arr: TArrayOfString; const Glue: string): string;
{
TemplArgs - pairs of (ArgName, ArgValue).
Example: f('Hello, ~UserName~. You are ~Years~ years old.', ['Years', '20', 'UserName', 'Bob'], '~') =>
=> 'Hello, Bob. You are 20 years old'.
}
function  BuildStr (const Template: string; TemplArgs: array of string; TemplChar: CHAR): string;
function  CharsetToStr (const Charset: Utils.TCharSet): string;
function  IntToRoman (Value: INTEGER): string;
function  CharToLower (c: CHAR): CHAR;
function  CharToUpper (c: CHAR): CHAR;
function  HexCharToByte (HexChar: CHAR): BYTE;
function  ByteToHexChar (ByteValue: BYTE): CHAR;
function  Concat (const Strings: array of string): string;
{
Base file name does not include extension.
}
function  ExtractBaseFileName (const FilePath: string): string;
function  SubstrBeforeChar (const Str: string; Ch: CHAR): string;
function  Match (const Str, Pattern: string): BOOLEAN;
function  ExtractFromPchar (Str: PCHAR; Count: INTEGER): string;


(***) implementation (***)


destructor TStrBuilder.Destroy;
begin
  Self.Clear;
end; // .destructor TStrBuilder.Destroy

procedure TStrBuilder.Append (const Str: string);
begin
  Self.AppendBuf(LENGTH(Str), POINTER(Str));
end; // .procedure TStrBuilder.Append

procedure TStrBuilder.AppendBuf (BufSize: INTEGER; {n} Buf: POINTER);
var
  LeftPartSize:   INTEGER;
  RightPartSize:  INTEGER;
  
begin
  {!} Assert(BufSize >= 0);
  {!} Assert((Buf <> nil) or (BufSize = 0));
  if BufSize > 0 then begin
    if Self.fRootItem = nil then begin
      NEW(Self.fRootItem);
      Self.fCurrItem  :=  Self.fRootItem;
      SetLength(Self.fCurrItem.Data, Math.Max(BufSize, Self.MIN_BLOCK_SIZE));
      Self.fCurrItem.DataSize :=  0;
      Self.fCurrItem.NextItem :=  nil;
    end; // .if
    LeftPartSize  :=  Math.Min(BufSize, LENGTH(Self.fCurrItem.Data) - Self.fCurrItem.DataSize);
    RightPartSize :=  BufSize - LeftPartSize;
    if LeftPartSize > 0 then begin
      Utils.CopyMem(LeftPartSize, Buf, @Self.fCurrItem.Data[Self.fCurrItem.DataSize]);
    end; // .if
    Self.fCurrItem.DataSize :=  Self.fCurrItem.DataSize + LeftPartSize;
    if RightPartSize > 0 then begin
      NEW(Self.fCurrItem.NextItem);
      Self.fCurrItem  :=  Self.fCurrItem.NextItem;
      SetLength(Self.fCurrItem.Data, Math.Max(RightPartSize, Self.MIN_BLOCK_SIZE));
      Self.fCurrItem.DataSize :=  RightPartSize;
      Self.fCurrItem.NextItem :=  nil;
      Utils.CopyMem
      (
        RightPartSize,
        Utils.PtrOfs(Buf, LeftPartSize),
        @Self.fCurrItem.Data[0]
      );
    end; // .if
    Self.fSize  :=  Self.fSize + BufSize;
  end; // .if
end; // .procedure TStrBuilder.AppendBuf

function TStrBuilder.BuildStr: string;
var
{U} Res:      POINTER;
{U} CurrItem: PListItem;
    Pos:      INTEGER;

begin
  Res       :=  nil;
  CurrItem  :=  Self.fRootItem;
  // * * * * * //
  SetLength(result, Self.fSize);
  Res :=  POINTER(result);
  Pos :=  0;
  while CurrItem <> nil do begin
    Utils.CopyMem(CurrItem.DataSize, @CurrItem.Data[0], Utils.PtrOfs(Res, Pos));
    Pos       :=  Pos + CurrItem.DataSize;
    CurrItem  :=  CurrItem.NextItem;
  end; // .while
end; // .function TStrBuilder.BuildStr

procedure TStrBuilder.Clear;
var
{Un}  CurrItem: PListItem;
{Un}  NextItem: PListItem;
  
begin
  CurrItem  :=  Self.fRootItem;
  NextItem  :=  nil;
  // * * * * * //
  while CurrItem <> nil do begin
    NextItem  :=  CurrItem.NextItem;
    DISPOSE(CurrItem);
    CurrItem  :=  NextItem;
  end; // .while
  Self.fRootItem  :=  nil;
  Self.fCurrItem  :=  nil;
  Self.fSize      :=  0;
end; // .procedure TStrBuilder.Clear

function InStrBounds (Pos: INTEGER; const Str: string): BOOLEAN;
begin
  result  :=  Math.InRange(Pos, 1, LENGTH(Str));
end; // .function InStrBounds

function BytesToAnsiString (PBytes: PBYTE; NumBytes: INTEGER): AnsiString;
begin
  {!} Assert(PBytes <> nil);
  {!} Assert(NumBytes >= 0);
  SetLength(result, NumBytes);
  Utils.CopyMem(NumBytes, PBytes, POINTER(result));
end; // .function BytesToAnsiString

function BytesToWideString (PBytes: PBYTE; NumBytes: INTEGER): WideString;
begin
  {!} Assert(PBytes <> nil);
  {!} Assert(NumBytes >= 0);
  {!} Assert(Utils.EVEN(NumBytes));
  SetLength(result, NumBytes shr 1);
  Utils.CopyMem(NumBytes, PBytes, POINTER(result));
end; // .function BytesToWideString

function FindCharEx (Ch: CHAR; const Str: string; StartPos: INTEGER; out CharPos: INTEGER): BOOLEAN;
var
  StrLen: INTEGER;
  i:      INTEGER;

begin
  StrLen  :=  LENGTH(Str);
  result  :=  Math.InRange(StartPos, 1, StrLen);
  if result then begin
    i :=  StartPos;
    while (i <= StrLen) and (Str[i] <> Ch) do begin
      INC(i);
    end; // .while
    result  :=  i <= StrLen;
    if result then begin
      CharPos :=  i;
    end; // .if
  end; // .if
end; // .function FindCharEx  

function ReverseFindCharEx
(
        Ch:       CHAR;
  const Str:      string;
        StartPos: INTEGER;
  out   CharPos:  INTEGER
): BOOLEAN;

var
  StrLen: INTEGER;
  i:      INTEGER;

begin
  StrLen  :=  LENGTH(Str);
  result  :=  Math.InRange(StartPos, 1, StrLen);
  if result then begin
    i :=  StartPos;
    while (i >= 1) and (Str[i] <> Ch) do begin
      DEC(i);
    end; // .while
    result  :=  i >= 1;
    if result then begin
      CharPos :=  i;
    end; // .if
  end; // .if
end; // .function ReverseFindCharEx

function FindChar (Ch: CHAR; const Str: string; out CharPos: INTEGER): BOOLEAN;
begin
  result  :=  FindCharEx(Ch, Str, 1, CharPos);
end; // .function FindChar

function ReverseFindChar (Ch: CHAR; const Str: string; out CharPos: INTEGER): BOOLEAN;
begin
  result  :=  ReverseFindCharEx(Ch, Str, LENGTH(Str), CharPos);
end; // .function ReverseFindChar

function FindCharsetEx (Charset: Utils.TCharSet; const Str: string; StartPos: INTEGER; out CharPos: INTEGER): BOOLEAN;
var
  StrLen: INTEGER;
  i:      INTEGER;

begin
  {!} Assert(StartPos >= 1);
  StrLen  :=  LENGTH(Str);
  result  :=  StartPos <= StrLen;
  if result then begin
    i :=  StartPos;
    while (i <= StrLen) and not (Str[i] in Charset) do begin
      INC(i);
    end; // .while
    result  :=  i <= StrLen;
    if result then begin
      CharPos :=  i;
    end; // .if
  end; // .if
end; // .function FindCharsetEx

function FindCharset (Charset: Utils.TCharSet; const Str: string; out CharPos: INTEGER): BOOLEAN;
begin
  result  :=  FindCharsetEx(Charset, Str, 1, CharPos);
end; // .function FindCharset

function FindSubstrEx (const Substr, Str: string; StartPos: INTEGER; out SubstrPos: INTEGER): BOOLEAN;
begin
  SubstrPos :=  StrUtils.PosEx(Substr, Str, StartPos);
  result    :=  SubstrPos <> 0;
end; // .function FindSubstrEx

function FindSubstr (const Substr, Str: string; out SubstrPos: INTEGER): BOOLEAN;
begin
  result  :=  FindSubstrEx(Substr, Str, 1, SubstrPos);
end; // .function FindSubstr

function ExplodeEx (const Str, Delim: string; InclDelim: BOOLEAN; LimitTokens: BOOLEAN; MaxTokens: INTEGER): TArrayOfString;
var
(* O *) DelimPosList:   Classes.TList {OF INTEGER};
        StrLen:         INTEGER;
        DelimLen:       INTEGER;
        DelimPos:       INTEGER;
        DelimsLimit:    INTEGER;
        NumDelims:      INTEGER;
        TokenStartPos:  INTEGER;
        TokenEndPos:    INTEGER;
        TokenLen:       INTEGER;
        i:              INTEGER;

begin
  {!} Assert(not LimitTokens or (MaxTokens > 0));
  DelimPosList  :=  Classes.TList.Create;
  // * * * * * //
  StrLen    :=  LENGTH(Str);
  DelimLen  :=  LENGTH(Delim);
  if StrLen > 0 then begin
    if not LimitTokens then begin
      MaxTokens :=  MAXLONGINT;
    end; // .if
    if DelimLen = 0 then begin
      SetLength(result, 1);
      result[0] :=  Str;
    end // .if
    else begin
      DelimsLimit :=  MaxTokens - 1;
      NumDelims   :=  0;
      DelimPos    :=  1;
      while (NumDelims < DelimsLimit) and FindSubstrEx(Delim, Str, DelimPos, DelimPos) do begin
        DelimPosList.Add(POINTER(DelimPos));
        INC(DelimPos);
        INC(NumDelims);
      end; // .while
      DelimPosList.Add(POINTER(StrLen + 1));
      SetLength(result, NumDelims + 1);
      TokenStartPos :=  1;
      for i:=0 to NumDelims do begin
        TokenEndPos   :=  INTEGER(DelimPosList[i]);
        TokenLen      :=  TokenEndPos - TokenStartPos;
        if InclDelim and (i < NumDelims) then begin
          TokenLen    :=  TokenLen + DelimLen;
        end; // .if
        result[i]     :=  Copy(Str, TokenStartPos, TokenLen);
        TokenStartPos :=  TokenStartPos + DelimLen + TokenLen;
      end; // .for
    end; // .else
  end; // .if
  // * * * * * //
  SysUtils.FreeAndNil(DelimPosList);
end; // .function ExplodeEx

function Explode (const Str: string; const Delim: string): TArrayOfString;
begin
  result  :=  ExplodeEx(Str, Delim, not INCLUDE_DELIM, not LIMIT_TOKENS, 0);
end; // .function Explode

function Join (const Arr: TArrayOfString; const Glue: string): string;
var
(* U *) Mem:        POINTER;
        ArrLen:     INTEGER;
        GlueLen:    INTEGER;
        NumPairs:   INTEGER;
        ResultSize: INTEGER;
        i:          INTEGER;

begin
  Mem :=  nil;
  // * * * * * //
  ArrLen  :=  LENGTH(Arr);
  GlueLen :=  LENGTH(Glue);
  if ArrLen > 0 then begin
    NumPairs    :=  ArrLen - 1;
    ResultSize  :=  0;
    for i:=0 to ArrLen - 1 do begin
      ResultSize  :=  ResultSize + LENGTH(Arr[i]);
    end; // .for
    ResultSize  :=  ResultSize + NumPairs * GlueLen;
    SetLength(result, ResultSize);
    Mem :=  POINTER(result);
    if GlueLen = 0 then begin
      for i:=0 to NumPairs - 1 do begin
        Utils.CopyMem(LENGTH(Arr[i]), POINTER(Arr[i]), Mem);
        Mem :=  Utils.PtrOfs(Mem, LENGTH(Arr[i]));
      end; // .for
    end // .if
    else begin
      for i:=0 to NumPairs - 1 do begin
        Utils.CopyMem(LENGTH(Arr[i]), POINTER(Arr[i]), Mem);
        Mem :=  Utils.PtrOfs(Mem, LENGTH(Arr[i]));
        Utils.CopyMem(LENGTH(Glue), POINTER(Glue), Mem);
        Mem :=  Utils.PtrOfs(Mem, LENGTH(Glue));
      end; // .for
    end; // .else
    Utils.CopyMem(LENGTH(Arr[NumPairs]), POINTER(Arr[NumPairs]), Mem);
  end; // .if
end; // .function Join

function BuildStr (const Template: string; TemplArgs: array of string; TemplChar: CHAR): string;
var
  TemplTokens:    TArrayOfString;
  NumTemplTokens: INTEGER;
  NumTemplVars:   INTEGER;
  NumTemplArgs:   INTEGER;
  TemplTokenInd:  INTEGER;
  i:              INTEGER;
  
  function FindTemplVar (const TemplVarName: string; out Ind: INTEGER): BOOLEAN;
  begin
    Ind :=  1;
    while (Ind < NumTemplTokens) and (TemplTokens[Ind] <> TemplVarName) do begin
      INC(Ind, 2);
    end; // .while
    result  :=  Ind < NumTemplTokens;
  end; // .function FindTemplVar

begin
  NumTemplArgs  :=  LENGTH(TemplArgs);
  {!} Assert(Utils.EVEN(NumTemplArgs));
  // * * * * * //
  TemplTokens     :=  Explode(Template, TemplChar);
  NumTemplTokens  :=  LENGTH(TemplTokens);
  NumTemplVars    :=  (NumTemplTokens - 1) div 2;
  if (NumTemplVars = 0) or (NumTemplArgs = 0) then begin
    result  :=  Template;
  end // .if
  else begin
    i :=  0;
    while (i < NumTemplArgs) do begin
      if FindTemplVar(TemplArgs[i], TemplTokenInd) then begin
        TemplTokens[TemplTokenInd]  :=  TemplArgs[i + 1];
      end; // .if
      INC(i, 2);
    end; // .while
    result  :=  StrLib.Join(TemplTokens, '');
  end; // .else
end; // .function BuildStr

function CharsetToStr (const Charset: Utils.TCharSet): string;
const
  CHARSET_CAPACITY  = 256;
  SPACE_PER_ITEM    = 3;
  DELIMETER         = ', ';
  DELIM_LEN         = LENGTH(DELIMETER);

var
(* U *) BufPos:       ^CHAR;
        Buffer:       array [0..(SPACE_PER_ITEM * CHARSET_CAPACITY + DELIM_LEN * (CHARSET_CAPACITY - 1)) - 1] of CHAR;
        BufSize:      INTEGER;
        StartItemInd: INTEGER;
        FinitItemInd: INTEGER;
        RangeLen:     INTEGER;
        
  procedure WriteItem (c: CHAR);
  begin
    if ORD(c) < ORD(' ') then begin
      BufPos^ :=  '#';                            INC(BufPos);
      BufPos^ :=  CHR(ORD(c) div 10 + ORD('0'));  INC(BufPos);
      BufPos^ :=  CHR(ORD(c) mod 10 + ORD('0'));  INC(BufPos);
    end // .if
    else begin
      BufPos^ :=  '"';  INC(BufPos);
      BufPos^ :=  c;    INC(BufPos);
      BufPos^ :=  '"';  INC(BufPos);
    end; // .else
    INC(BufSize, SPACE_PER_ITEM);
  end; // .procedure WriteItem

begin
  BufPos  :=  @Buffer[0];
  // * * * * * //
  BufSize       :=  0;
  StartItemInd  :=  0;
  while StartItemInd < CHARSET_CAPACITY do begin
    if CHR(StartItemInd) in Charset then begin
      if BufSize > 0 then begin
        BufPos^ :=  DELIMETER[1]; INC(BufPos);
        BufPos^ :=  DELIMETER[2]; INC(BufPos);
        INC(BufSize, DELIM_LEN);
      end; // .if
      FinitItemInd  :=  StartItemInd + 1;
      while (FinitItemInd < CHARSET_CAPACITY) and (CHR(FinitItemInd) in Charset) do begin
        INC(FinitItemInd);
      end; // .while
      RangeLen  :=  FinitItemInd - StartItemInd;
      WriteItem(CHR(StartItemInd));
      if RangeLen > 1 then begin
        if RangeLen > 2 then begin
          BufPos^ :=  '-';
          INC(BufPos);
          INC(BufSize);
        end; // .if
        WriteItem(CHR(FinitItemInd - 1));
      end; // .if
      StartItemInd  :=  FinitItemInd;
    end // .if
    else begin
      INC(StartItemInd);
    end; // .else
  end; // .while
  SetLength(result, BufSize);
  Utils.CopyMem(BufSize, @Buffer[0], POINTER(result));
end; // .function CharsetToStr

function IntToRoman (Value: INTEGER): string;
const
  Arabics:  array [0..12] of INTEGER  = (1,4,5,9,10,40,50,90,100,400,500,900,1000);
  Romans:   array [0..12] of string   = ('I','IV','V','IX','X','XL','L','XC','C','CD','D','CM','M');

var
  i:  INTEGER;

begin
  {!} Assert(Value > 0);
  result  :=  '';
  for i:=12 downto 0 do begin
    while Value >= Arabics[i] do begin
      Value   :=  Value - Arabics[i];
      result  :=  result + Romans[i];
    end; // .while
  end; // .for
end; // .function IntToRoman

function CharToLower (c: CHAR): CHAR;
begin
  result  :=  CHR(INTEGER(Windows.CharLower(Ptr(ORD(c)))));
end; // .function CharToLower

function CharToUpper (c: CHAR): CHAR;
begin
  result  :=  CHR(INTEGER(Windows.CharUpper(Ptr(ORD(c)))));
end; // .function CharToUpper

function HexCharToByte (HexChar: CHAR): BYTE;
begin
  HexChar :=  CharToLower(HexChar);
  if HexChar in ['0'..'9'] then begin
    result  :=  ORD(HexChar) - ORD('0');
  end // .if
  else if HexChar in ['a'..'f'] then begin
    result  :=  ORD(HexChar) - ORD('a') + 10;
  end // .ELSEIF
  else begin
    result  :=  0;
    {!} Assert(FALSE);
  end; // .else
end; // .function HexCharToByte

function ByteToHexChar (ByteValue: BYTE): CHAR;
begin
  {!} Assert(Math.InRange(ByteValue, $00, $0F));
  if ByteValue < 10 then begin
    result  :=  CHR(ByteValue + ORD('0'));
  end // .if
  else begin
    result  :=  CHR(ByteValue - 10 + ORD('A'));
  end; // .else
end; // .function ByteToHexChar

function Concat (const Strings: array of string): string;
var
  ResLen: INTEGER;
  Offset: INTEGER;
  StrLen: INTEGER;
  i:      INTEGER;

begin
  ResLen  :=  0;
  for i:=0 to HIGH(Strings) do begin
    ResLen  :=  ResLen + LENGTH(Strings[i]);
  end; // .for
  
  SetLength(result, ResLen);
  
  Offset  :=  0;
  for i:=0 to HIGH(Strings) do begin
    StrLen  :=  LENGTH(Strings[i]);
    if StrLen > 0 then begin
      Utils.CopyMem(StrLen, POINTER(Strings[i]), Utils.PtrOfs(POINTER(result), Offset));
      Offset  :=  Offset + StrLen;
    end; // .if
  end; // .for
end; // .function Concat

function ExtractBaseFileName (const FilePath: string): string;
var
  DotPos: INTEGER;

begin
  result  :=  SysUtils.ExtractFileName(FilePath);
  
  if ReverseFindChar('.', result, DotPos) then begin
    SetLength(result, DotPos - 1);
  end; // .if
end; // .function ExtractBaseFileName

function SubstrBeforeChar (const Str: string; Ch: CHAR): string;
var
  CharPos:  INTEGER;

begin
  if FindChar(Ch, Str, CharPos) then begin
    result  :=  COPY(Str, 1, CharPos - 1);
  end // .if
  else begin
    result  :=  Str;
  end; // .else
end; // .function SubstrBeforeChar

function Match (const Str, Pattern: string): BOOLEAN;
const
  ONE_SYM_WILDCARD  = '?';
  ANY_SYMS_WILDCARD = '*';
  WILDCARDS         = [ONE_SYM_WILDCARD, ANY_SYMS_WILDCARD];

type
  TState  =
  (
    STATE_STRICT_COMPARE,       // [abc]*?*?**cde?x*
    STATE_SKIP_WILDCARDS,       // abc[*?*?**]cde?x*
    STATE_FIRST_LETTER_SEARCH,  // abc*?*?**[c]de?x*
    STATE_MATCH_SUBSTR_TAIL,    // abc*?*?**c[de?x]*
    STATE_EXIT
  );

(*
  Non-greedy algorithm tries to treat ANY_SYMS_WILLCARD as the shortest possible string.
  Token is a substring between Base position and ANY_SYMS_WILLCARD or end of string in the template
  and corresponding matching substring in the string.
  
  Match "abcecd78e" against "a*cd*e": (Token is wrapped in parenthesis)
  
  (abcecd78e)
  (a*cd*e)
  
  => STRICT_COMPARE until * (success)
  
  (a  )(bcecd78e)
  (a* )(cd*e)
  
  => FIRST_LETTER_SEARCH "c" (success)
  
  (ab )(cecd78e)
  (a* )(cd*e)
  
  => MATCH_SUBSTR_TAIL "d" (fail)
  
  (abc)(ecd78e)
  (a* )(cd*e)
  
  => FIND_FIRST_LETTER "c" (success)
  
  (abce)(cd78e)
  (a*  )(cd*e)
  
  => MATCH_SUBSTR_TAIL "d" (success)
  
  (abce)(cd  )(78e)
  (a*  )(cd* )(e)
  
  => FIND_FIRST_LETTER "e" (success)
  
  (abce)(cd78)(e)
  (a*  )(cd* )(e)
  
  => exit
*)

(*
  Contracts for states:
    STATE_STRICT_COMPARE:
      - Entry state, not-reenterable
      - Matches character-to-character, including ONE_SYM_WILLCARD
      - Exits on mismatch
      - => STATE_SKIP_WILDCARDS
    STATE_SKIP_WILDCARDS:
      - Skips sequence of WILDCARDS
      - Increases position in the string for each ONE_SYM_WILDCARD
      - Exits on end of pattern
      - Initializes character "c" to current pattern character
      - => STATE_FIRST_LETTER_SEARCH
    STATE_FIRST_LETTER_SEARCH
      - Character [c] must be initialized before entering
      - Searches for character [c] in the string
      - Exits on end of string
      - Sets new token positions for string and token to current positions
      - => STATE_MATCH_SUBSTR_TAIL
    STATE_MATCH_SUBSTR_TAIL:
      - Matches character-to-character, including ONE_SYM_WILLCARD
      - Exits on end of string
      - Increases current token position in string by 1 and roll-backs to tokens positions in
        string and template on end of template or last character mismatch
      - => STATE_SKIP_WILDCARDS
*)
  
var
  State:          TState;
  StrLen:         INTEGER;
  PatternLen:     INTEGER;
  StrBasePos:     INTEGER;  // Start position of current token
  PatternBasePos: INTEGER;  // Start position of current token
  s:              INTEGER;  // Pos in Pattern
  p:              INTEGER;  // Pos in Str
  c:              CHAR;     // First letter to search for
  
  
  procedure SkipMatchingSubstr;
  begin
    while
      (p <= PatternLen)                 and
      (s <= StrLen)                     and
      (Pattern[p] <> ANY_SYMS_WILDCARD) and
      (
        (Str[s]     = Pattern[p]) or
        (Pattern[p] = ONE_SYM_WILDCARD)
      )
    do begin
      INC(p);
      INC(s);
    end; // .while
  end; // .procedure SkipMatchingSubstr

begin
  StrLen          :=  LENGTH(Str);
  PatternLen      :=  LENGTH(Pattern);
  StrBasePos      :=  1;
  PatternBasePos  :=  1;
  s               :=  1;
  p               :=  1;
  c               :=  #0;
  State           :=  STATE_STRICT_COMPARE;
  result          :=  FALSE;
  
  while State <> STATE_EXIT do begin
    case State of 
      STATE_STRICT_COMPARE:
        begin
          SkipMatchingSubstr;
          
          if (p > PatternLen) or (Pattern[p] <> ANY_SYMS_WILDCARD) then begin
            State :=  STATE_EXIT;
          end // .if
          else begin
            STATE :=  STATE_SKIP_WILDCARDS;
          end; // .else
        end; // .case STATE_STRICT_COMPARE
        
      STATE_SKIP_WILDCARDS:
        begin
          while (p <= PatternLen) and (Pattern[p] in WILDCARDS) do begin
            if Pattern[p] = ONE_SYM_WILDCARD then begin
              INC(s);
            end; // .if
            
            INC(p);
          end; // .while
          
          if p <= PatternLen then begin
            c     :=  Pattern[p];
            State :=  STATE_FIRST_LETTER_SEARCH;
          end // .if
          else begin
            if s <= StrLen then begin
              s :=  StrLen + 1;
            end; // .if
          
            State :=  STATE_EXIT;
          end; // .else
        end; // .case STATE_SKIP_WILDCARDS
        
      STATE_FIRST_LETTER_SEARCH:
        begin
          while (s <= StrLen) and (Str[s] <> c) do begin
            INC(s);
          end; // .while
          
          if s > StrLen then begin
            State :=  STATE_EXIT;
          end // .if
          else begin
            StrBasePos      :=  s;
            PatternBasePos  :=  p;
            INC(p);
            INC(s);
            State           :=  STATE_MATCH_SUBSTR_TAIL;
          end; // .else
        end; // .case STATE_FIRST_LETTER_SEARCH
        
      STATE_MATCH_SUBSTR_TAIL:
        begin
          SkipMatchingSubstr;
          
          if (p > PatternLen) or (Pattern[p] = ANY_SYMS_WILDCARD) then begin
            State :=  STATE_STRICT_COMPARE;
          end // .if
          else if ((PAttern[p]) = ONE_SYM_WILDCARD) or (s > StrLen) then begin
            STATE := STATE_EXIT;
          end // .ELSEIF
          else begin
            INC(StrBasePos);
            p     :=  PatternBasePos;
            s     :=  StrBasePos;
            State :=  STATE_FIRST_LETTER_SEARCH;
          end; // .else
        end; // .case STATE_MATCH_SUBSTR_TAIL
    end; // .SWITCH State
  end; // .while
  
  result  :=  (s = (StrLen + 1)) and (p = (PatternLen + 1));
end; // .function Match

function ExtractFromPchar (Str: PCHAR; Count: INTEGER): string;
var
  Buf:    PCHAR;
  StrLen: INTEGER;

begin
  {!} Assert(Str <> nil);
  {!} Assert(Count >= 0);
  Buf := Str;
  // * * * * * //
  if Count > 0 then begin
    while (Count > 0) and (Str^ <> #0) do begin
      DEC(Count);
      INC(Str);
    end; // .while
    
    StrLen := Str - Buf;
    SetLength(result, StrLen);
    Utils.CopyMem(StrLen, Buf, POINTER(result));
  end; // .if
end; // .function ExtractFromPchar

end.
