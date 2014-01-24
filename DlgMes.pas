unit DlgMes;
{
DESCRIPTION:  Simple dialogs for messages and debugging
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Windows, SysUtils, Math, Utils, StrLib, Lang, DlgMesLng;

const
  NO_WINDOW = 0;

  (* Icons *)
  NO_ICON       = Windows.MB_OK;
  ICON_ERROR    = Windows.MB_ICONSTOP;
  ICON_QUESTION = Windows.MB_ICONQUESTION;
  
  (* Ask results *)
  YES = TRUE;
  NO  = FALSE;
  
  ID_YES    = 0;
  ID_NO     = 1;
  ID_CANCEL = 2;


var
  hParentWindow:  INTEGER = NO_WINDOW;
  DialogsTitle:   string;


procedure MsgEx (const Mes, Title: string; Icon: INTEGER);
procedure MsgTitle (const Mes, Title: string);
procedure Msg (const Mes: string);
procedure MsgError(const Err: string);
procedure OK;
function  AskYesNo (const Question: string): BOOLEAN;
function  AskYesNoCancel (const Question: string): INTEGER;
function  AskOkCancel (const Question: string): BOOLEAN;
function  VarToString (const VarRec: TVarRec): string;
function  ToString (const Vars: array of const): string;
function  PArrItemToString (var PArrItem: POINTER; VarType: INTEGER): string;
function  PVarToString (PVar: POINTER; VarType: INTEGER): string;
procedure VarDump (const Vars: array of const; const Title: string);
procedure ArrDump
(
  const Arr:        POINTER;
        Count:      INTEGER;
  const ElemsType:  INTEGER;
  const Title:      string
);


(***)  implementation  (***)


var
{OU}  Lng:  DlgMesLng.PLangStringsArr;


procedure MsgEx (const Mes, Title: string; Icon: INTEGER);
begin
  Windows.MessageBox(hParentWindow, PCHAR(Mes), PCHAR(Title), Icon);
end; // .procedure MsgEx

procedure MsgTitle (const Mes, Title: string);
begin
  MsgEx(Mes, Title, NO_ICON);
end; // .procedure MsgTitle

procedure Msg (const Mes: string);
begin
  MsgEx(Mes, DialogsTitle, NO_ICON);
end; // .procedure Msg

procedure MsgError(const Err: string);
begin
  MsgEx(Err, DialogsTitle, ICON_ERROR);
end; // .procedure MsgError

procedure OK;
begin
  Msg('OK');
end; // .procedure OK

function AskYesNo (const Question: string): BOOLEAN;
begin
  result  :=  NO;
  
  if
    Windows.MessageBox
    (
      hParentWindow,
      PCHAR(Question),
      PCHAR(Lng[STR_QUESTION]),
      Windows.MB_YESNO + ICON_QUESTION
    ) = Windows.ID_YES
  then begin
    result  :=  YES;
  end; // .if
end; // .function AskYesNo

function AskOkCancel (const Question: string): BOOLEAN;
begin
  result  :=  NO;
  
  if Windows.MessageBox
  (
    hParentWindow,
    PCHAR(Question),
    PCHAR(Lng[STR_QUESTION]),
    Windows.MB_OKCANCEL + ICON_QUESTION
  ) = Windows.ID_OK
  then begin
    result  :=  YES;
  end; // .if
end; // .function AskOkCancel

function AskYesNoCancel (const Question: string): INTEGER;
begin
  result  :=  0;
  
  case
    Windows.MessageBox
    (
      hParentWindow,
      PCHAR(Question),
      PCHAR(Lng[STR_QUESTION]),
      Windows.MB_YESNOCANCEL + ICON_QUESTION
    )
  of 
    Windows.IDYES:      result  :=  ID_YES;
    Windows.IDNO:       result  :=  ID_NO;
    Windows.ID_CANCEL:  result  :=  ID_CANCEL;
  end; // .SWITCH
end; // .function AskYesNoCancel

function VarToString (const VarRec: TVarRec): string;
begin
  case VarRec.vType of
    vtBoolean:
      begin
        if VarRec.vBoolean then begin
          result  :=  'BOOLEAN: TRUE';
        end // .if
        else begin
          result  :=  'BOOLEAN: FALSE';
        end; // .else
      end; // .case vtBoolean
    vtInteger:    result  :=  'INTEGER: ' + SysUtils.IntToStr(VarRec.vInteger);
    vtChar:       result  :=  'CHAR: ' + VarRec.vChar;
    vtWideChar:   result  :=  'WIDECHAR: ' + VarRec.vWideChar;
    vtExtended:   result  :=  'REAL: ' + SysUtils.FloatToStr(VarRec.vExtended^);
    vtString:     result  :=  'string: ' + VarRec.vString^;
    vtPointer:    result  :=  'POINTER: $' + SysUtils.Format('%x',[INTEGER(VarRec.vPointer)]);
    vtPChar:      result  :=  'PCHAR: ' + VarRec.vPChar;
    vtPWideChar:  result  :=  'PWIDECHAR: ' + VarRec.vPWideChar;
    vtObject:     result  :=  'object: ' + VarRec.vObject.ClassName;
    vtClass:      result  :=  'class: ' + VarRec.vClass.ClassName;
    vtCurrency:   result  :=  'CURRENCY: ' + SysUtils.CurrToStr(VarRec.vCurrency^);
    vtAnsiString: result  :=  'ANSISTRING: ' + string(VarRec.vAnsiString);
    vtWideString: result  :=  'WIDESTRING: ' + WideString(VarRec.vWideString);
    vtVariant:    result  :=  'VARIANT: ' + string(VarRec.vVariant);
    vtInterface:  result  :=  'interface: $' + SysUtils.Format('%x',[INTEGER(VarRec.vInterface)]);
    vtInt64:      result  :=  'INT64: ' + SysUtils.IntToStr(VarRec.vInt64^);
  else
    result  :=  'UNKNOWN:';
  end; // .SWITCH VarRec.vType
end; // .function VarToString

function ToString (const Vars: array of const): string;
var
  ResArr: Utils.TArrayOfString;
  i:      INTEGER;

begin
  SetLength(ResArr, LENGTH(Vars));
  
  for i := 0 to High(Vars) do begin
    ResArr[i] :=  VarToString(Vars[i]);
  end; // .for
  
  result  :=  StrLib.Join(ResArr, #13#10);
end; // .function ToString

function PArrItemToString (var PArrItem: POINTER; VarType: INTEGER): string;
var
  VarRec: TVarRec;

begin
  {!} Assert(Math.InRange(VarType, 0, vtInt64));
  VarRec.vType  :=  VarType;
  
  case VarType of
    vtBoolean:    begin VarRec.vBoolean     :=  PBOOLEAN(PArrItem)^; INC(PBOOLEAN(PArrItem)); end;
    vtInteger:    begin VarRec.vInteger     :=  PINTEGER(PArrItem)^; INC(PINTEGER(PArrItem)); end;
    vtChar:       begin VarRec.vChar        :=  PCHAR(PArrItem)^; INC(PCHAR(PArrItem)); end;
    vtWideChar:   begin VarRec.vWideChar    :=  PWideChar(PArrItem)^; INC(PWideChar(PArrItem)); end;
    vtExtended:   begin VarRec.vExtended    :=  PArrItem; INC(PEXTENDED(PArrItem)); end;
    vtString:     begin VarRec.vString      :=  PArrItem; INC(PShortString(PArrItem)); end;
    vtPointer:    begin VarRec.vPointer     :=  PPOINTER(PArrItem)^; INC(PPOINTER(PArrItem)); end;
    vtPChar:      begin VarRec.vPChar       :=  PPCHAR(PArrItem)^; INC(PPCHAR(PArrItem)); end;
    vtPWideChar:
      begin
                        VarRec.vPWideChar   :=  PPWideChar(PArrItem)^; INC(PPWideChar(PArrItem));
      end;
    vtObject:     begin VarRec.vObject      :=  PObject(PArrItem)^; INC(PObject(PArrItem)); end;
    vtClass:      begin VarRec.vClass       :=  PClass(PArrItem)^; INC(PClass(PArrItem)); end;
    vtCurrency:   begin VarRec.vCurrency    :=  PArrItem; INC(PCURRENCY(PArrItem)); end;
    vtAnsiString: begin VarRec.vAnsiString  :=  PPOINTER(PArrItem)^; INC(PPOINTER(PArrItem)); end;
    vtWideString: begin VarRec.vWideString  :=  PPOINTER(PArrItem)^; INC(PPOINTER(PArrItem)); end;
    vtVariant:    begin VarRec.vVariant     :=  PArrItem; INC(PVARIANT(PArrItem)); end;
    vtInterface:  begin VarRec.vInterface   :=  PPOINTER(PArrItem)^; INC(PPOINTER(PArrItem)); end;
    vtInt64:      begin VarRec.vInt64       :=  PArrItem; INC(PINT64(PArrItem)); end;
  end; // .case PArrItem.vType
  result  :=  VarToString(VarRec);
end; // .function PArrItemToString

function PVarToString (PVar: POINTER; VarType: INTEGER): string;
var
{U} Temp: POINTER;

begin
  {!} Assert(Math.InRange(VarType, 0, vtInt64));
  Temp  :=  PVar;
  // * * * * * //
  result  :=  PArrItemToString(Temp, VarType);
end; // .function PVarToString

procedure VarDump (const Vars: array of const; const Title: string);
begin
  MsgTitle(ToString(Vars), Title);
end; // .procedure VarDump

procedure ArrDump
(
  const Arr:        POINTER;
        Count:      INTEGER;
  const ElemsType:  INTEGER;
  const Title:      string
);

const
  NUM_ITEMS_PER_DISPLAY = 20;

var
{U} CurrItem:           POINTER;
    CurrItemInd:        INTEGER;
    StrArr:             Utils.TArrayOfString; 
    DisplayN:           INTEGER;
    NumItemsToDisplay:  INTEGER;
    i:                  INTEGER;
  
begin
  CurrItemInd :=  0;
  CurrItem    :=  Arr;
  
  for DisplayN := 1 to Math.Ceil(Count / NUM_ITEMS_PER_DISPLAY) do begin
    NumItemsToDisplay :=  Math.Min(Count - CurrItemInd, NUM_ITEMS_PER_DISPLAY);
    SetLength(StrArr, NumItemsToDisplay);
    
    for i := 0 to NumItemsToDisplay - 1 do begin
      StrArr[i] :=  '[' + SysUtils.IntToStr(i) + ']: ' + PArrItemToString(CurrItem, ElemsType);
    end; // .for
    
    MsgTitle(StrLib.Join(StrArr, #13#10), Title);
  end; // .for
end; // .procedure ArrDump

begin
  Lng :=  @DlgMesLng.Strs;
  Lang.RegisterClient
  (
    'DlgMes',
    Lang.ENG,
    Lang.IS_ANSI,
    ORD(HIGH(DlgMesLng.TLangStrings)) + 1,
    @Lng,
    Lng
  );
end.
