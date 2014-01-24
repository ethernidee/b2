unit CLngStrArr;
{
DESCRIPTION:  Working with arrays of language strings
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses SysUtils, Math, Utils, StrLib, CLang, CBinString;

const
  LNGSTRARR_SIGNATURE = 'LAR';


type
  TLangName = packed record
    Name:     array [0..CLang.LANGNAME_LEN - 1] of AnsiChar;
    _Align4:  array [1..(4 - CLang.LANGNAME_LEN mod 4)] of BYTE;
  end; // .record TLangName

  PLngStrArrExtHeader = ^TLngStrArrExtHeader;
  TLngStrArrExtHeader = packed record
    NumBinStrings:  INTEGER;
    LangName:       TLangName;  // !Assert LangName is unique in parent structure
    Unicode:        LONGBOOL;   // !Assert Unicode = Parent.Unicode
  end; // .record TLngStrArrExtHeader
  
  PLngStrArr = ^TLngStrArr;
  TLngStrArr = packed record (* FORMAT *)
    Header:     CLang.TLngStructHeader;
    ExtHeader:  TLngStrArrExtHeader;
    (*
    BinStrings: array ExtHeader.NumBinStrings of TBinString;
    *)
    BinStrings: Utils.TEmptyRec;
  end; // .record TLngStrArr
      
  TLngStrArrReader = class
    (***) protected (***)
                fConnected:             BOOLEAN;
      (* Un *)  fLngStrArr:             PLngStrArr;
                fStructMemoryBlockSize: INTEGER;
                fCurrBinStringInd:      INTEGER;
      (* Un *)  fCurrBinString:         CBinString.PBinString;

      function  GetUnicode: BOOLEAN;
      function  GetLangName: string;
      function  GetStructSize: INTEGER;
      function  GetNumBinStrings: INTEGER;

    (***) public (***)
      procedure Connect (LngStrArr: PLngStrArr; StructMemoryBlockSize: INTEGER);
      procedure Disconnect;
      function  Validate (out Error: string): BOOLEAN;
      function  SeekBinString (SeekBinStringInd: INTEGER): BOOLEAN;
      function  ReadBinString ((* n *) var BinStringReader: CBinString.TBinStringReader): BOOLEAN;

      constructor Create;

      property  Connected:              BOOLEAN READ fConnected;
      property  LngStrArr:              PLngStrArr READ fLngStrArr;
      property  StructMemoryBlockSize:  INTEGER READ fStructMemoryBlockSize;
      property  LangName:               string READ GetLangName;
      property  StructSize:             INTEGER READ GetStructSize;
      property  NumBinStrings:          INTEGER READ GetNumBinStrings;
      property  Unicode:                BOOLEAN READ GetUnicode;
  end; // .class TLngStrArrReader


(***)  implementation  (***)


constructor TLngStrArrReader.Create;
begin
  Self.fConnected :=  FALSE;
end; // .constructor TLngStrArrReader.Create

procedure TLngStrArrReader.Connect (LngStrArr: PLngStrArr; StructMemoryBlockSize: INTEGER);
begin
  {!} Assert((LngStrArr <> nil) or (StructMemoryBlockSize = 0));
  {!} Assert(StructMemoryBlockSize >= 0);
  Self.fConnected             :=  TRUE;
  Self.fLngStrArr             :=  LngStrArr;
  Self.fStructMemoryBlockSize :=  StructMemoryBlockSize;
  Self.fCurrBinStringInd      :=  0;
  Self.fCurrBinString         :=  nil;
end; // .procedure TLngStrArrReader.Connect

procedure TLngStrArrReader.Disconnect;
begin
  Self.fConnected :=  FALSE;
end; // .procedure TLngStrArrReader.Disconnect

function TLngStrArrReader.Validate (out Error: string): BOOLEAN;
var
        NumBinStrings:    INTEGER;
        Unicode:          BOOLEAN;
        RealStructSize:   INTEGER;
(* U *) BinString:        CBinString.PBinString;
(* O *) BinStringReader:  CBinString.TBinStringReader;
        i:                INTEGER;

  function ValidateNumBinStringsField: BOOLEAN;
  begin
    NumBinStrings :=  Self.NumBinStrings;
    result        :=  (NumBinStrings >= 0) and ((NumBinStrings * SIZEOF(TBinString) + SIZEOF(TLngStrArr)) <= Self.StructMemoryBlockSize);
    if not result then begin
      Error :=  'Invalid NumBinStrings field: ' + SysUtils.IntToStr(NumBinStrings);
    end; // .if
  end; // .function ValidateNumBinStringsField  
  
  function ValidateLangNameField: BOOLEAN;
  var
    LangName: string;

  begin
    LangName  :=  Self.LangName;
    result    :=  CLang.IsValidLangName(LangName);
    if not result then begin
      Error :=  'Invalid LangName field: ' + LangName;
    end; // .if
  end; // .function ValidateLangNameField 

begin
  {!} Assert(Self.Connected);
  {!} Assert(Error = '');
  RealStructSize  :=  -1;
  BinString       :=  nil;
  BinStringReader :=  CBinString.TBinStringReader.Create;
  result          :=  CLang.ValidateLngStructHeader(@Self.LngStrArr.Header, Self.StructMemoryBlockSize, SIZEOF(TLngStrArr), LNGSTRARR_SIGNATURE, Error);
  // * * * * * //
  result  :=  result and
    ValidateNumBinStringsField and
    ValidateLangNameField;
  if result then begin
    Unicode         :=  Self.Unicode;
    RealStructSize  :=  SIZEOF(TLngStrArr);
    if NumBinStrings > 0 then begin
      i         :=  0;
      BinString :=  @Self.LngStrArr.BinStrings;
      while result and (i < NumBinStrings) do begin
        BinStringReader.Connect(BinString, Self.StructMemoryBlockSize - RealStructSize, Unicode);
        result  :=  BinStringReader.Validate(Error);
        if result then begin
          RealStructSize  :=  RealStructSize + BinStringReader.StructSize;
          INC(INTEGER(BinString), BinStringReader.StructSize);
        end; // .if
        INC(i);
      end; // .while
    end; // .if
  end; // .if
  result  :=  result and CLang.ValidateStructSize(Self.LngStrArr.Header.StructSize, RealStructSize, Error);
  // * * * * * //
  SysUtils.FreeAndNil(BinStringReader);
end; // .function TLngStrArrReader.Validate

function TLngStrArrReader.GetUnicode: BOOLEAN;
begin
  {!} Assert(Self.Connected);
  result  :=  Self.LngStrArr.ExtHeader.Unicode;
end; // .function TLngStrArrReader.GetStructSize

function TLngStrArrReader.GetLangName: string;
begin
  {!} Assert(Self.Connected);
  result  :=  StrLib.BytesToAnsiString(@Self.LngStrArr.ExtHeader.LangName.Name[0], CLang.LANGNAME_LEN);
end; // .function TLngStrArrReader.GetLangName

function TLngStrArrReader.GetStructSize: INTEGER;
begin
  {!} Assert(Self.Connected);
  result  :=  Self.LngStrArr.Header.StructSize;
end; // .function TLngStrArrReader.GetStructSize

function TLngStrArrReader.GetNumBinStrings: INTEGER;
begin
  {!} Assert(Self.Connected);
  result  :=  Self.LngStrArr.ExtHeader.NumBinStrings;
end; // .function TLngStrArrReader.GetNumBinStrings

function TLngStrArrReader.SeekBinString (SeekBinStringInd: INTEGER): BOOLEAN;
var
(* on *)  BinStringReader: CBinString.TBinStringReader;

begin
  {!} Assert(Self.Connected);
  {!} Assert(SeekBinStringInd >= 0);
  BinStringReader :=  nil;
  // * * * * * //
  result  :=  SeekBinStringInd < Self.NumBinStrings;
  if result then begin
    if Self.fCurrBinStringInd > SeekBinStringInd then begin
      Self.fCurrBinStringInd  :=  0;
    end; // .if
    while Self.fCurrBinStringInd < SeekBinStringInd do begin
      Self.ReadBinString(BinStringReader);
    end; // .while
  end; // .if
  // * * * * * //
  SysUtils.FreeAndNil(BinStringReader);
end; // .function TLngStrArrReader.SeekBinString

function TLngStrArrReader.ReadBinString ((* n *) var BinStringReader: CBinString.TBinStringReader): BOOLEAN;
begin
  {!} Assert(Self.Connected);
  result  :=  Self.fCurrBinStringInd < Self.NumBinStrings;
  if result then begin
    if BinStringReader = nil then begin
      BinStringReader :=  CBinString.TBinStringReader.Create;
    end; // .if
    if Self.fCurrBinStringInd = 0 then begin
      Self.fCurrBinString :=  @Self.LngStrArr.BinStrings;
    end; // .if
    BinStringReader.Connect(Self.fCurrBinString, Self.StructMemoryBlockSize - (INTEGER(Self.fCurrBinString) - INTEGER(Self.LngStrArr)), Self.Unicode);
    INC(INTEGER(Self.fCurrBinString), BinStringReader.StructSize);
    INC(Self.fCurrBinStringInd);
  end; // .if
end; // .function TLngStrArrReader.ReadBinString 

end.
