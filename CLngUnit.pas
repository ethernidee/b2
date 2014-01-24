unit CLngUnit;
{
DESCRIPTION:  Working with language units
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses SysUtils, Math, Classes, Utils, StrLib, CLang, CLngStrArr;

const
  LNGUNIT_SIGNATURE = 'LUN';


type
  PLngUnitExtHeader = ^TLngUnitExtHeader;
  TLngUnitExtHeader = packed record (* FORMAT *)
    NumLngStrArrays:  INTEGER;
    Unicode:          LONGBOOL;
    UnitNameLen:      INTEGER;
    (*
    UnitName: array UnitNameLen of AnsiChar;  // !Assert UnitName is unique in parent structure
    *)
    UnitName:         TEmptyRec;
  end; // .record TLngUnitExtHeader
  
  PLngUnit = ^TLngUnit;
  TLngUnit = packed record (* FORMAT *)
    Header:       TLngStructHeader;
    ExtHeader:    TLngUnitExtHeader;
    (*
    LngStrArrays: array ExtHeader.NumLngStrArrays of TLngStrArr;
    *)
  end; // .record TLngUnit
      
  TLngUnitReader = class
    (***) protected (***)
                fConnected:             BOOLEAN;
      (* Un *)  fLngUnit:               PLngUnit;
                fStructMemoryBlockSize: INTEGER;
                fCurrLngStrArrInd:      INTEGER;
      (* Un *)  fCurrLngStrArr:         CLngStrArr.PLngStrArr;

      function  GetUnitName: string;
      function  GetStructSize: INTEGER;
      function  GetNumLngStrArrays: INTEGER;
      function  GetUnicode: BOOLEAN;

    (***) public (***)
      procedure Connect (LngUnit: PLngUnit; StructMemoryBlockSize: INTEGER);
      procedure Disconnect;
      function  Validate (out Error: string): BOOLEAN;
      function  SeekLngStrArr (SeekLngStrArrInd: INTEGER): BOOLEAN;
      function  ReadLngStrArr ((* n *) var LngStrArrReader: CLngStrArr.TLngStrArrReader): BOOLEAN;
      function  FindLngStrArr (const LangName: string; out LngStrArrReader: CLngStrArr.TLngStrArrReader): BOOLEAN;

      constructor Create;

      property  Connected:              BOOLEAN READ fConnected;
      property  LngUnit:                PLngUnit READ fLngUnit;
      property  StructMemoryBlockSize:  INTEGER READ fStructMemoryBlockSize;
      property  Unicode:                BOOLEAN READ GetUnicode;
      property  UnitName:               string READ GetUnitName;
      property  StructSize:             INTEGER READ GetStructSize;
      property  NumLngStrArrays:        INTEGER READ GetNumLngStrArrays;
      property  CurrLngStrArrInd:       INTEGER READ fCurrLngStrArrInd;
  end; // .class TLngUnitReader


(***)  implementation  (***)


constructor TLngUnitReader.Create;
begin
  Self.fConnected :=  FALSE;
end; // .constructor TLngUnitReader.Create

procedure TLngUnitReader.Connect (LngUnit: PLngUnit; StructMemoryBlockSize: INTEGER);
begin
  {!} Assert((LngUnit <> nil) or (StructMemoryBlockSize = 0));
  {!} Assert(StructMemoryBlockSize >= 0);
  Self.fConnected             :=  TRUE;
  Self.fLngUnit               :=  LngUnit;
  Self.fStructMemoryBlockSize :=  StructMemoryBlockSize;
  Self.fCurrLngStrArrInd      :=  0;
  Self.fCurrLngStrArr         :=  nil;
end; // .procedure TLngUnitReader.Connect

procedure TLngUnitReader.Disconnect;
begin
  Self.fConnected :=  FALSE;
end; // .procedure TLngUnitReader.Disconnect

function TLngUnitReader.Validate (out Error: string): BOOLEAN;
var
        MinStructSize:    INTEGER;
        RealStructSize:   INTEGER;
        NumLngStrArrays:  INTEGER;
        UnitNameLen:      INTEGER;
        UnitName:         string;
        Unicode:          LONGBOOL;
(* O *) LangNames:        Classes.TStringList;
(* U *) LngStrArr:        CLngStrArr.PLngStrArr;
(* O *) LngStrArrReader:  CLngStrArr.TLngStrArrReader;
        i:                INTEGER;

  function ValidateNumLngStrArraysField: BOOLEAN;
  begin
    NumLngStrArrays :=  Self.NumLngStrArrays;
    MinStructSize   :=  MinStructSize + NumLngStrArrays * SIZEOF(TLngStrArr);
    result          :=  (NumLngStrArrays >= 0) and (MinStructSize <= Self.StructMemoryBlockSize);
    if not result then begin
      Error :=  'Invalid NumLngStrArrays field: ' + SysUtils.IntToStr(NumLngStrArrays);
    end; // .if
  end; // .function ValidateNumLngStrArraysField
  
  function ValidateUnitNameLenField: BOOLEAN;
  begin
    UnitNameLen   :=  Self.LngUnit.ExtHeader.UnitNameLen;
    MinStructSize :=  MinStructSize + UnitNameLen;
    result        :=  (UnitNameLen >= 0) and (MinStructSize <= Self.StructMemoryBlockSize);
    if not result then begin
      Error :=  'Invalid UnitNameLen field: ' + SysUtils.IntToStr(UnitNameLen);
    end; // .if
  end; // .function ValidateUnitNameLenField

  function ValidateUnitNameField: BOOLEAN;
  begin
    UnitName  :=  Self.UnitName;
    result    :=  CLang.IsValidClientName(UnitName);
    if not result then begin
      Error :=  'Invalid UnitName field: ' + UnitName;
    end; // .if
  end; // .function ValidateUnitNameField

begin
  {!} Assert(Self.Connected);
  {!} Assert(Error = '');
  RealStructSize  :=  -1;
  LangNames       :=  Classes.TStringList.Create;
  LngStrArr       :=  nil;
  LngStrArrReader :=  CLngStrArr.TLngStrArrReader.Create;
  MinStructSize   :=  SIZEOF(TLngUnit);
  result          :=  CLang.ValidateLngStructHeader(@Self.LngUnit.Header, Self.StructMemoryBlockSize, MinStructSize, LNGUNIT_SIGNATURE, Error);
  // * * * * * //
  LangNames.CaseSensitive :=  TRUE;
  LangNames.Duplicates    :=  Classes.dupError;
  LangNames.Sorted        :=  TRUE;
  result                  :=  result and
    ValidateNumLngStrArraysField and
    ValidateUnitNameLenField and
    ValidateUnitNameField;
  if result then begin
    Unicode         :=  Self.Unicode;
    RealStructSize  :=  SIZEOF(TLngUnit) + UnitNameLen;
    if NumLngStrArrays > 0 then begin
      i         :=  0;
      LngStrArr :=  POINTER(INTEGER(@Self.LngUnit.Header) + RealStructSize);
      while result and (i < NumLngStrArrays) do begin
        LngStrArrReader.Connect(LngStrArr, Self.StructMemoryBlockSize - RealStructSize);
        result  :=  LngStrArrReader.Validate(Error);
        if result then begin
          try
            LangNames.Add(LngStrArrReader.LangName);
          except
            Error   :=  'Duplicate LangName field in child structure: ' + LngStrArrReader.LangName;
            result  :=  FALSE;
          end; // .try
        end; // .if
        if result then begin
          result  :=  LngStrArrReader.Unicode = Unicode;
          if not result then begin
            Error :=  'Child structure has different encoding: Unicode = ' + SysUtils.IntToStr(BYTE(Unicode));
          end; // .if
        end; // .if
        if result then begin
          RealStructSize  :=  RealStructSize + LngStrArrReader.StructSize;
          INC(INTEGER(LngStrArr), LngStrArrReader.StructSize);
        end; // .if
        INC(i);
      end; // .while
    end; // .if
  end; // .if
  result  :=  result and CLang.ValidateStructSize(Self.LngUnit.Header.StructSize, RealStructSize, Error);
  // * * * * * //
  SysUtils.FreeAndNil(LangNames);
  SysUtils.FreeAndNil(LngStrArrReader);
end; // .function TLngUnitReader.Validate

function TLngUnitReader.GetUnitName: string;
begin
  {!} Assert(Self.Connected);
  result  :=  StrLib.BytesToAnsiString(@Self.LngUnit.ExtHeader.UnitName, Self.LngUnit.ExtHeader.UnitNameLen);
end; // .function TLngUnitReader.GetUnitName

function TLngUnitReader.GetStructSize: INTEGER;
begin
  {!} Assert(Self.Connected);
  result  :=  Self.LngUnit.Header.StructSize;
end; // .function TLngUnitReader.GetStructSize

function TLngUnitReader.GetNumLngStrArrays: INTEGER;
begin
  {!} Assert(Self.Connected);
  result  :=  Self.LngUnit.ExtHeader.NumLngStrArrays;
end; // .function TLngUnitReader.GetNumLngStrArrays

function TLngUnitReader.GetUnicode: BOOLEAN;
begin
  {!} Assert(Self.Connected);
  result  :=  Self.LngUnit.ExtHeader.Unicode;
end; // .function TLngUnitReader.GetUnicode

function TLngUnitReader.SeekLngStrArr (SeekLngStrArrInd: INTEGER): BOOLEAN;
var
(* on *)  LngStrArrReader: CLngStrArr.TLngStrArrReader;

begin
  {!} Assert(Self.Connected);
  {!} Assert(SeekLngStrArrInd >= 0);
  LngStrArrReader :=  nil;
  // * * * * * //
  result  :=  SeekLngStrArrInd < Self.NumLngStrArrays;
  if result then begin
    if Self.fCurrLngStrArrInd > SeekLngStrArrInd then begin
      Self.fCurrLngStrArrInd  :=  0;
    end; // .if
    while Self.fCurrLngStrArrInd < SeekLngStrArrInd do begin
      Self.ReadLngStrArr(LngStrArrReader);
    end; // .while
  end; // .if
  // * * * * * //
  SysUtils.FreeAndNil(LngStrArrReader);
end; // .function TLngUnitReader.SeekLngStrArr

function TLngUnitReader.ReadLngStrArr ((* n *) var LngStrArrReader: CLngStrArr.TLngStrArrReader): BOOLEAN;
begin
  {!} Assert(Self.Connected);
  result  :=  Self.fCurrLngStrArrInd < Self.NumLngStrArrays;
  if result then begin
    if LngStrArrReader = nil then begin
      LngStrArrReader :=  CLngStrArr.TLngStrArrReader.Create;
    end; // .if
    if Self.fCurrLngStrArrInd = 0 then begin
      Self.fCurrLngStrArr :=  POINTER(INTEGER(@Self.LngUnit.ExtHeader.UnitName) + Self.LngUnit.ExtHeader.UnitNameLen);
    end; // .if
    LngStrArrReader.Connect(Self.fCurrLngStrArr, Self.StructMemoryBlockSize - (INTEGER(Self.fCurrLngStrArr) - INTEGER(Self.LngUnit)));
    INC(INTEGER(Self.fCurrLngStrArr), LngStrArrReader.StructSize);
    INC(Self.fCurrLngStrArrInd);
  end; // .if
end; // .function TLngUnitReader.ReadLngStrArr 

function TLngUnitReader.FindLngStrArr (const LangName: string; out LngStrArrReader: CLngStrArr.TLngStrArrReader): BOOLEAN;
var
    SavedLngStrArrInd:  INTEGER;

begin
  {!} Assert(Self.Connected);
  {!} Assert(LngStrArrReader = nil);
  result  :=  FALSE;
  // * * * * * //
  SavedLngStrArrInd :=  Self.CurrLngStrArrInd;
  Self.SeekLngStrArr(0);
  while Self.ReadLngStrArr(LngStrArrReader) and not result do begin
    result  :=  LngStrArrReader.LangName = LangName;
  end; // .while
  Self.SeekLngStrArr(SavedLngStrArrInd);
  // * * * * * //
  if not result then begin
    SysUtils.FreeAndNil(LngStrArrReader);
  end; // .if
end; // .function TLngUnitReader.FindLngStrArr

end.
