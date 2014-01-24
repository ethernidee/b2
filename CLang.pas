unit CLang;
{
DESCRIPTION:  Auxiliary unit for Lang
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses SysUtils, Math, Utils, Crypto;

const
  (* Names sizes restrictions *)
  CLIENTNAME_MAXLEN = 64;
  LANGNAME_LEN      = 3;


type
  PLngStructHeader = ^TLngStructHeader;
  TLngStructHeader = packed record
    Signature:    array [0..3] of AnsiChar;
    StructSize:   INTEGER;
    BodyCRC32Sum: INTEGER;
    Body:         Utils.TEmptyRec;
  end; // .record TLngStructHeader


function  GetCharSize (Unicode: BOOLEAN): INTEGER;
function  IsValidLangName (const LangName: string): BOOLEAN;
function  IsValidClientName (const ClientName: string): BOOLEAN;
function  ValidateLngStructHeader
(
  (* n *)       Header:                 PLngStructHeader;
                StructMemoryBlockSize:  INTEGER;
                MinStructSize:          INTEGER;
          const Signature:              string;
          out   Error:                  string
): BOOLEAN;
function  ValidateStructSize (FormalSize, RealSize: INTEGER; out Error: string): BOOLEAN;
function  GetEncodingPrefix (Unicode: BOOLEAN): string;


(***)  implementation  (***)


function GetCharSize (Unicode: BOOLEAN): INTEGER;
begin
  if Unicode then begin
    result  :=  2;
  end // .if
  else begin
    result  :=  1;
  end; // .else
end; // .function GetCharSize

function IsValidLangName (const LangName: string): BOOLEAN;
const
  ALLOWED = ['a'..'z'];

var
  i:            INTEGER;
  LangNameLen:  INTEGER;
  
begin
  LangNameLen :=  LENGTH(LangName);
  result      :=  LangNameLen = LANGNAME_LEN;
  // * * * * * //
  i :=  1;
  while (i <= LANGNAME_LEN) and result do begin
    result  :=  LangName[i] in ALLOWED;
    INC(i);
  end; // .while
end; // .function IsValidLangName

function IsValidClientName (const ClientName: string): BOOLEAN;
const
  NO_DOTS_ALLOWED = FALSE;

begin
  result  :=  (LENGTH(ClientName) <= CLIENTNAME_MAXLEN) and SysUtils.IsValidIdent(ClientName, NO_DOTS_ALLOWED);
end; // .function IsValidClientName

function ValidateLngStructHeader
(
  (* Un *)        Header:                 PLngStructHeader;
                  StructMemoryBlockSize:  INTEGER;
                  MinStructSize:          INTEGER;
            const Signature:              string;
            out   Error:                  string
): BOOLEAN;

var
  StructSize: INTEGER;
  
  function ValidateMinStructSize: BOOLEAN;
  begin
    result  :=  StructMemoryBlockSize >= MinStructSize;
    if not result then begin
      Error :=  'The size of structure is too small: ' + SysUtils.IntToStr(StructMemoryBlockSize) + '/' + SysUtils.IntToStr(MinStructSize);
    end; // .if
  end; // .function ValidateMinStructSize

  function ValidateSignatureField: BOOLEAN;
  begin
    result  :=  Header.Signature = Signature;
    if not result then begin
      Error :=  'Structure signature is invalid: ' + Header.Signature + #13#10'. Expected: ' + Signature;
    end; // .if
  end; // .function ValidateSignatureField
  
  function ValidateStructSizeField: BOOLEAN;
  begin
    StructSize  :=  Header.StructSize;
    result      :=  Math.InRange(StructSize, MinStructSize, StructMemoryBlockSize);
    if not result then begin
      Error :=  'Invalid StructSize field: ' + SysUtils.IntToStr(StructSize);
    end; // .if
  end; // .function ValidateStructSizeField
  
  function ValidateBodyCrc32Field: BOOLEAN;
  var
    RealCRC32:  INTEGER;
  
  begin
    RealCRC32 :=  Crypto.CRC32(@Header.Body, StructSize - SIZEOF(TLngStructHeader));
    result    :=  Header.BodyCRC32Sum = RealCRC32;
    if not result then begin
      Error :=  'CRC32 check failed. Original: ' + SysUtils.IntToStr(Header.BodyCRC32Sum) + '. Current: ' + SysUtils.IntToStr(RealCRC32);
    end; // .if
  end; // .function ValidateBodyCrc32Field

begin
  {!} Assert((Header <> nil) or (StructMemoryBlockSize = 0));
  {!} Assert(StructMemoryBlockSize >= 0);
  {!} Assert(MinStructSize >= SIZEOF(TLngStructHeader));
  {!} Assert(Error = '');
  result  :=
    ValidateMinStructSize and
    ValidateSignatureField and
    ValidateStructSizeField and
    ValidateBodyCrc32Field;
end; // .function ValidateLngStructHeader

function ValidateStructSize (FormalSize, RealSize: INTEGER; out Error: string): BOOLEAN;
begin
  {!} Assert(FormalSize > 0);
  {!} Assert(RealSize >= 0);
  {!} Assert(Error = '');
  result  :=  FormalSize = RealSize;
  if not result then begin
    Error :=  'Invalid StructSize field: ' + SysUtils.IntToStr(FormalSize) + '. Real size: ' + SysUtils.IntToStr(RealSize);
  end; // .if
end; // .function ValidateStructSize

function GetEncodingPrefix (Unicode: BOOLEAN): string;
begin
  if Unicode then begin
    result  :=  'wide';
  end // .if
  else begin
    result  :=  'ansi';
  end; // .else
end; // .function GetEncodingPrefix

end.
