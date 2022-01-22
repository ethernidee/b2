unit Crypto;
{
DESCRIPTION:  Encryption and hashing functions
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)


uses
  Utils;

const
  Crc32Table: array[0..255] of cardinal =
  (
    $00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
  ); // Crc32Table

  ByteRedirTable: array [0..255] of byte  =
  (
    168,  196,  081,  104,  062,  151,  178,  175,
    249,  163,  001,  217,  187,  205,  207,  215,
    053,  076,  052,  074,  237,  065,  021,  090,
    036,  192,  197,  235,  035,  009,  164,  167,
    055,  172,  099,  039,  181,  248,  070,  185,
    048,  149,  239,  041,  111,  066,  118,  027,
    094,  005,  092,  152,  113,  234,  051,  186,
    150,  221,  106,  093,  223,  188,  142,  020,
    140,  244,  214,  218,  191,  022,  156,  054,
    173,  114,  102,  101,  232,  194,  064,  132,
    077,  110,  204,  199,  091,  047,  143,  073,
    183,  003,  147,  010,  148,  124,  179,  145,
    067,  100,  017,  126,  253,  012,  112,  226,
    211,  019,  031,  116,  144,  250,  030,  193,
    134,  014,  018,  000,  154,  119,  233,  086,
    107,  016,  254,  184,  117,  230,  190,  087,
    034,  129,  153,  013,  007,  082,  042,  125,
    141,  229,  080,  182,  135,  033,  224,  121,
    061,  015,  246,  155,  068,  252,  079,  109,
    137,  138,  245,  037,  176,  189,  139,  231,
    078,  103,  157,  115,  024,  120,  171,  028,
    180,  096,  038,  158,  023,  059,  058,  212,
    006,  056,  203,  095,  228,  071,  206,  174,
    044,  029,  083,  169,  063,  131,  045,  216,
    165,  069,  136,  209,  105,  084,  146,  108,
    213,  255,  159,  085,  046,  241,  160,  208,
    162,  008,  057,  247,  043,  219,  222,  122,
    127,  072,  200,  123,  177,  166,  097,  128,
    238,  060,  201,  011,  236,  040,  170,  004,
    075,  220,  161,  133,  088,  240,  026,  225,
    198,  210,  227,  049,  251,  098,  002,  032,
    130,  195,  242,  243,  089,  025,  202,  050
  );

  RevByteRedirTable:  array [0..255] of byte  =
  (
    115,  010,  246,  089,  231,  049,  176,  132,
    209,  029,  091,  227,  101,  131,  113,  145,
    121,  098,  114,  105,  063,  022,  069,  172,
    164,  253,  238,  047,  167,  185,  110,  106,
    247,  141,  128,  028,  024,  155,  170,  035,
    229,  043,  134,  212,  184,  190,  204,  085,
    040,  243,  255,  054,  018,  016,  071,  032,
    177,  210,  174,  173,  225,  144,  004,  188,
    078,  021,  045,  096,  148,  193,  038,  181,
    217,  087,  019,  232,  017,  080,  160,  150,
    138,  002,  133,  186,  197,  203,  119,  127,
    236,  252,  023,  084,  050,  059,  048,  179,
    169,  222,  245,  034,  097,  075,  074,  161,
    003,  196,  058,  120,  199,  151,  081,  044,
    102,  052,  073,  163,  107,  124,  046,  117,
    165,  143,  215,  219,  093,  135,  099,  216,
    223,  129,  248,  189,  079,  235,  112,  140,
    194,  152,  153,  158,  064,  136,  062,  086,
    108,  095,  198,  090,  092,  041,  056,  005,
    051,  130,  116,  147,  070,  162,  171,  202,
    206,  234,  208,  009,  030,  192,  221,  031,
    000,  187,  230,  166,  033,  072,  183,  007,
    156,  220,  006,  094,  168,  036,  139,  088,
    123,  039,  055,  012,  061,  157,  126,  068,
    025,  111,  077,  249,  001,  026,  240,  083,
    218,  226,  254,  178,  082,  013,  182,  014,
    207,  195,  241,  104,  175,  200,  066,  015,
    191,  011,  067,  213,  233,  057,  214,  060,
    142,  239,  103,  242,  180,  137,  125,  159,
    076,  118,  053,  027,  228,  020,  224,  042,
    237,  205,  250,  251,  065,  154,  146,  211,
    037,  008,  109,  244,  149,  100,  122,  201
  );


type
  TInt32 = packed array [0..3] of byte;


{
Name  : CRC-32(B)
Poly  : 0x04C11DB7  x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x + 1
Init  : 0xFFFFFFFF
Revert: true
XorOut: 0xFFFFFFFF
Check : 0xCBF43926  ("123456789")
MaxLen: 268 435 455 bytes (2 147 483 647 bit) - detects single, double, packet and all odd errors
}
function  Crc32 (PStr: pchar; StrLen: integer): integer;
function  AnsiCrc32 (const Str: string): integer;

(* Warning! Requires SSE 4.2 support. Shows 8X performance, as compared to 1-byte tabular CRC32 implementation *)
function Crc32c (PStr: pchar; StrLen: integer): integer;
function AnsiCrc32c (const Str: string): integer;

{
Name:         Bb2011
Version:      1.0
Author:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
Discription:  Generates reversable unique int-to-int hash with high bits distribution
}
function  Bb2011Encode (Value: integer): integer;
function  Bb2011Decode (Encoded: integer): integer;

(* Fast and reversable int32 hashing. Link: https://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key *)
function  Tm32Encode (Value: integer): integer;
function  Tm32Decode (Encoded: integer): integer;


var
  (* Hashing routines for runtime in-memory usage only *)
  FastHash:     function (PStr: pchar; StrLen: integer): integer;
  FastAnsiHash: function (const Str: string): integer;


(***)  implementation  (***)


var
  Sse42Supported: boolean = false;


function Crc32 (PStr: pchar; StrLen: integer): integer;
var
  i: integer;

begin
  {!} Assert((StrLen >= 0) and ((PStr <> nil) or (StrLen = 0)));
  result := -1;

  for i := 1 to StrLen do begin
    result := (result shr 8) xor integer(Crc32Table[(result xor ord(PStr^)) and $FF]);
    Inc(PStr);
  end;

  result := result xor -1;
end; // .function Crc32

function AnsiCrc32 (const Str: string): integer;
begin
  result := Crc32(pointer(Str), Length(Str));
end;

function Crc32c (PStr: pchar; StrLen: integer): integer; assembler;
asm
  push edi
  mov edi, eax
  xor eax, eax
  dec eax
  mov ecx, edx
  and ecx, $FFFFFFFC
  shr ecx, 2
  jz @@end_crc_dword
@@crc_dword:
  db $F2, $0F, $38, $F1, $07 // crc32 eax, [dword edi]
  add edi, 4
  dec ecx
  jnz @@crc_dword
@@end_crc_dword:
  mov ecx, edx
  and ecx, $00000003
  jz @@end_crc_byte
@@crc_byte:
  db $F2, $0F, $38, $F0, $07 // crc32 eax, [byte edi]
  inc edi
  dec ecx
  jnz @@crc_byte
@@end_crc_byte:
  xor eax, -1
  pop edi
end; // .function Crc32c

function AnsiCrc32c (const Str: string): integer;
begin
  result := Crc32c(pointer(Str), Length(Str));
end;

function Bb2011Encode (Value: integer): integer;
begin
  TInt32(result)[3] := ByteRedirTable[TInt32(Value)[0]];
  TInt32(result)[0] := ByteRedirTable[TInt32(Value)[3]];
  TInt32(result)[1] := ByteRedirTable[TInt32(Value)[2]];
  TInt32(result)[2] := ByteRedirTable[TInt32(Value)[1]];
end;

function Bb2011Decode (Encoded: integer): integer;
begin
  TInt32(result)[0] := RevByteRedirTable[TInt32(Encoded)[3]];
  TInt32(result)[3] := RevByteRedirTable[TInt32(Encoded)[0]];
  TInt32(result)[2] := RevByteRedirTable[TInt32(Encoded)[1]];
  TInt32(result)[1] := RevByteRedirTable[TInt32(Encoded)[2]];
end;

function Tm32Encode (Value: integer): integer;
begin
  result := ((Value shr 16) xor Value) * $45D9F3B;
  result := ((result shr 16) xor result) * $45D9F3B;
  result := (result shr 16) xor result;
end;

function Tm32Decode (Encoded: integer): integer;
begin
  result := ((Encoded shr 16) xor Encoded) * $119DE1F3;
  result := ((result shr 16) xor result) * $119DE1F3;
  result := (result shr 16) xor result;
end;

begin
  Sse42Supported := Utils.IsSse42Supported;
  FastHash       := Crc32;
  FastAnsiHash   := AnsiCrc32;

  if Sse42Supported then begin
    FastHash     := Crc32c;
    FastAnsiHash := AnsiCrc32c;
  end;
end.
