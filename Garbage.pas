function GetOrigApiAddr (HookAddr: pointer): pointer;
begin
  {!} Assert(HookAddr <> nil);
  result  :=  pointer
  (
    integer(HookAddr)                 +
    sizeof(THookRec)                  +
    PINTEGER(integer(HookAddr) + 1)^  +
    BRIDGE_DEF_CODE_OFS
  );
end; // .function GetOrigApiAddr

function RecallWinApi (Context: PHookContext; NumArgs: integer): integer;
var
  ApiAddr:  pointer;
  PtrArgs:  integer;
  ApiRes:   integer;
   
begin
  ApiAddr :=  GetOrigApiAddr(Ptr(PINTEGER(Context.ESP)^ - sizeof(THookRec)));
  PtrArgs :=  integer(GetStdcallArg(Context, NumArgs));
  
  asm
    MOV ECX, NumArgs
    MOV EDX, PtrArgs
  
  @PUSHARGS:
    PUSH [EDX]
    SUB EDX, 4
    Dec ECX
    JNZ @PUSHARGS
    
    MOV EAX, ApiAddr
    CALL EAX
    MOV ApiRes, EAX
  end; // .asm
  
  result :=  ApiRes;
end; // .function RecallWinApi

function FindHookHandlerAddr ({n} Addr: pointer; ModuleList: TStrList {of TModuleInfo};
                             out ModuleInd, out IsHook: boolean): boolean;
const
  MAX_HOOK_WRAPPER_SIZE = 256;

var
  Disasm:    hde32.TDisasm;
  BufPos:    integer;
  HookFound: boolean;
  HookAddr:  pointer;

begin
  result := FindModuleByAddr(Addr, ModuleList, ModuleInd);
  ?? try except
  if not result then begin
    HookFound := false;
    BufPos    := 0;

    while (BufPos < MAX_HOOK_WRAPPER_SIZE) and not HookFound do begin
      hde32.hde32_disasm(Utils.PtrOfs(Addr, BufPos), Disasm);
      
      if Disasm.Opcode in RET_OPCODES then begin
        BufPos := MAX_HOOK_WRAPPER_SIZE;
      end else if (Disasm.Len = sizeof(THookRec)) and (Disasm.Opcode in [OPCODE_JUMP, OPCODE_CALL])
      then begin
        HookFound := true;
        HookAddr  := Utils.PtrOfs(Addr, BufPos + sizeof(THookRec)
                                               + pinteger(Utils.PtrOfs(Addr, BufPos + 1))^);
      end;
      
      Inc(BufPos, Disasm.Len);
    end; // .while
  end else begin
    IsHook := false;
  end; // .else
end; // .function FindHookHandlerAddr

DEFAULT_LINE_END_MARKER = #10;
DIGITS                  = ['0'..'9'];
SIGNS                   = ['+', '-'];
INT_HEAD                = SIGNS + DIGITS;
HEX_LETTERS             = ['a'..'f', 'A'..'F'];
HEX_DIGITS              = DIGITS + HEX_LETTERS;
HEX_INT_PASCAL_PREFIX   = '$';
HEX_INT_C_PREFIX        = ['x', 'X'];
FLOAT_DELIM             = '.';
BLANKS                  = [#0..#32];
INLINE_BLANKS           = BLANKS - [DEFAULT_LINE_END_MARKER];
IDENT_HEAD              = ['_', 'a'..'z', 'A'..'Z'];
IDENT_BODY              = IDENT_HEAD + DIGITS;

function  IsInt:    boolean;
function  IsHexInt: boolean;
function  IsFloat:  boolean;
function  IsNumber: boolean;
function  IsIdent:  boolean;

function TTextScanner.IsInt: boolean;
var
  Ch: char;

begin
  Ch     := Self.c;
  result := (Ch in DIGITS) or ((Ch in SIGNS) and (CharsRel[1] in DIGITS));
end;

function TTextScanner.IsHexInt: boolean;
var
  Ch: char;

begin???????? what is int???
  Ch     := Self.c;
  result := (Ch in DIGITS) or
            ((Ch = HEX_INT_PASCAL_PREFIX) and CharsRel[1] in DIGITS) or
  (Ch in (DIGITS + HEX_INT_PASCAL_PREFIX)??? $ + digit) or
            ((Ch = '0') and (CharsRel[1] in HEX_INT_C_PREFIX) and (CharsRel[2] in DIGITS));
end; // .function TTextScanner.IsHexInt

function TTextScanner.IsFloat: boolean;
begin
  result := IsInt or ((Self.c = FLOAT_DELIM) and (CharsRel[1] in DIGITS));
end;

function TTextScanner.IsNumber: boolean;
begin
  result := IsFloat or IsHexInt;
end;

function TTextScanner.IsIdent: boolean;
begin
  result := Self.c in IDENT_HEAD;
end;

function TTextScanner.ReadAnyHexInt (out Res: integer): boolean;
var
  Ch:       char;
  StartPos: integer;
  StrValue: string;
  ResValue: integer;

begin
  Ch     := Self.c;
  result := Ch in (HEX_DIGITS + ['$']);

  if result then begin
    if (Ch = '0') and (CharsRel[1] in ['x', 'X']) then begin
      GotoRelPos( +2 );
    end else if Ch = '$' then begin
      GotoNextChar;
    end;

    StartPos := fPos;
    SkipCharset(HEX_DIGITS);
    StrValue := GetSubstrAtPos(StartPos, fPos - StartPos);
    result   := (StrValue <> '') and TryStrToInt('$' + StrValue, ResValue);

    if result then begin
      Res := ResValue;
    end;
  end; // .if
end; // .function TTextScanner.ReadAnyHexInt