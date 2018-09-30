unit ApiJack;
(*
  Code/data patching utilities and API hooking.
  Unit without third-party dependencies.
*)

(***)  interface  (***)

uses
  Windows, SysUtils, hde32, Utils, Math, Alg, DataLib, PatchForge;

const
  // Function argument. Size must be detected automatically.
  AUTO_SIZE = -1;

type
  (* Safely writes patch at any code/data section, preserving attributes of memory pages *)
  TMemPatchingFunc = function (NumBytes: integer; {n} Src, {n} Dst: pointer): boolean;

  (* Possible flags for {Splice} function: *)
  TSpliceFlag = (
    PASS_ORIG_FUNC_AS_ARG // Pass address of bridge to original function as the last extra argument in stack
  );
  
  TSpliceFlags = set of TSpliceFlag;

  PSpliceInternalInfo = ^TSpliceInternalInfo;
  TSpliceInternalInfo = packed record
    fOrigCodeSize:  integer;
    fHookedCodePtr: pointer;
    fHookerCodePtr: pointer;
    Reserved:       integer;
  end;

  PSpliceResult = ^TSpliceResult;
  TSpliceResult = packed record
   private
    (* Pointer to original function prolog + jump to continuation point. Internal info is stored in record, located right before this field.
       Memory is allocated via GetMem at internal record address. *)
    {O} fOrigFunc: pointer;

    function GetInfo: PSpliceInternalInfo;

   public
    property OrigFunc: pointer read fOrigFunc;
  end;

  TPatchMaker = class
   private
    fBuf:            Utils.TArrayOfByte;
    fPos:            integer;
    fSize:           integer;
    fRealMemOffsets: DataLib.TList {OF BufPos: integer};

    procedure SetPos (NewPos: integer); inline;
    procedure Grow (NewSize: integer);

   protected
    (* Returns true if given position is valid position *)
    function IsValidPos (Pos: integer): boolean; inline;

    (* Handles specified command, including command arguments. Adjusts argument index. Raises error on unsupported command.
       Arguments index must point to the last used argument or command itself on exit. *)
    procedure HandleCmd (Cmd: TClass; ArgPtr: System.PVarRec; var i: integer; NumArgs: integer); virtual;

   public
    constructor Create;
    destructor Destroy; override;

    (* Writes NumBytes bytes from buffer. Increases position pointer. Returns self. *)
    function WriteBytes (NumBytes: integer; {n} Buf: pointer): {U} TPatchMaker;

    (* Writes list of [int32/int64/float32/AnsiString without #0/WideString without #0/any pointer, object, interface, PChar, PWideChar as pointer/boolean/AnsiChar/WideChar]
       and increases position pointer. Returns self.
       Class arguments are treated specially as commands. A few arguments, following the command, are treated as command arguments. For a list of supported commands see CMD_XXX constants *)
    function Write (const Args: array of const): {U} TPatchMaker;

    (* Seeks to specified existing position or raises error. Returns self. *)
    function Seek (Pos: integer): {U} TPatchMaker;

    (* Ensures, that there is enough soace from current position and returns real temporary pointer to buffer part.
       Existing data is preserved. Calling any class method (except property getters) may invalid the pointer. *)
    function Alloc (NumBytes: integer): {Un} pointer;

    (* Returns real temporary pointer for existing position in buffer. Calling any class method (except property getters) may invalid the pointer. *)
    function GetAddrByPos (Pos: integer): {Un} pointer;

    (* Marks specified position. During patch application real address of that dword will be substracted from dword value.
       Default position is current position. Returns self. *)
    function MarkDwordAsMemOffset (Pos: integer = -1): {U} TPatchMaker;

    (* Returns not applied path in the form of raw bytes array of capacity, greater or equal to patch size. Internal buffer is cleared afterwards. *)
    function GetPatch: {O} Utils.TArrayOfByte;

    (* Applies patch at specified address. Automatically fixes marked offsets. Returns pointer to memory location right after the patch.
       Memory must be writable. *)
    function ApplyPatch ({n} TargetAddr: pointer): {n} pointer;

    property Pos:  integer read fPos write SetPos;
    property Size: integer read fSize;
  end; // .class TPatchMaker

  CMD_MEM_OFFSET = class end;

(* Safely writes patch at any code/data section, preserving attributes of memory pages *)
function WritePatch (NumBytes: integer; {n} Src, {n} Dst: pointer): boolean;

(* Installs new function to be used for all memory patchings. Returns the previous one or nil *)
function InstallMemPatchingFunc (NewMemPatchingFunc: TMemPatchingFunc): {n} TMemPatchingFunc;

(* Replaces original function with the new one with the same prototype and  *)
function Splice (OrigCodePtr, HookCodePtr: pointer; MinPatchSize: integer = AUTO_SIZE; Flags: TSpliceFlags = [PASS_ORIG_FUNC_AS_ARG]): {On} TSpliceResult;


(***)  implementation  (***)


const
  OPCODE_JUMP_CONST32 = $E9;
  OPCODE_CALL_CONST32 = $E8;
  OPCODE_PUSH_CONST32 = $68;

type
  (* Hook code caller *)
  PHookRec = ^THookRec;
  THookRec = packed record
    Opcode: byte;
    Offset: integer;

    (* Changes relative offset of hook opcode so, that it point to specified address *)
    procedure PointTo (Addr: pointer);
  end;


var
  MemPatchingFunc: TMemPatchingFunc;


procedure THookRec.PointTo (Addr: pointer);
begin
  Self.Offset := integer(Addr) - integer(@Self) - sizeof(Self);
end;

function TSpliceResult.GetInfo: PSpliceInternalInfo;
begin
  result := Utils.PtrOfs(Self.fOrigFunc, -sizeof(TSpliceInternalInfo));
end;

function TPatchMaker.IsValidPos (Pos: integer): boolean;
begin
  result := Math.InRange(Self.fPos, 0, Self.Size);
end;

procedure TPatchMaker.SetPos (NewPos: integer);
begin
  Self.Seek(NewPos);
end;

procedure TPatchMaker.Grow (NewSize: integer);
const
  MIN_FREE_SPACE           = 16;
  HEUR_MEMORY_MANAGER_LOAD = 8;

begin
  {!} Assert(NewSize > Length(Self.fBuf));

  SetLength(Self.fBuf, (1 shl Alg.IntLog2(NewSize + MIN_FREE_SPACE + HEUR_MEMORY_MANAGER_LOAD)) - HEUR_MEMORY_MANAGER_LOAD);
  Self.fSize := NewSize;
end;

TPatchMakerCmd
Patcher.Write([PatchLib.AsmPushad, PatchLib.Jump(HookAddr), PatchLib.Offset(TargetData)]);
Patcher.Write([PatchLib.Jump(Addr)]);

type
  PBridgeCodePart1 = ^TBridgeCodePart1;
  TBridgeCodePart1 = packed record
    Pushad:               byte;
    PushEsp:              byte;
    MovEaxConst32:        byte;
    HandlerAddr:          pointer;
    CallEax:              word;
    TestEaxEax:           word;
    JzOffset8:            byte;
    OffsetToAfterDefCode: byte;
    Label_ExecDefCode:    Utils.TEmptyRec;
    Popad:                byte;
    AddEsp4:              array [0..2] of byte;
    // < Default code > //
  end; // .record TBridgeCodePart1

  PBridgeCodePart2 = ^TBridgeCodePart2;
  TBridgeCodePart2 = packed record
    PushConst32:           byte;
    RetAddr:               pointer;
    Ret_1:                 byte;
    Label_DontExecDefCode: Utils.TEmptyRec;
    Popad:                 byte;
    Ret_2:                 byte;
  end; // .record TBridgeCodePart2

Patcher.Write([
  (* PUSHAD, PUSH ESP *) StrLib.HexDec('49AFB06013'),
  PatchLib.Call(HandlerAddr),
  (* TEST EAX, EAX; JZ CONST8_XXX *) StrLib.HexDec('13FAB8'),
  PatchLib.GetPos(JmpToDontExecDefCode),
  PatchLib.Byte(0),
  (* POPAD; ADD ESP, 4 *) StrLib.HexDec('E17791'),
  PatchLib.Jump(Utils.PtrOfs(OrigCode, OrigCodeSize)),
  PatchLib.GetPos(LabelDontExecDefCode),
  PatchLib.Ret
]);

Patcher.WriteAt(JmpToDontExecDefCode, [LabelDontExecDefCode - JmpToDontExecDefCode - 1]);

использовать большие прыжки и автоматически заполняемые метки и адреса

Patcher.Write([
  (* PUSHAD, PUSH ESP *) StrLib.HexDec('49AFB06013'),
  PatchLib.Call(HandlerAddr),
  (* TEST EAX, EAX; JZ CONST8_XXX *) StrLib.HexDec('13FAB8'),
  PatchLib.Jump(),
  PatchLib.Byte(0),
  (* POPAD; ADD ESP, 4 *) StrLib.HexDec('E17791'),
  PatchLib.Jump(Utils.PtrOfs(OrigCode, OrigCodeSize)),
  PatchLib.GetPos(LabelDontExecDefCode),
  PatchLib.Ret
]);

прыжок лучше на nop-ы, так безопаснее, то есть сразу за THookRec
эм, внутренние адреса?

procedure TPatchMaker.HandleCmd (Cmd: TClass; ArgPtr: System.PVarRec; var i: integer; NumArgs: integer);
begin
  {!} Assert(false, 'Unknown TPatchMaker command: ' + Cmd.ClassName);
end;

constructor TPatchMaker.Create;
begin
  Self.fRealMemOffsets := DataLib.NewList(not Utils.OWNS_ITEMS);
end;

destructor TPatchMaker.Destroy;
begin
  SysUtils.FreeAndNil(Self.fRealMemOffsets);
end;

function TPatchMaker.WriteBytes (NumBytes: integer; {n} Buf: pointer): {U} TPatchMaker;
begin
  Utils.CopyMem(NumBytes, Buf, Self.Alloc(NumBytes));
  Inc(Self.fPos, NumBytes);
  result := Self;
end;

function TPatchMaker.Write (const Args: array of const): {U} TPatchMaker;
var
  FloatValue: single;
  NumArgs:    integer;
  i:          integer;

begin
  NumArgs := Length(Args);
  i       := 0;

  while i < NumArgs do begin
    with Args[i] do begin
      case vType of
        vtBoolean:    Self.WriteBytes(sizeof(vBoolean),                @vBoolean);
        vtInteger:    Self.WriteBytes(sizeof(vInteger),                @vInteger);
        vtChar:       Self.WriteBytes(sizeof(vChar),                   @vChar);
        vtWideChar:   Self.WriteBytes(sizeof(vWideChar),               @vWideChar);
        vtExtended:   begin FloatValue := vExtended^; Self.WriteBytes(sizeof(FloatValue), @FloatValue); end;
        vtString:     Self.WriteBytes(Length(vString^),                @vString^[1]);
        vtPointer:    Self.WriteBytes(sizeof(vPointer),                @vPointer);
        vtPChar:      Self.WriteBytes(sizeof(vPChar),                  @vPChar);
        vtPWideChar:  Self.WriteBytes(sizeof(PWideChar),               @vPWideChar);
        vtObject:     Self.WriteBytes(sizeof(vObject),                 @vObject);
        vtClass:      Self.HandleCmd(TClass(vClass), @Args[i], i, NumArgs);
        vtAnsiString: Self.WriteBytes(Length(string(vAnsiString)),     vAnsiString);
        vtWideString: Self.WriteBytes(Length(WideString(vWideString)), vWideString);
        vtInterface:  Self.WriteBytes(sizeof(vInterface),              @vInterface);
        vtInt64:      Self.WriteBytes(sizeof(vInt64),                  @vInt64);
      else
        {!} Assert(false, 'TPatchMaker.Write: unsupported vType: ' + SysUtils.IntToStr(vType));
      end; // .case
    end; // .with

    Inc(i);
  end; // .while
end; // .function TPatchMaker.Write

function TPatchMaker.Seek (Pos: integer): {U} TPatchMaker;
begin
   {!} Assert(Self.IsValidPos(Pos));
end;

function TPatchMaker.Alloc (NumBytes: integer): {Un} pointer;
var
  NewSize: integer;

begin
  {!} Assert(NumBytes >= 0);

  NewSize := Self.Pos + NumBytes;

  if NewSize > Self.Size then begin
    if NewSize > Length(Self.fBuf) then begin
      Self.Grow(NewSize);
    end else begin
      Self.fSize := NewSize;
    end;
  end;

  result := @Self.fBuf[Self.Pos];
end; // .function TPatchMaker.Alloc

function TPatchMaker.GetAddrByPos (Pos: integer): {Un} pointer;
begin
  {!} Assert(Self.IsValidPos(Pos));
  result := @Self.fBuf[Pos];
end;

function TPatchMaker.MarkDwordAsMemOffset (Pos: integer = -1): {U} TPatchMaker;
begin
  {!} Assert((Pos = -1) or Self.IsValidPos(Pos));
  Self.fRealMemOffsets.Add(Ptr(Pos));
end;

function TPatchMaker.GetPatch: {O} Utils.TArrayOfByte;
begin
  result     := Self.fBuf; Self.fBuf := nil;
  Self.fSize := 0;
  Self.fPos  := 0;
end;

function TPatchMaker.ApplyPatch ({n} TargetAddr: pointer): {n} pointer;
begin
  TOffsetToMem
end;

ApiJack.WriteComplexPatch(TargetCode, [#$69#$69#$69#$69#$69, ApiJack.CMD_JUMP_TO, ]);

function GetExecutableMem (Size: integer): {n} pointer;
begin
  {!} Assert(Size >= 0);
  result := nil;

  if Size > 0 then begin
    GetMem(result, Size);
  end;
end;

procedure FreeExecutableMem ({n} Ptr: pointer);
begin
  FreeMem(Ptr);
end;

function WritePatch (NumBytes: integer; {n} Src, {n} Dst: pointer): boolean;
begin
  result := MemPatchingFunc(NumBytes, Src, Dst);
end;

function StdWritePatch (NumBytes: integer; {n} Src, {n} Dst: pointer): boolean;
var
  OldPageProtect: integer;

begin
  {!} Assert(Utils.IsValidBuf(Src, NumBytes));
  {!} Assert(Utils.IsValidBuf(Dst, NumBytes));
  result := NumBytes = 0;

  if not result then begin
    try
      result := Windows.VirtualProtect(Dst, NumBytes, Windows.PAGE_EXECUTE_READWRITE, @OldPageProtect);
      
      if result then begin
        Utils.CopyMem(NumBytes, Src, Dst);
        Windows.VirtualProtect(Dst, NumBytes, OldPageProtect, @OldPageProtect);
      end;
    except
      result := false;
    end; // .try
  end; // .if
end; // .function StdWritePatch

function InstallMemPatchingFunc (NewMemPatchingFunc: TMemPatchingFunc): {n} TMemPatchingFunc;
begin
  {!} Assert(@NewMemPatchingFunc <> nil);
  result          := @MemPatchingFunc;
  MemPatchingFunc := @NewMemPatchingFunc;
end;

(* Calculates minimal hook size. It's assumed, that hook must nop the last partially overwritten instruction *)
function CalcHookSize (Code: pointer): integer;
var
  InstrPtr: pbyte;
  Disasm:   hde32.TDisasm;

begin
  {!} Assert(Code <> nil);
  InstrPtr := Code;
  // * * * * * //
  result := 0;

  while result < sizeof(THookRec) do begin
    hde32.hde32_disasm(InstrPtr, Disasm);
    {!} Assert((Disasm.Len >= 1) and (Disasm.Len <= 100), Format('Failed to disassemble code at %x', [integer(InstrPtr)]));
    
    result := result + Disasm.Len;
    Inc(InstrPtr, Disasm.len);
  end;
end; // .function CalcHookSize

(* Fixes long calls/jumps with relative offsets for code, that was moved to another location *)
procedure FixRelativeOffsetsInMovedCode (CodeAddr, OldCodeAddr: pointer; CodeSize: integer);
var
  Delta:  integer;
  CmdPtr: PAnsiChar;
  EndPtr: PAnsiChar;
  Disasm: hde32.TDisasm;

begin
  {!} Assert(OldCodeAddr <> nil);
  {!} Assert(CodeAddr <> nil);
  {!} Assert(CodeSize >= 0);
  CmdPtr := CodeAddr;
  EndPtr := Utils.PtrOfs(CmdPtr, CodeSize);
  // * * * * * //
  if CodeSize >= sizeof(THookRec) then begin
    Delta := integer(CodeAddr) - integer(OldCodeAddr);

    while CmdPtr < EndPtr do begin
      hde32.hde32_disasm(CmdPtr, Disasm);
      
      if (Disasm.Len = sizeof(THookRec)) and (Disasm.Opcode in [OPCODE_JUMP_CONST32, OPCODE_CALL_CONST32]) then begin
        Dec(PHookRec(CmdPtr).Offset, Delta);
      end;
      
      Inc(CmdPtr, Disasm.Len);
    end; // .while
  end; // .if
end; // .procedure FixRelativeOffsetsInMovedCode

function Splice (OrigCodePtr, HookCodePtr: pointer; MinPatchSize: integer = AUTO_SIZE; Flags: TSpliceFlags = [PASS_ORIG_FUNC_AS_ARG]): {On} TSpliceResult;
type
  TPushOrigFuncAsArg = packed record

  end;

  add custom data as optional parameter

const
  BRIDGE_TO_HOOK_SIZE              = 24;
  BRIDGE_TO_HOOK                   = #$8F#$44#$E4#$F8#$68#$44#$33#$22#$11#$83#$EC#$04#$E9#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90;
  ORIG_FUNC_ADDR_IN_BRIDGE_TO_HOOK = 5;

var
{O} SpliceInfo:       PSpliceInternalInfo;
{n} BridgeToHook:     pbyte;
{n} BridgeToOrigFunc: pbyte;
    OrigCodeSize:     integer;
    Transport:        string;

begin
  {!} Assert(OrigCodePtr <> nil);
  {!} Assert(HookCodePtr <> nil);
  {!} Assert(HookCodePtr <> OrigCodePtr);
  SpliceInfo         := nil;
  BridgeToHook       := nil;
  BridgeToOrigFunc   := nil;
  ppointer(@result)^ := nil;
  // * * * * * //
  // Scan original code and determine optimal hook size
  OrigCodeSize := CalcHookSize(OrigCodePtr);

  p := TPatchMaker.Create();

  with PSpliceInfo(p.Alloc(sizeof(TSpliceInternalInfo), ALLOC_SKIP))^ do begin
    fOrigCodeSize  := OrigCodeSize;
    fHookedCodePtr := OrigCodePtr;
    fHookerCodePtr := HookCodePtr;
  end;

  if PASS_ORIG_FUNC_AS_ARG in Flags then begin
    p.Write(#$8F#$44#$E4#$F8#$68);
    OrigFuncBridgePlaceholder := p.Pos;
    p.MarkOffset();
    p.Skip(sizeof(integer));
  end;

  p.AsmJump(HookCodePtr);
  p.FillBytes(p.Pos mod 8, $90);

  if PASS_ORIG_FUNC_AS_ARG in Flags then begin
    p.WriteAt(p.Pos, OrigFuncBridgePlaceholder);
  end;

  p.CopyCode(OrigCodeSize, OrigCodePtr);
  p.AsmJump(Utils.PtrOfs(OrigCodePtr, OrigCodeSize));
  
  SpliceInfo := GetExecutableMem(p.Size);
  p.Apply(SpliceInfo);
  p.Clear();

  if PASS_ORIG_FUNC_AS_ARG in Flags then begin

  end else begin
    
  end;



  ------------------------------------------------------------------------------------
  
  // Allocate executable memory for splice, initialize info block
  SpliceInfo                := GetExecutableMem(sizeof(TSpliceInternalInfo) + BRIDGE_TO_HOOK_SIZE + OrigCodeSize + sizeof(THookRec));
  SpliceInfo.fOrigCodeSize  := OrigCodeSize;
  SpliceInfo.fHookedCodePtr := OrigCodePtr;
  SpliceInfo.fHookerCodePtr := HookCodePtr;
  
  // Write bridge to hook function
  BridgeToHook     := PtrOfs(SpliceInfo, sizeof(SpliceInfo^));
  Transport        := BRIDGE_TO_HOOK;
  Utils.CopyMem(Length(Transport), pointer(Transport), BridgeToHook);
  BridgeToOrigFunc := Utils.PtrOfs(BridgeToHook, Length(Transport));
  ppointer(Utils.PtrOfs(BridgeToHook, ORIG_FUNC_ADDR_IN_BRIDGE_TO_HOOK))^ := BridgeToOrigFunc;

  // Write bridge to original function
  Utils.CopyMem(OrigCodeSize, OrigCodePtr, BridgeToOrigFunc);
  FixRelativeOffsetsInMovedCode(BridgeToOrigFunc, OrigCodePtr, OrigCodeSize);
  Inc(BridgeToOrigFunc, OrigCodeSize);
 
  with PHookRec(BridgeToOrigFunc)^ do begin
    Opcode := OPCODE_JUMP_CONST32;
    PointTo(Utils.PtrOfs(OrigCodePtr, OrigCodeSize));
  end;

  // Write the hook and nops
  Transport := '';
  SetLength(Transport, OrigCodeSize);

  with PHookRec(Transport)^ do begin
    Opcode := OPCODE_JUMP_CONST32;
    Offset := integer(BridgeToHook) - integer(OrigCodePtr) - sizeof(THookRec);
  end;

  if OrigCodeSize > sizeof(THookRec) then begin
    FillChar(pbyte(@Transport[sizeof(THookRec) + 1])^, OrigCodeSize - sizeof(THookRec), #$90);
  end;

  if WritePatch(Length(Transport), pointer(Transport), OrigCodePtr) then begin
    ppointer(@result)^ := PtrOfs(SpliceInfo, sizeof(SpliceInfo^)); SpliceInfo := nil;
  end;

  asm int 3; end;
  // * * * * * //
  FreeMem(SpliceInfo); SpliceInfo := nil;
end; // .function Splice

begin
  InstallMemPatchingFunc(@StdWritePatch);
end.