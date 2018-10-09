unit ApiJack;
(*
  Code/data patching utilities and API hooking.
  Unit without third-party dependencies.

  Tags: #FROZEN, #DEPRECATED, #NEEDS_REWRITING
  TODO: Write PatchMan unit, implementing PatchApi functionality using PatchForge.
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
    end;
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
    end;
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

  // * * * * * //
  FreeMem(SpliceInfo); SpliceInfo := nil;
end; // .function Splice

begin
  InstallMemPatchingFunc(@StdWritePatch);
end.