unit PatchForge;
(*
  Code/data patching utilities and API hooking.
  Unit without third-party dependencies.
*)

(***)  interface  (***)


uses
  Windows, SysUtils, hde32, Utils, Math, Alg, DataLib, TypeWrappers;

type
  (* Import *)
  TString = TypeWrappers.TString;

const
  (* Used in all functions, requiring pos *)
  CURRENT_POS = -1;

  (* Assembler opcodes *)
  OPCODE_NOP              = $90;
  OPCODE_INT3             = $CC;
  OPCODE_JMP_CONST32      = $E9;
  OPCODE_JE_CONST32       = $840F;
  OPCODE_JNE_CONST32      = $850F;
  OPCODE_JA_CONST32       = $870F;
  OPCODE_JAE_CONST32      = $830F;
  OPCODE_JB_CONST32       = $820F;
  OPCODE_JBE_CONST32      = $860F;
  OPCODE_JG_CONST32       = $8F0F;
  OPCODE_JGE_CONST32      = $8D0F;
  OPCODE_JL_CONST32       = $8C0F;
  OPCODE_JLE_CONST32      = $8E0F;
  OPCODE_JMP_SHORT_CONST8 = $EB;
  OPCODE_JE_SHORT_CONST8  = $74;
  OPCODE_JNE_SHORT_CONST8 = $75;
  OPCODE_JA_SHORT_CONST8  = $77;
  OPCODE_JAE_SHORT_CONST8 = $73;
  OPCODE_JB_SHORT_CONST8  = $72;
  OPCODE_JBE_SHORT_CONST8 = $76;
  OPCODE_JG_SHORT_CONST8  = $7F;
  OPCODE_JGE_SHORT_CONST8 = $7D;
  OPCODE_JL_SHORT_CONST8  = $7C;
  OPCODE_JLE_SHORT_CONST8 = $7E;
  OPCODE_CALL_CONST32     = $E8;
  OPCODE_PUSH_CONST32     = $68;
  OPCODE_MOV_EAX_CONST32  = $B8;
  OPCODE_JMP_EAX          = $E0FF;
  OPCODE_RET              = $C3;
  OPCODE_RET_CONST16      = $C2;
  OPCODE_TEST_EAX_EAX     = $C085;


type
  (* Jump/call OFFSET 32 instruction *)
  PJumpCall32Rec = ^TJumpCall32Rec;
  TJumpCall32Rec = packed record
    Opcode: byte;
    Offset: integer;

    (* Changes relative Offset field so, that it point to specified absolute address *)
    procedure SetTargetAddr ({n} Addr: pointer);
  end;

  (* Jump/call OFFSET 8 instruction *)
  PJumpCall8Rec = ^TJumpCall8Rec;
  TJumpCall8Rec = packed record
    Opcode: byte;
    Offset: shortint;
  end;

  TPatchAction     = class;
  TPostponedAction = class;

  TPatchMaker = class
   private
   {O} fPostponedActions: {O} DataLib.TList {OF TPostponedAction};
   {O} fLabels:           DataLib.TDict {OF Pos: integer};
       fBuf:              Utils.TArrayOfByte;
       fPos:              integer;
       fSize:             integer;
       fLabelAutoId:      integer;


    procedure SetPos (NewPos: integer); inline;
    procedure Grow (NewSize: integer);
    procedure ExecPostponedActions (TargetAddr: pointer);

   protected
    (* Returns true if given position is valid position *)
    function IsValidPos (Pos: integer): boolean; inline;

   public
    constructor Create;
    destructor Destroy; override;

    (* Returns validated position. Value of CURRENT_POS is replaced with current position *)
    function NormalizePos (Pos: integer): integer;

    (* Writes NumBytes bytes from buffer. Increases position pointer. Returns self. *)
    function WriteBytes (NumBytes: integer; {n} Buf: pointer): {U} TPatchMaker;

    (* Writes list of [int32/int64/float32/AnsiString without #0/WideString without #0/any pointer, object, class, interface, PChar, PWideChar as pointer/boolean/AnsiChar/WideChar]
       and increases position pointer. Returns self.
       Objects of class TPatchMakerCmd are treated specially as commands. Their Execute method is called with PatchMaker instance as the only argument. *)
    function Write (const Args: array of const): {U} TPatchMaker;

    (* Seeks to specified existing position or raises error. Returns self. *)
    function Seek (Pos: integer): {U} TPatchMaker;

    (* Ensures, that there is enough soace from current position and returns real temporary pointer to buffer part.
       Existing data is preserved. Calling any class method (except property getters) may invalid the pointer. *)
    function Alloc (NumBytes: integer): {Un} pointer;

    (* Returns real temporary pointer for existing position in buffer. Calling any class method (except property getters) may invalid the pointer. *)
    function GetPosTempAddr (Pos: integer = CURRENT_POS): {Un} pointer;

    (* Generates and returns new unique label *)
    function NewLabel: string;

    (* Gives current position a named label. Throws exception if label with the same name is used elsewhere. *)
    function PutLabel (const LabelName: string): {U} TPatchMaker;

    (* Returns label position by name or -1 for not yet resolved label *)
    function GetLabelPos (const LabelName: string): integer;

    (* Enqueues action to be executed at specified position during patch application on target buffer. Actions are executed in order (FIFO). *)
    function ExecActionOnApply ({O} Action: TPatchAction; Pos: integer = CURRENT_POS): {U} TPatchMaker;

    (* Returns not applied path in the form of raw bytes array of capacity, greater or equal to patch size. Internal buffer is cleared afterwards. *)
    function GetPatch: {O} Utils.TArrayOfByte;

    (* Applies patch at specified address. Automatically fixes marked offsets. Returns pointer to memory location right after the patch.
       Memory must be writable. *)
    function ApplyPatch ({n} TargetAddr: pointer): {n} pointer;

    (* Resets and truncates patch buffer and all auxiliary structures *)
    function Clear: {U} TPatchMaker;

    property Pos:  integer read fPos write SetPos;
    property Size: integer read fSize;
  end; // .class TPatchMaker

  (* Custom patch command, which can be executed either separately or as one of TPatchMaker.Write arguments *)
  IPatchCmd = interface ['{EBD3A1C8-E3F0-4059-8D76-3A164E7E1163}']
    procedure Execute (p: TPatchMaker);
  end;

  (* Custom action, that is executed at definite position in target patch buffer during patch application. It can modify data,
     resolve labels, convert local addresses to real addresses, etc.
  *)
  TPatchAction = class
   protected
    (* Real execution routine. All arguments are already checked and valid. *)
    procedure _Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker); virtual;

   public
    (* Evaluates some expression and probably modifies item datar *)
    procedure Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker);
  end; // .class TPatchAction

  (* Patch action, postponed to be executed at specified position during patch application stage *)
  TPostponedAction = class
   protected
    {O} fAction: TPatchAction;
        fPos:    integer;

   public
    constructor Create (Action: TPatchAction; Pos: integer);
    destructor Destroy; override;

    procedure Execute (PatchBufAddr: pointer; PatchMaker: TPatchMaker);
  end;

  (* Adds real address of dword to dword value *)
  TAddRealAddrAction = class (TPatchAction)
   public
    procedure _Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker); override;
  end;

  (* Adds real address of byte to byte value *)
  TAddRealAddrByteAction = class (TPatchAction)
   public
    procedure _Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker); override;
  end;

  (* Subtracts real address of dword to dword value *)
  TSubRealAddrAction = class (TPatchAction)
   public
    procedure _Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker); override;
  end;

  (* Subtracts real address of byte to byte value *)
  TSubRealAddrByteAction = class (TPatchAction)
   public
    procedure _Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker); override;
  end;

  (* Adds label position to dword value *)
  TAddLabelPosAction = class (TPatchAction)
   protected
    fLabelName: string;

   public
    constructor Create (const LabelName: string);
    
    procedure _Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker); override;
  end;

  (* Adds label position to byte value *)
  TAddLabelPosByteAction = class (TPatchAction)
   protected
    fLabelName: string;

   public
    constructor Create (const LabelName: string);
    
    procedure _Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker); override;
  end;

  TJumpType = (JMP, JE, JNE, JA, JAE, JB, JBE, JG, JGE, JL, JLE, JMP_SHORT, JE_SHORT, JNE_SHORT, JA_SHORT, JAE_SHORT, JB_SHORT, JBE_SHORT, JG_SHORT, JGE_SHORT, JL_SHORT, JLE_SHORT);

  (* Powerful wrapper for TPatchMaker instance *)
  TPatchHelper = packed record
   public
    (* Wrapped patch making engine *)
    {U} PatchMaker: TPatchMaker;

    (* Wraps PatchMaker into local helper wrapper and returns the helper *)
    class function Init ({U} PatchMaker: TPatchMaker): TPatchHelper; inline; static;
    
    function WriteByte (Value: byte): TPatchHelper;
    function WriteWord (Value: word): TPatchHelper;
    function WriteInt  (Value: integer): TPatchHelper;
    
    function WriteBytes (NumBytes: integer; {n} Buf: pointer): TPatchHelper; inline;
    function Write (const Args: array of const): TPatchHelper;
    function Seek (Pos: integer): TPatchHelper; inline;

    (* Seeks to relative position from the current one. Returns self. *)
    function SeekRel (RelPos: integer): TPatchHelper;

    function Alloc (NumBytes: integer): {Un} pointer; inline;

    (* Allocated block of bytes and seeks to block end. See {@Alloc} *)
    function AllocAndSkip (NumBytes: integer): {Un} pointer;

    function GetPosTempAddr (Pos: integer = CURRENT_POS): {Un} pointer; inline;
    function NewLabel: string; inline;
    function PutLabel (const LabelName: string): TPatchHelper; inline;
    function GetLabelPos (const LabelName: string): integer; inline;
    function ExecActionOnApply ({O} Action: TPatchAction; Pos: integer = CURRENT_POS): {U} TPatchHelper; inline;
    function GetPatch: {O} Utils.TArrayOfByte; inline;
    function ApplyPatch ({n} TargetAddr: pointer): {n} pointer; inline;
    function Clear: {U} TPatchHelper; inline;
    function Pos: integer; inline;
    function Size: integer; inline;

    (* Writes assembler instruction to jump to patch label position. Returns self. *)
    function JumpLabel (JumpType: TJumpType; const LabelName: string): TPatchHelper;
    
    (* Writes assembler instruction to jump to patch buffer position. Returns self. *)
    function JumpPos (JumpType: TJumpType; Pos: integer): TPatchHelper;

    (* Writes assembler instruction/s to jump to specified real address. Returns self. *)
    function Jump (JumpType: TJumpType; Addr: pointer): TPatchHelper;

    (* Writes assembler instruction to call routine at patch label position. Returns self. *)
    function CallLabel (const LabelName: string): TPatchHelper;
    
    (* Writes assembler instruction to call routine at patch buffer position. Returns self. *)
    function CallPos (Pos: integer): TPatchHelper;

    (* Writes assembler instruction/s to call routine at specified real address. Returns self. *)
    function Call (Addr: pointer): TPatchHelper;

    (* Writes ret instruction, cleaning NumArgs dword stack arguments. Returns self. *)
    function Ret (NumArgs: integer = 0): TPatchHelper;
    
    (* Fills block of memory with byte value. Returns self. *)
    function FillBytes (NumBytes: integer; Value: byte): TPatchHelper;

    (* Writes sequentional nop instructions. Returns self. *)
    function Nop (NumNops: integer = 1): TPatchHelper;

    (* Pushes const32 to stack. Returns self. *)
    function PushConst32 (Value: integer): TPatchHelper;

    (* Allocates space and copies CPU instructions from source to patch buffer. Ensures, that all relative
       addresses will be valid after patch application. If MinNumBytes block is too small to keep the
       last instruction from source, block size is increased to hold the whole last instruction. Returns self. *)
    function WriteCode (MinNumBytes: integer; {n} CodeSource: pointer): TPatchHelper;
  end; // .record TPatchHelper


(***)  implementation  (***)


function GetJumpOpcode (JumpType: TJumpType; out OpcodeSize: integer; out ArgSize: integer): integer;
begin
  // Set debug values for unknown jump types
  result     := OPCODE_INT3;
  ArgSize    := 0;
  OpcodeSize := 1;

  if JumpType = JMP then begin
    result     := OPCODE_JMP_CONST32;
    OpcodeSize := 1;
    ArgSize    := sizeof(integer);
  end else begin
    case JumpType of
      JE:  result := OPCODE_JE_CONST32;
      JNE: result := OPCODE_JNE_CONST32;
      JA:  result := OPCODE_JA_CONST32;
      JAE: result := OPCODE_JAE_CONST32;
      JB:  result := OPCODE_JB_CONST32;
      JBE: result := OPCODE_JBE_CONST32;
      JG:  result := OPCODE_JG_CONST32;
      JGE: result := OPCODE_JGE_CONST32;
      JL:  result := OPCODE_JL_CONST32;
      JLE: result := OPCODE_JLE_CONST32;
    end; // .switch

    if result <> OPCODE_INT3 then begin
      OpcodeSize := 2;
      ArgSize    := sizeof(integer);
    end else begin
      case JumpType of
        JMP_SHORT: result := OPCODE_JMP_SHORT_CONST8;
        JE_SHORT:  result := OPCODE_JE_SHORT_CONST8;
        JNE_SHORT: result := OPCODE_JNE_SHORT_CONST8;
        JA_SHORT:  result := OPCODE_JA_SHORT_CONST8;
        JAE_SHORT: result := OPCODE_JAE_SHORT_CONST8;
        JB_SHORT:  result := OPCODE_JB_SHORT_CONST8;
        JBE_SHORT: result := OPCODE_JBE_SHORT_CONST8;
        JG_SHORT:  result := OPCODE_JG_SHORT_CONST8;
        JGE_SHORT: result := OPCODE_JGE_SHORT_CONST8;
        JL_SHORT:  result := OPCODE_JL_SHORT_CONST8;
        JLE_SHORT: result := OPCODE_JLE_SHORT_CONST8;
      else
        Assert(false, 'Unknown jump type: ' + SysUtils.IntToStr(ord(JumpType)));
      end; // .switch

      if result <> OPCODE_INT3 then begin
        OpcodeSize := 1;
        ArgSize    := sizeof(byte);
      end;
    end; // .else
  end; // .else
end; // .function GetJumpOpcode

procedure TJumpCall32Rec.SetTargetAddr ({n} Addr: pointer);
begin
  Self.Offset := integer(Addr) - integer(@Self) - sizeof(Self);
end;

procedure TPatchAction._Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker);
begin
end;

procedure TPatchAction.Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker);
begin
  {!} Assert(ItemAddr <> nil);
  {!} Assert(PatchMaker <> nil);
  {!} Assert(ItemPos >= 0);
  Self._Execute(ItemAddr, ItemPos, PatchMaker);
end;

constructor TPostponedAction.Create (Action: TPatchAction; Pos: integer);
begin
  {!} Assert(Action <> nil);
  {!} Assert(Pos >= 0);
  Self.fAction := Action;
  Self.fPos    := Pos;
end;

destructor TPostponedAction.Destroy;
begin
  SysUtils.FreeAndNil(Self.fAction);
end;

procedure TPostponedAction.Execute (PatchBufAddr: pointer; PatchMaker: TPatchMaker);
begin
  {!} Assert(PatchBufAddr <> nil);
  {!} Assert(PatchMaker <> nil);
  Self.fAction.Execute(Utils.PtrOfs(PatchBufAddr, Self.fPos), Self.fPos, PatchMaker);
end;

procedure TAddRealAddrAction._Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker);
begin
  Inc(pinteger(ItemAddr)^, integer(ItemAddr));
end;

procedure TAddRealAddrByteAction._Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker);
begin
  Inc(pbyte(ItemAddr)^, integer(ItemAddr));
end;

procedure TSubRealAddrAction._Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker);
begin
  Dec(pinteger(ItemAddr)^, integer(ItemAddr));
end;

procedure TSubRealAddrByteAction._Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker);
begin
  Dec(pbyte(ItemAddr)^, integer(ItemAddr));
end;

constructor TAddLabelPosAction.Create (const LabelName: string);
begin
  Self.fLabelName := LabelName;
end;

procedure TAddLabelPosAction._Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker);
var
  LabelPos: integer;

begin
  LabelPos := PatchMaker.GetLabelPos(Self.fLabelName);
  {!} Assert(LabelPos >= 0, Format('Unresolved label "%s". Cannot apply action for position %d', [Self.fLabelName, ItemPos]));
  Inc(pinteger(ItemAddr)^, LabelPos);
end;

constructor TAddLabelPosByteAction.Create (const LabelName: string);
begin
  Self.fLabelName := LabelName;
end;

procedure TAddLabelPosByteAction._Execute (ItemAddr: pointer; ItemPos: integer; PatchMaker: {U} TPatchMaker);
var
  LabelPos: integer;

begin
  LabelPos := PatchMaker.GetLabelPos(Self.fLabelName);
  {!} Assert(LabelPos >= 0, Format('Unresolved label "%s". Cannot apply action for position %d', [Self.fLabelName, ItemPos]));
  Inc(pbyte(ItemAddr)^, LabelPos);
end;

constructor TPatchMaker.Create;
begin
  Self.fPostponedActions := DataLib.NewList(Utils.OWNS_ITEMS);
  Self.fLabels           := DataLib.NewDict(not Utils.OWNS_ITEMS, DataLib.CASE_SENSITIVE);
  Self.fLabelAutoId      := 1;
end;

destructor TPatchMaker.Destroy;
begin
  SysUtils.FreeAndNil(Self.fPostponedActions);
  SysUtils.FreeAndNil(Self.fLabels);
end;

function TPatchMaker.IsValidPos (Pos: integer): boolean;
begin
  result := Math.InRange(Pos, 0, Self.Size);
end;

function TPatchMaker.NormalizePos (Pos: integer): integer;
begin
  result := Utils.IfThen(Pos = CURRENT_POS, Self.fPos, Pos);
  {!} Assert(Self.IsValidPos(result));
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
  Cmd:        IPatchCmd;
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
        
        vtObject: begin
          if SysUtils.Supports(vObject, IPatchCmd, Cmd) then begin
            Cmd.Execute(Self);
          end else begin
            Self.WriteBytes(sizeof(vObject), @vObject);
          end;
        end;
        
        vtClass:      Self.WriteBytes(sizeof(vClass),                  @vClass);
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

  result := Self;
end; // .function TPatchMaker.Write

function TPatchMaker.Seek (Pos: integer): {U} TPatchMaker;
begin
   Self.fPos := Self.NormalizePos(Pos);
   result    := Self;
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

function TPatchMaker.GetPosTempAddr (Pos: integer = CURRENT_POS): {Un} pointer;
begin
  result := @Self.fBuf[Self.NormalizePos(Pos)];
end;

function TPatchMaker.NewLabel: string;
begin
  result := '`' + SysUtils.IntToStr(Self.fLabelAutoId);
  Inc(Self.fLabelAutoId);
end;

function TPatchMaker.PutLabel (const LabelName: string): {U} TPatchMaker;
var
  LabelPos: integer;

begin
  result   := Self;
  LabelPos := integer(Self.fLabels[LabelName]) - 1;
  {!} Assert((LabelPos = -1) or (LabelPos = Self.Pos), Format('TPatchMaker: Label "%s" is already binded to %d position. Cannot reassign it to %d position', [LabelName, LabelPos, Self.Pos]));
  Self.fLabels[LabelName] := Ptr(Self.Pos + 1);
end;

function TPatchMaker.GetLabelPos (const LabelName: string): integer;
begin
  result := integer(Self.fLabels[LabelName]) - 1;
end;

function TPatchMaker.ExecActionOnApply ({O} Action: TPatchAction; Pos: integer = CURRENT_POS): {U} TPatchMaker;
begin
  result := Self;
  Self.fPostponedActions.Add(TPostponedAction.Create(Action, Self.NormalizePos(Pos)));
end;

function TPatchMaker.GetPatch: {O} Utils.TArrayOfByte;
begin
  result     := Self.fBuf; Self.fBuf := nil;
  Self.fSize := 0;
  Self.fPos  := 0;
end;

procedure TPatchMaker.ExecPostponedActions (TargetAddr: pointer);
var
  i: integer;

begin
  {!} Assert(TargetAddr <> nil);

  for i := 0 to Self.fPostponedActions.Count - 1 do begin
    TPostponedAction(Self.fPostponedActions[i]).Execute(TargetAddr, Self);
  end;
end;

function TPatchMaker.ApplyPatch ({n} TargetAddr: pointer): {n} pointer;
begin
  {!} Assert(Utils.IsValidBuf(TargetAddr, Self.Size));
  result := Utils.PtrOfs(TargetAddr, Self.Size);
  // * * * * * //
  Utils.CopyMem(Self.Size, pointer(Self.fBuf), TargetAddr);
  Self.ExecPostponedActions(TargetAddr);
end;

function TPatchMaker.Clear: {U} TPatchMaker;
begin
  result            := Self;
  Self.fBuf         := nil;
  Self.fPos         := 0;
  Self.fSize        := 0;
  Self.fLabelAutoId := 1;
  Self.fPostponedActions.Clear;
  Self.fLabels.Clear;
end;

(* CopyCode: relative offsets are transform to internal and AddRealAddrLater is called for each *)

(* Fixes long calls/jumps with relative offsets for code, that was moved to another location *)
// procedure FixRelativeOffsetsInMovedCode (CodeAddr, OldCodeAddr: pointer; CodeSize: integer);
// var
//   Delta:  integer;
//   CmdPtr: PAnsiChar;
//   EndPtr: PAnsiChar;
//   Disasm: hde32.TDisasm;

// begin
//   {!} Assert(OldCodeAddr <> nil);
//   {!} Assert(CodeAddr <> nil);
//   {!} Assert(CodeSize >= 0);
//   CmdPtr := CodeAddr;
//   EndPtr := Utils.PtrOfs(CmdPtr, CodeSize);
//   // * * * * * //
//   if CodeSize >= JMP_CALL_CONST32_SIZE then begin
//     Delta := integer(CodeAddr) - integer(OldCodeAddr);

//     while CmdPtr < EndPtr do begin
//       hde32.hde32_disasm(CmdPtr, Disasm);
      
//       if (Disasm.Len = JMP_CALL_CONST32_SIZE) and (Disasm.Opcode in [OPCODE_JMP_CONST32, OPCODE_CALL_CONST32]) then begin
//         Dec(PJumpCall32Rec(CmdPtr).Offset, Delta);
//       end;
      
//       Inc(CmdPtr, Disasm.Len);
//     end; // .while
//   end; // .if
// end; // .procedure FixRelativeOffsetsInMovedCode

class function TPatchHelper.Init ({U} PatchMaker: TPatchMaker): TPatchHelper;
begin
  result.PatchMaker := PatchMaker;
end;

function TPatchHelper.WriteByte (Value: byte): TPatchHelper;
begin
  pbyte(Self.AllocAndSkip(sizeof(byte)))^ := Value;
end;

function TPatchHelper.WriteWord (Value: word): TPatchHelper;
begin
  pword(Self.AllocAndSkip(sizeof(word)))^ := Value;
end;

function TPatchHelper.WriteInt (Value: integer): TPatchHelper;
begin
  pinteger(Self.AllocAndSkip(sizeof(integer)))^ := Value;
end;

function TPatchHelper.WriteBytes (NumBytes: integer; {n} Buf: pointer): TPatchHelper;
begin
  Self.PatchMaker.WriteBytes(NumBytes, Buf);
  result := Self;
end;

function TPatchHelper.Write (const Args: array of const): TPatchHelper;
begin
  Self.PatchMaker.Write(Args);
  result := Self;
end;

function TPatchHelper.Seek (Pos: integer): TPatchHelper;
begin
  Self.PatchMaker.Seek(Pos);
  result := Self;
end;

function TPatchHelper.SeekRel (RelPos: integer): TPatchHelper;
begin
  result := Self.Seek(Self.Pos + RelPos);
end;

function TPatchHelper.Alloc (NumBytes: integer): {Un} pointer;
begin
  result := Self.PatchMaker.Alloc(NumBytes);
end;

function TPatchHelper.AllocAndSkip (NumBytes: integer): {Un} pointer;
begin
  result := Self.Alloc(NumBytes);
  Self.Seek(Self.Pos + NumBytes);
end;

function TPatchHelper.GetPosTempAddr (Pos: integer = CURRENT_POS): {Un} pointer;
begin
  result := Self.PatchMaker.GetPosTempAddr(Pos);
end;

function TPatchHelper.NewLabel: string;
begin
  result := Self.PatchMaker.NewLabel;
end;

function TPatchHelper.PutLabel (const LabelName: string): TPatchHelper;
begin
  Self.PatchMaker.PutLabel(LabelName);
  result := Self;
end;

function TPatchHelper.GetLabelPos (const LabelName: string): integer;
begin
  result := Self.PatchMaker.GetLabelPos(LabelName);
end;

function TPatchHelper.ExecActionOnApply ({O} Action: TPatchAction; Pos: integer = CURRENT_POS): {U} TPatchHelper;
begin
  Self.PatchMaker.ExecActionOnApply(Action, Pos);
  result := Self;
end;

function TPatchHelper.GetPatch: {O} Utils.TArrayOfByte;
begin
  result := Self.PatchMaker.GetPatch();
end;

function TPatchHelper.ApplyPatch ({n} TargetAddr: pointer): {n} pointer;
begin
  result := Self.PatchMaker.ApplyPatch(TargetAddr);
end;

function TPatchHelper.Clear: {U} TPatchHelper;
begin
  Self.PatchMaker.Clear;
  result := Self;
end;

function TPatchHelper.Pos: integer;
begin
  result := Self.PatchMaker.Pos;
end;

function TPatchHelper.Size: integer;
begin
  result := Self.PatchMaker.Size;
end;

function TPatchHelper.JumpLabel (JumpType: TJumpType; const LabelName: string): TPatchHelper;
var
  Opcode:     integer;
  OpcodeSize: integer;
  ArgSize:    integer;

begin
  Opcode := GetJumpOpcode(JumpType, OpcodeSize, ArgSize);
  Self.WriteBytes(OpcodeSize, @Opcode);

  if ArgSize > 1 then begin
    Self.ExecActionOnApply(TAddLabelPosAction.Create(LabelName));
    result := Self.WriteInt(-Self.Pos - sizeof(PJumpCall32Rec(nil)^.Offset));
  end else begin
    Self.ExecActionOnApply(TAddLabelPosByteAction.Create(LabelName));
    result := Self.WriteByte(-Self.Pos - sizeof(PJumpCall8Rec(nil)^.Offset));
  end;
end; // .function TPatchHelper.JumpLabel

function TPatchHelper.JumpPos (JumpType: TJumpType; Pos: integer): TPatchHelper;
var
  Opcode:     integer;
  OpcodeSize: integer;
  ArgSize:    integer;
  Offset:     integer;

begin
  Opcode := GetJumpOpcode(JumpType, OpcodeSize, ArgSize);
  Self.WriteBytes(OpcodeSize, @Opcode);

  if ArgSize > 1 then begin
    result := Self.WriteInt(Pos - Self.Pos - sizeof(PJumpCall32Rec(nil)^.Offset));
  end else begin
    Offset := Pos - Self.Pos - sizeof(PJumpCall8Rec(nil)^.Offset);
    {!} Assert(Math.InRange(Offset, low(smallint), high(smallint)), Format('Cannot write short jump, offset is too high: %d', [Offset]));
    result := Self.WriteByte(Offset);
  end;
end; // .function TPatchHelper.JumpPos

function TPatchHelper.Jump (JumpType: TJumpType; Addr: pointer): TPatchHelper;
var
  Opcode:     integer;
  OpcodeSize: integer;
  ArgSize:    integer;

begin
  {!} Assert(Addr <> nil);
  Opcode := GetJumpOpcode(JumpType, OpcodeSize, ArgSize);
  Self.WriteBytes(OpcodeSize, @Opcode);

  if ArgSize > 1 then begin
    Self.ExecActionOnApply(TSubRealAddrAction.Create);
    result := Self.WriteInt(integer(Addr) - sizeof(PJumpCall32Rec(nil)^.Offset));
  end else begin
    Self.ExecActionOnApply(TSubRealAddrByteAction.Create);
    result := Self.WriteByte(integer(Addr) - sizeof(PJumpCall8Rec(nil)^.Offset));
  end;
end; // .function TPatchHelper.Jump

function TPatchHelper.CallLabel (const LabelName: string): TPatchHelper;
begin
  Self.WriteByte(OPCODE_CALL_CONST32);
  Self.ExecActionOnApply(TAddLabelPosAction.Create(LabelName));
  result := Self.WriteInt(-Self.Pos - sizeof(PJumpCall32Rec(nil)^.Offset));
end;

function TPatchHelper.CallPos (Pos: integer): TPatchHelper;
begin
  Self.WriteByte(OPCODE_CALL_CONST32);
  result := Self.WriteInt(Pos - Self.Pos - sizeof(PJumpCall32Rec(nil)^.Offset));
end;

function TPatchHelper.Call (Addr: pointer): TPatchHelper;
begin
  {!} Assert(Addr <> nil);
  Self.WriteByte(OPCODE_CALL_CONST32);
  Self.ExecActionOnApply(TSubRealAddrAction.Create);
  result := Self.WriteInt(integer(Addr) - sizeof(PJumpCall32Rec(nil)^.Offset));
end;

function TPatchHelper.Ret (NumArgs: integer = 0): TPatchHelper;
begin
  {!} Assert(NumArgs >= 0);
  result := Self.WriteByte(Utils.IfThen(NumArgs = 0, OPCODE_RET, OPCODE_RET_CONST16));

  if NumArgs > 0 then begin
    Self.WriteWord(NumArgs * sizeof(integer));
  end;
end;

function TPatchHelper.FillBytes (NumBytes: integer; Value: byte): TPatchHelper;
begin
  {!} Assert(NumBytes >= 0);
  result := Self;

  if NumBytes > 0 then begin
    System.FillChar(pbyte(Self.AllocAndSkip(NumBytes))^, NumBytes, Value);
  end;
end;

function TPatchHelper.Nop (NumNops: integer = 1): TPatchHelper;
begin
  result := Self.FillBytes(NumNops, OPCODE_NOP);
end;

function TPatchHelper.PushConst32 (Value: integer): TPatchHelper;
begin
  Self.WriteByte(OPCODE_PUSH_CONST32);
  result := Self.WriteInt(Value);
end;

function TPatchHelper.WriteCode (MinNumBytes: integer; {n} CodeSource: pointer): TPatchHelper;
begin
  {!} Assert(Utils.IsValidBuf(CodeSource, MinNumBytes));

  if MinNumBytes > 0 then begin
    
  end; // .if
end; // .function TPatchHelper.WriteCode

end.