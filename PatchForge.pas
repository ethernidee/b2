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
  OPCODE_JMP_CONST32     = $E9;
  OPCODE_JE_CONST32      = $0F84;
  OPCODE_JNE_CONST32     = $0F85;
  OPCODE_JA_CONST32      = $0F87;
  OPCODE_JAE_CONST32     = $0F83;
  OPCODE_JB_CONST32      = $0F82;
  OPCODE_JBE_CONST32     = $0F86;
  OPCODE_JG_CONST32      = $0F8F;
  OPCODE_JGE_CONST32     = $0F8D;
  OPCODE_JL_CONST32      = $0F8C;
  OPCODE_JLE_CONST32     = $0F8E;
  OPCODE_CALL_CONST32    = $E8;
  OPCODE_PUSH_CONST32    = $68;
  OPCODE_MOV_EAX_CONST32 = $B8;

  (* TPatchHelper.Call/Jump flags *)
  JUMP_ABSOLUTE      = 1; // Jump using absolute address. Otherwise: using relative offset.
  JUMP_TO_LOCAL_ADDR = 2; // Jump to position inside patch (address is position). Otherwise: to external memory location.


type
  (* Jump/call OFFSET 32 instruction *)
  PJumpCall32Rec = ^TJumpCall32Rec;
  TJumpCall32Rec = packed record
    Opcode: byte;
    Offset: integer;

    (* Changes relative Offset field so, that it point to specified absolute address *)
    procedure SetTargetAddr ({n} Addr: pointer);
  end;

  TPatchMaker = class
   private
   {O} fUsedRealAddrs: DataLib.TList {OF Pos: integer};
   {O} fLabels:        DataLib.TDict {OF Pos: integer};
   {O} fUsedLabels:    {O} DataLib.TObjDict {OF Pos: integer => UsedLabel: TypeWrappers.TString};
       fBuf:           Utils.TArrayOfByte;
       fPos:           integer;
       fSize:          integer;


    procedure SetPos (NewPos: integer); inline;
    procedure Grow (NewSize: integer);
    procedure ApplyAddLabelPosToTarget (TargetAddr: pointer);
    procedure ApplyAddRealAddrToTarget (TargetAddr: pointer);

   protected
    (* Returns true if given position is valid position *)
    function IsValidPos (Pos: integer): boolean; inline;

    (* Returns validated position. Value of CURRENT_POS is replaced with current position *)
    function NormalizePos (Pos: integer): integer;

   public
    constructor Create;
    destructor Destroy; override;

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

    (* Gives current position a named label. Throws exception if label with the same name is used elsewhere. *)
    function PutLabel (const LabelName: string): {U} TPatchMaker;

    (* Marks specified position. During patch application real address of that dword will be substracted from dword value.
       Default position is current position. Returns self. *)
    function AddRealAddrLater (Pos: integer = CURRENT_POS): {U} TPatchMaker;

    (* Marks specified position. During patch application position of the label will be added to dword value at marked position.
       Default position is current position. Returns self. *)
    function AddLabelPosLater (const LabelName: string; Pos: integer = CURRENT_POS): {U} TPatchMaker;

    (* Returns not applied path in the form of raw bytes array of capacity, greater or equal to patch size. Internal buffer is cleared afterwards. *)
    function GetPatch: {O} Utils.TArrayOfByte;

    (* Applies patch at specified address. Automatically fixes marked offsets. Returns pointer to memory location right after the patch.
       Memory must be writable. *)
    function ApplyPatch ({n} TargetAddr: pointer): {n} pointer;

    property Pos:  integer read fPos write SetPos;
    property Size: integer read fSize;
  end; // .class TPatchMaker

  (* Custom patch command, which can be executed either separately or as one of TPatchMaker.Write arguments *)
  IPatchCmd = interface ['{EBD3A1C8-E3F0-4059-8D76-3A164E7E1163}']
    procedure Execute (p: TPatchMaker);
  end;

  TAddrSubtype   = (ADDR_LOCAL, ADDR_RELATIVE);
  TAddrType      = set of TAddrSubtype;
  TJumpCondition = (JMP, JE, JNE, JA, JAE, JB, JBE, JG, JGE, JL, JLE);

  TPatchHelper = packed record
   public
    (* Wrapped patch making engine *)
    {U} PatchMaker: TPatchMaker;

    (* Wraps PatchMaker into local helper wrapper and returns the helper *)
    class function Init ({U} PatchMaker: TPatchMaker): TPatchHelper; inline; static;
    
    function WriteByte (Value: byte): TPatchHelper;
    function WriteWord (Value: word): TPatchHelper;
    function WriteInt (Value: integer): TPatchHelper;
    function WriteBytes (NumBytes: integer; {n} Buf: pointer): TPatchHelper; inline;
    function Write (const Args: array of const): TPatchHelper;
    function Seek (Pos: integer): TPatchHelper; inline;
    function Alloc (NumBytes: integer): {Un} pointer; inline;
    function GetPosTempAddr (Pos: integer = CURRENT_POS): {Un} pointer; inline;
    function PutLabel (const LabelName: string): TPatchHelper; inline;
    function AddRealAddrLater (Pos: integer = CURRENT_POS): TPatchHelper; inline;
    function AddLabelPosLater (const LabelName: string; Pos: integer = CURRENT_POS): TPatchHelper; inline;
    function GetPatch: {O} Utils.TArrayOfByte; inline;
    function ApplyPatch ({n} TargetAddr: pointer): {n} pointer; inline;
    function Pos: integer; inline;
    function Size: integer; inline;

    (* Writes assembler instruction/s to jump to specified address. If JUMP_ABSOLUTE is set, EAX register is used to store target address.
       By default relative jump to external (non-patch) location is assumed. If JUMP_TO_RELATIVE_ADDR flag is set, Addr must be position
       in patch buffer. *)
    function Jump ({n} Addr: pointer; JumpFlags: integer = 0; Condition: TJumpCondition = JMP): TPatchHelper;
  end;


(***)  implementation  (***)


procedure TJumpCall32Rec.SetTargetAddr ({n} Addr: pointer);
begin
  Self.Offset := integer(Addr) - integer(@Self) - sizeof(Self);
end;

function TPatchMaker.IsValidPos (Pos: integer): boolean;
begin
  result := Math.InRange(Pos, 0, Self.Size);
end;

function TPatchMaker.NormalizePos (Pos: integer): integer;
begin
  result := Utils.IfElse(Pos = CURRENT_POS, Self.fPos, Pos);
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

constructor TPatchMaker.Create;
begin
  Self.fUsedRealAddrs := DataLib.NewList(not Utils.OWNS_ITEMS);
  Self.fLabels        := DataLib.NewDict(not Utils.OWNS_ITEMS, DataLib.CASE_SENSITIVE);
  Self.fUsedLabels    := DataLib.NewObjDict(Utils.OWNS_ITEMS);
end;

destructor TPatchMaker.Destroy;
begin
  SysUtils.FreeAndNil(Self.fUsedRealAddrs);
  SysUtils.FreeAndNil(Self.fLabels);
  SysUtils.FreeAndNil(Self.fUsedLabels);
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

function TPatchMaker.PutLabel (const LabelName: string): {U} TPatchMaker;
var
  LabelPos: integer;

begin
  result   := Self;
  LabelPos := integer(Self.fLabels[LabelName]) - 1;
  {!} Assert((LabelPos = -1) or (LabelPos = Self.Pos), Format('TPatchMaker: Label "%s" is already binded to %d position. Cannot reassign it to %d position', [LabelName, LabelPos, Self.Pos]));
  Self.fLabels[LabelName] := Ptr(Self.Pos + 1);
end;

function TPatchMaker.AddRealAddrLater (Pos: integer = CURRENT_POS): {U} TPatchMaker;
begin
  Self.fUsedRealAddrs.Add(Ptr(Self.NormalizePos(Pos)));
  result := Self;
end;

function TPatchMaker.AddLabelPosLater (const LabelName: string; Pos: integer = CURRENT_POS): {U} TPatchMaker;
var
{Un} ExistingPosLabel: TString;

begin
  Pos              := Self.NormalizePos(Pos);
  ExistingPosLabel := TString(Self.fUsedLabels[Ptr(Pos)]);
  result           := Self;
  // * * * * * //
  Assert(ExistingPosLabel = nil, Format(
    'TPatchMaker: Cannot add positions of two labels to the same dword at position %d. The first label: "%s". The second one: "%s"', [Pos, ExistingPosLabel.Value, LabelName]
  ));

  Self.fUsedLabels[Ptr(Pos)] := TString.Create(LabelName);
end; // .function TPatchMaker.AddLabelPosLater

function TPatchMaker.GetPatch: {O} Utils.TArrayOfByte;
begin
  result     := Self.fBuf; Self.fBuf := nil;
  Self.fSize := 0;
  Self.fPos  := 0;
end;

procedure TPatchMaker.ApplyAddLabelPosToTarget (TargetAddr: pointer);
var
  LabelName: string;
  LabelPos:  integer;

begin
  {!} Assert(TargetAddr <> nil);
  
  with DataLib.IterateObjDict(Self.fUsedLabels) do begin
    while IterNext() do begin
      LabelName := TString(IterValue).Value;
      LabelPos  := integer(Self.fLabels[LabelName]) - 1;
      {!} Assert(LabelPos >= 0, Format('TPatchMaker: position of unregistered label "%s" cannot be added to item at position %d', [LabelName, integer(IterKey)]));
      Inc(pinteger(Utils.PtrOfs(TargetAddr, integer(IterKey)))^, LabelPos);
    end;
  end;
end; // .procedure TPatchMaker.ApplyAddLabelPosToTarget

procedure TPatchMaker.ApplyAddRealAddrToTarget (TargetAddr: pointer);
var
  PrevHandledPos: integer;
  Pos:            integer;
  i:              integer;

begin
  {!} Assert(TargetAddr <> nil);
  fUsedRealAddrs.Sort;
  PrevHandledPos := -1;

  for i := 0 to Self.fUsedRealAddrs.Count - 1 do begin
    Pos := integer(Self.fUsedRealAddrs[i]);
    
    if Pos <> PrevHandledPos then begin
      Inc(pinteger(Utils.PtrOfs(TargetAddr, Pos))^, integer(TargetAddr));
      PrevHandledPos := Pos;
    end;
  end;
end; // .procedure TPatchMaker.ApplyAddRealAddrToTarget

function TPatchMaker.ApplyPatch ({n} TargetAddr: pointer): {n} pointer;
begin
  {!} Assert(Utils.IsValidBuf(TargetAddr, Self.Size));
  result := Utils.PtrOfs(TargetAddr, Self.Size);
  // * * * * * //
  if Self.Size > 0 then begin
    Utils.CopyMem(Self.Size, pointer(Self.fBuf), TargetAddr);
    Self.ApplyAddLabelPosToTarget(TargetAddr);
    Self.ApplyAddRealAddrToTarget(TargetAddr);
  end; // .if
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
  pbyte(Self.PatchMaker.Alloc(sizeof(byte)))^ := Value;
end;

function TPatchHelper.WriteWord (Value: word): TPatchHelper;
begin
  pword(Self.PatchMaker.Alloc(sizeof(word)))^ := Value;
end;

function TPatchHelper.WriteInt (Value: integer): TPatchHelper;
begin
  pinteger(Self.PatchMaker.Alloc(sizeof(integer)))^ := Value;
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

function TPatchHelper.Alloc (NumBytes: integer): {Un} pointer;
begin
  result := Self.PatchMaker.Alloc(NumBytes);
end;

function TPatchHelper.GetPosTempAddr (Pos: integer = CURRENT_POS): {Un} pointer;
begin
  result := Self.PatchMaker.GetPosTempAddr(Pos);
end;

function TPatchHelper.PutLabel (const LabelName: string): TPatchHelper;
begin
  Self.PatchMaker.PutLabel(LabelName);
  result := Self;
end;

function TPatchHelper.AddRealAddrLater (Pos: integer = CURRENT_POS): TPatchHelper;
begin
  Self.PatchMaker.AddRealAddrLater(Pos);
  result := Self;
end;

function TPatchHelper.AddLabelPosLater (const LabelName: string; Pos: integer = CURRENT_POS): TPatchHelper;
begin
  Self.PatchMaker.AddLabelPosLater(LabelName, Pos);
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

function TPatchHelper.Pos: integer;
begin
  result := Self.PatchMaker.Pos;
end;

function TPatchHelper.Size: integer;
begin
  result := Self.PatchMaker.Size;
end;

function TPatchHelper.Jump ({n} Addr: pointer; JumpFlags: integer = 0; Condition: TJumpCondition = JMP): TPatchHelper;
var
  Opcode16: word;

begin
  // Absolute jump via MOV EAX, Addr; JMP EAX
  if Utils.FlagSet(JUMP_ABSOLUTE, JumpFlags) then begin
    Self.WriteByte(OPCODE_MOV_EAX_CONST32);

    if Utils.FlagSet(JUMP_TO_LOCAL_ADDR, JumpFlags) then begin
      Self.AddRealAddrLater;
    end;

    Self.WriteInt(integer(Addr));
  end
  // Relative jump
  else begin
    if Condition = JMP then begin
      Self.WriteByte(OPCODE_JMP_CONST32);
    end else begin
      case Condition of
        JE:  Opcode16 := OPCODE_JE_CONST32;
        JNE: Opcode16 := OPCODE_JNE_CONST32;
        JA:  Opcode16 := OPCODE_JA_CONST32;
        JAE: Opcode16 := OPCODE_JAE_CONST32;
        JB:  Opcode16 := OPCODE_JB_CONST32;
        JBE: Opcode16 := OPCODE_JBE_CONST32;
        JG:  Opcode16 := OPCODE_JG_CONST32;
        JGE: Opcode16 := OPCODE_JGE_CONST32;
        JL:  Opcode16 := OPCODE_JL_CONST32;
        JLE: Opcode16 := OPCODE_JLE_CONST32;
      else
        Assert(false);
      end; // .switch

      Self.WriteWord(Opcode16);
    end; // .else

    if not Utils.FlagSet(JUMP_TO_LOCAL_ADDR, JumpFlags) then begin
      Self.AddRealAddrLater;
    end;

    Self.WriteInt(integer(Addr) - Self.Pos - sizeof(PJumpCall32Rec(nil)^.Offset));
  end; // .else
end; // .function TPatchHelper.Jump

end.