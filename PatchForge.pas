unit PatchForge;
(*
  Code/data patching utilities and API hooking.
  Unit without third-party dependencies.
*)

(***)  interface  (***)


uses
  Windows, SysUtils, hde32, Utils, Math, Alg, DataLib;

type
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

PatchForge.WriteComplexPatch(TargetCode, [#$69#$69#$69#$69#$69, PatchForge.CMD_JUMP_TO, ]);


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

end.