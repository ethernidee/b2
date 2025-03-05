unit ApiJack;
(*
  Description: Code/data patching utilities and API hooking.
               All hooks are thread-safe.
*)

(***)  interface  (***)

uses
  Math,
  SysUtils,
  Windows,

  PatchForge,
  Utils;

type
  TCallingConv = (
    CONV_FIRST = 0,

    // Right-to-left, caller clean-up
    CONV_CDECL = 0,

    // Right-to-left
    CONV_STDCALL = 1,

    // Right-to-left, first argument in ECX
    CONV_THISCALL = 2,

    // Right-to-left, first two arguments in ECX, EDX
    CONV_FASTCALL = 3,

    // Left-to-right, first three arguments in EAX, EDX, ECX
    CONV_REGISTER = 4,

    // Left-to-right
    CONV_PASCAL = 5,

    CONV_LAST = 5
  ); // TCallingConv

  PAppliedPatch = ^TAppliedPatch;
  TAppliedPatch = record
         Addr:     pointer;
         OldBytes: Utils.TArrayOfByte;
         NewBytes: Utils.TArrayOfByte;
    {On} AuxBuf:   pointer;

    procedure Rollback;
    procedure Free;
    function IsOverwritten: boolean;
  end;

  TPatcher = class (PatchForge.TPatchMaker)
   protected
   {n} fTargetAddr: pointer;

   public
    constructor Create ({n} TargetAddr: pointer = nil);

    function SetTargetAddr ({n} NewTargetAddr: pointer): {U} TPatcher;
    function Clear: {U} TPatcher;

    (* Applies patch using module code writer, which differs from simple memory copying. Returns success flag *)
    function Apply: boolean;

    (* Extended version of {@see Apply} with autofree *)
    function ApplyAndFree: boolean;
  end;

  THookType = (HOOKTYPE_FIRST = 0, HOOKTYPE_BRIDGE = 0, HOOKTYPE_CALL = 1, HOOKTYPE_JUMP = 2, HOOKTYPE_LAST = 2);

  PHookContext = ^THookContext;

  THookContext = packed record
    EDI, ESI, EBP, ESP, EBX, EDX, ECX, EAX: integer;
    RetAddr:                                pointer;
  end;

  THookHandler = function (Context: PHookContext): LONGBOOL; stdcall;

  (* Writes arbitrary data to any write-protected section *)
  TWriteAtCode = function (NumBytes: integer; {n} Src, {n} Dst: pointer): boolean; stdcall;


(* Replaces original function with the new one with the same prototype and 1-2 extra arguments.
   Calling convention is changed to STDCALL. The new argument is callable pointer, which can be used to
   execute original function. The pointer is passed as THE FIRST argument. If custom parameter address
   is given, the value of custom parameter will be passed to handler as THE SECOND argument. If AppliedPatch
   pointer is given, it will be assigned an opaque pointer to applied patch data structure. This
   pointer can be used to rollback the patch (remove splicing).
   Returns address of the bridge to original function *)
function StdSplice (OrigFunc, HandlerFunc: pointer; CallingConv: TCallingConv; NumArgs: integer; {n} CustomParam: pinteger = nil; {n} AppliedPatch: PAppliedPatch = nil): {n} pointer;

(* Writes call to user handler at specified location. If handler returns true, overwritten commands are executed
   in original way. Otherwise they are skipped and Context.RetAddr is used to determine return address.
   Returns address to a default code bridge. It's possible to specify minimum number of bytes to be overriden by hook patch or nopped *)
function Hook (Addr: pointer; HandlerFunc: THookHandler; {n} AppliedPatch: PAppliedPatch = nil; MinPatchSize: integer = 0; HookType: THookType = HOOKTYPE_BRIDGE): {n} pointer;

(* Installs new code writing routine. Returns the previous one or nil *)
function SetCodeWriter (CodeWriter: TWriteAtCode): {n} TWriteAtCode;

(* Creates and returns new patcher instance *)
function CreatePatcher ({n} TargetAddr: pointer = nil): {O} TPatcher; inline;

(* Functions, allowing to call arbitrary functions with 'thiscall' calling convention without performance hit *)
function CallThis (Func: pointer; Arg1: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15: integer): integer; inline; overload;
function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15, Arg16: integer): integer; inline; overload;

(* Functions, allowing to call arbitrary functions with 'fastcall' calling convention without performance hit *)
function CallFast (Func: pointer; Arg1, Arg2: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15: integer): integer; inline; overload;
function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15, Arg16: integer): integer; inline; overload;


var
  WriteAtCode: TWriteAtCode = nil; // readonly, use SetCodeWriter to change


type
  TCallThis1 = function (_1, _2, Arg1: integer): integer; register;
  TCallThis2 = function (_1, _2, Arg1, Arg2: integer): integer; register;
  TCallThis3 = function (_1, _2, Arg1, Arg2, Arg3: integer): integer; register;
  TCallThis4 = function (_1, _2, Arg1, Arg2, Arg3, Arg4: integer): integer; register;
  TCallThis5 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5: integer): integer; register;
  TCallThis6 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6: integer): integer; register;
  TCallThis7 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7: integer): integer; register;
  TCallThis8 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8: integer): integer; register;
  TCallThis9 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9: integer): integer; register;
  TCallThis10 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10: integer): integer; register;
  TCallThis11 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11: integer): integer; register;
  TCallThis12 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12: integer): integer; register;
  TCallThis13 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13: integer): integer; register;
  TCallThis14 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14: integer): integer; register;
  TCallThis15 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15: integer): integer; register;
  TCallThis16 = function (_1, _2, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15, Arg16: integer): integer; register;
  TCallFast2 = function (_1, Arg1, Arg2: integer): integer; register;
  TCallFast3 = function (_1, Arg1, Arg2, Arg3: integer): integer; register;
  TCallFast4 = function (_1, Arg1, Arg2, Arg3, Arg4: integer): integer; register;
  TCallFast5 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5: integer): integer; register;
  TCallFast6 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6: integer): integer; register;
  TCallFast7 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7: integer): integer; register;
  TCallFast8 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8: integer): integer; register;
  TCallFast9 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9: integer): integer; register;
  TCallFast10 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10: integer): integer; register;
  TCallFast11 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11: integer): integer; register;
  TCallFast12 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12: integer): integer; register;
  TCallFast13 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13: integer): integer; register;
  TCallFast14 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14: integer): integer; register;
  TCallFast15 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15: integer): integer; register;
  TCallFast16 = function (_1, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15, Arg16: integer): integer; register;


(***)  implementation  (***)


const
  RET_ADDR_SIZE = sizeof(integer);

type
  (* Import *)
  TPatch      = PatchForge.TPatch;
  TPatchMaker = PatchForge.TPatchMaker;

  CodeMemoryManager = record
    class procedure Alloc (var Addr: pointer; Size: integer); static;
    class procedure FreeAndNil (var {n} Addr: pointer); static;
  end;


class procedure CodeMemoryManager.Alloc (var Addr: pointer; Size: integer);
begin
  {!} Assert(@Addr <> nil);
  {!} Assert(Size >= 0);
  System.GetMem(Addr, Size);
  // TODO: unsure, that FastMM returns blocks with PAGE_EXECUTE_READWRITE attribute to not trigger DEP
end;

class procedure CodeMemoryManager.FreeAndNil (var {n} Addr: pointer);
begin
  {!} Assert(@Addr <> nil);
  System.FreeMem(Addr);
  Addr := nil;
end;

constructor TPatcher.Create ({n} TargetAddr: pointer);
begin
  inherited Create;
  Self.fTargetAddr := TargetAddr;
end;

function TPatcher.SetTargetAddr ({n} NewTargetAddr: pointer): {U} TPatcher;
begin
  Self.fTargetAddr := NewTargetAddr;
  result           := Self;
end;

function TPatcher.Clear: {U} TPatcher;
begin
  Self.Patch.Clear;
  Self.fTargetAddr := nil;
  result           := Self;
end;

function TPatcher.Apply: boolean;
var
{On} PatchDynamicBuf: Utils.PEndlessByteArr;
{U}  PatchBuf:        pointer;
     PatchStaticBuf:  array [0..255] of byte;

begin
  {!} Assert(Self.fTargetAddr <> nil, 'TPatcher.Apply: cannot apply to nil address');
  PatchDynamicBuf := nil;
  PatchBuf        := @PatchStaticBuf;
  // * * * * * //
  if Self.Patch.Size > sizeof(PatchStaticBuf) then begin
    GetMem(pointer(PatchDynamicBuf), Self.Patch.Size);
    PatchBuf := PatchDynamicBuf;
  end;

  Self.Patch.Apply(PatchBuf, Self.fTargetAddr);
  result := WriteAtCode(Self.Patch.Size, PatchBuf, Self.fTargetAddr);
  // * * * * * //
  FreeMem(PatchDynamicBuf);
end;

function TPatcher.ApplyAndFree: boolean;
begin
  result := Self.Apply;
  Self.Free;
end;

function CreatePatcher ({n} TargetAddr: pointer = nil): {O} TPatcher;
begin
  result := TPatcher.Create(TargetAddr);
end;

function SetCodeWriter (CodeWriter: TWriteAtCode): {n} TWriteAtCode;
begin
  {!} Assert(@CodeWriter <> nil);
  result      := WriteAtCode;
  WriteAtCode := CodeWriter;
end;

(* Writes arbitrary data to any write-protected section *)
function StdWriteAtCode (NumBytes: integer; {n} Src, {n} Dst: pointer): boolean; stdcall;
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
  end;
end;

procedure FillAppliedPatch ({n} AppliedPatch: PAppliedPatch; Addr: pointer; p: PatchForge.TPatch; {n} AuxBuf: pointer);
begin
  if AppliedPatch <> nil then begin
    AppliedPatch.Addr := Addr;
    SetLength(AppliedPatch.OldBytes, p.Size);
    SetLength(AppliedPatch.NewBytes, p.Size);
    Utils.CopyMem(p.Size, Addr, @AppliedPatch.OldBytes[0]);
    p.Apply(@AppliedPatch.NewBytes[0], Addr);
    AppliedPatch.AuxBuf := AuxBuf;
  end;
end;

function StdSplice (OrigFunc, HandlerFunc: pointer; CallingConv: TCallingConv; NumArgs: integer; {n} CustomParam: pinteger = nil; {n} AppliedPatch: PAppliedPatch = nil): pointer;
const
  CODE_ADDR_ALIGNMENT = 8;

var
{O}  p:                      TPatcher;
{OI} SpliceBridge:           pbyte; // Memory is owned by AppliedPatch or never freed
     NumStackArgs:           integer;
     ShouldDuplicateArgs:    boolean;
     OrigCodeBridgeStartPos: integer;
     OverwrittenCodeSize:    integer;

begin
  {!} Assert(OrigFunc <> nil);
  {!} Assert(HandlerFunc <> nil);
  {!} Assert(NumArgs >= 0);
  p            := TPatcher.Create(OrigFunc);
  SpliceBridge := nil;
  result       := nil;
  // * * * * * //
  if NumArgs = 0 then begin
    CallingConv := CONV_STDCALL;
  end;

  if (NumArgs = 1) and (CallingConv = CONV_FASTCALL) then begin
    CallingConv := CONV_THISCALL;
  end;

  NumStackArgs := NumArgs;

  if CallingConv = CONV_FASTCALL then begin
    Dec(NumStackArgs, 2);
  end else if CallingConv = CONV_THISCALL then begin
    Dec(NumStackArgs);
  end else if CallingConv = CONV_REGISTER then begin
    Dec(NumStackArgs, Min(3, NumArgs));
  end;

  ShouldDuplicateArgs := (NumStackArgs > 0) and ((CallingConv = CONV_PASCAL) or (CallingConv = CONV_REGISTER) or (CallingConv = CONV_CDECL));

  // === BEGIN generating SpliceBridge ===
  if ShouldDuplicateArgs then begin
    (* Prepare to duplicate arguments *)
    //   PUSH ESI
    //   PUSH EDI
    //   LEA ESI, [ESP + 12.]
    //   MOV EDI, ESI
    //   ADD ESI, NumStackArgs * 4
    p.WriteHex('56578D74E40C89F781C6').WriteInt(NumStackArgs * sizeof(integer));
  end;

  (* Push pascal/delphi register arguments and extra arguments *)
  if (CallingConv = CONV_PASCAL) or (CallingConv = CONV_REGISTER) then begin
    // PUSH OrigFuncBridge
    p.WriteByte($68).ExecActionOnApply(PatchForge.TAddLabelRealAddrAction.Create('OrigFuncBridge')).WriteInt(0);

    if CustomParam <> nil then begin
      // PUSH CustomParam
      p.WriteByte($68).WriteInt(CustomParam^);
    end;

    if CallingConv = CONV_REGISTER then begin
      // PUSH EAX
      p.WriteByte($50);

      if NumArgs >= 2 then begin
        // PUSH EDX
        p.WriteByte($52);
      end;

      if NumArgs >= 3 then begin
        // PUSH ECX
        p.WriteByte($51);
      end;
    end; // .if
  end; // .if

  if ShouldDuplicateArgs then begin
    (* Duplicate stack arguments *)
    // @Loop:
    //   SUB ESI, 4
    //   PUSH [DWORD ESI]
    //   CMP ESI, EDI
    //   JNE @Loop
    p.PutLabel('LoopCopyArgs').WriteHex('83EE04FF3639FE');
    p.JumpLabel(PatchForge.JNE, 'LoopCopyArgs');
  end;

  if not ShouldDuplicateArgs then begin
    // POP EAX; remember return address
    p.WriteByte($58);
  end;

  (* Push register arguments and extra parameters for right-to-left conventions *)
  if CallingConv = CONV_THISCALL then begin
    // PUSH ECX
    p.WriteByte($51);
  end else if CallingConv = CONV_FASTCALL then begin
    // PUSH EDX
    p.WriteByte($52);
    // PUSH ECX
    p.WriteByte($51);
  end;

  if (CallingConv <> CONV_PASCAL) and (CallingConv <> CONV_REGISTER) then begin
    if CustomParam <> nil then begin
      // PUSH CustomParam
      p.WriteByte($68).WriteInt(CustomParam^);
    end;

    // PUSH OrigFuncBridge
    p.WriteByte($68).ExecActionOnApply(PatchForge.TAddLabelRealAddrAction.Create('OrigFuncBridge')).WriteInt(0);
  end;

  if ShouldDuplicateArgs then begin
    // Call new handler
    p.Call(HandlerFunc);

    // POP EDI
    // POP ESI
    p.WriteWord($5E5F);

    // Perform stack cleanup for non-cdecl
    if CallingConv <> CONV_CDECL then begin
      // RET NumStackArgs * 4
      {!} Assert(NumStackArgs <= high(word) div 4, 'Too big number of stack arguments');
      p.WriteByte($C2).WriteWord(NumStackArgs * 4);
    end else begin
      // RET; original function will perform stack cleanup manually
      p.WriteByte($C3);
    end;
  end else begin
    // PUSH EAX; push return address
    p.WriteByte($50);

    // Jump to new handler, the return will lead to original calling function
    p.Jump(PatchForge.JMP, HandlerFunc);
  end; // .else

  // Ensure original code bridge is aligned
  p.Nop(p.Pos mod CODE_ADDR_ALIGNMENT);

  // Set result to offset from splice bridge start to original function bridge
  result := pointer(p.Pos);

  // Write original function bridge
  p.PutLabel('OrigFuncBridge');
  OrigCodeBridgeStartPos := p.Pos;
  p.WriteFromCode(OrigFunc, PatchForge.TMinCodeSizeDetector.Create(sizeof(PatchForge.TJumpCall32Rec)));
  OverwrittenCodeSize := p.Pos - OrigCodeBridgeStartPos;
  p.Jump(PatchForge.JMP, Utils.PtrOfs(OrigFunc, OverwrittenCodeSize));
  // === END generating SpliceBridge ===

  // Persist splice bridge
  CodeMemoryManager.Alloc(pointer(SpliceBridge), p.Size);

  // Write splice bridge code
  p.SetTargetAddr(SpliceBridge).Apply;

  // Turn result from offset to absolute address
  result := Ptr(integer(SpliceBridge) + integer(result));

  // Create and apply hook at target function start
  p.Clear();
  p.Jump(PatchForge.JMP, SpliceBridge);
  p.Nop(OverwrittenCodeSize - p.Pos);

  FillAppliedPatch(AppliedPatch, OrigFunc, p.Patch, SpliceBridge);
  p.SetTargetAddr(OrigFunc).Apply;
  // * * * * * //
  SysUtils.FreeAndNil(p);
end; // .function StdSplice

(* Hooks code by writing call/jmp instruction right to handler without any bridge and possibility to execute overwritten code *)
procedure DirectHook (Addr: pointer; HandlerFunc: THookHandler; HookType: THookType; {n} AppliedPatch: PAppliedPatch = nil; MinPatchSize: integer = 0);
begin
  {!} Assert(Addr <> nil);
  {!} Assert(@HandlerFunc <> nil);
  // * * * * * //
  with CreatePatcher(Addr) do begin
    if HookType = HOOKTYPE_JUMP then begin
      Jump(PatchForge.JMP, @HandlerFunc);
    end else if HookType = HOOKTYPE_CALL then begin
      Call(@HandlerFunc);
    end;

    Nop(Math.Max(MinPatchSize, Patch.Size) - Patch.Size);
    FillAppliedPatch(AppliedPatch, Addr, Patch, nil);
    ApplyAndFree;
  end;
end;

function BridgeHook (Addr: pointer; HandlerFunc: THookHandler; {n} AppliedPatch: PAppliedPatch = nil; MinPatchSize: integer = 0): pointer;
const
  INSTR_PUSHAD       = $60;
  INSTR_PUSH_ESP     = $54;
  INSTR_TEST_EAX_EAX = $C085;
  INSTR_POPAD        = $61;
  INSTR_ADD_ESP_4    = $04C483;

var
{O}  p:                     TPatcher;
{OI} HandlerBridge:         pbyte; // Memory is owned by AppliedPatch or never freed
     DontExecOrigCodeLabel: string;
     OverwrittenCodeSize:   integer;

begin
  {!} Assert(Addr <> nil);
  {!} Assert(@HandlerFunc <> nil);
  p             := TPatcher.Create;
  HandlerBridge := nil;
  result        := nil;
  // * * * * * //
  // === BEGIN generating HandlerBridge ===
  // Preserve registers and Push registers context as the only argument
  p.WriteByte(INSTR_PUSHAD);
  p.WriteByte(INSTR_PUSH_ESP);

  // Call new handler
  p.Call(@HandlerFunc);

  // If result is TRUE then restore registers and execute default code
  p.WriteWord(INSTR_TEST_EAX_EAX);
  p.JumpLabel(PatchForge.JE, p.NewAutoLabel(DontExecOrigCodeLabel));
  p.WriteByte(INSTR_POPAD);
  p.WriteTribyte(INSTR_ADD_ESP_4);

  // Set result to offset from handler bridge start to original code bridge
  result := pointer(p.Pos);

  // Write original code bridge
  OverwrittenCodeSize := Math.Max(MinPatchSize, PatchForge.GetCodeSize(Addr, sizeof(PatchForge.TJumpCall32Rec)));
  p.WriteFromCode(Addr, PatchForge.TFixedCodeSizeDetector.Create(OverwrittenCodeSize));
  p.Jump(PatchForge.JMP, Utils.PtrOfs(Addr, OverwrittenCodeSize));

  // :DontExecOrigCodeLabel
  p.PutLabel(DontExecOrigCodeLabel);
  p.WriteByte(INSTR_POPAD);
  p.WriteByte(PatchForge.OPCODE_RET);
  // === END generating HandlerBridge ===

  // Persist handler bridge
  CodeMemoryManager.Alloc(pointer(HandlerBridge), p.Size);
  p.SetTargetAddr(HandlerBridge).Apply;

  // Turn result from offset to absolute address
  result := Ptr(integer(HandlerBridge) + integer(result));

  // Create and apply hook at target address
  p.Clear;
  p.Call(HandlerBridge);
  p.Nop(OverwrittenCodeSize - p.Size);

  FillAppliedPatch(AppliedPatch, Addr, p.Patch, HandlerBridge);
  p.SetTargetAddr(Addr).Apply;
  // * * * * * //
  SysUtils.FreeAndNil(p);
end; // .function BridgeHook

function Hook (Addr: pointer; HandlerFunc: THookHandler; {n} AppliedPatch: PAppliedPatch = nil; MinPatchSize: integer = 0; HookType: THookType = HOOKTYPE_BRIDGE): {n} pointer;
begin
  if (HookType = HOOKTYPE_JUMP) or (HookType = HOOKTYPE_CALL) then begin
    result := nil;
    DirectHook(Addr, HandlerFunc, HookType, AppliedPatch, MinPatchSize);
  end else begin
    result := BridgeHook(Addr, HandlerFunc, AppliedPatch, MinPatchSize);
  end;
end;

procedure TAppliedPatch.Rollback;
begin
  if Self.OldBytes <> nil then begin
    WriteAtCode(Length(Self.OldBytes), @Self.OldBytes[0], Self.Addr);
  end;

  Self.Free;
end;

procedure TAppliedPatch.Free;
begin
  Self.OldBytes := nil;
  Self.NewBytes := nil;

  if Self.AuxBuf <> nil then begin
    CodeMemoryManager.FreeAndNil(Self.AuxBuf);
  end;
end;

function TAppliedPatch.IsOverwritten: boolean;
begin
  result := false;

  if Self.NewBytes <> nil then begin
    result := not SysUtils.CompareMem(Self.Addr, pointer(Self.NewBytes), Length(Self.NewBytes));
  end;
end;

function CallThis (Func: pointer; Arg1: integer): integer;
begin
  result := TCallThis1(Func)(0, 0, Arg1);
end;

function CallThis (Func: pointer; Arg1, Arg2: integer): integer;
begin
  result := TCallThis2(Func)(0, 0, Arg1, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3: integer): integer;
begin
  result := TCallThis3(Func)(0, 0, Arg1, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4: integer): integer;
begin
  result := TCallThis4(Func)(0, 0, Arg1, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5: integer): integer;
begin
  result := TCallThis5(Func)(0, 0, Arg1, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6: integer): integer;
begin
  result := TCallThis6(Func)(0, 0, Arg1, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7: integer): integer;
begin
  result := TCallThis7(Func)(0, 0, Arg1, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8: integer): integer;
begin
  result := TCallThis8(Func)(0, 0, Arg1, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9: integer): integer;
begin
  result := TCallThis9(Func)(0, 0, Arg1, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10: integer): integer;
begin
  result := TCallThis10(Func)(0, 0, Arg1, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11: integer): integer;
begin
  result := TCallThis11(Func)(0, 0, Arg1, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12: integer): integer;
begin
  result := TCallThis12(Func)(0, 0, Arg1, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13: integer): integer;
begin
  result := TCallThis13(Func)(0, 0, Arg1, Arg13, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14: integer): integer;
begin
  result := TCallThis14(Func)(0, 0, Arg1, Arg14, Arg13, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15: integer): integer;
begin
  result := TCallThis15(Func)(0, 0, Arg1, Arg15, Arg14, Arg13, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallThis (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15, Arg16: integer): integer;
begin
  result := TCallThis16(Func)(0, 0, Arg1, Arg16, Arg15, Arg14, Arg13, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3, Arg2);
end;

function CallFast (Func: pointer; Arg1, Arg2: integer): integer;

begin
  result := TCallFast2(Func)(0, Arg2, Arg1);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3: integer): integer;

begin
  result := TCallFast3(Func)(0, Arg2, Arg1, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4: integer): integer;

begin
  result := TCallFast4(Func)(0, Arg2, Arg1, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5: integer): integer;

begin
  result := TCallFast5(Func)(0, Arg2, Arg1, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6: integer): integer;

begin
  result := TCallFast6(Func)(0, Arg2, Arg1, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7: integer): integer;

begin
  result := TCallFast7(Func)(0, Arg2, Arg1, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8: integer): integer;

begin
  result := TCallFast8(Func)(0, Arg2, Arg1, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9: integer): integer;

begin
  result := TCallFast9(Func)(0, Arg2, Arg1, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10: integer): integer;

begin
  result := TCallFast10(Func)(0, Arg2, Arg1, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11: integer): integer;

begin
  result := TCallFast11(Func)(0, Arg2, Arg1, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12: integer): integer;

begin
  result := TCallFast12(Func)(0, Arg2, Arg1, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13: integer): integer;

begin
  result := TCallFast13(Func)(0, Arg2, Arg1, Arg13, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14: integer): integer;

begin
  result := TCallFast14(Func)(0, Arg2, Arg1, Arg14, Arg13, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15: integer): integer;

begin
  result := TCallFast15(Func)(0, Arg2, Arg1, Arg15, Arg14, Arg13, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

function CallFast (Func: pointer; Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9, Arg10, Arg11, Arg12, Arg13, Arg14, Arg15, Arg16: integer): integer;

begin
  result := TCallFast16(Func)(0, Arg2, Arg1, Arg16, Arg15, Arg14, Arg13, Arg12, Arg11, Arg10, Arg9, Arg8, Arg7, Arg6, Arg5, Arg4, Arg3);
end;

begin
  WriteAtCode := StdWriteAtCode;
end.