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
      end; // .else
      
      Inc(BufPos, Disasm.Len);
    end; // .while
  end else begin
    IsHook := false;
  end; // .else
end; // .function FindHookHandlerAddr