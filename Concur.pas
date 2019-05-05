unit Concur;
(* Author:      EtherniDee aka Berserker
   Desctiption: Support for concurrency *)

interface
uses Windows;

type
  TCritSection = record
   private
    fCritSection: Windows.TRTLCriticalSection;

   public
    procedure Init;
    procedure Enter;
    procedure SafeEnter (var ThreadVar_SectionEntered: boolean);
    procedure Leave;
    procedure SafeLeave (var ThreadVar_SectionEntered: boolean);
    procedure Delete;
  end; // .record TCritSection


(***)  implementation  (***)


procedure TCritSection.Init;
begin
  if System.IsMultiThread then begin
    Windows.InitializeCriticalSection(fCritSection);
  end else begin
    Windows.InitializeCriticalSectionAndSpinCount(fCritSection, 0);
  end;
end;

procedure TCritSection.Enter;
begin
  Windows.EnterCriticalSection(fCritSection);
end;

procedure TCritSection.SafeEnter (var ThreadVar_SectionEntered: boolean);
begin
  if not ThreadVar_SectionEntered then begin
    Windows.EnterCriticalSection(fCritSection);
    ThreadVar_SectionEntered := true;
  end;
end;

procedure TCritSection.Leave;
begin
  Windows.LeaveCriticalSection(fCritSection);
end;

procedure TCritSection.SafeLeave (var ThreadVar_SectionEntered: boolean);
begin
  if ThreadVar_SectionEntered then begin
    Windows.LeaveCriticalSection(fCritSection);
    ThreadVar_SectionEntered := false;
  end;
end;

procedure TCritSection.Delete;
begin
  Windows.DeleteCriticalSection(fCritSection);
end;

end.