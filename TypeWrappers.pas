unit TypeWrappers;
{
DESCRIPTION:  Wrappers for primitive types into objects suitable for storing in containers
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Utils;

type
  TString = class (Utils.TCloneable)
    Value: string;
    
    constructor Create (const Value: string);
    procedure Assign (Source: Utils.TCloneable); override;

    class function ToPchar ({n} Str: TString): pchar; static;
    class function ToString ({n} Str: TString): string; static;
  end; // .class TString

  TWideString = class (Utils.TCloneable)
    Value: WideString;
    
    constructor Create (const Value: WideString);
    procedure Assign (Source: Utils.TCloneable); override;
  end;

  TEventHandler = class (Utils.TCloneable)
    Handler:  Utils.TEventHandler;
    
    constructor Create (Handler: Utils.TEventHandler);
    procedure Assign (Source: Utils.TCloneable); override;
  end; // .class TEventHandler
  

(***) implementation (***)


constructor TString.Create (const Value: string);
begin
  Self.Value := Value;
end;

procedure TString.Assign (Source: Utils.TCloneable);
begin
  Self.Value := (Source AS TString).Value;
end;

class function TString.ToPchar ({n} Str: TString): pchar;
begin
  if Str = nil then begin
    result := '';
  end else begin
    result := pchar(Str.Value);
  end;
end;

class function TString.ToString ({n} Str: TString): string;
begin
  if Str = nil then begin
    result := '';
  end else begin
    result := Str.Value;
  end;
end;

constructor TWideString.Create (const Value: WideString);
begin
  Self.Value := Value;
end;

procedure TWideString.Assign (Source: Utils.TCloneable);
begin
  Self.Value := (Source AS TWideString).Value;
end;

constructor TEventHandler.Create (Handler: Utils.TEventHandler);
begin
  Self.Handler := Handler;
end; // .constructor TEventHandler.Create

procedure TEventHandler.Assign (Source: Utils.TCloneable);
begin
  Self.Handler := (Source AS TEventHandler).Handler;
end;

end.
