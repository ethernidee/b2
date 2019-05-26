unit Utils;
{
DESCRIPTION:  Addition to System unit
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

{$ASSERTIONS ON}

(***)  interface  (***)
uses Math, SysUtils;

const
  (* Relations between containers and their items *)
  OWNS_ITEMS        = TRUE;
  ITEMS_ARE_OBJECTS = TRUE;
  
  (* Items guards *)
  ALLOW_NIL     = TRUE;
  NO_TYPEGUARD  = nil;

  (* For any bit-flags: no flags at all is 0 *)
  NO_FLAGS = 0;


type
  char = System.AnsiChar;
  int  = integer;

  (* Item pointers *)
  pobject   = ^TObject;
  pclass    = ^TClass;
  PCharByte = ^char;
  plongbool = ^LONGBOOL;
  
  (* array pointers *)
  TEndlessByteArr       = array [0..MAXLONGINT div sizeof(byte) - 1] of byte;
  PEndlessByteArr       = ^TEndlessByteArr;
  TEndlessWordArr       = array [0..MAXLONGINT div sizeof(word) - 1] of word;
  PEndlessWordArr       = ^TEndlessWordArr;
  TEndlessIntArr        = array [0..MAXLONGINT div sizeof(integer) - 1] of integer;
  PEndlessIntArr        = ^TEndlessIntArr;
  TEndlessBoolArr       = array [0..MAXLONGINT div sizeof(boolean) - 1] of boolean;
  PEndlessBoolArr       = ^TEndlessBoolArr;
  TEndlessCharArr       = array [0..MAXLONGINT div sizeof(char) - 1] of char;
  PEndlessCharArr       = ^TEndlessCharArr;
  TEndlessWideCharArr   = array [0..MAXLONGINT div sizeof(WideChar) - 1] of WideChar;
  PEndlessWideCharArr   = ^TEndlessWideCharArr;
  TEndlessSingleArr     = array [0..MAXLONGINT div sizeof(single) - 1] of single;
  PEndlessSingleArr     = ^TEndlessSingleArr;
  TEndlessExtArr        = array [0..MAXLONGINT div sizeof(extended) - 1] of extended;
  PEndlessExtArr        = ^TEndlessExtArr;
  TEndlessShortStrArr   = array [0..MAXLONGINT div sizeof(ShortString) - 1] of ShortString;
  PEndlessShortStrArr   = ^TEndlessShortStrArr;
  TEndlessPtrArr        = array [0..MAXLONGINT div sizeof(pointer) - 1] of pointer;
  PEndlessPtrArr        = ^TEndlessPtrArr;
  TEndlessPCharArr      = array [0..MAXLONGINT div sizeof(pchar) - 1] of pchar;
  PEndlessPCharArr      = ^TEndlessPCharArr;
  TEndlessPWideCharArr  = array [0..MAXLONGINT div sizeof(PWideChar) - 1] of PWideChar;
  PEndlessPWideCharArr  = ^TEndlessPWideCharArr;
  TEndlessObjArr        = array [0..MAXLONGINT div sizeof(TObject) - 1] of TObject;
  PEndlessObjArr        = ^TEndlessObjArr;
  TEndlessCurrArr       = array [0..MAXLONGINT div sizeof(currency) - 1] of currency;
  PEndlessCurrArr       = ^TEndlessCurrArr;
  TEndlessAnsiStrArr    = array [0..MAXLONGINT div sizeof(AnsiString) - 1] of AnsiString;
  PEndlessAnsiStrArr    = ^TEndlessAnsiStrArr;

  TArrayOfByte     = array of byte;
  TArrayOfInt      = array of integer;
  TArrayOfStr      = array of string;
  TArrayOfWideChar = array of WideChar;
  
  TCharSet  = set of char;
  
  TEmptyRec = packed record end;
  
  TProcedure    = procedure;
  TObjProcedure = procedure of object;
  
  TCloneable  = class
    procedure Assign (Source: TCloneable); virtual;
    function  Clone: TCloneable;
  end; // .class TCloneable
  
  (* Containers items guards *)
  TItemGuard      = TCloneable;
  TItemGuardProc  = function ({n} Item: pointer; ItemIsObject: boolean; {n} Guard: TCloneable): boolean;
  
  TDefItemGuard = class (TCloneable)
    ItemType: TClass;
    AllowNIL: boolean;
    
    procedure Assign (Source: TCloneable); override;
  end; // .class TDefItemGuard
  
  TEventHandler = procedure ({n} Mes: TObject) of object;

  (* Any Delphi's interface implementation, capable to be converted back to object *)
  IGetSelf = interface ['{96F10D1D-56EE-4997-BFE3-645D12ED24F9}']
    (* Fastest and reliable way to convert interface to object *)
    function GetSelf: TObject;
  end;

  TInterfaceAwareObject = class;

  IObjInterface = interface ['{DFB594D8-CCAF-40F3-985C-6C5C50A0A16B}']
    (* Converts interface to TInterfaceAwareObject, increasing RefCount and preserving all interface references.
       If HasMainOwner() is true, an exception will be thrown. *)
    function BecomeMainOwner: {O} TInterfaceAwareObject;

    (* Returns true if object has main owner in the form of regular object pointer *)
    function HasMainOwner: boolean;

    (* Fastest and reliable way to convert interface to object *)
    function GetSelf: TInterfaceAwareObject;
  end;

  (* Improved System.TInterfacedObject class, capable to be converted from interface back to raw object
     pointer. Has 0 ref count after construction and is totally interface driven, unless _AddRef is called. *)
  TInterfaceAwareObject = class (System.TInterfacedObject, IObjInterface)
   protected
    fHasMainOwner: integer;

   public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    (* Makes object fully managable via interfaces only if HasMainOwner() is true. Raw object pointer must not be used afterwards *)
    procedure ReleaseMainOwnage;
    
    function GetSelf: TInterfaceAwareObject;
    function HasMainOwner: boolean;
    function BecomeMainOwner: {O} TInterfaceAwareObject;
  end; // .class TInterfaceAwareObject

  (* Object must be accessible via interface reference only and cannot be Freed manually by default. *)
  TManagedObject = class (TInterfaceAwareObject)
   public
    procedure AfterConstruction; override;
  end;


(* Low level functions *)
function  PtrOfs ({n} BasePtr: pointer; Offset: integer): pointer; inline;
function  IsValidBuf ({n} Buf: pointer; BufSize: integer): boolean;
procedure CopyMem (Count: integer; {n} Source, Destination: pointer);
procedure Exchange (var A, B: integer);
procedure SetPcharValue (What: pchar; const Value: string; BufSize: integer);

(* Returns true if simple or complex bit flag is set *)
function  HasFlag (Flag, Flags: integer): boolean; inline;

(* Extra system functions *)
function  Even (Num: integer): boolean;

(* Item guards *)
function  NoItemGuardProc ({n} Item: pointer; ItemIsObject: boolean; {n} Guard: TCloneable): boolean;
function  DefItemGuardProc ({n} Item: pointer; ItemIsObject: boolean; {n} Guard: TCloneable): boolean;

function  EqualMethods (A, B: TMethod): boolean;

// Casts Obj to Class and assigns Res to Obj. Frees object on fail and assings nil to Res.
procedure CastOrFree ({On} Obj: TObject; CastToType: TClass; out {O} Res);

(* Given method. Returns its object/class *)
function ObjFromMethod (Method: TObjProcedure): TObject; inline;
function ClassFromMethod (Method: TObjProcedure): TClass; inline;

(* Ternary operator *)
function IfThen (Condition: boolean; SuccessResult: string; FailureResult: string): string; inline; overload;
function IfThen (Condition: boolean; SuccessResult: integer; FailureResult: integer): integer; inline; overload;
function IfThen (Condition: boolean; SuccessResult: pointer; FailureResult: pointer): pointer; inline; overload;

(* Attemps to convert interface to TObject. Currently object MUST implement IGetSelf for success *)
function ToObject (Intf: System.IInterface): {n} TObject;


(***)  implementation  (***)


function PtrOfs ({n} BasePtr: pointer; Offset: integer): pointer;
begin
  result := pointer(integer(BasePtr) + Offset);
end;

function IsValidBuf ({n} Buf: pointer; BufSize: integer): boolean;
begin
  {Buf <> NIL and BufSize = 0 is OK. Buf = NIL and BufSize > 0 is BAD. !BufSize >= 0}
  result := (BufSize >= 0) and ((Buf <> nil) or (BufSize = 0));
end;

procedure CopyMem (Count: integer; {n} Source, Destination: pointer);
begin
  {!} Assert(Count >= 0);
  {!} Assert((Count = 0) or ((Source <> nil) and (Destination <> nil)));
  System.Move(Source^, Destination^, Count);
end;

procedure Exchange (var A, B: integer);
var
  C:  integer;

begin
  C :=  A;
  A :=  B;
  B :=  C;
end;

procedure SetPcharValue (What: pchar; const Value: string; BufSize: integer);
var
  NumBytesToCopy: integer;
   
begin
  {!} Assert(What <> nil);
  {!} Assert(BufSize > 0);
  NumBytesToCopy := Math.Min(Length(Value), BufSize - 1);
  
  if NumBytesToCopy > 0 then begin
    CopyMem(Length(Value), pchar(Value), What);
  end;
  
  PCharByte(PtrOfs(What, NumBytesToCopy))^ := #0;
end; // .procedure SetPcharValue

function HasFlag (Flag, Flags: integer): boolean;
begin
  result := (Flags and Flag) = Flag;
end;

procedure TCloneable.Assign (Source: TCloneable);
begin
end;

function TCloneable.Clone: TCloneable;
begin
  result := TCloneable(Self.ClassType.Create);
  result.Assign(Self);
end;

procedure TDefItemGuard.Assign (Source: TCloneable);
var
(* U *) SrcItemGuard: TDefItemGuard;

begin
  {!} Assert(Source <> nil);
  SrcItemGuard := Source AS TDefItemGuard;
  // * * * * * //
  Self.ItemType := SrcItemGuard.ItemType;
  Self.AllowNIL := SrcItemGuard.AllowNIL;
end;

function Even (Num: integer): boolean;
begin
  result := not ODD(Num);
end;

function NoItemGuardProc ({n} Item: pointer; ItemIsObject: boolean; {n} Guard: TCloneable): boolean;
begin
  result := TRUE;
end;

function DefItemGuardProc ({n} Item: pointer; ItemIsObject: boolean; {n} Guard: TCloneable): boolean;
var
(* U *) MyGuard:  TDefItemGuard;
  
begin
  {!} Assert(Guard <> nil);
  MyGuard :=  Guard AS TDefItemGuard;
  // * * * * * //
  result := (Item <> nil) or (MyGuard.AllowNIL);
  if ItemIsObject and (Item <> nil) and (MyGuard.ItemType <> NO_TYPEGUARD) then begin
    result := result and (TObject(Item) IS MyGuard.ItemType);
  end;
end; // .function DefItemGuardProc

function EqualMethods (A, B: TMethod): boolean;
begin
  result := (A.Code = B.Code) and (A.Data = B.Data);
end;

procedure CastOrFree ({On} Obj: TObject; CastToType: TClass; out {O} Res);
begin
  if Obj is CastToType then begin
    TObject(Res) := Obj;
  end else begin
    Obj.Free;
    TObject(Res) := nil;
  end;
end;

function ObjFromMethod (Method: TObjProcedure): TObject; inline;
begin
  result := TObject(TMethod(Method).Data);
end;

function ClassFromMethod (Method: TObjProcedure): TClass; inline;
begin
  result := TClass(TMethod(Method).Data);
end;

function IfThen (Condition: boolean; SuccessResult: string; FailureResult: string): string; overload;    begin if Condition then result := SuccessResult else result := FailureResult; end;
function IfThen (Condition: boolean; SuccessResult: integer; FailureResult: integer): integer; overload; begin if Condition then result := SuccessResult else result := FailureResult; end;
function IfThen (Condition: boolean; SuccessResult: pointer; FailureResult: pointer): pointer; overload; begin if Condition then result := SuccessResult else result := FailureResult; end;

function InterlockedCompareExchange (var Destination: integer; NewValue, Comperand: integer): integer; stdcall; external 'kernel32' name 'InterlockedCompareExchange';
function InterlockedIncrement       (var Value: integer): integer; stdcall; external 'kernel32' name 'InterlockedIncrement';
function InterlockedDecrement       (var Value: integer): integer; stdcall; external 'kernel32' name 'InterlockedDecrement';

procedure TInterfaceAwareObject.AfterConstruction;
begin
  Self.fHasMainOwner := 1;
end;

procedure TInterfaceAwareObject.BeforeDestruction;
begin
  {!} Assert(Self.fHasMainOwner = Self.fRefCount, 'TInterfaceAwareObject consistency was broken. An attempt to destroy object with non-zero reference count or existing external owner');
end;

procedure TInterfaceAwareObject.ReleaseMainOwnage;
begin
  if (Self.fHasMainOwner <> 0) and (InterlockedCompareExchange(Self.fHasMainOwner, 0, 1) = 0) then begin
    Self._Release();
  end;
end;

function TInterfaceAwareObject.GetSelf: TInterfaceAwareObject;
begin
  result := Self;
end;

function TInterfaceAwareObject.HasMainOwner: boolean;
begin
  result := Self.fHasMainOwner <> 0;
end;

function TInterfaceAwareObject.BecomeMainOwner: {O} TInterfaceAwareObject;
begin
  {!} Assert(InterlockedIncrement(Self.fRefCount) > 1, 'Error trying to become main owner of freed interfaced object');
  {!} Assert((Self.fHasMainOwner = 0) and (InterlockedCompareExchange(Self.fHasMainOwner, 1, 0) = 1), 'Another pointer is main owner of interfaced object. Cannot become main owner');
  result := Self;
end;

procedure TManagedObject.AfterConstruction;
begin
  Self.fRefCount := 0;
end;

function ToObject (Intf: System.IInterface): {n} TObject;
var
  SelfGetter:  IObjInterface;
  SelfGetter2: IGetSelf;

begin
  result := nil;

  if SysUtils.Supports(Intf, IObjInterface, SelfGetter) then begin
    result := SelfGetter.GetSelf();
  end else if SysUtils.Supports(Intf, IGetSelf, SelfGetter2) then begin
    result := SelfGetter2.GetSelf();
  end;
end;

end.
