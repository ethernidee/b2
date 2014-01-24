unit Utils;
{
DESCRIPTION:  Addition to System unit
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Math;

const
  (* Global extra debugging marker *)
  DEBUG = TRUE;

  (* Relations between containers and their items *)
  OWNS_ITEMS        = TRUE;
  ITEMS_ARE_OBJECTS = TRUE;
  
  (* Items guards *)
  ALLOW_NIL     = TRUE;
  NO_TYPEGUARD  = nil;


type
  INT = INTEGER;

  (* Item pointers *)
  POBJECT   = ^TObject;
  PCLASS    = ^TClass;
  PCharByte = ^CHAR;
  PLONGBOOL = ^LONGBOOL;
  
  (* array pointers *)
  TEndlessByteArr       = array [0..MAXLONGINT div SIZEOF(BYTE) - 1] of BYTE;
  PEndlessByteArr       = ^TEndlessByteArr;
  TEndlessIntArr        = array [0..MAXLONGINT div SIZEOF(INTEGER) - 1] of INTEGER;
  PEndlessIntArr        = ^TEndlessIntArr;
  TEndlessBoolArr       = array [0..MAXLONGINT div SIZEOF(BOOLEAN) - 1] of BOOLEAN;
  PEndlessBoolArr       = ^TEndlessBoolArr;
  TEndlessCharArr       = array [0..MAXLONGINT div SIZEOF(CHAR) - 1] of CHAR;
  PEndlessCharArr       = ^TEndlessCharArr;
  TEndlessWideCharArr   = array [0..MAXLONGINT div SIZEOF(WideChar) - 1] of WideChar;
  PEndlessWideCharArr   = ^TEndlessWideCharArr;
  TEndlessSingleArr     = array [0..MAXLONGINT div SIZEOF(SINGLE) - 1] of SINGLE;
  PEndlessSingleArr     = ^TEndlessSingleArr;
  TEndlessExtArr        = array [0..MAXLONGINT div SIZEOF(EXTENDED) - 1] of EXTENDED;
  PEndlessExtArr        = ^TEndlessExtArr;
  TEndlessShortStrArr   = array [0..MAXLONGINT div SIZEOF(ShortString) - 1] of ShortString;
  PEndlessShortStrArr   = ^TEndlessShortStrArr;
  TEndlessPtrArr        = array [0..MAXLONGINT div SIZEOF(POINTER) - 1] of POINTER;
  PEndlessPtrArr        = ^TEndlessPtrArr;
  TEndlessPCharArr      = array [0..MAXLONGINT div SIZEOF(PCHAR) - 1] of PCHAR;
  PEndlessPCharArr      = ^TEndlessPCharArr;
  TEndlessPWideCharArr  = array [0..MAXLONGINT div SIZEOF(PWideChar) - 1] of PWideChar;
  PEndlessPWideCharArr  = ^TEndlessPWideCharArr;
  TEndlessObjArr        = array [0..MAXLONGINT div SIZEOF(TObject) - 1] of TObject;
  PEndlessObjArr        = ^TEndlessObjArr;
  TEndlessCurrArr       = array [0..MAXLONGINT div SIZEOF(CURRENCY) - 1] of CURRENCY;
  PEndlessCurrArr       = ^TEndlessCurrArr;
  TEndlessAnsiStrArr    = array [0..MAXLONGINT div SIZEOF(AnsiString) - 1] of AnsiString;
  PEndlessAnsiStrArr    = ^TEndlessAnsiStrArr;

  TArrayOfByte    = array of BYTE;
  TArrayOfInteger = array of INTEGER;
  TArrayOfString  = array of string;
  
  TCharSet  = set of CHAR;
  
  TEmptyRec = packed record end;
  
  TProcedure    = procedure;
  TObjProcedure = procedure of object;
  
  TCloneable  = class
    procedure Assign (Source: TCloneable); virtual;
    function  Clone: TCloneable;
  end; // .class TCloneable
  
  (* Containers items guards *)
  TItemGuard      = TCloneable;
  TItemGuardProc  = function ({n} Item: POINTER; ItemIsObject: BOOLEAN; {n} Guard: TCloneable): BOOLEAN;
  
  TDefItemGuard = class (TCloneable)
    ItemType: TClass;
    AllowNIL: BOOLEAN;
    
    procedure Assign (Source: TCloneable); override;
  end; // .class TDefItemGuard
  
  TEventHandler = procedure ({n} Mes: TObject) of object;


(* Low level functions *)
function  PtrOfs ({n} BasePtr: POINTER; Offset: INTEGER): POINTER; inline;
function  IsValidBuf ({n} Buf: POINTER; BufSize: INTEGER): BOOLEAN;
procedure CopyMem (Count: INTEGER; {n} Source, Destination: POINTER);
procedure Exchange (var A, B: INTEGER);
procedure SetPcharValue (What: PCHAR; const Value: string; BufSize: INTEGER);

(* Extra system functions *)
function  EVEN (Num: INTEGER): BOOLEAN;

(* Item guards *)
function  NoItemGuardProc ({n} Item: POINTER; ItemIsObject: BOOLEAN; {n} Guard: TCloneable): BOOLEAN;
function  DefItemGuardProc ({n} Item: POINTER; ItemIsObject: BOOLEAN; {n} Guard: TCloneable): BOOLEAN;

function  EqualMethods (A, B: TMethod): BOOLEAN;


(***)  implementation  (***)


function PtrOfs ({n} BasePtr: POINTER; Offset: INTEGER): POINTER;
begin
  result := POINTER(INTEGER(BasePtr) + Offset);
end; // .function PtrOfs

function IsValidBuf ({n} Buf: POINTER; BufSize: INTEGER): BOOLEAN;
begin
  {Buf <> NIL and BufSize = 0 is OK. Buf = NIL and BufSize > 0 is BAD. !BufSize >= 0}
  result := (BufSize >= 0) and ((Buf <> nil) or (BufSize = 0));
end; // .function IsValidBuf

procedure CopyMem (Count: INTEGER; {n} Source, Destination: POINTER);
begin
  {!} Assert(Count >= 0);
  {!} Assert((Count = 0) or ((Source <> nil) and (Destination <> nil)));
  System.MOVE(Source^, Destination^, Count);
end; // .procedure CopyMem

procedure Exchange (var A, B: INTEGER);
var
  C:  INTEGER;

begin
  C :=  A;
  A :=  B;
  B :=  C;
end; // .procedure Exchange

procedure SetPcharValue (What: PCHAR; const Value: string; BufSize: INTEGER);
var
  NumBytesToCopy: INTEGER;
   
begin
  {!} Assert(What <> nil);
  {!} Assert(BufSize > 0);
  NumBytesToCopy := Math.Min(LENGTH(Value), BufSize - 1);
  
  if NumBytesToCopy > 0 then begin
    CopyMem(LENGTH(Value), PCHAR(Value), What);
  end; // .if
  
  PCharByte(PtrOfs(What, NumBytesToCopy))^ := #0;
end; // .procedure SetPcharValue

procedure TCloneable.Assign (Source: TCloneable);
begin
end; // .procedure TCloneable.Assign

function TCloneable.Clone: TCloneable;
begin
  result := TCloneable(Self.ClassType.Create);
  result.Assign(Self);
end; // .function TCloneable.CreateNew

procedure TDefItemGuard.Assign (Source: TCloneable);
var
(* U *) SrcItemGuard: TDefItemGuard;

begin
  {!} Assert(Source <> nil);
  SrcItemGuard := Source AS TDefItemGuard;
  // * * * * * //
  Self.ItemType := SrcItemGuard.ItemType;
  Self.AllowNIL := SrcItemGuard.AllowNIL;
end; // .procedure TDefItemGuard.Assign

function EVEN (Num: INTEGER): BOOLEAN;
begin
  result := not ODD(Num);
end; // .function EVEN

function NoItemGuardProc ({n} Item: POINTER; ItemIsObject: BOOLEAN; {n} Guard: TCloneable): BOOLEAN;
begin
  result := TRUE;
end; // .function NoItemGuardProc

function DefItemGuardProc ({n} Item: POINTER; ItemIsObject: BOOLEAN; {n} Guard: TCloneable): BOOLEAN;
var
(* U *) MyGuard:  TDefItemGuard;
  
begin
  {!} Assert(Guard <> nil);
  MyGuard :=  Guard AS TDefItemGuard;
  // * * * * * //
  result := (Item <> nil) or (MyGuard.AllowNIL);
  if ItemIsObject and (Item <> nil) and (MyGuard.ItemType <> NO_TYPEGUARD) then begin
    result := result and (TObject(Item) IS MyGuard.ItemType);
  end; // .if
end; // .function DefItemGuardProc

function EqualMethods (A, B: TMethod): BOOLEAN;
begin
  result := (A.Code = B.Code) and (A.Data = B.Data);
end; // .function EqualMethods

end.
