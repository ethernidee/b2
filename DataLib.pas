unit DataLib;
{
DESCRIPTION:  Convinient and widely used data structures
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses
  SysUtils,
  Utils, Crypto, Lists, AssocArrays;

const
  CASE_SENSITIVE    = FALSE; 
  CASE_INSENSITIVE  = not CASE_SENSITIVE;


type
  TDict     = AssocArrays.TAssocArray {OF TObject};
  TObjDict  = AssocArrays.TObjArray {OF TObject};
  TList     = Lists.TList {OF TObject};
  TStrList  = Lists.TStringList {OF TObject};

  (*  Combines access speed of TDist and order of TStrList  *)
  THashedList = class abstract
    protected
      function  GetItem (const Key: string): {Un} TObject; virtual; abstract;
      function  GetKey (Ind: INTEGER): string; virtual; abstract;
      function  GetValue (Ind: INTEGER): {Un} TObject; virtual; abstract;
      procedure SetValue (Ind: INTEGER; {OUn} NewValue: TObject); virtual; abstract;
      function  GetCount: INTEGER; virtual; abstract;
      
    public
      function  LinearFind (const Key: string; out Ind: INTEGER): BOOLEAN; virtual; abstract;
      procedure InsertBefore
      (
              const Key:        string;
        {OUn}       Value:      TObject;
                    BeforeInd:  INTEGER
      ); virtual; abstract;

      function  Take (Ind: INTEGER): {OUn} TObject; virtual; abstract;
      procedure Delete (Ind: INTEGER); virtual; abstract;
      procedure Clear; virtual; abstract;
      procedure  Add (const Key: string; {OUn} Value: TObject);

      property Count: INTEGER READ GetCount;
      
      property Items  [const Key: string]:  {n} TObject READ GetItem; default;
      property Keys   [Ind: INTEGER]:       string READ GetKey;
      property Values [Ind: INTEGER]:       {n} TObject READ GetValue WRITE SetValue;
  end; // .class THashedList
  
  IDictIterator = interface
    procedure BeginIterate ({U} Dict: TDict);
    function  IterNext: BOOLEAN;
    procedure EndIterate;
    function  GetIterKey: string;
    function  GetIterValue: {Un} TObject;
    
    property IterKey:   string READ GetIterKey;
    property IterValue: {n} TObject READ GetIterValue;
  end; // .interface IDictIterator
  
  IObjDictIterator = interface
    procedure BeginIterate (aDict: TObjDict);
    function  IterNext: BOOLEAN;
    procedure EndIterate;
    function  GetIterKey: {n} POINTER;
    function  GetIterValue: {n} TObject;
    
    property IterKey:   POINTER READ GetIterKey;
    property IterValue: {n} TObject READ GetIterValue;
  end; // .interface IObjDictIterator


function  NewDict (OwnsItems, CaseInsensitive: BOOLEAN): {O} TDict;
function  NewObjDict (OwnsItems: BOOLEAN): {O} TObjDict;
function  NewList (OwnsItems: BOOLEAN): {O} TList;
function  NewStrList (OwnsItems: BOOLEAN; CaseInsensitive: BOOLEAN): {O} TStrList;
function  NewStrListFromStrArr (StrArr: Utils.TArrayOfString;
                                OwnsItems: BOOLEAN; CaseInsensitive: BOOLEAN): {O} TStrList;
function  NewHashedList (OwnsItems, CaseInsensitive: BOOLEAN): {O} THashedList;
function  IterateDict ({U} Dict: TDict): IDictIterator;
function  IterateObjDict (aObjDict: TObjDict): IObjDictIterator;
procedure JoinLists (MainList, DependentList: TList);


(***) implementation (***)


type
  TStdHashedList = class (THashedList)
    protected
      {O} fItemList:  {OU} TStrList;
      {O} fItems:     {U}  TDict;

      function  GetItem (const Key: string): {Un} TObject; override;
      function  GetKey (Ind: INTEGER): string; override;
      function  GetValue (Ind: INTEGER): {Un} TObject; override;
      procedure SetValue (Ind: INTEGER; {OUn} NewValue: TObject); override;
      function  GetCount: INTEGER; override;

    public
      constructor Create (OwnsItems, CaseInsensitive: BOOLEAN);
      destructor  Destroy; override;

      function  LinearFind (const Key: string; out Ind: INTEGER): BOOLEAN; override;
      procedure InsertBefore
      (
              const Key:        string;
        {OUn}       Value:      TObject;
                    BeforeInd:  INTEGER
      ); override;

      function  Take (Ind: INTEGER): {OUn} TObject; override;
      procedure Delete (Ind: INTEGER); override;
      procedure Clear; override;
  end; // .class TStdHashedList
  
  TDictIterator = class (TInterfacedObject, IDictIterator)
    protected
      {U}   fDict:      TDict;
      {Un}  fIterValue: TObject;
            fIterKey:   string;
            fIterating: BOOLEAN;
            
    public
      procedure BeginIterate ({U} Dict: TDict);
      function  IterNext: BOOLEAN;
      procedure EndIterate;
      function  GetIterKey: string;
      function  GetIterValue: {Un} TObject;
  end; // .class TDictIterator
  
  TObjDictIterator = class (TInterfacedObject, IObjDictIterator)
    protected
      {U}   fObjDict:   TObjDict;
      {Un}  fIterValue: TObject;
      {Un}  fIterKey:   POINTER;
            fIterating: BOOLEAN;
            
    public
      procedure BeginIterate ({U} aObjDict: TObjDict);
      function  IterNext: BOOLEAN;
      procedure EndIterate;
      function  GetIterKey: {n} POINTER;
      function  GetIterValue: {n} TObject;
  end; // .class TObjDictIterator


function NewDict (OwnsItems, CaseInsensitive: BOOLEAN): {O} TDict;
var
  KeyPreprocessFunc:  AssocArrays.TKeyPreprocessFunc;

begin
  if CaseInsensitive then begin
    KeyPreprocessFunc :=  SysUtils.AnsiLowerCase;
  end // .if
  else begin
    KeyPreprocessFunc :=  nil;
  end; // .else

  result  :=  AssocArrays.NewAssocArr
  (
    Crypto.AnsiCRC32,
    KeyPreprocessFunc,
    OwnsItems,
    Utils.ITEMS_ARE_OBJECTS and OwnsItems,
    Utils.NO_TYPEGUARD,
    Utils.ALLOW_NIL
  );
end; // .function NewDict

function NewObjDict (OwnsItems: BOOLEAN): {O} TObjDict;
begin
  result  :=  AssocArrays.NewObjArr
  (
    OwnsItems,
    Utils.ITEMS_ARE_OBJECTS and OwnsItems,
    Utils.NO_TYPEGUARD,
    Utils.ALLOW_NIL
  );
end; // .function NewObjDict

function NewList (OwnsItems: BOOLEAN): {O} TList;
begin
  result  :=  Lists.NewList
  (
    OwnsItems,
    Utils.ITEMS_ARE_OBJECTS and OwnsItems,
    Utils.NO_TYPEGUARD,
    Utils.ALLOW_NIL
  );
end; // .function NewList

function NewStrList (OwnsItems: BOOLEAN; CaseInsensitive: BOOLEAN): {O} TStrList;
begin
  result := Lists.NewStrList
  (
    OwnsItems,
    Utils.ITEMS_ARE_OBJECTS and OwnsItems,
    Utils.NO_TYPEGUARD,
    Utils.ALLOW_NIL
  );
  
  result.CaseInsensitive := CaseInsensitive;
end; // .function NewStrList

function NewStrListFromStrArr (StrArr: Utils.TArrayOfString;
                               OwnsItems: BOOLEAN; CaseInsensitive: BOOLEAN): {O} TStrList;
var
  i: INTEGER;

begin
  result := NewStrList(OwnsItems, CaseInsensitive);

  for i := 0 to High(StrArr) do begin
    result.Add(StrArr[i]);
  end; // .for
end; // .function NewStrListFromStrArr

function NewHashedList (OwnsItems, CaseInsensitive: BOOLEAN): {O} THashedList;
begin
  result  :=  TStdHashedList.Create(OwnsItems, CaseInsensitive);
end; // .function NewHashedList

procedure THashedList.Add (const Key: string; {OUn} Value: TObject);
begin
  Self.InsertBefore(Key, Value, Self.Count);
end; // .procedure THashedList.Add

constructor TStdHashedList.Create (OwnsItems, CaseInsensitive: BOOLEAN);
begin
  Self.fItemList  :=  NewStrList(OwnsItems, CaseInsensitive);
  Self.fItems     :=  NewDict(not Utils.OWNS_ITEMS, CaseInsensitive);
end; // .constructor TStdHashedList.Create

destructor TStdHashedList.Destroy;
begin
  SysUtils.FreeAndNil(Self.fItems);
  SysUtils.FreeAndNil(Self.fItemList);
end; // .destructor TStdHashedList.Destroy

function TStdHashedList.GetItem (const Key: string): {Un} TObject;
begin
  result  :=  Self.fItems[Key];
end; // .function TStdHashedList.GetItem

function TStdHashedList.GetKey (Ind: INTEGER): string;
begin
  result  :=  Self.fItemList.Keys[Ind];
end; // .function TStdHashedList.GetKey

function TStdHashedList.GetValue (Ind: INTEGER): {Un} TObject;
begin
  result  :=  Self.fItemList.Values[Ind];
end; // .function TStdHashedList.GetValue

procedure TStdHashedList.SetValue (Ind: INTEGER; {OUn} NewValue: TObject);
begin
  Self.fItemList.Values[Ind]        :=  NewValue;
  Self.fItems[Self.fItemList[Ind]]  :=  NewValue;
end; // .procedure TStdHashedList.SetValue

function TStdHashedList.GetCount: INTEGER;
begin
  result  :=  Self.fItemList.Count;
end; // .function TStdHashedList.GetCount

function TStdHashedList.LinearFind (const Key: string; out Ind: INTEGER): BOOLEAN;
begin
  result  :=  Self.fItemList.Find(Key, Ind);
end; // .function TStdHashedList.LinearFind 

procedure TStdHashedList.InsertBefore
(
        const Key:        string;
  {OUn}       Value:      TObject;
              BeforeInd:  INTEGER
);

var
{U} OldValue: TObject;

begin
  OldValue  :=  nil;
  // * * * * * //
  {!} Assert(not Self.fItems.GetExistingValue(Key, POINTER(OldValue)));
  Self.fItemList.InsertObj(Key, Value, BeforeInd);
  Self.fItems[Key]  :=  Value;
end; // .procedure TStdHashedList.InsertBefore

procedure TStdHashedList.Delete (Ind: INTEGER);  
begin
  Self.fItems.DeleteItem(Self.fItemList[Ind]);
  Self.fItemList.Delete(Ind);
end; // .procedure TStdHashedList.Delete

function TStdHashedList.Take (Ind: INTEGER): {OUn} TObject;
begin
  result  :=  nil;
  Self.fItems.TakeValue(Self.fItemList[Ind], POINTER(result));
  Self.fItemList.TakeValue(Ind);
end; // .function TStdHashedList.Take

procedure TStdHashedList.Clear;
begin
  Self.fItemList.Clear;
  Self.fItems.Clear;
end; // .procedure TStdHashedList.Clear

procedure TDictIterator.BeginIterate ({U} Dict: TDict);
begin
  {!} Assert(Dict <> nil);
  {!} Assert(not Dict.Locked);
  Self.fDict      :=  Dict;
  Self.fIterating :=  TRUE;
  Dict.BeginIterate;
end; // .procedure TDictIterator.BeginIterate

function TDictIterator.IterNext: BOOLEAN;
begin
  {!} Assert(Self.fIterating);
  Self.fIterValue :=  nil;
  result          :=  Self.fDict.IterateNext(Self.fIterKey, POINTER(Self.fIterValue));
  
  if not result then begin
    Self.EndIterate;
  end; // .if
end; // .function TDictIterator.IterNext

procedure TDictIterator.EndIterate;
begin
  if Self.fIterating then begin
    Self.fDict.EndIterate;
    Self.fDict      :=  nil;
    Self.fIterating :=  FALSE;
  end; // .if
end; // .procedure TDictIterator.EndIterate

function TDictIterator.GetIterKey: string;
begin
  {!} Assert(Self.fIterating);
  result  :=  Self.fIterKey;
end; // .function TDictIterator.GetIterKey

function TDictIterator.GetIterValue: {Un} TObject;
begin
  {!} Assert(Self.fIterating);
  result  :=  Self.fIterValue;
end; // .function TDictIterator.GetIterValue

function IterateDict ({U} Dict: TDict): IDictIterator;
var
{O} DictIterator: TDictIterator;

begin
  {!} Assert(Dict <> nil);
  DictIterator  :=  TDictIterator.Create;
  // * * * * * //
  DictIterator.BeginIterate(Dict);
  result  :=  DictIterator; DictIterator  :=  nil;
end; // .function IterateDict

procedure TObjDictIterator.BeginIterate (aObjDict: TObjDict);
begin
  {!} Assert(aObjDict <> nil);
  {!} Assert(not aObjDict.Locked);
  Self.fObjDict   := aObjDict;
  Self.fIterating := TRUE;
  aObjDict.BeginIterate;
end; // .procedure TObjDictIterator.BeginIterate

function TObjDictIterator.IterNext: BOOLEAN;
begin
  {!} Assert(Self.fIterating);
  Self.fIterKey   := nil;
  Self.fIterValue := nil;
  result          := Self.fObjDict.IterateNext(Self.fIterKey, POINTER(Self.fIterValue));
  
  if not result then begin
    Self.EndIterate;
  end; // .if
end; // .function TObjDictIterator.IterNext

procedure TObjDictIterator.EndIterate;
begin
  if Self.fIterating then begin
    Self.fObjDict.EndIterate;
    Self.fObjDict   := nil;
    Self.fIterating := FALSE;
  end; // .if
end; // .procedure TObjDictIterator.EndIterate

function TObjDictIterator.GetIterKey: {n} POINTER;
begin
  {!} Assert(Self.fIterating);
  result := Self.fIterKey;
end; // .function TObjDictIterator.GetIterKey

function TObjDictIterator.GetIterValue: {n} TObject;
begin
  {!} Assert(Self.fIterating);
  result := Self.fIterValue;
end; // .function TObjDictIterator.GetIterValue

function IterateObjDict (aObjDict: TObjDict): IObjDictIterator;
var
{O} ObjDictIterator: TObjDictIterator;

begin
  {!} Assert(aObjDict <> nil);
  ObjDictIterator := TObjDictIterator.Create;
  // * * * * * //
  ObjDictIterator.BeginIterate(aObjDict);
  result := ObjDictIterator; ObjDictIterator := nil;
end; // .function IterateObjDict

procedure JoinLists (MainList, DependentList: TList);
var
  i: INTEGER;
   
begin
  {!} Assert(MainList <> nil);
  {!} Assert(DependentList <> nil);
  for i := 0 to DependentList.Count - 1 do begin
    MainList.Add(DependentList[i]);
  end; // .for
end; // .procedure JoinLists

end.
