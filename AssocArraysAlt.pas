unit AssocArraysAlt;
{
DESCRIPTION:  Associative array implementation
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(*
The implementation uses binary tree and (in case of array with string keys) user provided hash function to store and retrieve data.
Tree is automatically rebalanced when critical search depth is met which is equal to 2X of balanced tree height.
Rebalancing is done by converting tree to linear node array and inserting nodes in empty tree.
*)

(*
  Robin Hood hashing, linear probing, preprocessed key storage (for case-insensitive dictionaries),
  open addressing, hash table.

*)

(*
@todo
.SearchDistance = EMPTY_ITEM_SEARCH_DISTANCE => IsEmpty

*)

// Grow, Shrink, Rehash, Shrink load factor is delete + load factor = max load factor / 4, but no less min
// Add shrink check to addvalue too
// Do not shrink in DeleteItem during iteration


(***)  interface  (***)


uses
  SysUtils,
  Math, // FIXME DELETEME
  DlgMes, //FIXME DELETEME

  Alg,
  Crypto,
  Utils;

const
  NO_KEY_PREPROCESS_FUNC = nil;

  EMPTY_ITEM_SEARCH_DISTANCE = 0;


type
  THashFunc          = function (const Str: string): integer;
  TKeyPreprocessFunc = function (const OrigKey: string): string;

  PHashTableItem = ^THashTableItem;
  THashTableItem = record
    SearchDistance:  integer; // Counts from 1. 0 means empty
    Hash:            integer;
    PreprocessedKey: string;
    Value:           pointer;
    Key:             string;

    procedure MakeEmpty; inline;
    procedure Exchange (var OtherItem: THashTableItem); inline;
  end;

  THashTableItems = array of THashTableItem;
  THashTableItemUnmanaged = array [0..sizeof(THashTableItem) - 1] of byte;
  THashTableItemsUnmanaged = array of THashTableItemUnmanaged;

  THashTable = class (Utils.TCloneable)
   const
    MIN_CAPACITY    = 16; // Must be power of 2
    MAX_LOAD_FACTOR = 0.85;
    GROWTH_FACTOR   = 2;  // Must be power of 2

   protected
         fItems:             THashTableItems;
         fSize:              integer;
         fCapacity:          integer;
         fCritCapacity:      integer;
         fModCapacityMask:   integer; // X and fModCapacityMask = X mod fCapacity
         fHashFunc:          THashFunc;
         fOwnsItems:         boolean;
         fItemsAreObjects:   boolean;
    {n}  fKeyPreprocessFunc: TKeyPreprocessFunc;
    {On} fItemGuard:         Utils.TItemGuard;
         fItemGuardProc:     Utils.TItemGuardProc;
         fIterPos:           integer;
    //       fIterNodes:         array of {U} PAssocArrayNode;
    // {U}   fIterCurrItem:      PAssocArrayItem;
    //       fIterNodeInd:       integer;
         fLocked:            boolean;

    function GetPreprocessedKey (const Key: string): string; inline;
    function CalcModCapacityMask: integer; inline;
    procedure FreeItemValue (Item: PHashTableItem);
    function FindItem (Hash: integer; const PreprocessedKey: string; var ItemInd: integer): {Un} PHashTableItem;
    procedure AddValue (Hash: integer; const Key, PreprocessedKey: string; {OUn} NewValue: pointer; ItemInd: integer);
    procedure Grow;

   public
    fTotalSearchDistance: integer;
    fMaxSearchDistance: integer;
    constructor Create (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc; OwnsItems, ItemsAreObjects: boolean; ItemGuardProc: Utils.TItemGuardProc;
                        {On} ItemGuard: Utils.TItemGuard);

    destructor  Destroy; override;

    // procedure Assign (Source: Utils.TCloneable); override;
    procedure Clear;
    // function  GetPreprocessedKey (const Key: string): string;
    function  IsValidValue ({n} Value: pointer): boolean;
    // function  CalcCritDepth: integer; inline;
    // procedure Rebuild;
    function  GetValue (const Key: string): {n} pointer;
    // function  HasKey (const Key: string): boolean;
    // function  GetExistingValue (const Key: string; out {Un} Res: pointer): boolean;
    procedure SetValue (const Key: string; {OUn} NewValue: pointer);
    function  DeleteItem (const Key: string): boolean;

    // (* Returns value with specified key and NILify it in the array *)
    // function  TakeValue (Key: string; out {OUn} Value: pointer): boolean;

    // (* Returns old value *)
    // function  ReplaceValue
    // (
    //             Key:      string;
    //       {OUn} NewValue: pointer;
    //   out {OUn} OldValue: pointer
    // ): boolean;

    // procedure Merge (Source: THashTable);

    procedure BeginIterate;
    function  IterateNext (out Key: string; out {Un} Value: pointer): boolean;
    procedure EndIterate;

    // property  HashFunc:          THashFunc read fHashFunc;
    // property  KeyPreprocessFunc: TKeyPreprocessFunc read fKeyPreprocessFunc;
    // property  OwnsItems:         boolean read fOwnsItems;
    // property  ItemsAreObjects:   boolean read fItemsAreObjects;
    property  ItemCount:         integer read fSize;
    // property  ItemGuardProc:     Utils.TItemGuardProc read fItemGuardProc;
    // property  Locked:            boolean read fLocked;
    property  Items[const Key: string]: pointer read {n} GetValue write {OUn} SetValue; default;
    property Count: integer read fSize;
  end; // .class THashTable

  TAssocArray = THashTable;

function NewAssocArr (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc; OwnsItems, ItemsAreObjects: boolean; {n} ItemType: TClass; AllowNil: boolean): THashTable;


(***)  implementation  (***)


procedure THashTableItem.MakeEmpty;
begin
  if Self.SearchDistance <> EMPTY_ITEM_SEARCH_DISTANCE then begin
    Self.Key             := '';
    Self.PreprocessedKey := '';
    Self.SearchDistance  := EMPTY_ITEM_SEARCH_DISTANCE;
  end;
end;

procedure THashTableItem.Exchange (var OtherItem: THashTableItem);
var
  TempBuf: THashTableItemUnmanaged;
  // Temp: THashTableItem;

begin
  // Temp      := Self;
  // Self      := OtherItem;
  // OtherItem := Temp;
  System.Move(Self,      TempBuf,   sizeof(Self));
  System.Move(OtherItem, Self,      sizeof(Self));
  System.Move(TempBuf,   OtherItem, sizeof(Self));
end;

constructor THashTable.Create (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc; OwnsItems, ItemsAreObjects: boolean; ItemGuardProc: Utils.TItemGuardProc;
                               {On} ItemGuard: Utils.TItemGuard);
begin
  {!} Assert(@HashFunc      <> nil);
  {!} Assert(@ItemGuardProc <> nil);
  Self.fHashFunc          := HashFunc;
  Self.fKeyPreprocessFunc := KeyPreprocessFunc;
  Self.fOwnsItems         := OwnsItems;
  Self.fItemsAreObjects   := ItemsAreObjects;
  Self.fItemGuardProc     := ItemGuardProc;
  Self.fItemGuard         := ItemGuard;
  Self.fCapacity          := Self.MIN_CAPACITY;
  Self.fCritCapacity      := trunc(Self.MIN_CAPACITY * Self.MAX_LOAD_FACTOR) - 1;
  Self.fModCapacityMask   := Self.CalcModCapacityMask;
  SetLength(Self.fItems, Self.MIN_CAPACITY);
end; // .constructor THashTable.Create

destructor THashTable.Destroy;
begin
  Self.Clear;
  SysUtils.FreeAndNil(Self.fItemGuard);
end;

procedure THashTable.Clear;
var
  Item: PHashTableItem;
  i:    integer;

begin
  //
  // Values are not freed at all if owned and strings can be deallocated automatically.
  //

  Item := @Self.fItems[0];

  for i := 0 to Self.fCapacity - 1 do begin
    Item.MakeEmpty;
    Inc(Item);
  end;

  Self.fSize         := 0;
  Self.fCapacity     := Self.MIN_CAPACITY;
  Self.fCritCapacity := trunc(Self.MIN_CAPACITY * Self.MAX_LOAD_FACTOR) - 1;
  SetLength(Self.fItems, Self.MIN_CAPACITY);
end;

function THashTable.IsValidValue ({n} Value: pointer): boolean;
begin
  result := Self.fItemGuardProc(Value, Self.fItemsAreObjects, Utils.TItemGuard(Self.fItemGuard));
end;

function THashTable.GetPreprocessedKey (const Key: string): string;
begin
  if @Self.fKeyPreprocessFunc = nil then begin
    result := Key;
  end else begin
    result := Self.fKeyPreprocessFunc(Key);
  end;
end;

function THashTable.CalcModCapacityMask: integer;
begin
  result := (1 shl Alg.IntLog2(Self.fCapacity)) - 1;
end;

procedure THashTable.FreeItemValue (Item: PHashTableItem);
begin
  {!} Assert(Item <> nil);

  if Self.fOwnsItems then begin
    if Self.fItemsAreObjects then begin
      TObject(Item.Value).Free;
    end else begin
      FreeMem(Item.Value);
    end;
  end;
end;

function THashTable.FindItem (Hash: integer; const PreprocessedKey: string; var ItemInd: integer): {Un} PHashTableItem;
var
  Item:            PHashTableItem;
  SearchDistance:  integer;
  ModCapacityMask: integer;
  //ItemIndTemp:     integer;

label
  Quit;

begin
  result          := nil;
  ModCapacityMask := Self.fModCapacityMask;
  ItemInd         := Hash and ModCapacityMask;
  Item            := @Self.fItems[ItemInd];
  SearchDistance  := 1;

  while true do begin
    if (Item.SearchDistance = EMPTY_ITEM_SEARCH_DISTANCE) or (SearchDistance > Item.SearchDistance) then begin
      //inc(Self.fTotalSearchDistance, SearchDistance);
      //Self.fMaxSearchDistance := Max(Self.fMaxSearchDistance, SearchDistance);
      exit;
    end;

    if (Item.Hash = Hash) and (Item.PreprocessedKey = PreprocessedKey) then begin
      //inc(fTotalSearchDistance, SearchDistance);
      //Self.fMaxSearchDistance := Max(Self.fMaxSearchDistance, SearchDistance);
      result := Item;
      exit;
    end;

    // ItemInd не нужен вообще, вычислить MaxItemInd как @Self.fItems[Self.fCapacity] и проверять на него
    // ModCapacityMask будет использовать только один раз в этом случае

    Inc(Item);
    Inc(SearchDistance);

    ItemInd := (ItemInd + 1) and ModCapacityMask;

    if ItemInd = 0 then begin
      Item := @Self.fItems[0];
    end;
  end;

Quit:
  //ItemInd := ItemIndTemp;
end; // .function THashTable.FindItem

procedure THashTable.SetValue (const Key: string; {OUn} NewValue: pointer);
var
{U} Item:            PHashTableItem;
    PreprocessedKey: string;
    Hash:            integer;
    ItemInd:         integer;

begin
  {!} Assert(Self.IsValidValue(NewValue));
  PreprocessedKey := Self.GetPreprocessedKey(Key);
  Hash            := Self.fHashFunc(PreprocessedKey);
  Item            := Self.FindItem(Hash, PreprocessedKey, ItemInd);

  if Item <> nil then begin
    if Item.Value <> NewValue then begin
      Self.FreeItemValue(Item);
      Item.Value := NewValue;
    end;
  end else begin
    Self.AddValue(Hash, Key, PreprocessedKey, NewValue, ItemInd);
  end;
end; // .procedure THashTable.SetValue

procedure THashTable.AddValue (Hash: integer; const Key, PreprocessedKey: string; {OUn} NewValue: pointer; ItemInd: integer);
var
  ModCapacityMask: integer;
  Item:            PHashTableItem;
  NewItem:         THashTableItem;

begin
  {!} Assert(not Self.fLocked);
  Item                    := @Self.fItems[ItemInd];
  ModCapacityMask         := Self.fModCapacityMask;
  NewItem.Hash            := Hash;
  NewItem.Key             := Key;
  NewItem.PreprocessedKey := PreprocessedKey;
  NewItem.Value           := NewValue;
  NewItem.SearchDistance  := ItemInd - (Hash and ModCapacityMask) + 1;

  while Item.SearchDistance <> EMPTY_ITEM_SEARCH_DISTANCE do begin
    if NewItem.SearchDistance > Item.SearchDistance then begin
      NewItem.Exchange(Item^);
    end;

    Inc(Item);
    Inc(NewItem.SearchDistance);

    ItemInd := (ItemInd + 1) and ModCapacityMask;

    if ItemInd = 0 then begin
      Item := @Self.fItems[0];
    end;
  end;

  // Finally swap with empty item
  NewItem.Exchange(Item^);

  Inc(Self.fSize);

  if Self.fSize >= Self.fCritCapacity then begin
    Self.Grow;
  end;
end; // .procedure THashTable.AddValue

function THashTable.GetValue (const Key: string): {n} pointer;
var
{U} Item:            PHashTableItem;
    ItemInd:         integer;
    PreprocessedKey: string;

begin
  PreprocessedKey := Self.GetPreprocessedKey(Key);
  Item            := Self.FindItem(Self.fHashFunc(PreprocessedKey), PreprocessedKey, ItemInd);
  result          := nil;

  if Item <> nil then begin
    result := Item.Value;
  end;
end;

procedure THashTable.Grow;
var
  OldCapacity:     integer;
  OldItems:        THashTableItems;
  OldItem:         PHashTableItem;
  Item:            PHashTableItem;
  ItemInd:         integer;
  NewItem:         THashTableItem;
  ModCapacityMask: integer;
  i:               integer;

begin
  OldItems           := Self.fItems;
  Self.fItems        := nil;
  OldCapacity        := Self.fCapacity;
  Self.fCapacity     := Self.fCapacity * GROWTH_FACTOR;
  Self.fCritCapacity := trunc(Self.fCapacity * Self.MAX_LOAD_FACTOR) - 1;
  Self.fModCapacityMask := Self.CalcModCapacityMask;
  ModCapacityMask    := Self.fModCapacityMask;
  SetLength(Self.fItems, Self.fCapacity);


  NewItem.Key := '';
  NewItem.PreprocessedKey := '';
  // TNotManagedHashTableItems(OldItems) := nil; Fast clear old items if they do not contain any string !!!

  for i := 0 to OldCapacity - 1 do begin
    OldItem := @OldItems[i];

    if OldItem.SearchDistance <> EMPTY_ITEM_SEARCH_DISTANCE then begin
      NewItem.Exchange(OldItem^);
      ItemInd := NewItem.Hash and ModCapacityMask;
      Item    := @Self.fItems[ItemInd];

      // NewItem.Hash            := Hash;
      // NewItem.Key             := Key;
      // NewItem.PreprocessedKey := PreprocessedKey;
      // NewItem.Value           := NewValue;
      NewItem.SearchDistance := 1;

      while Item.SearchDistance <> EMPTY_ITEM_SEARCH_DISTANCE do begin
        if NewItem.SearchDistance > Item.SearchDistance then begin
          NewItem.Exchange(Item^);
        end;

        Inc(Item);
        Inc(NewItem.SearchDistance);

        ItemInd := (ItemInd + 1) and ModCapacityMask;

        if ItemInd = 0 then begin
          Item := @Self.fItems[0];
        end;
      end;

      // Finally swap with empty item
      NewItem.Exchange(Item^);
    end; // .if
  end; // .for

  THashTableItemsUnmanaged(OldItems) := nil; //UNSAFE IF ANY OLD ITEM HAD STRINGS
end;

function THashTable.DeleteItem (const Key: string): boolean;
var
{Un} PrevItem:        PHashTableItem;
{Un} Item:            PHashTableItem;
     PreprocessedKey: string;
     ItemInd:         integer;
     StartInd:        integer;
     ModCapacityMask: integer;

begin
  {!} Assert(not Self.fLocked);
  ModCapacityMask := Self.fModCapacityMask;
  PreprocessedKey := Self.GetPreprocessedKey(Key);
  PrevItem        := nil;
  Item            := Self.FindItem(Self.fHashFunc(PreprocessedKey), PreprocessedKey, ItemInd);
  result          := Item <> nil;

  if result then begin
    Self.FreeItemValue(Item);
    Item.MakeEmpty;
    Dec(Self.fSize);
    StartInd := (ItemInd + 1) and ModCapacityMask;

    while true do begin
      PrevItem := Item;
      Inc(Item);

      ItemInd := (ItemInd + 1) and ModCapacityMask;

      if ItemInd = 0 then begin
        Item := @Self.fItems[0];
      end;

      // Chain is ended, because this item is empty or on its own place
      if Item.SearchDistance <= 1 then begin
        break;
      end;

      Dec(Item.SearchDistance);
      System.Move(Item^, PrevItem^, sizeof(Item^));
    end; // .while

    // If displacement took place, the previous item is the moved one, we need to erase it memory
    if ItemInd <> StartInd then begin
      System.FillChar(PrevItem^, sizeof(PrevItem^), #0);
    end;
  end; // .if
end; // .function THashTable.DeleteItem

procedure THashTable.BeginIterate;
begin
  Self.fIterPos := 0;
  Self.fLocked  := true;
end;

function THashTable.IterateNext (out Key: string; out {Un} Value: pointer): boolean;
var
{Un} Item: PHashTableItem;

begin
  {!} Assert(Self.fLocked);
  Item := @Self.fItems[Self.fIterPos];

  while (Self.fIterPos < Self.fCapacity) and (Item.SearchDistance = EMPTY_ITEM_SEARCH_DISTANCE) do begin
    Inc(Item);
    Inc(Self.fIterPos);
  end;

  result := Self.fIterPos < Self.fCapacity;

  if result then begin
    Value := Item.Value;
    Key   := Item.Key;
    Inc(Self.fIterPos);
  end;
end;

procedure THashTable.EndIterate;
begin
  Self.fLocked := false;
end;

function NewAssocArr (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc; OwnsItems, ItemsAreObjects: boolean; {n} ItemType: TClass; AllowNil: boolean): THashTable;
var
{O} ItemGuard: Utils.TDefItemGuard;

begin
  {!} Assert(ItemsAreObjects or (ItemType = Utils.NO_TYPEGUARD));
  ItemGuard := Utils.TDefItemGuard.Create;
  // * * * * * //
  ItemGuard.ItemType := ItemType;
  ItemGuard.AllowNil := AllowNil;
  result             := THashTable.Create(HashFunc, KeyPreprocessFunc, OwnsItems, ItemsAreObjects, Utils.DefItemGuardProc, ItemGuard);
end;

end.
