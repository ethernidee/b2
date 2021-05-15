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
  Math,
  SysUtils,

  Alg,
  Crypto,
  Utils;

{_DEFINE INSPECT_SEARCH_DISTANCE}

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

  THashTableItems          = array of THashTableItem;
  THashTableItemUnmanaged  = array [0..sizeof(THashTableItem) - 1] of byte;
  THashTableItemsUnmanaged = array of THashTableItemUnmanaged;

  THashTable = class (Utils.TCloneable)
   const
    MIN_CAPACITY    = 16;                  // Must be power of 2
    MAX_LOAD_FACTOR = 0.85;
    MIN_LOAD_FACTOR = MAX_LOAD_FACTOR / 4;
    GROWTH_FACTOR   = 2;                   // Must be power of 2

   protected
         fItems:             THashTableItems;
         fCapacity:          integer;
         fModCapacityMask:   integer; // (X and fModCapacityMask) = (X mod fCapacity)
         fSize:              integer;
         fMinSize:           integer;
         fMaxSize:           integer;
         fOwnsItems:         boolean;
         fItemsAreObjects:   boolean;
         fHashFunc:          THashFunc;
    {n}  fKeyPreprocessFunc: TKeyPreprocessFunc;
    {On} fItemGuard:         Utils.TItemGuard;
         fItemGuardProc:     Utils.TItemGuardProc;
         fIterPos:           integer;
         fLocked:            boolean;

    function GetPreprocessedKey (const Key: string): string; inline;
    procedure FreeItemValue (Item: PHashTableItem);
    procedure FreeValues;

    (* Discards existing storage and recreates the new one. Be sure to free/move values first *)
    procedure CreateNewStorage (NewCapacity: integer);

    (* On failure ItemInd is index to continue checking for possible displacement *)
    function FindItem (Hash: integer; const PreprocessedKey: string; var ItemInd: integer): {Un} PHashTableItem;

    procedure AddValue (Hash: integer; const Key, PreprocessedKey: string; {OUn} NewValue: pointer; ItemInd: integer);
    procedure Rehash (NewCapacity: integer);
    procedure Grow;
    procedure Shrink;

   public
    {$IFDEF INSPECT_SEARCH_DISTANCE}
      TotalSearchDistance: integer; // Increased by item search distance during items search only, not on rehashing
      MaxSearchDistance:   integer; // Maximum search distance, met during items search
    {$ENDIF}

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

    property HashFunc:          THashFunc read fHashFunc;
    property KeyPreprocessFunc: TKeyPreprocessFunc read fKeyPreprocessFunc;
    property OwnsItems:         boolean read fOwnsItems;
    property ItemsAreObjects:   boolean read fItemsAreObjects;
    property ItemCount:         integer read fSize;
    // property  ItemGuardProc:     Utils.TItemGuardProc read fItemGuardProc;
    // property  Locked:            boolean read fLocked;
    property Items[const Key: string]: pointer read {n} GetValue write {OUn} SetValue; default;

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

begin
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
  Self.CreateNewStorage(Self.MIN_CAPACITY);
end;

destructor THashTable.Destroy;
begin
  Self.FreeValues;
  SysUtils.FreeAndNil(Self.fItemGuard);
end;

procedure THashTable.FreeItemValue (Item: PHashTableItem);
begin
  if Self.fOwnsItems then begin
    if Self.fItemsAreObjects then begin
      TObject(Item.Value).Free;
    end else begin
      FreeMem(Item.Value);
    end;
  end;
end;

procedure THashTable.FreeValues;
var
  Item:     PHashTableItem;
  ItemsEnd: PHashTableItem;

begin
  if Self.fOwnsItems then begin
    Item     := @Self.fItems[0];
    ItemsEnd := @Self.fItems[Self.fCapacity];

    while cardinal(Item) < cardinal(ItemsEnd) do begin
      Self.FreeItemValue(Item);
      Inc(Item);
    end;
  end;
end;

procedure THashTable.CreateNewStorage (NewCapacity: integer);
begin
  // Disallow null-capacity hash tables and non power of 2 capacities
  {!} Assert((NewCapacity > 0) and ((1 shl Alg.IntLog2(NewCapacity)) = NewCapacity));

  Self.fCapacity        := NewCapacity;
  Self.fMinSize         := Math.Max(Self.MIN_CAPACITY, trunc(Self.fCapacity * Self.MIN_LOAD_FACTOR));
  Self.fMaxSize         := trunc(Self.fCapacity * Self.MAX_LOAD_FACTOR) - 1;
  Self.fModCapacityMask := (1 shl Alg.IntLog2(Self.fCapacity)) - 1;
  Self.fItems           := nil;
  SetLength(Self.fItems, Self.fCapacity);
end;

procedure THashTable.Clear;
begin
  Self.FreeValues;
  Self.CreateNewStorage(Self.MIN_CAPACITY);
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

function THashTable.FindItem (Hash: integer; const PreprocessedKey: string; var ItemInd: integer): {Un} PHashTableItem;
var
  Item:            PHashTableItem;
  SearchDistance:  integer;
  ModCapacityMask: integer;

begin
  result          := nil;
  ModCapacityMask := Self.fModCapacityMask;
  ItemInd         := Hash and ModCapacityMask;
  Item            := @Self.fItems[ItemInd];
  SearchDistance  := 1;

  while true do begin
    if (Item.SearchDistance = EMPTY_ITEM_SEARCH_DISTANCE) or (SearchDistance > Item.SearchDistance) then begin
      {$IFDEF INSPECT_SEARCH_DISTANCE}
        Inc(Self.TotalSearchDistance, SearchDistance);
        Self.MaxSearchDistance := Max(Self.MaxSearchDistance, SearchDistance);
      {$ENDIF}

      exit;
    end;

    if (Item.Hash = Hash) and (Item.PreprocessedKey = PreprocessedKey) then begin
      {$IFDEF INSPECT_SEARCH_DISTANCE}
        Inc(Self.TotalSearchDistance, SearchDistance);
        Self.MaxSearchDistance := Max(Self.MaxSearchDistance, SearchDistance);
      {$ENDIF}

      result := Item;
      exit;
    end;

    Inc(Item);
    Inc(SearchDistance);

    ItemInd := (ItemInd + 1) and ModCapacityMask;

    if ItemInd = 0 then begin
      Item := @Self.fItems[0];
    end;
  end; // .while
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

  if Self.fSize >= Self.fMaxSize then begin
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

    if Self.fSize <= Self.fMinSize then begin
      Self.Shrink;
    end;
  end; // .if
end; // .function THashTable.DeleteItem

procedure THashTable.Grow;
begin
  Self.Rehash(Self.fCapacity * GROWTH_FACTOR);
end;

procedure THashTable.Shrink;
begin
  Self.Rehash(Self.fCapacity div GROWTH_FACTOR);
end;

procedure THashTable.Rehash (NewCapacity: integer);
var
  OldItems:        THashTableItems;
  OldItemsEnd:     PHashTableItem;
  OldItem:         PHashTableItem;
  Item:            PHashTableItem;
  ItemInd:         integer;
  NewItem:         THashTableItem;
  ModCapacityMask: integer;

begin
  OldItems        := Self.fItems;
  OldItem         := @OldItems[0];
  OldItemsEnd     := @OldItems[Self.fCapacity];
  Self.CreateNewStorage(NewCapacity);
  ModCapacityMask := Self.fModCapacityMask;

  NewItem.MakeEmpty;

  while cardinal(OldItem) < cardinal(OldItemsEnd) do begin
    if OldItem.SearchDistance <> EMPTY_ITEM_SEARCH_DISTANCE then begin
      NewItem.Exchange(OldItem^);
      NewItem.SearchDistance := 1;

      ItemInd := NewItem.Hash and ModCapacityMask;
      Item    := @Self.fItems[ItemInd];

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

    Inc(OldItem);
  end; // .for

  // All old items strings must be '' and OldItems.RefCount = 1
  // Fast items deallocation without calling finalizer for each item to free string memory
  THashTableItemsUnmanaged(OldItems) := nil;
end; // .procedure THashTable.Rehash

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
