unit AssocArraysAlt;
(*
  Author: Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft).
  Hash tables implementation.
  Robin Hood open address hashing, linear probing, preprocessed string key storage (for case-insensitive dictionaries).
*)


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

  PStrHashTableItem = ^TStrHashTableItem;
  TStrHashTableItem = record
    SearchDistance:  integer; // Counts from 1. 0 means empty
    Hash:            integer;
    PreprocessedKey: string;
    Value:           pointer;
    Key:             string;

    procedure MakeEmpty; inline;
    procedure Exchange (var OtherItem: TStrHashTableItem); inline;
  end;

  TStrHashTableItems          = array of TStrHashTableItem;
  TStrHashTableItemUnmanaged  = array [0..sizeof(TStrHashTableItem) - 1] of byte;
  TStrHashTableItemsUnmanaged = array of TStrHashTableItemUnmanaged;

  TStrHashTable = class (Utils.TCloneable)
   const
    MIN_CAPACITY    = 16;                  // Must be power of 2
    MAX_LOAD_FACTOR = 0.85;
    MIN_LOAD_FACTOR = MAX_LOAD_FACTOR / 4;
    GROWTH_FACTOR   = 2;                   // Must be power of 2

   protected
         fItems:             TStrHashTableItems;
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
    procedure FreeItemValue (Item: PStrHashTableItem);
    procedure FreeValues;

    (* Must be called each time, the capacity is changed to maintain dependent values *)
    procedure OnChangeCapacity;

    (* Discards existing storage and recreates the new one. Be sure to free/move values first *)
    procedure CreateNewStorage (NewCapacity: integer);

    (* On failure ItemInd is index to continue checking for possible displacement *)
    function FindItem (Hash: integer; const PreprocessedKey: string; var ItemInd: integer): {Un} PStrHashTableItem;

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

    procedure Assign (Source: Utils.TCloneable); override;
    procedure Clear;
    function  IsValidValue ({n} Value: pointer): boolean;
    function  GetValue (const Key: string): {n} pointer;
    function  HasKey (const Key: string): boolean;
    function  GetExistingValue (const Key: string; {out} var {Un} Res: pointer): boolean;
    procedure SetValue (const Key: string; {OUn} NewValue: pointer);
    function  DeleteItem (const Key: string): boolean;

    (* Returns value with specified key and nilify it in the array *)
    function TakeValue (const Key: string; {out} var {OUn} Value: pointer): boolean;

    (* Returns old value *)
    function ReplaceValue (const Key: string; {OUn} NewValue: pointer; {out} var {OUn} OldValue: pointer): boolean;

    procedure BeginIterate;
    function  IterateNext (out Key: string; out {Un} Value: pointer): boolean;
    procedure EndIterate;

    property HashFunc:          THashFunc read fHashFunc;
    property KeyPreprocessFunc: TKeyPreprocessFunc read fKeyPreprocessFunc;
    property OwnsItems:         boolean read fOwnsItems;
    property ItemsAreObjects:   boolean read fItemsAreObjects;
    property ItemCount:         integer read fSize;
    property ItemGuardProc:     Utils.TItemGuardProc read fItemGuardProc;
    property Locked:            boolean read fLocked;
    property Items[const Key: string]: pointer read {n} GetValue write {OUn} SetValue; default;

  end; // .class TStrHashTable

  TAssocArray = TStrHashTable;

function NewAssocArr (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc; OwnsItems, ItemsAreObjects: boolean; {n} ItemType: TClass; AllowNil: boolean): TStrHashTable;

procedure MergeArrays (Destination, Source: TStrHashTable);


(***)  implementation  (***)


procedure TStrHashTableItem.MakeEmpty;
begin
  if Self.SearchDistance <> EMPTY_ITEM_SEARCH_DISTANCE then begin
    Self.Key             := '';
    Self.PreprocessedKey := '';
    Self.SearchDistance  := EMPTY_ITEM_SEARCH_DISTANCE;
  end;
end;

procedure TStrHashTableItem.Exchange (var OtherItem: TStrHashTableItem);
var
  TempBuf: TStrHashTableItemUnmanaged;

begin
  System.Move(Self,      TempBuf,   sizeof(Self));
  System.Move(OtherItem, Self,      sizeof(Self));
  System.Move(TempBuf,   OtherItem, sizeof(Self));
end;

constructor TStrHashTable.Create (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc; OwnsItems, ItemsAreObjects: boolean; ItemGuardProc: Utils.TItemGuardProc;
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

destructor TStrHashTable.Destroy;
begin
  Self.FreeValues;
  SysUtils.FreeAndNil(Self.fItemGuard);
end;

procedure TStrHashTable.FreeItemValue (Item: PStrHashTableItem);
begin
  if Self.fOwnsItems then begin
    if Self.fItemsAreObjects then begin
      TObject(Item.Value).Free;
    end else begin
      FreeMem(Item.Value);
    end;
  end;
end;

procedure TStrHashTable.FreeValues;
var
  Item:     PStrHashTableItem;
  ItemsEnd: PStrHashTableItem;

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

procedure TStrHashTable.OnChangeCapacity;
begin
  Self.fMinSize         := Math.Max(Self.MIN_CAPACITY, trunc(Self.fCapacity * Self.MIN_LOAD_FACTOR));
  Self.fMaxSize         := trunc(Self.fCapacity * Self.MAX_LOAD_FACTOR) - 1;
  Self.fModCapacityMask := (1 shl Alg.IntLog2(Self.fCapacity)) - 1;
end;

procedure TStrHashTable.CreateNewStorage (NewCapacity: integer);
begin
  // Disallow null-capacity hash tables and non power of 2 capacities
  {!} Assert((NewCapacity > 0) and ((1 shl Alg.IntLog2(NewCapacity)) = NewCapacity));

  Self.fCapacity := NewCapacity;
  Self.fItems    := nil;
  SetLength(Self.fItems, Self.fCapacity);
  Self.OnChangeCapacity;
end;

procedure TStrHashTable.Clear;
begin
  Self.FreeValues;
  Self.CreateNewStorage(Self.MIN_CAPACITY);
end;

function TStrHashTable.IsValidValue ({n} Value: pointer): boolean;
begin
  result := Self.fItemGuardProc(Value, Self.fItemsAreObjects, Utils.TItemGuard(Self.fItemGuard));
end;

function TStrHashTable.GetPreprocessedKey (const Key: string): string;
begin
  if @Self.fKeyPreprocessFunc = nil then begin
    result := Key;
  end else begin
    result := Self.fKeyPreprocessFunc(Key);
  end;
end;

function TStrHashTable.FindItem (Hash: integer; const PreprocessedKey: string; var ItemInd: integer): {Un} PStrHashTableItem;
var
  Item:            PStrHashTableItem;
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
end; // .function TStrHashTable.FindItem

procedure TStrHashTable.SetValue (const Key: string; {OUn} NewValue: pointer);
var
{U} Item:            PStrHashTableItem;
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
end; // .procedure TStrHashTable.SetValue

procedure TStrHashTable.AddValue (Hash: integer; const Key, PreprocessedKey: string; {OUn} NewValue: pointer; ItemInd: integer);
var
  ModCapacityMask: integer;
  Item:            PStrHashTableItem;
  NewItem:         TStrHashTableItem;

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
end; // .procedure TStrHashTable.AddValue

function TStrHashTable.GetValue (const Key: string): {n} pointer;
var
{U} Item:            PStrHashTableItem;
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

function TStrHashTable.HasKey (const Key: string): boolean;
var
{U} Item:            PStrHashTableItem;
    ItemInd:         integer;
    PreprocessedKey: string;

begin
  PreprocessedKey := Self.GetPreprocessedKey(Key);
  Item            := Self.FindItem(Self.fHashFunc(PreprocessedKey), PreprocessedKey, ItemInd);
  result          := Item <> nil;
end;

function TStrHashTable.GetExistingValue (const Key: string; {out} var {Un} Res: pointer): boolean;
var
{U} Item:            PStrHashTableItem;
    ItemInd:         integer;
    PreprocessedKey: string;

begin
  PreprocessedKey := Self.GetPreprocessedKey(Key);
  Item            := Self.FindItem(Self.fHashFunc(PreprocessedKey), PreprocessedKey, ItemInd);
  result          := Item <> nil;

  if result then begin
    Res := Item.Value;
  end;
end;

function TStrHashTable.TakeValue (const Key: string; {out} var {OUn} Value: pointer): boolean;
var
{U} Item:            PStrHashTableItem;
    ItemInd:         integer;
    PreprocessedKey: string;

begin
  {!} Assert(Self.IsValidValue(nil));
  PreprocessedKey := Self.GetPreprocessedKey(Key);
  Item            := Self.FindItem(Self.fHashFunc(PreprocessedKey), PreprocessedKey, ItemInd);
  result          := Item <> nil;

  if Item <> nil then begin
    Value      := Item.Value;
    Item.Value := nil;
  end;
end;

function TStrHashTable.ReplaceValue (const Key: string; {OUn} NewValue: pointer; {out} var {OUn} OldValue: pointer): boolean;
var
{U} Item:            PStrHashTableItem;
    ItemInd:         integer;
    PreprocessedKey: string;

begin
  {!} Assert(Self.IsValidValue(NewValue));
  PreprocessedKey := Self.GetPreprocessedKey(Key);
  Item            := Self.FindItem(Self.fHashFunc(PreprocessedKey), PreprocessedKey, ItemInd);
  result          := Item <> nil;

  if Item <> nil then begin
    OldValue   := Item.Value;
    Item.Value := NewValue;
  end;
end;

procedure MergeArrays (Destination, Source: TStrHashTable);
var
{OUn} Value: pointer;
      Key:   string;

begin
  {!} Assert(Source <> nil);
  {!} Assert(not Destination.OwnsItems or (Destination.ItemsAreObjects and Source.ItemsAreObjects), 'Incompatible hash tables for merging due to items type and ownage');
  Source.BeginIterate;

  while Source.IterateNext(Key, Value) do begin
    {!} Assert(Destination.IsValidValue(Value));

    if Destination.OwnsItems then begin
      {!} Assert(TObject(Value) is Utils.TCloneable, 'Cannot merge array with non-clonable items. Got item: ' + TObject(Value).ClassName);
      Destination.SetValue(Key, Utils.TCloneable(Value).Clone);
    end else begin
      Destination.SetValue(Key, Value);
    end;
  end;

  Source.EndIterate;
end;

function TStrHashTable.DeleteItem (const Key: string): boolean;
var
{Un} PrevItem:        PStrHashTableItem;
{Un} Item:            PStrHashTableItem;
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
end; // .function TStrHashTable.DeleteItem

procedure TStrHashTable.Grow;
begin
  Self.Rehash(Self.fCapacity * GROWTH_FACTOR);
end;

procedure TStrHashTable.Shrink;
begin
  Self.Rehash(Self.fCapacity div GROWTH_FACTOR);
end;

procedure TStrHashTable.Rehash (NewCapacity: integer);
var
  OldItems:        TStrHashTableItems;
  OldItemsEnd:     PStrHashTableItem;
  OldItem:         PStrHashTableItem;
  Item:            PStrHashTableItem;
  ItemInd:         integer;
  NewItem:         TStrHashTableItem;
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
  TStrHashTableItemsUnmanaged(OldItems) := nil;
end; // .procedure TStrHashTable.Rehash

procedure TStrHashTable.BeginIterate;
begin
  Self.fIterPos := 0;
  Self.fLocked  := true;
end;

function TStrHashTable.IterateNext (out Key: string; out {Un} Value: pointer): boolean;
var
{Un} Item: PStrHashTableItem;

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

procedure TStrHashTable.EndIterate;
begin
  Self.fLocked := false;
end;

procedure TStrHashTable.Assign (Source: Utils.TCloneable);
var
{U} SrcTable: TStrHashTable;
    Item:     PStrHashTableItem;
    ItemsEnd: PStrHashTableItem;

begin
  {!} Assert(Source <> nil);
  {!} Assert(not Self.fLocked);
  SrcTable := Source as TStrHashTable;
  // * * * * * //
  if Self <> Source then begin
    {!} Assert(not SrcTable.fOwnsItems or SrcTable.fItemsAreObjects, 'Improssible to clone hash table if it contains owned non-object pointers');
    Self.FreeValues;
    Self.fHashFunc          := SrcTable.fHashFunc;
    Self.fKeyPreprocessFunc := SrcTable.fKeyPreprocessFunc;
    Self.fOwnsItems         := SrcTable.fOwnsItems;
    Self.fItemsAreObjects   := SrcTable.fItemsAreObjects;
    Self.fItemGuardProc     := SrcTable.fItemGuardProc;
    Self.fItemGuard         := SrcTable.fItemGuard.Clone;
    Self.fSize              := SrcTable.fSize;
    Self.fCapacity          := SrcTable.fCapacity;
    Self.OnChangeCapacity;
    Self.fItems             := Copy(SrcTable.fItems);

    if Self.fOwnsItems and Self.fItemsAreObjects then begin
      Item     := @Self.fItems[0];
      ItemsEnd := @Self.fItems[Self.fCapacity];

      while cardinal(Item) < cardinal(ItemsEnd) do begin
        if Item.Value <> nil then begin
          Item.Value := (TObject(Item.Value) as Utils.TCloneable).Clone;
        end;

        Inc(Item);
      end;
    end; // .if
  end; // .if
end; // .procedure TStrHashTable.Assign

function NewAssocArr (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc; OwnsItems, ItemsAreObjects: boolean; {n} ItemType: TClass; AllowNil: boolean): TStrHashTable;
var
{O} ItemGuard: Utils.TDefItemGuard;

begin
  {!} Assert(ItemsAreObjects or (ItemType = Utils.NO_TYPEGUARD));
  ItemGuard := Utils.TDefItemGuard.Create;
  // * * * * * //
  ItemGuard.ItemType := ItemType;
  ItemGuard.AllowNil := AllowNil;
  result             := TStrHashTable.Create(HashFunc, KeyPreprocessFunc, OwnsItems, ItemsAreObjects, Utils.DefItemGuardProc, ItemGuard);
end;

end.
