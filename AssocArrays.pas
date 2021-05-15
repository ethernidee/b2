unit AssocArrays;
(*
  DESCRIPTION:  Associative array implementation
  AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
*)

(*
  The implementation uses binary tree and (in case of array with string keys) user provided hash function to store and retrieve data.
  Tree is automatically rebalanced when critical search depth is met which is equal to 2X of balanced tree height.
  Rebalancing is done by converting tree to linear node array and inserting nodes in empty tree.
  Possible improvement: compare hash first and preprocessed key then, thus getting rid of item chains and leaving nodes only.
  In this case no O(n^2) performance is possible.
*)

(***)  interface  (***)
uses
  Math,
  SysUtils,

  Alg,
  Crypto,
  Utils;

const
  LEFT_CHILD  = false;
  RIGHT_CHILD = true;

  NO_KEY_PREPROCESS_FUNC = nil;


type
  THashFunc          = function (const Str: string): integer;
  TKeyPreprocessFunc = function (const OrigKey: string): string;

  TChildNodeSide = boolean;

  PStrBinTreeItem = ^TStrBinTreeItem;
  TStrBinTreeItem = record
         Key:      string;
    {OUn}Value:    pointer;
    {On} NextItem: PStrBinTreeItem;
  end;

  PStrBinTreeNode = ^TStrBinTreeNode;
  TStrBinTreeNode = record
        Hash:       integer;
    {O} Item:       PStrBinTreeItem;
        ChildNodes: array [LEFT_CHILD..RIGHT_CHILD] of {On} PStrBinTreeNode;
  end;

  TStrBinTreeNodeArray = array of {O} PStrBinTreeNode;

  TStrBinTreeLinearNodeArray = record
    NodeArray: TStrBinTreeNodeArray; // Nodes are sorted by hash
    NodeCount: integer;
    ItemCount: integer;
  end;

  TStrBinTree = class (Utils.TCloneable)
    (***) protected (***)
      {On}  fRoot:              PStrBinTreeNode;
            fHashFunc:          THashFunc;
      {n}   fKeyPreprocessFunc: TKeyPreprocessFunc;
            fOwnsItems:         boolean;
            fItemsAreObjects:   boolean;
            fItemGuardProc:     Utils.TItemGuardProc;
      {On}  fItemGuard:         Utils.TItemGuard;
            fItemCount:         integer;
            fNodeCount:         integer;
            fIterNodes:         array of {U} PStrBinTreeNode;
      {U}   fIterCurrItem:      PStrBinTreeItem;
            fIterNodeInd:       integer;
            fLocked:            boolean;


      function  CloneItem (Item: PStrBinTreeItem): {O} PStrBinTreeItem;
      function  CloneNode ({n} Node: PStrBinTreeNode): {On} PStrBinTreeNode;
      procedure FreeItemValue (Item: PStrBinTreeItem);
      procedure FreeNode ({IN} var {n} Node: PStrBinTreeNode);
      procedure RemoveNode ({n} ParentNode: PStrBinTreeNode; ItemNode: PStrBinTreeNode);
      procedure RemoveItem
      (
        {n} ParentNode: PStrBinTreeNode;
            ItemNode:   PStrBinTreeNode;
        {n} ParentItem: PStrBinTreeItem;
            Item:       PStrBinTreeItem
      );

      (*
        All nodes are placed in NodeArray and disconnected from each other.
        Original binary tree is emptied. Nodes are sorted by hash.
      *)
      procedure ConvertToLinearNodeArray (out Res: TStrBinTreeLinearNodeArray);

      function  FindItem
      (
                    Hash:       integer;
              const Key:        string;
        out {ni}    ParentNode: PStrBinTreeNode;
        out {ni}    ItemNode:   PStrBinTreeNode;
        out {ni}    ParentItem: PStrBinTreeItem;
        out {ni}    Item:       PStrBinTreeItem
      ): boolean;

    (***) public (***)
      constructor Create
      (
                      HashFunc:           THashFunc;
                  {n} KeyPreprocessFunc:  TKeyPreprocessFunc;
                      OwnsItems:          boolean;
                      ItemsAreObjects:    boolean;
                      ItemGuardProc:      Utils.TItemGuardProc;
        {IN} var  {n} ItemGuard:          Utils.TItemGuard
      );

      destructor  Destroy; override;

      procedure Assign (Source: Utils.TCloneable); override;
      procedure Clear;
      function  GetPreprocessedKey (const Key: string): string;
      function  IsValidValue ({n} Value: pointer): boolean;
      function  CalcCritDepth: integer; inline;
      procedure Rebuild;
      function  GetValue (const Key: string): {n} pointer;
      function  HasKey (const Key: string): boolean;
      function  GetExistingValue (const Key: string; out {Un} Res: pointer): boolean;
      procedure SetValue (const Key: string; {OUn} NewValue: pointer);
      function  DeleteItem (const Key: string): boolean;

      (* Returns value with specified key and NILify it in the array *)
      function  TakeValue (Key: string; out {OUn} Value: pointer): boolean;

      (* Returns old value *)
      function  ReplaceValue
      (
                  Key:      string;
            {OUn} NewValue: pointer;
        out {OUn} OldValue: pointer
      ): boolean;

      procedure Merge (Source: TStrBinTree);

      procedure BeginIterate;
      function  IterateNext (out Key: string; out {Un} Value: pointer): boolean;
      procedure EndIterate;

      property  HashFunc:          THashFunc read fHashFunc;
      property  KeyPreprocessFunc: TKeyPreprocessFunc read fKeyPreprocessFunc;
      property  OwnsItems:         boolean read fOwnsItems;
      property  ItemsAreObjects:   boolean read fItemsAreObjects;
      property  ItemCount:         integer read fItemCount;
      property  ItemGuardProc:     Utils.TItemGuardProc read fItemGuardProc;
      property  NodeCount:         integer read fNodeCount;
      property  Locked:            boolean read fLocked;
      property  Items[const Key: string]: pointer read {n} GetValue write {OUn} SetValue; default;
  end; // .class TStrBinTree

  PObjBinTreeNode = ^TObjBinTreeNode;
  TObjBinTreeNode = record
          Hash:       integer;  // Hash is encoded {U} Key: pointer
    {OUn} Value:      pointer;
          ChildNodes: array [LEFT_CHILD..RIGHT_CHILD] of {On} PObjBinTreeNode;
  end;

  TObjBinTreeNodeArray = array of {O} PObjBinTreeNode;

  TObjBinTreeLinearNodeArray = record
    NodeArray: TObjBinTreeNodeArray; // Nodes are sorted by hash
    NodeCount: integer;
  end;

  TObjArray = class (Utils.TCloneable)
    (***) protected (***)
      {On}  fRoot:            PObjBinTreeNode;
            fOwnsItems:       boolean;
            fItemsAreObjects: boolean;
            fItemGuardProc:   Utils.TItemGuardProc;
      {On}  fItemGuard:       Utils.TItemGuard;
            fNodeCount:       integer;
            fIterNodes:       array of {U} PObjBinTreeNode;
            fIterNodeInd:     integer;
            fLocked:          boolean;

      function  HashToKey (Hash: integer): {n} pointer;
      function  KeyToHash (Key: {n} pointer): integer;
      procedure FreeNodeValue (Node: PObjBinTreeNode);
      procedure FreeNode ({IN} var {n} Node: PObjBinTreeNode);
      procedure RemoveNode ({n} ParentNode: PObjBinTreeNode; Node: PObjBinTreeNode);
      function  CloneNode (Node: PObjBinTreeNode): {O} PObjBinTreeNode;

      (*
        All nodes are placed in NodeArray and disconnected from each other.
        Original binary tree is emptied. Nodes are sorted by hash.
      *)
      procedure ConvertToLinearNodeArray (out Res: TObjBinTreeLinearNodeArray);

      function  FindItem
      (
            {n}   Key:        pointer;
        out {ni}  ParentNode: PObjBinTreeNode;
        out {ni}  ItemNode:   PObjBinTreeNode
      ): boolean;

    (***) public (***)
      constructor Create
      (
                      OwnsItems:        boolean;
                      ItemsAreObjects:  boolean;
                      ItemGuardProc:    Utils.TItemGuardProc;
        {IN} var  {n} ItemGuard:        Utils.TItemGuard
      );
      destructor  Destroy; override;
      procedure Assign (Source: Utils.TCloneable); override;
      procedure Clear;
      function  IsValidValue ({n} Value: pointer): boolean;
      function  CalcCritDepth: integer; inline;
      procedure Rebuild;
      function  GetValue ({n} Key: pointer): {n} pointer;
      function  GetExistingValue ({n} Key: pointer; out {Un} Res: pointer): boolean;
      procedure SetValue ({n} Key: pointer; {OUn} NewValue: pointer);
      function  DeleteItem ({n} Key: pointer): boolean;

      (* Returns value with specified key and NILify it in the array *)
      function  TakeValue ({n} Key: pointer; out {OUn} Value: pointer): boolean;

      {Returns old value}
      function  ReplaceValue
      (
            {n}   Key:      pointer;
            {OUn} NewValue: pointer;
        out {OUn} OldValue: pointer
      ): boolean;

      procedure BeginIterate;
      function  IterateNext (out {Un} Key: pointer; out {Un} Value: pointer): boolean;
      procedure EndIterate;

      property  OwnsItems:                boolean read fOwnsItems;
      property  ItemsAreObjects:          boolean read fItemsAreObjects;
      property  ItemCount:                integer read fNodeCount;
      property  ItemGuardProc:            Utils.TItemGuardProc read fItemGuardProc;
      property  NodeCount:                integer read fNodeCount;
      property  Locked:                   boolean read fLocked;
      property  Items[{n} Key: pointer]:  {OUn} pointer read GetValue write SetValue; default;
  end; // .class TObjArray

function NewAssocArr (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc; OwnsItems, ItemsAreObjects: boolean; ItemType: TClass; AllowNil: boolean): TStrBinTree;
function NewSimpleAssocArr (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc): TStrBinTree;

function NewStrictAssocArr ({n} TypeGuard: TClass; OwnsItems: boolean = true): TStrBinTree;
function NewObjArr (OwnsItems, ItemsAreObjects: boolean; ItemType: TClass; AllowNil: boolean): TObjArray;

function NewSimpleObjArr: TObjArray;
function NewStrictObjArr ({n} TypeGuard: TClass): TObjArray;


(***)  implementation  (***)


const
  DEBUG_BUILD = false;


constructor TStrBinTree.Create
(
                HashFunc:           THashFunc;
            {n} KeyPreprocessFunc:  TKeyPreprocessFunc;
                OwnsItems:          boolean;
                ItemsAreObjects:    boolean;
                ItemGuardProc:      Utils.TItemGuardProc;
  {IN} var  {n} ItemGuard:          Utils.TItemGuard
);
begin
  {!} Assert(@HashFunc      <> nil);
  {!} Assert(@ItemGuardProc <> nil);
  Self.fHashFunc          :=  HashFunc;
  Self.fKeyPreprocessFunc :=  KeyPreprocessFunc;
  Self.fOwnsItems         :=  OwnsItems;
  Self.fItemsAreObjects   :=  ItemsAreObjects;
  Self.fItemGuardProc     :=  ItemGuardProc;
  Self.fItemGuard         :=  ItemGuard;
  Self.fItemCount         :=  0;
  Self.fNodeCount         :=  0;
  ItemGuard               :=  nil;
end; // .constructor TStrBinTree.Create

destructor TStrBinTree.Destroy;
begin
  Self.Clear;
  SysUtils.FreeAndNil(Self.fItemGuard);
end; // .destructor TStrBinTree.Destroy

function TStrBinTree.CloneItem (Item: PStrBinTreeItem): {O} PStrBinTreeItem;
begin
  {!} Assert(Item <> nil);
  {!} Assert(Self.IsValidValue(Item.Value));
  New(result);
  result.Key := Item.Key;

  if Item.NextItem <> nil then begin
    result.NextItem := Self.CloneItem(Item.NextItem);
  end else begin
    result.NextItem := nil;
  end;

  if (Item.Value = nil) or (not Self.OwnsItems) then begin
    result.Value := Item.Value;
  end else begin
    {!} Assert(Self.ItemsAreObjects);
    {!} Assert(TObject(Item.Value) IS Utils.TCloneable);
    result.Value := Utils.TCloneable(Item.Value).Clone;
  end;
end; // .function TStrBinTree.CloneItem

function TStrBinTree.CloneNode ({n} Node: PStrBinTreeNode): {On} PStrBinTreeNode;
begin
  if Node = nil then begin
    result  := nil;
  end else begin
    New(result);
    result.Hash                    := Node.Hash;
    result.Item                    := Self.CloneItem(Node.Item);
    result.ChildNodes[LEFT_CHILD]  := Self.CloneNode(Node.ChildNodes[LEFT_CHILD]);
    result.ChildNodes[RIGHT_CHILD] := Self.CloneNode(Node.ChildNodes[RIGHT_CHILD]);
  end;
end; // .function TStrBinTree.CloneNode

procedure TStrBinTree.Assign (Source: Utils.TCloneable);
var
{U} SrcArr: TStrBinTree;

begin
  {!} Assert(not Self.Locked);
  {!} Assert(Source <> nil);
  SrcArr  := Source AS TStrBinTree;
  // * * * * * //
  if Self <> Source then begin
    Self.Clear;
    Self.fHashFunc          := SrcArr.HashFunc;
    Self.fKeyPreprocessFunc := SrcArr.KeyPreprocessFunc;
    Self.fOwnsItems         := SrcArr.OwnsItems;
    Self.fItemsAreObjects   := SrcArr.ItemsAreObjects;
    Self.fItemGuardProc     := SrcArr.ItemGuardProc;
    Self.fItemGuard         := SrcArr.fItemGuard.Clone;
    Self.fItemCount         := SrcArr.ItemCount;
    Self.fNodeCount         := SrcArr.NodeCount;
    Self.fRoot              := Self.CloneNode(SrcArr.fRoot);
  end; // .if
end; // .procedure TStrBinTree.Assign

procedure TStrBinTree.FreeItemValue (Item: PStrBinTreeItem);
begin
  {!} Assert(Item <> nil);
  if Self.OwnsItems then begin
    if Self.ItemsAreObjects then begin
      TObject(Item.Value).Free;
    end else begin
      FreeMem(Item.Value);
    end;
  end;

  Item.Value := nil;
end; // .procedure TStrBinTree.FreeItemValue

procedure TStrBinTree.RemoveNode ({n} ParentNode: PStrBinTreeNode; ItemNode: PStrBinTreeNode);
var
{U} RightClosestNodeParent: PStrBinTreeNode;
{U} RightClosestNode:       PStrBinTreeNode;
    ItemNodeIsRoot:         boolean;
    ItemNodeSide:           TChildNodeSide;

begin
  {!} Assert(ItemNode <> nil);
  RightClosestNodeParent := nil;
  RightClosestNode       := nil;
  ItemNodeSide           := false;
  // * * * * * //
  ItemNodeIsRoot := ParentNode = nil;

  if Self.NodeCount = 1 then begin
    {!} Assert(ItemNodeIsRoot);
    {!} Assert(ItemNode = Self.fRoot);
    Dispose(Self.fRoot); Self.fRoot := nil;
  end else begin
    if not ItemNodeIsRoot then begin
      ItemNodeSide := ItemNode.Hash >= ParentNode.Hash;
    end;

    (* N
      - -
    *)
    if
      (ItemNode.ChildNodes[LEFT_CHILD] = nil) and
      (ItemNode.ChildNodes[RIGHT_CHILD] = nil)
    then begin
      ParentNode.ChildNodes[ItemNodeSide] := nil;
      Dispose(ItemNode); ItemNode := nil;
    end // .if
    (* N
      - R
    *)
    else if ItemNode.ChildNodes[LEFT_CHILD] = nil then begin
      if ItemNodeIsRoot then begin
        Self.fRoot := ItemNode.ChildNodes[RIGHT_CHILD];
      end else begin
        ParentNode.ChildNodes[ItemNodeSide] := ItemNode.ChildNodes[RIGHT_CHILD];
      end;

      Dispose(ItemNode); ItemNode := nil;
    end // .elseif
    (* N
      L -
    *)
    else if ItemNode.ChildNodes[RIGHT_CHILD] = nil then begin
      if ItemNodeIsRoot then begin
        Self.fRoot := ItemNode.ChildNodes[LEFT_CHILD];
      end else begin
        ParentNode.ChildNodes[ItemNodeSide] := ItemNode.ChildNodes[LEFT_CHILD];
      end;

      Dispose(ItemNode); ItemNode := nil;
    end // .elseif
    (* N
      L R
    *)
    else begin
      RightClosestNodeParent := ItemNode;
      RightClosestNode       := ItemNode.ChildNodes[RIGHT_CHILD];

      while RightClosestNode.ChildNodes[LEFT_CHILD] <> nil do begin
        RightClosestNodeParent := RightClosestNode;
        RightClosestNode       := RightClosestNode.ChildNodes[LEFT_CHILD];
      end;

      ItemNode.Item := RightClosestNode.Item; RightClosestNode.Item := nil;
      ItemNode.Hash := RightClosestNode.Hash;
      Self.RemoveNode(RightClosestNodeParent, RightClosestNode);
    end; // .else
  end; // .else
end; // .procedure TStrBinTree.RemoveNode

procedure TStrBinTree.RemoveItem
(
  {n} ParentNode: PStrBinTreeNode;
      ItemNode:   PStrBinTreeNode;
  {n} ParentItem: PStrBinTreeItem;
      Item:       PStrBinTreeItem
);

begin
  {!} Assert(ItemNode <> nil);
  {!} Assert(Item <> nil);
  Self.FreeItemValue(Item);

  if (ItemNode.Item = Item) and (Item.NextItem = nil) then begin
    Self.RemoveNode(ParentNode, ItemNode);
    (* RemoveNode is recursive procedure not affecting the counter *)
    Dec(Self.fNodeCount);
  end else begin
    if ItemNode.Item = Item then begin
      ItemNode.Item := Item.NextItem;
    end else begin
      {!} Assert(ParentItem <> nil);
      ParentItem.NextItem := Item.NextItem;
    end;
  end; // .else

  Dispose(Item); Item := nil;
  Dec(Self.fItemCount);
end; // .procedure TStrBinTree.RemoveItem

procedure TStrBinTree.FreeNode ({IN} var {n} Node: PStrBinTreeNode);
var
{U} Item:     PStrBinTreeItem;
{U} NextItem: PStrBinTreeItem;

begin
  Item    := nil;
  NextItem := nil;
  // * * * * * //
  if Node <> nil then begin
    Item := Node.Item;

    while Item <> nil do begin
      NextItem := Item.NextItem;
      Self.FreeItemValue(Item);
      Dispose(Item); Item := nil;
      Item := NextItem;
    end;

    Self.FreeNode(Node.ChildNodes[LEFT_CHILD]);
    Self.FreeNode(Node.ChildNodes[RIGHT_CHILD]);
    Dispose(Node); Node := nil;
  end; // .if
end; // .procedure TStrBinTree.FreeNode

procedure TStrBinTree.Clear;
begin
  {!} Assert(not Self.Locked);
  Self.FreeNode(Self.fRoot);
  Self.fItemCount := 0;
  Self.fNodeCount := 0;
end;

function TStrBinTree.GetPreprocessedKey (const Key: string): string;
begin
  if @Self.KeyPreprocessFunc = nil then begin
    result := Key;
  end else begin
    result := Self.KeyPreprocessFunc(Key);
  end;
end;

function TStrBinTree.IsValidValue ({n} Value: pointer): boolean;
begin
  result := Self.ItemGuardProc(Value, Self.ItemsAreObjects, Utils.TItemGuard(Self.fItemGuard));
end;

function TStrBinTree.CalcCritDepth: integer;
begin
  result := Alg.IntLog2(Self.NodeCount + 1) shl 1;
end;

function AssocArrayCompareNodes (A, B: integer): integer;
begin
  if PStrBinTreeNode(A).Hash > PStrBinTreeNode(B).Hash then begin
    result := +1;
  end else if PStrBinTreeNode(A).Hash < PStrBinTreeNode(B).Hash then begin
    result := -1;
  end else begin
    result := 0;
  end;
end;

procedure TStrBinTree.ConvertToLinearNodeArray (out Res: TStrBinTreeLinearNodeArray);
var
    LeftInd:              integer;
    RightInd:             integer;
    RightCheckInd:        integer;
    NumNotProcessedNodes: integer;
{U} CurrNode:             PStrBinTreeNode;
    i:                    integer;

begin
  SetLength(Res.NodeArray, Self.NodeCount);
  Res.NodeCount := Self.NodeCount;
  Res.ItemCount := Self.ItemCount;

  if Self.NodeCount > 0 then begin
    CurrNode                := Self.fRoot;
    LeftInd                 := 0;
    Res.NodeArray[LeftInd]  := CurrNode;
    RightInd                := Self.NodeCount;
    RightCheckInd           := RightInd - 1;
    NumNotProcessedNodes    := Self.NodeCount - 1;

    while NumNotProcessedNodes > 0 do begin
      if CurrNode.ChildNodes[RIGHT_CHILD] <> nil then begin
        Dec(RightInd);
        Res.NodeArray[RightInd] :=  CurrNode.ChildNodes[RIGHT_CHILD];
        Dec(NumNotProcessedNodes);
      end;

      if CurrNode.ChildNodes[LEFT_CHILD] <> nil then begin
        CurrNode  :=  CurrNode.ChildNodes[LEFT_CHILD];
        Inc(LeftInd);
        Res.NodeArray[LeftInd]  :=  CurrNode;
        Dec(NumNotProcessedNodes);
      end else begin
        CurrNode  :=  Res.NodeArray[RightCheckInd];
        Dec(RightCheckInd);
      end;
    end; // .while

    for i:=0 to Self.NodeCount - 1 do begin
      Res.NodeArray[i].ChildNodes[LEFT_CHILD]   :=  nil;
      Res.NodeArray[i].ChildNodes[RIGHT_CHILD]  :=  nil;
    end;

    Self.fRoot      :=  nil;
    Self.fNodeCount :=  0;
    Self.fItemCount :=  0;
    Alg.CustomQuickSort(pointer(Res.NodeArray), 0, Res.NodeCount - 1, AssocArrayCompareNodes);
  end; // .if
end; // .procedure TStrBinTree.ConvertToLinearNodeArray

procedure TStrBinTree.Rebuild;
var
  LinearNodeArray:  TStrBinTreeLinearNodeArray;
  NodeArray:        TStrBinTreeNodeArray;

  procedure InsertNode (InsNode: PStrBinTreeNode);
  var
  {U} ParentNode: PStrBinTreeNode;
  {U} CurrNode:   PStrBinTreeNode;

  begin
    {!} Assert(InsNode <> nil);
    ParentNode  :=  nil;
    CurrNode    :=  Self.fRoot;
    // * * * * * //
    while CurrNode <> nil do begin
      ParentNode  :=  CurrNode;
      CurrNode    :=  CurrNode.ChildNodes[InsNode.Hash >= CurrNode.Hash];
    end;

    ParentNode.ChildNodes[InsNode.Hash >= ParentNode.Hash]  :=  InsNode;
  end; // .procedure InsertNode

  procedure InsertNodeRange (MinInd, MaxInd: integer);
  var
  {U} InsNode:    PStrBinTreeNode;
      RangeLen:   integer;
      MiddleInd:  integer;

  begin
    RangeLen  :=  MaxInd - MinInd + 1;
    {!} Assert(RangeLen > 0);
    {!} Assert((MinInd >= 0) and (MaxInd < Length(NodeArray)));
    // * * * * * //
    MiddleInd :=  MinInd + (MaxInd - MinInd) shr 1;
    InsNode   :=  NodeArray[MiddleInd];

    if Self.fRoot = nil then begin
      Self.fRoot  :=  InsNode;
    end else begin
      InsertNode(InsNode);
    end;

    if RangeLen > 2 then begin
      InsertNodeRange(MinInd, MiddleInd - 1);
      InsertNodeRange(MiddleInd + 1, MaxInd);
    end else if RangeLen = 2 then begin
      InsertNode(NodeArray[MiddleInd + 1]);
    end;
  end; // .procedure InsertNodeRange

begin
  {!} Assert(not Self.Locked);
  if Self.NodeCount > 2 then begin
    Self.ConvertToLinearNodeArray(LinearNodeArray);
    Self.fNodeCount :=  LinearNodeArray.NodeCount;
    Self.fItemCount :=  LinearNodeArray.ItemCount;
    NodeArray       :=  LinearNodeArray.NodeArray;
    InsertNodeRange(0, Self.NodeCount - 1);
  end;
end; // .procedure TStrBinTree.Rebuild

function TStrBinTree.FindItem
(
           Hash:       integer;
     const Key:        string;
  out {ni} ParentNode: PStrBinTreeNode;
  out {ni} ItemNode:   PStrBinTreeNode;
  out {ni} ParentItem: PStrBinTreeItem;
  out {ni} Item:       PStrBinTreeItem
): boolean;

var
  SearchDepth:     integer;
  CritSearchDepth: integer;

begin
  if DEBUG_BUILD then begin
    {!} Assert(ParentNode = nil);
    {!} Assert(ItemNode = nil);
    {!} Assert(ParentItem = nil);
    {!} Assert(Item = nil);
  end;

  result := false;

  if Self.NodeCount > 0 then begin
    CritSearchDepth := Self.CalcCritDepth;
    SearchDepth     := 1;
    ItemNode        := Self.fRoot;

    while (ItemNode <> nil) and (ItemNode.Hash <> Hash) do begin
      Inc(SearchDepth);
      ParentNode := ItemNode;
      ItemNode   := ItemNode.ChildNodes[Hash >= ItemNode.Hash];
    end;

    if SearchDepth > CritSearchDepth then begin
      Self.Rebuild;
      ParentNode := nil;
      ItemNode   := nil;
      ParentItem := nil;
      Item       := nil;
      result     := Self.FindItem(Hash, Key, ParentNode, ItemNode, ParentItem, Item);
    end else if ItemNode <> nil then begin
      Item := ItemNode.Item;

      while (Item <> nil) and (Self.GetPreprocessedKey(Item.Key) <> Key) do begin
        ParentItem := Item;
        Item       := Item.NextItem;
      end;

      result := Item <> nil;
    end; // .elseif
  end; // .if
end; // .function TStrBinTree.FindItem

function TStrBinTree.GetValue (const Key: string): {n} pointer;
var
{U} ItemNode:        PStrBinTreeNode;
{U} ParentNode:      PStrBinTreeNode;
{U} Item:            PStrBinTreeItem;
{U} ParentItem:      PStrBinTreeItem;
    PreprocessedKey: string;

begin
  ItemNode   := nil;
  ParentNode := nil;
  Item       := nil;
  ParentItem := nil;
  // * * * * * //

  PreprocessedKey := Self.GetPreprocessedKey(Key);
  result          := nil;

  if Self.FindItem(Self.HashFunc(PreprocessedKey), PreprocessedKey, ParentNode, ItemNode, ParentItem, Item) then begin
    result := Item.Value;
  end;
end; // .function TStrBinTree.GetValue

function TStrBinTree.HasKey (const Key: string): boolean;
var
{U} ItemNode:        PStrBinTreeNode;
{U} ParentNode:      PStrBinTreeNode;
{U} Item:            PStrBinTreeItem;
{U} ParentItem:      PStrBinTreeItem;
    PreprocessedKey: string;

begin
  ItemNode   := nil;
  ParentNode := nil;
  Item       := nil;
  ParentItem := nil;
  // * * * * * //

  PreprocessedKey := Self.GetPreprocessedKey(Key);
  result          := Self.FindItem(Self.HashFunc(PreprocessedKey), PreprocessedKey, ParentNode, ItemNode, ParentItem, Item);
end;

function TStrBinTree.GetExistingValue (const Key: string; out {Un} Res: pointer): boolean;
var
{U} ItemNode:        PStrBinTreeNode;
{U} ParentNode:      PStrBinTreeNode;
{U} Item:            PStrBinTreeItem;
{U} ParentItem:      PStrBinTreeItem;
    PreprocessedKey: string;

begin
  {!} Assert(Res = nil);
  ItemNode   := nil;
  ParentNode := nil;
  Item       := nil;
  ParentItem := nil;
  // * * * * * //

  PreprocessedKey := Self.GetPreprocessedKey(Key);
  result          := Self.FindItem(Self.HashFunc(PreprocessedKey), PreprocessedKey, ParentNode, ItemNode, ParentItem, Item);

  if result then begin
    Res := Item.Value;
  end;
end; // .function TStrBinTree.GetExistingValue

procedure TStrBinTree.SetValue (const Key: string; {OUn} NewValue: pointer);
var
{U} ItemNode:         PStrBinTreeNode;
{U} ParentNode:       PStrBinTreeNode;
{U} Item:             PStrBinTreeItem;
{U} ParentItem:       PStrBinTreeItem;
{O} NewItem:          PStrBinTreeItem;
{O} NewNode:          PStrBinTreeNode;
    PreprocessedKey:  string;
    Hash:             integer;

begin
  ItemNode   := nil;
  ParentNode := nil;
  Item       := nil;
  ParentItem := nil;
  NewItem    := nil;
  NewNode    := nil;
  // * * * * * //
  {!} Assert(Self.IsValidValue(NewValue));
  PreprocessedKey := Self.GetPreprocessedKey(Key);
  Hash            := Self.HashFunc(PreprocessedKey);

  if Self.FindItem(Hash, PreprocessedKey, ParentNode, ItemNode, ParentItem, Item) then begin
    if Item.Value <> NewValue then begin
      Self.FreeItemValue(Item);
      Item.Value := NewValue;
    end;
  end else begin
    {!} Assert(not Self.Locked);
    New(NewItem);
    NewItem.Key      := Key;
    NewItem.Value    := NewValue;
    NewItem.NextItem := nil;
    Inc(Self.fItemCount);

    if ItemNode <> nil then begin
      ParentItem.NextItem := NewItem; NewItem := nil;
    end else begin
      New(NewNode);
      NewNode.Hash                    := Hash;
      NewNode.ChildNodes[LEFT_CHILD]  := nil;
      NewNode.ChildNodes[RIGHT_CHILD] := nil;
      NewNode.Item                    := NewItem; NewItem := nil;
      Inc(Self.fNodeCount);

      if Self.NodeCount > 1 then begin
        ParentNode.ChildNodes[NewNode.Hash >= ParentNode.Hash] := NewNode; NewNode := nil;
      end else begin
        Self.fRoot := NewNode; NewNode := nil;
      end;
    end; // .else
  end; // .else
end; // .procedure TStrBinTree.SetValue

function TStrBinTree.DeleteItem (const Key: string): boolean;
var
{U} ParentNode:      PStrBinTreeNode;
{U} ItemNode:        PStrBinTreeNode;
{U} ParentItem:      PStrBinTreeItem;
{U} Item:            PStrBinTreeItem;
    PreprocessedKey: string;

begin
  {!} Assert(not Self.Locked);
  ItemNode   := nil;
  ParentNode := nil;
  Item       := nil;
  ParentItem := nil;
  // * * * * * //
  PreprocessedKey := Self.GetPreprocessedKey(Key);
  result          := Self.FindItem(Self.HashFunc(PreprocessedKey), PreprocessedKey, ParentNode, ItemNode, ParentItem, Item);

  if result then begin
    Self.RemoveItem(ParentNode, ItemNode, ParentItem, Item);
  end;
end; // .function TStrBinTree.DeleteItem

function TStrBinTree.TakeValue (Key: string; out {OUn} Value: pointer): boolean;
var
{U} ParentNode: PStrBinTreeNode;
{U} ItemNode:   PStrBinTreeNode;
{U} ParentItem: PStrBinTreeItem;
{U} Item:       PStrBinTreeItem;
    Hash:       integer;


begin
  {!} Assert(Value = nil);
  ItemNode   := nil;
  ParentNode := nil;
  Item       := nil;
  ParentItem := nil;
  // * * * * * //
  Key    := Self.GetPreprocessedKey(Key);
  Hash   := Self.HashFunc(Key);
  result := Self.FindItem(Hash, Key, ParentNode, ItemNode, ParentItem, Item);

  if result then begin
    Value := Item.Value;
    {!} Assert(Self.IsValidValue(nil));
    Item.Value := nil;
  end;
end; // .function TStrBinTree.TakeValue

function TStrBinTree.ReplaceValue
(
            Key:      string;
      {OUn} NewValue: pointer;
  out {OUn} OldValue: pointer
): boolean;

var
{U} ParentNode: PStrBinTreeNode;
{U} ItemNode:   PStrBinTreeNode;
{U} ParentItem: PStrBinTreeItem;
{U} Item:       PStrBinTreeItem;
    Hash:       integer;

begin
  {!} Assert(OldValue = nil);
  {!} Assert(Self.IsValidValue(NewValue));
  ItemNode   := nil;
  ParentNode := nil;
  Item       := nil;
  ParentItem := nil;
  // * * * * * //
  Key    := Self.GetPreprocessedKey(Key);
  Hash   := Self.HashFunc(Key);
  result := Self.FindItem(Hash, Key, ParentNode, ItemNode, ParentItem, Item);

  if result then begin
    OldValue   := Item.Value;
    Item.Value := NewValue;
  end;
end; // .function TStrBinTree.ReplaceValue

procedure TStrBinTree.Merge (Source: TStrBinTree);
var
{OUn} Value: pointer;
      Key:   string;

begin
  {!} Assert(Source <> nil);
  {!} Assert(not Self.fOwnsItems or (Self.fItemsAreObjects and Source.fItemsAreObjects), 'Incompatible arrays for merging due to items type and ownage');
  Source.BeginIterate;

  while Source.IterateNext(Key, Value) do begin
    {!} Assert(Self.IsValidValue(Value));

    if Self.fOwnsItems then begin
      {!} Assert(TObject(Value) is Utils.TCloneable, 'Cannot merge array with non-clonable items. Got item: ' + TObject(Value).ClassName);
      Self.SetValue(Key, Utils.TCloneable(Value).Clone);
    end else begin
      Self.SetValue(Key, Value);
    end;
  end;

  Source.EndIterate;
end;

procedure TStrBinTree.EndIterate;
begin
  {!} Assert(Self.fLocked);
  Self.fLocked := false;
end;

procedure TStrBinTree.BeginIterate;
var
  OptimalNumIterNodes: integer;

begin
  {!} Assert(not Self.fLocked);
  OptimalNumIterNodes :=  Self.CalcCritDepth + 1;

  if Length(Self.fIterNodes) < OptimalNumIterNodes then begin
    SetLength(Self.fIterNodes, OptimalNumIterNodes);
  end;

  Self.fIterCurrItem  :=  nil;

  if Self.NodeCount > 0 then begin
    Self.fIterNodeInd   :=  0;
    Self.fIterNodes[0]  :=  Self.fRoot;
  end else begin
    Self.fIterNodeInd :=  -1;
  end;

  Self.fLocked  :=  true;
end; // .procedure TStrBinTree.BeginIterate

function TStrBinTree.IterateNext (out Key: string; out {Un} Value: pointer): boolean;
var
{U} IterNode: PStrBinTreeNode;

begin
  {!} Assert(Self.Locked);
  {!} Assert(Value = nil);
  IterNode  :=  nil;
  // * * * * * //
  result  :=  (Self.fIterNodeInd >= 0) or (Self.fIterCurrItem <> nil);

  if result then begin
    if Self.fIterCurrItem = nil then begin
      IterNode            :=  Self.fIterNodes[Self.fIterNodeInd];
      Self.fIterCurrItem  :=  IterNode.Item;
      Dec(Self.fIterNodeInd);

      if IterNode.ChildNodes[LEFT_CHILD] <> nil then begin
        Inc(Self.fIterNodeInd);
        Self.fIterNodes[Self.fIterNodeInd]  :=  IterNode.ChildNodes[LEFT_CHILD];
      end;
      if IterNode.ChildNodes[RIGHT_CHILD] <> nil then begin
        Inc(Self.fIterNodeInd);
        Self.fIterNodes[Self.fIterNodeInd]  :=  IterNode.ChildNodes[RIGHT_CHILD];
      end;
    end; // .if

    Key                 :=  Self.fIterCurrItem.Key;
    Value               :=  Self.fIterCurrItem.Value;
    Self.fIterCurrItem  :=  Self.fIterCurrItem.NextItem;
  end; // .if
end; // .function TStrBinTree.IterateNext

constructor TObjArray.Create
(

                OwnsItems:        boolean;
                ItemsAreObjects:  boolean;
                ItemGuardProc:    Utils.TItemGuardProc;
  {IN} var  {n} ItemGuard:        Utils.TItemGuard
);

begin
  {!} Assert(@ItemGuardProc <> nil);
  Self.fOwnsItems       := OwnsItems;
  Self.fItemsAreObjects := ItemsAreObjects;
  Self.fItemGuardProc   := ItemGuardProc;
  Self.fItemGuard       := ItemGuard;
  ItemGuard             := nil;
end; // .constructor TObjArray.Create

destructor TObjArray.Destroy;
begin
  Self.Clear;
  SysUtils.FreeAndNil(Self.fItemGuard);
end;

function TObjArray.KeyToHash ({n} Key: pointer): integer;
begin
  result := Crypto.Tm32Encode(integer(Key));
end;

function TObjArray.HashToKey (Hash: integer): {n} pointer;
begin
  result := pointer(Crypto.Tm32Decode(Hash));
end;

function TObjArray.IsValidValue ({n} Value: pointer): boolean;
begin
  result := Self.ItemGuardProc(Value, Self.ItemsAreObjects, Utils.TItemGuard(Self.fItemGuard));
end;

function TObjArray.CalcCritDepth: integer;
begin
  result := Alg.IntLog2(Self.NodeCount + 1) shl 1;
end;

procedure TObjArray.FreeNodeValue (Node: PObjBinTreeNode);
begin
  {!} Assert(Node <> nil);
  if Self.OwnsItems then begin
    if Self.ItemsAreObjects then begin
      TObject(Node.Value).Free;
    end else begin
      FreeMem(Node.Value);
    end;
  end;

  Node.Value := nil;
end; // .procedure TObjArray.FreeNodeValue

procedure TObjArray.FreeNode ({IN} var {n} Node: PObjBinTreeNode);
begin
  if Node <> nil then begin
    Self.FreeNodeValue(Node);
    Self.FreeNode(Node.ChildNodes[LEFT_CHILD]);
    Self.FreeNode(Node.ChildNodes[RIGHT_CHILD]);
    Dispose(Node); Node := nil;
  end;
end;

procedure TObjArray.RemoveNode ({n} ParentNode: PObjBinTreeNode; Node: PObjBinTreeNode);
var
{U} RightClosestNodeParent: PObjBinTreeNode;
{U} RightClosestNode:       PObjBinTreeNode;
    NodeIsRoot:             boolean;
    NodeSide:               TChildNodeSide;

begin
  {!} Assert(Node <> nil);
  RightClosestNodeParent := nil;
  RightClosestNode       := nil;
  NodeSide               := false;
  // * * * * * //
  Self.FreeNodeValue(Node);
  NodeIsRoot := ParentNode = nil;

  if Self.NodeCount = 1 then begin
    {!} Assert(NodeIsRoot);
    {!} Assert(Node = Self.fRoot);
    Dispose(Self.fRoot); Self.fRoot := nil;
  end else begin
    if not NodeIsRoot then begin
      NodeSide := Node.Hash >= ParentNode.Hash;
    end;
    (* N
      - -
    *)
    if (Node.ChildNodes[LEFT_CHILD] = nil) and (Node.ChildNodes[RIGHT_CHILD] = nil) then begin
      ParentNode.ChildNodes[NodeSide] := nil;
      Dispose(Node); Node := nil;
    end // .if
    (* N
      - R
    *)
    else if Node.ChildNodes[LEFT_CHILD] = nil then begin
      if NodeIsRoot then begin
        Self.fRoot := Node.ChildNodes[RIGHT_CHILD];
      end else begin
        ParentNode.ChildNodes[NodeSide] := Node.ChildNodes[RIGHT_CHILD];
      end;

      Dispose(Node); Node := nil;
    end // .elseif
    (* N
      L -
    *)
    else if Node.ChildNodes[RIGHT_CHILD] = nil then begin
      if NodeIsRoot then begin
        Self.fRoot := Node.ChildNodes[LEFT_CHILD];
      end else begin
        ParentNode.ChildNodes[NodeSide] := Node.ChildNodes[LEFT_CHILD];
      end;

      Dispose(Node); Node := nil;
    end // .elseif
    (* N
      L R
    *)
    else begin
      RightClosestNodeParent := Node;
      RightClosestNode       := Node.ChildNodes[RIGHT_CHILD];

      while RightClosestNode.ChildNodes[LEFT_CHILD] <> nil do begin
        RightClosestNodeParent := RightClosestNode;
        RightClosestNode       := RightClosestNode.ChildNodes[LEFT_CHILD];
      end;

      Node.Value := RightClosestNode.Value; RightClosestNode.Value := nil;
      Node.Hash  := RightClosestNode.Hash;
      Self.RemoveNode(RightClosestNodeParent, RightClosestNode);
    end; // .else
  end; // .else
end; // .procedure TObjArray.RemoveNode

procedure TObjArray.Clear;
begin
  {!} Assert(not Self.Locked);
  Self.FreeNode(Self.fRoot);
  Self.fNodeCount := 0;
end;

function TObjArray.CloneNode ({n} Node: PObjBinTreeNode): {On} PObjBinTreeNode;
begin
  if Node = nil then begin
    result := nil;
  end else begin
    New(result);
    result.Hash := Node.Hash;
    {!} Assert(Self.IsValidValue(Node.Value));

    if (Node.Value = nil) or (not Self.OwnsItems) then begin
      result.Value := Node.Value;
    end else begin
      {!} Assert(Self.ItemsAreObjects);
      {!} Assert(TObject(Node.Value) IS Utils.TCloneable);
      result.Value := Utils.TCloneable(Node.Value).Clone;
    end;

    result.ChildNodes[LEFT_CHILD]  := Self.CloneNode(Node.ChildNodes[LEFT_CHILD]);
    result.ChildNodes[RIGHT_CHILD] := Self.CloneNode(Node.ChildNodes[RIGHT_CHILD]);
  end; // .else
end; // .function TObjArray.CloneNode

procedure TObjArray.Assign (Source: Utils.TCloneable);
var
{U} SrcArr: TObjArray;

begin
  {!} Assert(not Self.Locked);
  {!} Assert(Source <> nil);
  SrcArr  :=  Source AS TObjArray;
  // * * * * * //
  if Self <> Source then begin
    Self.Clear;
    Self.fOwnsItems       :=  SrcArr.OwnsItems;
    Self.fItemsAreObjects :=  SrcArr.ItemsAreObjects;
    Self.fItemGuardProc   :=  SrcArr.ItemGuardProc;
    Self.fItemGuard       :=  SrcArr.fItemGuard.Clone;
    Self.fNodeCount       :=  SrcArr.NodeCount;
    Self.fRoot            :=  Self.CloneNode(SrcArr.fRoot);
  end;
end; // .procedure TObjArray.Assign

function ObjArrayCompareNodes (A, B: integer): integer;
begin
  if PObjBinTreeNode(A).Hash > PObjBinTreeNode(B).Hash then begin
    result  :=  +1;
  end else if PObjBinTreeNode(A).Hash < PObjBinTreeNode(B).Hash then begin
    result  :=  -1;
  end else begin
    result  :=  0;
  end;
end;

procedure TObjArray.ConvertToLinearNodeArray (out Res: TObjBinTreeLinearNodeArray);
var
    LeftInd:              integer;
    RightInd:             integer;
    RightCheckInd:        integer;
    NumNotProcessedNodes: integer;
{U} CurrNode:             PObjBinTreeNode;
    i:                    integer;

begin
  SetLength(Res.NodeArray, Self.NodeCount);
  Res.NodeCount :=  Self.NodeCount;

  if Self.NodeCount > 0 then begin
    CurrNode                :=  Self.fRoot;
    LeftInd                 :=  0;
    Res.NodeArray[LeftInd]  :=  CurrNode;
    RightInd                :=  Self.NodeCount;
    RightCheckInd           :=  RightInd - 1;
    NumNotProcessedNodes    :=  Self.NodeCount - 1;

    while NumNotProcessedNodes > 0 do begin
      if CurrNode.ChildNodes[RIGHT_CHILD] <> nil then begin
        Dec(RightInd);
        Res.NodeArray[RightInd] :=  CurrNode.ChildNodes[RIGHT_CHILD];
        Dec(NumNotProcessedNodes);
      end;

      if CurrNode.ChildNodes[LEFT_CHILD] <> nil then begin
        CurrNode  :=  CurrNode.ChildNodes[LEFT_CHILD];
        Inc(LeftInd);
        Res.NodeArray[LeftInd]  :=  CurrNode;
        Dec(NumNotProcessedNodes);
      end else begin
        CurrNode  :=  Res.NodeArray[RightCheckInd];
        Dec(RightCheckInd);
      end;
    end; // .while

    for i:=0 to Self.NodeCount - 1 do begin
      Res.NodeArray[i].ChildNodes[LEFT_CHILD]   :=  nil;
      Res.NodeArray[i].ChildNodes[RIGHT_CHILD]  :=  nil;
    end;

    Self.fRoot      :=  nil;
    Self.fNodeCount :=  0;
    Alg.CustomQuickSort(pointer(Res.NodeArray), 0, Res.NodeCount - 1, ObjArrayCompareNodes);
  end; // .if
end; // .procedure TObjArray.ConvertToLinearNodeArray

procedure TObjArray.Rebuild;
var
  LinearNodeArray:  TObjBinTreeLinearNodeArray;
  NodeArray:        TObjBinTreeNodeArray;

  procedure InsertNode (InsNode: PObjBinTreeNode);
  var
  {U} ParentNode: PObjBinTreeNode;
  {U} CurrNode:   PObjBinTreeNode;

  begin
    {!} Assert(InsNode <> nil);
    ParentNode := nil;
    CurrNode   := Self.fRoot;
    // * * * * * //
    while CurrNode <> nil do begin
      ParentNode := CurrNode;
      CurrNode   := CurrNode.ChildNodes[InsNode.Hash >= CurrNode.Hash];
    end;

    ParentNode.ChildNodes[InsNode.Hash >= ParentNode.Hash] := InsNode;
  end; // .procedure InsertNode

  procedure InsertNodeRange (MinInd, MaxInd: integer);
  var
      RangeLen:  integer;
      MiddleInd: integer;
  {U} InsNode:   PObjBinTreeNode;

  begin
    RangeLen  := MaxInd - MinInd + 1;
    {!} Assert(RangeLen > 0);
    {!} Assert((MinInd >= 0) and (MaxInd < Length(NodeArray)));
    // * * * * * //
    MiddleInd := MinInd + (MaxInd - MinInd) shr 1;
    InsNode   := NodeArray[MiddleInd];

    if Self.fRoot = nil then begin
      Self.fRoot := InsNode;
    end else begin
      InsertNode(InsNode);
    end;

    if RangeLen > 2 then begin
      InsertNodeRange(MinInd, MiddleInd - 1);
      InsertNodeRange(MiddleInd + 1, MaxInd);
    end else if RangeLen = 2 then begin
      InsertNode(NodeArray[MiddleInd + 1]);
    end;
  end; // .procedure InsertNodeRange

begin
  {!} Assert(not Self.Locked);
  if Self.NodeCount > 2 then begin
    Self.ConvertToLinearNodeArray(LinearNodeArray);
    Self.fNodeCount :=  LinearNodeArray.NodeCount;
    NodeArray       :=  LinearNodeArray.NodeArray;
    InsertNodeRange(0, Self.NodeCount - 1);
  end;
end; // .procedure TObjArray.Rebuild

function TObjArray.FindItem
(
      {n}   Key:        pointer;
  out {ni}  ParentNode: PObjBinTreeNode;
  out {ni}  ItemNode:   PObjBinTreeNode
): boolean;

var
  Hash:            integer;
  SearchDepth:     integer;
  CritSearchDepth: integer;

begin
  {!} Assert(ParentNode = nil);
  {!} Assert(ItemNode = nil);
  result := false;

  if Self.NodeCount > 0 then begin
    Hash            := Self.KeyToHash(Key);
    CritSearchDepth := Self.CalcCritDepth;
    SearchDepth     := 1;
    ItemNode        := Self.fRoot;

    while (ItemNode <> nil) and (ItemNode.Hash <> Hash) do begin
      Inc(SearchDepth);
      ParentNode := ItemNode;
      ItemNode   := ItemNode.ChildNodes[Hash >= ItemNode.Hash];
    end;

    if SearchDepth > CritSearchDepth then begin
      Self.Rebuild;
      ParentNode := nil;
      ItemNode   := nil;
      result     := Self.FindItem(Key, ParentNode, ItemNode);
    end;

    result := ItemNode <> nil;
  end; // .if
end; // .function TObjArray.FindItem

function TObjArray.GetValue ({n} Key: pointer): {n} pointer;
var
{U} ItemNode:   PObjBinTreeNode;
{U} ParentNode: PObjBinTreeNode;

begin
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  // * * * * * //
  if Self.FindItem(Key, ParentNode, ItemNode) then begin
    result  :=  ItemNode.Value;
  end else begin
    result  :=  nil;
  end;
end; // .function TObjArray.GetValue

function TObjArray.GetExistingValue ({n} Key: pointer; out {Un} Res: pointer): boolean;
var
{U} ItemNode:   PObjBinTreeNode;
{U} ParentNode: PObjBinTreeNode;

begin
  {!} Assert(Res = nil);
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  // * * * * * //
  result  :=  Self.FindItem(Key, ParentNode, ItemNode);

  if result then begin
    Res :=  ItemNode.Value;
  end;
end; // .function TObjArray.GetExistingValue

procedure TObjArray.SetValue ({n} Key: pointer; {OUn} NewValue: pointer);
var
{U} ItemNode:   PObjBinTreeNode;
{U} ParentNode: PObjBinTreeNode;
{O} NewNode:    PObjBinTreeNode;

begin
  ItemNode   := nil;
  ParentNode := nil;
  NewNode    := nil;
  // * * * * * //
  {!} Assert(Self.IsValidValue(NewValue));
  if Self.FindItem(Key, ParentNode, ItemNode) then begin
    if ItemNode.Value <> NewValue then begin
      Self.FreeNodeValue(ItemNode);
      ItemNode.Value := NewValue;
    end;
  end else begin
    New(NewNode);
    NewNode.Hash  := Self.KeyToHash(Key);
    NewNode.Value := NewValue;
    NewNode.ChildNodes[LEFT_CHILD]  := nil;
    NewNode.ChildNodes[RIGHT_CHILD] := nil;
    Inc(Self.fNodeCount);

    if Self.NodeCount > 1 then begin
      ParentNode.ChildNodes[NewNode.Hash >= ParentNode.Hash] := NewNode; NewNode := nil;
    end else begin
      Self.fRoot  := NewNode; NewNode  := nil;
    end;
  end; // .else
end; // .procedure TObjArray.SetValue

function TObjArray.DeleteItem ({n} Key: pointer): boolean;
var
{U} ParentNode: PObjBinTreeNode;
{U} ItemNode:   PObjBinTreeNode;

begin
  {!} Assert(not Self.Locked);
  ItemNode   := nil;
  ParentNode := nil;
  // * * * * * //
  result := Self.FindItem(Key, ParentNode, ItemNode);

  if result then begin
    Self.RemoveNode(ParentNode, ItemNode);
    Dec(Self.fNodeCount);
  end;
end; // .function TObjArray.DeleteItem

function TObjArray.TakeValue ({n} Key: pointer; out {OUn} Value: pointer): boolean;
var
{U} ParentNode: PObjBinTreeNode;
{U} ItemNode:   PObjBinTreeNode;

begin
  {!} Assert(Value = nil);
  ItemNode   := nil;
  ParentNode := nil;
  // * * * * * //
  result := Self.FindItem(Key, ParentNode, ItemNode);

  if result then begin
    Value := ItemNode.Value;
    {!} Assert(Self.IsValidValue(nil));
    ItemNode.Value := nil;
  end;
end; // .function TObjArray.TakeValue

function TObjArray.ReplaceValue ({n} Key: pointer; {OUn} NewValue: pointer; out {OUn} OldValue: pointer): boolean;
var
{U} ParentNode: PObjBinTreeNode;
{U} ItemNode:   PObjBinTreeNode;

begin
  {!} Assert(OldValue = nil);
  {!} Assert(Self.IsValidValue(NewValue));
  ItemNode   := nil;
  ParentNode := nil;
  // * * * * * //
  result := Self.FindItem(Key, ParentNode, ItemNode);

  if result then begin
    OldValue       := ItemNode.Value;
    ItemNode.Value := NewValue;
  end;
end; // .function TObjArray.ReplaceValue

procedure TObjArray.EndIterate;
begin
  {!} Assert(Self.fLocked);
  Self.fLocked := false;
end;

procedure TObjArray.BeginIterate;
var
  OptimalNumIterNodes: integer;

begin
  {!} Assert(not Self.fLocked);
  OptimalNumIterNodes := Self.CalcCritDepth + 1;

  if Length(Self.fIterNodes) < OptimalNumIterNodes then begin
    SetLength(Self.fIterNodes, OptimalNumIterNodes);
  end;

  if Self.NodeCount > 0 then begin
    Self.fIterNodeInd  := 0;
    Self.fIterNodes[0] := Self.fRoot;
  end else begin
    Self.fIterNodeInd := -1;
  end;

  Self.fLocked := true;
end; // .procedure TObjArray.BeginIterate

function TObjArray.IterateNext (out {Un} Key: pointer; out {Un} Value: pointer): boolean;
var
{U} IterNode: PObjBinTreeNode;

begin
  {!} Assert(Self.Locked);
  {!} Assert(Key = nil);
  {!} Assert(Value = nil);
  IterNode := nil;
  // * * * * * //
  result := Self.fIterNodeInd >= 0;

  if result then begin
    IterNode := Self.fIterNodes[Self.fIterNodeInd];
    Dec(Self.fIterNodeInd);

    if IterNode.ChildNodes[LEFT_CHILD] <> nil then begin
      Inc(Self.fIterNodeInd);
      Self.fIterNodes[Self.fIterNodeInd] := IterNode.ChildNodes[LEFT_CHILD];
    end;

    if IterNode.ChildNodes[RIGHT_CHILD] <> nil then begin
      Inc(Self.fIterNodeInd);
      Self.fIterNodes[Self.fIterNodeInd] := IterNode.ChildNodes[RIGHT_CHILD];
    end;

    Key   := Self.HashToKey(IterNode.Hash);
    Value := IterNode.Value;
  end; // .if
end; // .function TObjArray.IterateNext

function NewAssocArr (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc; OwnsItems,ItemsAreObjects: boolean; ItemType: TClass; AllowNil: boolean): TStrBinTree;
var
{O} ItemGuard: Utils.TDefItemGuard;

begin
  {!} Assert(ItemsAreObjects or (ItemType = Utils.NO_TYPEGUARD));
  ItemGuard := Utils.TDefItemGuard.Create;
  // * * * * * //
  ItemGuard.ItemType := ItemType;
  ItemGuard.AllowNil := AllowNil;
  result             := TStrBinTree.Create(HashFunc, KeyPreprocessFunc, OwnsItems, ItemsAreObjects, @Utils.DefItemGuardProc, Utils.TItemGuard(ItemGuard));
end;

function NewSimpleAssocArr (HashFunc: THashFunc; {n} KeyPreprocessFunc: TKeyPreprocessFunc): TStrBinTree;
var
{O} ItemGuard: Utils.TCloneable;

begin
  ItemGuard := nil;
  result    := TStrBinTree.Create (HashFunc, KeyPreprocessFunc, not Utils.OWNS_ITEMS, not Utils.ITEMS_ARE_OBJECTS, @Utils.NoItemGuardProc, ItemGuard);
end;

function NewStrictAssocArr ({n} TypeGuard: TClass; OwnsItems: boolean = true): TStrBinTree;
begin
  result := NewAssocArr(Crypto.FastAnsiHash, SysUtils.AnsiLowerCase, OwnsItems, Utils.ITEMS_ARE_OBJECTS, TypeGuard, Utils.ALLOW_NIL);
end;

function NewObjArr (OwnsItems, ItemsAreObjects: boolean; ItemType: TClass; AllowNil: boolean): TObjArray;

var
{O} ItemGuard: Utils.TDefItemGuard;

begin
  {!} Assert(ItemsAreObjects or (ItemType = Utils.NO_TYPEGUARD));
  ItemGuard :=  Utils.TDefItemGuard.Create;
  // * * * * * //
  ItemGuard.ItemType := ItemType;
  ItemGuard.AllowNil := AllowNil;
  result             := TObjArray.Create(OwnsItems, ItemsAreObjects, @Utils.DefItemGuardProc, Utils.TItemGuard(ItemGuard));
end; // .function NewObjArr

function NewSimpleObjArr: TObjArray;
var
{O} ItemGuard: Utils.TCloneable;

begin
  ItemGuard :=  nil;
  result    := TObjArray.Create (not Utils.OWNS_ITEMS, not Utils.ITEMS_ARE_OBJECTS, @Utils.NoItemGuardProc, ItemGuard);
end; // .function NewSimpleObjArr

function NewStrictObjArr ({n} TypeGuard: TClass): TObjArray;
begin
  result := NewObjArr(Utils.OWNS_ITEMS, Utils.ITEMS_ARE_OBJECTS, TypeGuard, Utils.ALLOW_NIL);
end;

end.
