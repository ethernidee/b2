unit AssocArrays;
{
DESCRIPTION:  Associative array implementation
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(*
The implementation uses binary tree and (in case of array with string keys) user provided hash function to store and retrieve data.
Tree is automatically rebalanced when critical search depth is met which is equal to 2X of balanced tree height.
Rebalancing is done by converting tree to linear node array and inserting nodes in empty tree.
*)

(***)  interface  (***)
uses SysUtils, Utils, Alg, Crypto;

const
  LEFT_CHILD  = FALSE;
  RIGHT_CHILD = TRUE;
  
  NO_KEY_PREPROCESS_FUNC  = nil;


type
  TChildNodeSide  = BOOLEAN;

  PAssocArrayItem = ^TAssocArrayItem;
  TAssocArrayItem = record
          Key:      string;
    {OUn} Value:    POINTER;
    {On}  NextItem: PAssocArrayItem;
  end; // .record TAssocArrayItem

  PAssocArrayNode = ^TAssocArrayNode;
  TAssocArrayNode = record
        Hash:       INTEGER;
    {O} Item:       PAssocArrayItem;
        ChildNodes: array [LEFT_CHILD..RIGHT_CHILD] of {On} PAssocArrayNode;
  end; // .record TAssocArrayNode

  THashFunc           = function (const Str: string): INTEGER;
  TKeyPreprocessFunc  = function (const OrigKey: string): string;
  
  TNodeArray  = array of {O} PAssocArrayNode;
  
  TLinearNodeArray  = record
    NodeArray:  TNodeArray; // Nodes are sorted by hash
    NodeCount:  INTEGER;
    ItemCount:  INTEGER;
  end; // .record TLinearNodeArray
  
  TAssocArray = class (Utils.TCloneable)
    (***) protected (***)
      {On}  fRoot:              PAssocArrayNode;
            fHashFunc:          THashFunc;
      {n}   fKeyPreprocessFunc: TKeyPreprocessFunc;
            fOwnsItems:         BOOLEAN;
            fItemsAreObjects:   BOOLEAN;
            fItemGuardProc:     Utils.TItemGuardProc;
      {On}  fItemGuard:         Utils.TItemGuard;
            fItemCount:         INTEGER;
            fNodeCount:         INTEGER;
            fIterNodes:         array of {U} PAssocArrayNode;
      {U}   fIterCurrItem:      PAssocArrayItem;
            fIterNodeInd:       INTEGER;
            fLocked:            BOOLEAN;

      
      function  CloneItem (Item: PAssocArrayItem): {O} PAssocArrayItem;
      function  CloneNode ({n} Node: PAssocArrayNode): {On} PAssocArrayNode;
      procedure FreeItemValue (Item: PAssocArrayItem);
      procedure FreeNode ({IN} var {n} Node: PAssocArrayNode);
      procedure RemoveNode ({n} ParentNode: PAssocArrayNode; ItemNode: PAssocArrayNode);
      procedure RemoveItem
      (
        {n} ParentNode: PAssocArrayNode;
            ItemNode:   PAssocArrayNode;
        {n} ParentItem: PAssocArrayItem;
            Item:       PAssocArrayItem
      );
      
      (*
        All nodes are placed in NodeArray and disconnected from each other.
        Original binary tree is emptied. Nodes are sorted by hash.
      *)
      procedure ConvertToLinearNodeArray (out Res: TLinearNodeArray);
      
      function  FindItem
      (
                    Hash:       INTEGER;
              const Key:        string;
        out {ni}   ParentNode: PAssocArrayNode;
        out {ni}   ItemNode:   PAssocArrayNode;
        out {ni}   ParentItem: PAssocArrayItem;
        out {ni}   Item:       PAssocArrayItem
      ): BOOLEAN;

    (***) public (***)
      constructor Create
      (
                      HashFunc:           THashFunc;
                  {n} KeyPreprocessFunc:  TKeyPreprocessFunc;
                      OwnsItems:          BOOLEAN;
                      ItemsAreObjects:    BOOLEAN;
                      ItemGuardProc:      Utils.TItemGuardProc;
        {IN} var  {n} ItemGuard:          Utils.TItemGuard
      );
      destructor  Destroy; override;
      procedure Assign (Source: Utils.TCloneable); override;
      procedure Clear;
      function  GetPreprocessedKey (const Key: string): string;
      function  IsValidValue ({n} Value: POINTER): BOOLEAN;
      function  CalcCritDepth: INTEGER;
      procedure Rebuild;
      function  GetValue (Key: string): {n} POINTER;
      function  GetExistingValue (Key: string; out {Un} Res: POINTER): BOOLEAN;
      procedure SetValue (Key: string; {OUn} NewValue: POINTER);
      function  DeleteItem (Key: string): BOOLEAN;
      
      (* Returns value with specified key and NILify it in the array *)
      function  TakeValue (Key: string; out {OUn} Value: POINTER): BOOLEAN;
      
      (* Returns old value *)
      function  ReplaceValue
      (
                  Key:      string;
            {OUn} NewValue: POINTER;
        out {OUn} OldValue: POINTER
      ): BOOLEAN;
      
      procedure BeginIterate;
      function  IterateNext (out Key: string; out {Un} Value: POINTER): BOOLEAN;
      procedure EndIterate;

      property  HashFunc:           THashFunc READ fHashFunc;
      property  KeyPreprocessFunc:  TKeyPreprocessFunc READ fKeyPreprocessFunc;
      property  OwnsItems:          BOOLEAN READ fOwnsItems;
      property  ItemsAreObjects:    BOOLEAN READ fItemsAreObjects;
      property  ItemCount:          INTEGER READ fItemCount;
      property  ItemGuardProc:      Utils.TItemGuardProc READ fItemGuardProc;
      property  NodeCount:          INTEGER READ fNodeCount;
      property  Locked:             BOOLEAN READ fLocked;
      property  Items[Key: string]: POINTER READ {n} GetValue WRITE {OUn} SetValue; default;
  end; // .class TAssocArray

  PObjArrayNode = ^TObjArrayNode;
  TObjArrayNode = record
          Hash:       INTEGER;  // Hash is encoded {U} Key: POINTER
    {OUn} Value:      POINTER;
          ChildNodes: array [LEFT_CHILD..RIGHT_CHILD] of {On} PObjArrayNode;
  end; // .record TObjArrayItem
  
  TObjNodeArray = array of {O} PObjArrayNode;
  
  TLinearObjNodeArray = record
    NodeArray:  TObjNodeArray;  // Nodes are sorted by hash
    NodeCount:  INTEGER;
  end; // .record TLinearObjNodeArray
  
  TObjArray = class (Utils.TCloneable)
    (***) protected (***)
      {On}  fRoot:            PObjArrayNode;
            fOwnsItems:       BOOLEAN;
            fItemsAreObjects: BOOLEAN;
            fItemGuardProc:   Utils.TItemGuardProc;
      {On}  fItemGuard:       Utils.TItemGuard;
            fNodeCount:       INTEGER;
            fIterNodes:       array of {U} PObjArrayNode;
            fIterNodeInd:     INTEGER;
            fLocked:          BOOLEAN;
      
      function  HashToKey (Hash: INTEGER): {n} POINTER;
      function  KeyToHash (Key: {n} POINTER): INTEGER;
      procedure FreeNodeValue (Node: PObjArrayNode);
      procedure FreeNode ({IN} var {n} Node: PObjArrayNode);
      procedure RemoveNode ({n} ParentNode: PObjArrayNode; Node: PObjArrayNode);
      function  CloneNode (Node: PObjArrayNode): {O} PObjArrayNode;
      
      (*
        All nodes are placed in NodeArray and disconnected from each other.
        Original binary tree is emptied. Nodes are sorted by hash.
      *)
      procedure ConvertToLinearNodeArray (out Res: TLinearObjNodeArray);

      function  FindItem
      (
            {n}   Key:        POINTER;
        out {ni}  ParentNode: PObjArrayNode;
        out {ni}  ItemNode:   PObjArrayNode
      ): BOOLEAN;

    (***) public (***)
      constructor Create
      (
                      OwnsItems:        BOOLEAN;
                      ItemsAreObjects:  BOOLEAN;
                      ItemGuardProc:    Utils.TItemGuardProc;
        {IN} var  {n} ItemGuard:        Utils.TItemGuard
      );
      destructor  Destroy; override;
      procedure Assign (Source: Utils.TCloneable); override;
      procedure Clear;
      function  IsValidValue ({n} Value: POINTER): BOOLEAN;
      function  CalcCritDepth: INTEGER;
      procedure Rebuild;
      function  GetValue ({n} Key: POINTER): {n} POINTER;
      function  GetExistingValue ({n} Key: POINTER; out {Un} Res: POINTER): BOOLEAN;
      procedure SetValue ({n} Key: POINTER; {OUn} NewValue: POINTER);
      function  DeleteItem ({n} Key: POINTER): BOOLEAN;
      
      (* Returns value with specified key and NILify it in the array *)
      function  TakeValue ({n} Key: POINTER; out {OUn} Value: POINTER): BOOLEAN;
      
      {Returns old value}
      function  ReplaceValue
      (
            {n}   Key:      POINTER;
            {OUn} NewValue: POINTER;
        out {OUn} OldValue: POINTER
      ): BOOLEAN;

      procedure BeginIterate;
      function  IterateNext (out {Un} Key: POINTER; out {Un} Value: POINTER): BOOLEAN;
      procedure EndIterate;

      property  OwnsItems:                BOOLEAN READ fOwnsItems;
      property  ItemsAreObjects:          BOOLEAN READ fItemsAreObjects;
      property  ItemCount:                INTEGER READ fNodeCount;
      property  ItemGuardProc:            Utils.TItemGuardProc READ fItemGuardProc;
      property  NodeCount:                INTEGER READ fNodeCount;
      property  Locked:                   BOOLEAN READ fLocked;
      property  Items[{n} Key: POINTER]:  {OUn} POINTER READ GetValue WRITE SetValue; default;
  end; // .class TObjArray

function  NewAssocArr
(
      HashFunc:           THashFunc;
  {n} KeyPreprocessFunc:  TKeyPreprocessFunc;
      OwnsItems:          BOOLEAN;
      ItemsAreObjects:    BOOLEAN;
      ItemType:           TClass;
      AllowNIL:           BOOLEAN
): TAssocArray;

function  NewSimpleAssocArr
(
      HashFunc:           THashFunc;
  {n} KeyPreprocessFunc:  TKeyPreprocessFunc
): TAssocArray;

function  NewStrictAssocArr ({n} TypeGuard: TClass; OwnsItems: BOOLEAN = TRUE): TAssocArray;
function  NewObjArr
(
  OwnsItems:        BOOLEAN;
  ItemsAreObjects:  BOOLEAN;
  ItemType:         TClass;
  AllowNIL:         BOOLEAN
): TObjArray;

function  NewSimpleObjArr: TObjArray;
function  NewStrictObjArr ({n} TypeGuard: TClass): TObjArray;


(***)  implementation  (***)


constructor TAssocArray.Create
(
                HashFunc:           THashFunc;
            {n} KeyPreprocessFunc:  TKeyPreprocessFunc;
                OwnsItems:          BOOLEAN;
                ItemsAreObjects:    BOOLEAN;
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
end; // .constructor TAssocArray.Create

destructor TAssocArray.Destroy;
begin
  Self.Clear;
  SysUtils.FreeAndNil(Self.fItemGuard);
end; // .destructor TAssocArray.Destroy

function TAssocArray.CloneItem (Item: PAssocArrayItem): {O} PAssocArrayItem;
begin
  {!} Assert(Item <> nil);
  {!} Assert(Self.IsValidValue(Item.Value));
  NEW(result);
  result.Key  :=  Item.Key;
  
  if Item.NextItem <> nil then begin
    result.NextItem :=  Self.CloneItem(Item.NextItem);
  end // .if
  else begin
    result.NextItem :=  nil;
  end; // .else
  
  if (Item.Value = nil) or (not Self.OwnsItems) then begin
    result.Value  :=  Item.Value;
  end // .if
  else begin
    {!} Assert(Self.ItemsAreObjects);
    {!} Assert(TObject(Item.Value) IS Utils.TCloneable);
    result.Value  :=  Utils.TCloneable(Item.Value).Clone;
  end; // .else
end; // .function TAssocArray.CloneItem

function TAssocArray.CloneNode ({n} Node: PAssocArrayNode): {On} PAssocArrayNode;
begin
  if Node = nil then begin
    result  :=  nil;
  end // .if
  else begin
    NEW(result);
    result.Hash                     :=  Node.Hash;
    result.Item                     :=  Self.CloneItem(Node.Item);
    result.ChildNodes[LEFT_CHILD]   :=  Self.CloneNode(Node.ChildNodes[LEFT_CHILD]);
    result.ChildNodes[RIGHT_CHILD]  :=  Self.CloneNode(Node.ChildNodes[RIGHT_CHILD]);
  end; // .else
end; // .function TAssocArray.CloneNode

procedure TAssocArray.Assign (Source: Utils.TCloneable);
var
{U} SrcArr: TAssocArray;
  
begin
  {!} Assert(not Self.Locked);
  {!} Assert(Source <> nil);
  SrcArr  :=  Source AS TAssocArray;
  // * * * * * //
  if Self <> Source then begin
    Self.Clear;
    Self.fHashFunc          :=  SrcArr.HashFunc;
    Self.fKeyPreprocessFunc :=  SrcArr.KeyPreprocessFunc;
    Self.fOwnsItems         :=  SrcArr.OwnsItems;
    Self.fItemsAreObjects   :=  SrcArr.ItemsAreObjects;
    Self.fItemGuardProc     :=  SrcArr.ItemGuardProc;
    Self.fItemGuard         :=  SrcArr.fItemGuard.Clone;
    Self.fItemCount         :=  SrcArr.ItemCount;
    Self.fNodeCount         :=  SrcArr.NodeCount;
    Self.fRoot              :=  Self.CloneNode(SrcArr.fRoot);
  end; // .if
end; // .procedure TAssocArray.Assign

procedure TAssocArray.FreeItemValue (Item: PAssocArrayItem);
begin
  {!} Assert(Item <> nil);
  if Self.OwnsItems then begin
    if Self.ItemsAreObjects then begin
      TObject(Item.Value).Free;
    end // .if
    else begin
      FreeMem(Item.Value);
    end; // .else
  end; // .if
  
  Item.Value  :=  nil;
end; // .procedure TAssocArray.FreeItemValue

procedure TAssocArray.RemoveNode ({n} ParentNode: PAssocArrayNode; ItemNode: PAssocArrayNode);
var
{U} RightClosestNodeParent: PAssocArrayNode;
{U} RightClosestNode:       PAssocArrayNode;
    ItemNodeIsRoot:         BOOLEAN;
    ItemNodeSide:           TChildNodeSide;
  
begin
  {!} Assert(ItemNode <> nil);
  RightClosestNodeParent  :=  nil;
  RightClosestNode        :=  nil;
  ItemNodeSide            :=  FALSE;
  // * * * * * //
  ItemNodeIsRoot  :=  ParentNode = nil;
  
  if Self.NodeCount = 1 then begin
    {!} Assert(ItemNodeIsRoot);
    {!} Assert(ItemNode = Self.fRoot);
    DISPOSE(Self.fRoot); Self.fRoot :=  nil;
  end // .if
  else begin
    if not ItemNodeIsRoot then begin
      ItemNodeSide  :=  ItemNode.Hash >= ParentNode.Hash;
    end; // .if
    
    (* N
      - -
    *)
    if
      (ItemNode.ChildNodes[LEFT_CHILD] = nil) and
      (ItemNode.ChildNodes[RIGHT_CHILD] = nil)
    then begin
      ParentNode.ChildNodes[ItemNodeSide] :=  nil;
      DISPOSE(ItemNode); ItemNode :=  nil;
    end // .if
    (* N
      - R
    *)
    else if ItemNode.ChildNodes[LEFT_CHILD] = nil then begin
      if ItemNodeIsRoot then begin
        Self.fRoot  :=  ItemNode.ChildNodes[RIGHT_CHILD];
      end // .if
      else begin
        ParentNode.ChildNodes[ItemNodeSide] :=  ItemNode.ChildNodes[RIGHT_CHILD];
      end; // .else
      
      DISPOSE(ItemNode); ItemNode :=  nil;
    end // .ELSEIF
    (* N
      L -
    *)
    else if ItemNode.ChildNodes[RIGHT_CHILD] = nil then begin
      if ItemNodeIsRoot then begin
        Self.fRoot  :=  ItemNode.ChildNodes[LEFT_CHILD];
      end // .if
      else begin
        ParentNode.ChildNodes[ItemNodeSide] :=  ItemNode.ChildNodes[LEFT_CHILD];
      end; // .else
      
      DISPOSE(ItemNode); ItemNode :=  nil;
    end // .ELSEIF
    (* N
      L R
    *)
    else begin
      RightClosestNodeParent  :=  ItemNode;
      RightClosestNode        :=  ItemNode.ChildNodes[RIGHT_CHILD];
      
      while RightClosestNode.ChildNodes[LEFT_CHILD] <> nil do begin
        RightClosestNodeParent  :=  RightClosestNode;
        RightClosestNode        :=  RightClosestNode.ChildNodes[LEFT_CHILD];
      end; // .while
      
      ItemNode.Item :=  RightClosestNode.Item; RightClosestNode.Item  :=  nil;
      ItemNode.Hash :=  RightClosestNode.Hash;
      Self.RemoveNode(RightClosestNodeParent, RightClosestNode);
    end; // .else
  end; // .else
end; // .procedure TAssocArray.RemoveNode

procedure TAssocArray.RemoveItem
(
  {n} ParentNode: PAssocArrayNode;
      ItemNode:   PAssocArrayNode;
  {n} ParentItem: PAssocArrayItem;
      Item:       PAssocArrayItem
);

begin
  {!} Assert(ItemNode <> nil);
  {!} Assert(Item <> nil);
  Self.FreeItemValue(Item);
  
  if (ItemNode.Item = Item) and (Item.NextItem = nil) then begin
    Self.RemoveNode(ParentNode, ItemNode);
    (* RemoveNode is recursive procedure not affecting the counter *)
    DEC(Self.fNodeCount);
  end // .if
  else begin
    if ItemNode.Item = Item then begin
      ItemNode.Item :=  Item.NextItem;
    end // .if
    else begin
      {!} Assert(ParentItem <> nil);
      ParentItem.NextItem :=  Item.NextItem;
    end; // .else
  end; // .else
  
  DISPOSE(Item); Item :=  nil;
  DEC(Self.fItemCount);
end; // .procedure TAssocArray.RemoveItem

procedure TAssocArray.FreeNode ({IN} var {n} Node: PAssocArrayNode);
var
{U} Item:     PAssocArrayItem;
{U} NextItem: PAssocArrayItem;

begin
  Item      :=  nil;
  NextItem  :=  nil;
  // * * * * * //
  if Node <> nil then begin
    Item  :=  Node.Item;
    
    while Item <> nil do begin
      NextItem  :=  Item.NextItem;
      Self.FreeItemValue(Item);
      DISPOSE(Item); Item :=  nil;
      Item  :=  NextItem;
    end; // .while
    
    Self.FreeNode(Node.ChildNodes[LEFT_CHILD]);
    Self.FreeNode(Node.ChildNodes[RIGHT_CHILD]);
    DISPOSE(Node); Node :=  nil;
  end; // .if
end; // .procedure TAssocArray.FreeNode

procedure TAssocArray.Clear;
begin
  {!} Assert(not Self.Locked);
  Self.FreeNode(Self.fRoot);
  Self.fItemCount :=  0;
  Self.fNodeCount :=  0;
end; // .procedure TAssocArray.Clear

function TAssocArray.GetPreprocessedKey (const Key: string): string;
begin
  if @Self.KeyPreprocessFunc = nil then begin
    result  :=  Key;
  end // .if
  else begin
    result  :=  Self.KeyPreprocessFunc(Key);
  end; // .else
end; // .function TAssocArray.GetPreprocessedKey

function TAssocArray.IsValidValue ({n} Value: POINTER): BOOLEAN;
begin
  result  :=  Self.ItemGuardProc(Value, Self.ItemsAreObjects, Utils.TItemGuard(Self.fItemGuard));
end; // .function TAssocArray.IsValidValue 

function TAssocArray.CalcCritDepth: INTEGER;
begin
  result  :=  Alg.IntLog2(Self.NodeCount + 1) shl 1;
end; // .function TAssocArray.CalcCritDepth

function AssocArrayCompareNodes (A, B: INTEGER): INTEGER;
begin
  if PAssocArrayNode(A).Hash > PAssocArrayNode(B).Hash then begin
    result  :=  +1;
  end // .if
  else if PAssocArrayNode(A).Hash < PAssocArrayNode(B).Hash then begin
    result  :=  -1;
  end // .ELSEIF
  else begin
    result  :=  0;
  end; // .else
end; // .function AssocArrayCompareNodes

procedure TAssocArray.ConvertToLinearNodeArray (out Res: TLinearNodeArray);
var
    LeftInd:              INTEGER;
    RightInd:             INTEGER;
    RightCheckInd:        INTEGER;
    NumNotProcessedNodes: INTEGER;
{U} CurrNode:             PAssocArrayNode;
    i:                    INTEGER;
  
begin
  SetLength(Res.NodeArray, Self.NodeCount);
  Res.NodeCount :=  Self.NodeCount;
  Res.ItemCount :=  Self.ItemCount;
  
  if Self.NodeCount > 0 then begin
    CurrNode                :=  Self.fRoot;
    LeftInd                 :=  0;
    Res.NodeArray[LeftInd]  :=  CurrNode;
    RightInd                :=  Self.NodeCount;
    RightCheckInd           :=  RightInd - 1;
    NumNotProcessedNodes    :=  Self.NodeCount - 1;
    
    while NumNotProcessedNodes > 0 do begin
      if CurrNode.ChildNodes[RIGHT_CHILD] <> nil then begin
        DEC(RightInd);
        Res.NodeArray[RightInd] :=  CurrNode.ChildNodes[RIGHT_CHILD];
        DEC(NumNotProcessedNodes);
      end; // .if
      
      if CurrNode.ChildNodes[LEFT_CHILD] <> nil then begin
        CurrNode  :=  CurrNode.ChildNodes[LEFT_CHILD];
        INC(LeftInd);
        Res.NodeArray[LeftInd]  :=  CurrNode;
        DEC(NumNotProcessedNodes);
      end // .if
      else begin
        CurrNode  :=  Res.NodeArray[RightCheckInd];
        DEC(RightCheckInd);
      end; // .else
    end; // .while
    
    for i:=0 to Self.NodeCount - 1 do begin
      Res.NodeArray[i].ChildNodes[LEFT_CHILD]   :=  nil;
      Res.NodeArray[i].ChildNodes[RIGHT_CHILD]  :=  nil;
    end; // .for
    
    Self.fRoot      :=  nil;
    Self.fNodeCount :=  0;
    Self.fItemCount :=  0;
    Alg.CustomQuickSort(POINTER(Res.NodeArray), 0, Res.NodeCount - 1, AssocArrayCompareNodes);
  end; // .if
end; // .procedure TAssocArray.ConvertToLinearNodeArray

procedure TAssocArray.Rebuild;
var
  LinearNodeArray:  TLinearNodeArray;
  NodeArray:        TNodeArray;
  
  procedure InsertNode (InsNode: PAssocArrayNode);
  var
  {U} ParentNode: PAssocArrayNode;
  {U} CurrNode:   PAssocArrayNode;
    
  begin
    {!} Assert(InsNode <> nil);
    ParentNode  :=  nil;
    CurrNode    :=  Self.fRoot;
    // * * * * * //
    while CurrNode <> nil do begin
      ParentNode  :=  CurrNode;
      CurrNode    :=  CurrNode.ChildNodes[InsNode.Hash >= CurrNode.Hash];
    end; // .while
    
    ParentNode.ChildNodes[InsNode.Hash >= ParentNode.Hash]  :=  InsNode;
  end; // .procedure InsertNode
  
  procedure InsertNodeRange (MinInd, MaxInd: INTEGER);
  var
  {U} InsNode:    PAssocArrayNode;
      RangeLen:   INTEGER;
      MiddleInd:  INTEGER;

  begin
    RangeLen  :=  MaxInd - MinInd + 1;
    {!} Assert(RangeLen > 0);
    {!} Assert((MinInd >= 0) and (MaxInd < LENGTH(NodeArray)));
    // * * * * * //
    MiddleInd :=  MinInd + (MaxInd - MinInd) shr 1;
    InsNode   :=  NodeArray[MiddleInd];
    
    if Self.fRoot = nil then begin
      Self.fRoot  :=  InsNode;
    end // .if
    else begin
      InsertNode(InsNode);
    end; // .else
    
    if RangeLen > 2 then begin
      InsertNodeRange(MinInd, MiddleInd - 1);
      InsertNodeRange(MiddleInd + 1, MaxInd);
    end // .if
    else if RangeLen = 2 then begin
      InsertNode(NodeArray[MiddleInd + 1]);
    end; // .ELSEIF
  end; // .procedure InsertNodeRange
  
begin
  {!} Assert(not Self.Locked);
  if Self.NodeCount > 2 then begin
    Self.ConvertToLinearNodeArray(LinearNodeArray);
    Self.fNodeCount :=  LinearNodeArray.NodeCount;
    Self.fItemCount :=  LinearNodeArray.ItemCount;
    NodeArray       :=  LinearNodeArray.NodeArray;
    InsertNodeRange(0, Self.NodeCount - 1);
  end; // .if
end; // .procedure TAssocArray.Rebuild

function TAssocArray.FindItem
(
              Hash:       INTEGER;
        const Key:        string;
  out {ni}   ParentNode: PAssocArrayNode;
  out {ni}   ItemNode:   PAssocArrayNode;
  out {ni}   ParentItem: PAssocArrayItem;
  out {ni}   Item:       PAssocArrayItem
): BOOLEAN;

var
  SearchDepth:      INTEGER;
  CritSearchDepth:  INTEGER;
  
begin
  {!} Assert(ParentNode = nil);
  {!} Assert(ItemNode = nil);
  {!} Assert(ParentItem = nil);
  {!} Assert(Item = nil);
  result  :=  FALSE;
  
  if Self.NodeCount > 0 then begin
    CritSearchDepth :=  Self.CalcCritDepth;
    SearchDepth     :=  1;
    ItemNode        :=  Self.fRoot;
    
    while (ItemNode <> nil) and (ItemNode.Hash <> Hash) do begin
      INC(SearchDepth);
      ParentNode  :=  ItemNode;
      ItemNode    :=  ItemNode.ChildNodes[Hash >= ItemNode.Hash];
    end; // .while
    
    if SearchDepth > CritSearchDepth then begin
      Self.Rebuild;
      ParentNode  :=  nil;
      ItemNode    :=  nil;
      ParentItem  :=  nil;
      Item        :=  nil;
      result      :=  Self.FindItem(Hash, Key, ParentNode, ItemNode, ParentItem, Item);
    end // .if
    else if ItemNode <> nil then begin
      Item  :=  ItemNode.Item;

      while (Item <> nil) and (Self.GetPreprocessedKey(Item.Key) <> Key) do begin
        ParentItem  :=  Item;
        Item        :=  Item.NextItem;
      end; // .while

      result  :=  Item <> nil;
    end; // .ELSEIF
  end; // .if
end; // .function TAssocArray.FindItem

function TAssocArray.GetValue (Key: string): {n} POINTER;
var
{U} ItemNode:   PAssocArrayNode;
{U} ParentNode: PAssocArrayNode;
{U} Item:       PAssocArrayItem;
{U} ParentItem: PAssocArrayItem;
    Hash:       INTEGER;
  
begin
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  Item        :=  nil;
  ParentItem  :=  nil;
  // * * * * * //
  Key   :=  Self.GetPreprocessedKey(Key);
  Hash  :=  Self.HashFunc(Key);
  
  if Self.FindItem(Hash, Key, ParentNode, ItemNode, ParentItem, Item) then begin
    result  :=  Item.Value;
  end // .if
  else begin
    result  :=  nil;
  end; // .else
end; // .function TAssocArray.GetValue

function TAssocArray.GetExistingValue (Key: string; out {Un} Res: POINTER): BOOLEAN;
var
{U} ItemNode:   PAssocArrayNode;
{U} ParentNode: PAssocArrayNode;
{U} Item:       PAssocArrayItem;
{U} ParentItem: PAssocArrayItem;
    Hash:       INTEGER;
  
begin
  {!} Assert(Res = nil);
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  Item        :=  nil;
  ParentItem  :=  nil;
  // * * * * * //
  Key     :=  Self.GetPreprocessedKey(Key);
  Hash    :=  Self.HashFunc(Key);
  result  :=  Self.FindItem(Hash, Key, ParentNode, ItemNode, ParentItem, Item);
  
  if result then begin
    Res :=  Item.Value;
  end; // .if
end; // .function TAssocArray.GetExistingValue

procedure TAssocArray.SetValue (Key: string; {OUn} NewValue: POINTER);
var
{U} ItemNode:         PAssocArrayNode;
{U} ParentNode:       PAssocArrayNode;
{U} Item:             PAssocArrayItem;
{U} ParentItem:       PAssocArrayItem;
{O} NewItem:          PAssocArrayItem;
{O} NewNode:          PAssocArrayNode;
    PreprocessedKey:  string;
    Hash:             INTEGER;
  
begin
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  Item        :=  nil;
  ParentItem  :=  nil;
  NewItem     :=  nil;
  NewNode     :=  nil;
  // * * * * * //
  {!} Assert(Self.IsValidValue(NewValue));
  PreprocessedKey :=  Self.GetPreprocessedKey(Key);
  Hash            :=  Self.HashFunc(PreprocessedKey);
  
  if Self.FindItem(Hash, PreprocessedKey, ParentNode, ItemNode, ParentItem, Item) then begin
    if Item.Value <> NewValue then begin
      Self.FreeItemValue(Item);
      Item.Value  :=  NewValue;
    end; // .if
  end // .if
  else begin
    NEW(NewItem);
    NewItem.Key       :=  Key;
    NewItem.Value     :=  NewValue;
    NewItem.NextItem  :=  nil;
    INC(Self.fItemCount);
    
    if ItemNode <> nil then begin
      ParentItem.NextItem :=  NewItem; NewItem  :=  nil;
    end // .if
    else begin
      NEW(NewNode);
      NewNode.Hash                    :=  Hash;
      NewNode.ChildNodes[LEFT_CHILD]  :=  nil;
      NewNode.ChildNodes[RIGHT_CHILD] :=  nil;
      NewNode.Item                    :=  NewItem; NewItem  :=  nil;
      INC(Self.fNodeCount);
      
      if Self.NodeCount > 1 then begin
        ParentNode.ChildNodes[NewNode.Hash >= ParentNode.Hash]  :=  NewNode; NewNode  :=  nil;
      end // .if
      else begin
        Self.fRoot  :=  NewNode; NewNode  :=  nil;
      end; // .else
    end; // .else
  end; // .else   
end; // .procedure TAssocArray.SetValue

function TAssocArray.DeleteItem (Key: string): BOOLEAN;
var
{U} ParentNode: PAssocArrayNode;
{U} ItemNode:   PAssocArrayNode;
{U} ParentItem: PAssocArrayItem;
{U} Item:       PAssocArrayItem;
    Hash:       INTEGER;
  
begin
  {!} Assert(not Self.Locked);
  ItemNode          :=  nil;
  ParentNode        :=  nil;
  Item              :=  nil;
  ParentItem        :=  nil;
  // * * * * * //
  Key     :=  Self.GetPreprocessedKey(Key);
  Hash    :=  Self.HashFunc(Key);
  result  :=  Self.FindItem(Hash, Key, ParentNode, ItemNode, ParentItem, Item);
  
  if result then begin
    Self.RemoveItem(ParentNode, ItemNode, ParentItem, Item);
  end; // .if
end; // .function TAssocArray.DeleteItem

function TAssocArray.TakeValue (Key: string; out {OUn} Value: POINTER): BOOLEAN;
var
{U} ParentNode:       PAssocArrayNode;
{U} ItemNode:         PAssocArrayNode;
{U} ParentItem:       PAssocArrayItem;
{U} Item:             PAssocArrayItem;
    Hash:             INTEGER;
  

begin
  {!} Assert(Value = nil);
  ItemNode          :=  nil;
  ParentNode        :=  nil;
  Item              :=  nil;
  ParentItem        :=  nil;
  // * * * * * //
  Key     :=  Self.GetPreprocessedKey(Key);
  Hash    :=  Self.HashFunc(Key);
  result  :=  Self.FindItem(Hash, Key, ParentNode, ItemNode, ParentItem, Item);
  
  if result then begin
    Value :=  Item.Value;
    {!} Assert(Self.IsValidValue(nil));
    Item.Value  :=  nil;
  end; // .if
end; // .function TAssocArray.TakeValue

function TAssocArray.ReplaceValue
(
            Key:      string;
      {OUn} NewValue: POINTER;
  out {OUn} OldValue: POINTER
): BOOLEAN;

var
{U} ParentNode:       PAssocArrayNode;
{U} ItemNode:         PAssocArrayNode;
{U} ParentItem:       PAssocArrayItem;
{U} Item:             PAssocArrayItem;
    Hash:             INTEGER;

begin
  {!} Assert(OldValue = nil);
  {!} Assert(Self.IsValidValue(NewValue));
  ItemNode          :=  nil;
  ParentNode        :=  nil;
  Item              :=  nil;
  ParentItem        :=  nil;
  // * * * * * //
  Key     :=  Self.GetPreprocessedKey(Key);
  Hash    :=  Self.HashFunc(Key);
  result  :=  Self.FindItem(Hash, Key, ParentNode, ItemNode, ParentItem, Item);
  
  if result then begin
    OldValue    :=  Item.Value;
    Item.Value  :=  NewValue;
  end; // .if
end; // .function TAssocArray.ReplaceValue

procedure TAssocArray.EndIterate;
begin
  {!} Assert(Self.fLocked);
  Self.fLocked  :=  FALSE;
end; // .procedure TAssocArray.EndIterate

procedure TAssocArray.BeginIterate;
var
  OptimalNumIterNodes:  INTEGER;
  
begin
  {!} Assert(not Self.fLocked);
  OptimalNumIterNodes :=  Self.CalcCritDepth + 1;
  
  if LENGTH(Self.fIterNodes) < OptimalNumIterNodes then begin
    SetLength(Self.fIterNodes, OptimalNumIterNodes);
  end; // .if
  
  Self.fIterCurrItem  :=  nil;
  
  if Self.NodeCount > 0 then begin
    Self.fIterNodeInd   :=  0;
    Self.fIterNodes[0]  :=  Self.fRoot;
  end // .if
  else begin
    Self.fIterNodeInd :=  -1;
  end; // .else
  
  Self.fLocked  :=  TRUE;
end; // .procedure TAssocArray.BeginIterate

function TAssocArray.IterateNext (out Key: string; out {Un} Value: POINTER): BOOLEAN;
var
{U} IterNode: PAssocArrayNode;  

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
      DEC(Self.fIterNodeInd);
      
      if IterNode.ChildNodes[LEFT_CHILD] <> nil then begin
        INC(Self.fIterNodeInd);
        Self.fIterNodes[Self.fIterNodeInd]  :=  IterNode.ChildNodes[LEFT_CHILD];
      end; // .if
      if IterNode.ChildNodes[RIGHT_CHILD] <> nil then begin
        INC(Self.fIterNodeInd);
        Self.fIterNodes[Self.fIterNodeInd]  :=  IterNode.ChildNodes[RIGHT_CHILD];
      end; // .if
    end; // .if
    
    Key                 :=  Self.fIterCurrItem.Key;
    Value               :=  Self.fIterCurrItem.Value;
    Self.fIterCurrItem  :=  Self.fIterCurrItem.NextItem;
  end; // .if
end; // .function TAssocArray.IterateNext

constructor TObjArray.Create
(

                OwnsItems:        BOOLEAN;
                ItemsAreObjects:  BOOLEAN;
                ItemGuardProc:    Utils.TItemGuardProc;
  {IN} var  {n} ItemGuard:        Utils.TItemGuard
);

begin
  {!} Assert(@ItemGuardProc <> nil);
  Self.fOwnsItems       :=  OwnsItems;
  Self.fItemsAreObjects :=  ItemsAreObjects;
  Self.fItemGuardProc   :=  ItemGuardProc;
  Self.fItemGuard       :=  ItemGuard;
  ItemGuard             :=  nil;
end; // .constructor TObjArray.Create

destructor TObjArray.Destroy;
begin
  Self.Clear;
  SysUtils.FreeAndNil(Self.fItemGuard);
end; // .destructor TObjArray.Destroy

function TObjArray.KeyToHash ({n} Key: POINTER): INTEGER;
begin
  result  :=  Crypto.Bb2011Encode(INTEGER(Key));
end; // .function TObjArray.KeyToHash

function TObjArray.HashToKey (Hash: INTEGER): {n} POINTER;
begin
  result  :=  POINTER(Crypto.Bb2011Decode(Hash));
end; // .function TObjArray.HashToKey

function TObjArray.IsValidValue ({n} Value: POINTER): BOOLEAN;
begin
  result  :=  Self.ItemGuardProc(Value, Self.ItemsAreObjects, Utils.TItemGuard(Self.fItemGuard));
end; // .function TObjArray.IsValidValue 

function TObjArray.CalcCritDepth: INTEGER;
begin
  result  :=  Alg.IntLog2(Self.NodeCount + 1) shl 1;
end; // .function TObjArray.CalcCritDepth

procedure TObjArray.FreeNodeValue (Node: PObjArrayNode);
begin
  {!} Assert(Node <> nil);
  if Self.OwnsItems then begin
    if Self.ItemsAreObjects then begin
      TObject(Node.Value).Free;
    end // .if
    else begin
      FreeMem(Node.Value);
    end; // .else
  end; // .if
  
  Node.Value  :=  nil;
end; // .procedure TObjArray.FreeNodeValue

procedure TObjArray.FreeNode ({IN} var {n} Node: PObjArrayNode);
begin
  if Node <> nil then begin
    Self.FreeNodeValue(Node);
    Self.FreeNode(Node.ChildNodes[LEFT_CHILD]);
    Self.FreeNode(Node.ChildNodes[RIGHT_CHILD]);
    DISPOSE(Node); Node :=  nil;
  end; // .if
end; // .procedure TObjArray.FreeNode

procedure TObjArray.RemoveNode ({n} ParentNode: PObjArrayNode; Node: PObjArrayNode);
var
{U} RightClosestNodeParent: PObjArrayNode;
{U} RightClosestNode:       PObjArrayNode;
    NodeIsRoot:             BOOLEAN;
    NodeSide:               TChildNodeSide;
  
begin
  {!} Assert(Node <> nil);
  RightClosestNodeParent  :=  nil;
  RightClosestNode        :=  nil;
  NodeSide                :=  FALSE;
  // * * * * * //
  NodeIsRoot  :=  ParentNode = nil;
  
  if Self.NodeCount = 1 then begin
    {!} Assert(NodeIsRoot);
    {!} Assert(Node = Self.fRoot);
    DISPOSE(Self.fRoot); Self.fRoot :=  nil;
  end // .if
  else begin
    if not NodeIsRoot then begin
      NodeSide  :=  Node.Hash >= ParentNode.Hash;
    end; // .if
    (* N
      - -
    *)
    if (Node.ChildNodes[LEFT_CHILD] = nil) and (Node.ChildNodes[RIGHT_CHILD] = nil) then begin
      ParentNode.ChildNodes[NodeSide] :=  nil;
      DISPOSE(Node); Node :=  nil;
    end // .if
    (* N
      - R
    *)
    else if Node.ChildNodes[LEFT_CHILD] = nil then begin
      if NodeIsRoot then begin
        Self.fRoot  :=  Node.ChildNodes[RIGHT_CHILD];
      end // .if
      else begin
        ParentNode.ChildNodes[NodeSide] :=  Node.ChildNodes[RIGHT_CHILD];
      end; // .else
      
      DISPOSE(Node); Node :=  nil;
    end // .ELSEIF
    (* N
      L -
    *)
    else if Node.ChildNodes[RIGHT_CHILD] = nil then begin
      if NodeIsRoot then begin
        Self.fRoot  :=  Node.ChildNodes[LEFT_CHILD];
      end // .if
      else begin
        ParentNode.ChildNodes[NodeSide] :=  Node.ChildNodes[LEFT_CHILD];
      end; // .else
      
      DISPOSE(Node); Node :=  nil;
    end // .ELSEIF
    (* N
      L R
    *)
    else begin
      RightClosestNodeParent  :=  Node;
      RightClosestNode        :=  Node.ChildNodes[RIGHT_CHILD];
      
      while RightClosestNode.ChildNodes[LEFT_CHILD] <> nil do begin
        RightClosestNodeParent  :=  RightClosestNode;
        RightClosestNode        :=  RightClosestNode.ChildNodes[LEFT_CHILD];
      end; // .while
      
      Node.Value  :=  RightClosestNode.Value; RightClosestNode.Value  :=  nil;
      Node.Hash   :=  RightClosestNode.Hash;
      Self.RemoveNode(RightClosestNodeParent, RightClosestNode);
    end; // .else
  end; // .else
end; // .procedure TObjArray.RemoveNode

procedure TObjArray.Clear;
begin
  {!} Assert(not Self.Locked);
  Self.FreeNode(Self.fRoot);
  Self.fNodeCount :=  0;
end; // .procedure TObjArray.Clear

function TObjArray.CloneNode ({n} Node: PObjArrayNode): {On} PObjArrayNode;
begin
  if Node = nil then begin
    result  :=  nil;
  end // .if
  else begin
    NEW(result);
    result.Hash :=  Node.Hash;
    {!} Assert(Self.IsValidValue(Node.Value));
    
    if (Node.Value = nil) or (not Self.OwnsItems) then begin
      result.Value  :=  Node.Value;
    end // .if
    else begin
      {!} Assert(Self.ItemsAreObjects);
      {!} Assert(TObject(Node.Value) IS Utils.TCloneable);
      result.Value  :=  Utils.TCloneable(Node.Value).Clone;
    end; // .else
    
    result.ChildNodes[LEFT_CHILD]   :=  Self.CloneNode(Node.ChildNodes[LEFT_CHILD]);
    result.ChildNodes[RIGHT_CHILD]  :=  Self.CloneNode(Node.ChildNodes[RIGHT_CHILD]);
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
  end; // .if
end; // .procedure TObjArray.Assign

function ObjArrayCompareNodes (A, B: INTEGER): INTEGER;
begin
  if PObjArrayNode(A).Hash > PObjArrayNode(B).Hash then begin
    result  :=  +1;
  end // .if
  else if PObjArrayNode(A).Hash < PObjArrayNode(B).Hash then begin
    result  :=  -1;
  end // .ELSEIF
  else begin
    result  :=  0;
  end; // .else
end; // .function ObjArrayCompareNodes

procedure TObjArray.ConvertToLinearNodeArray (out Res: TLinearObjNodeArray);
var
    LeftInd:              INTEGER;
    RightInd:             INTEGER;
    RightCheckInd:        INTEGER;
    NumNotProcessedNodes: INTEGER;
{U} CurrNode:             PObjArrayNode;
    i:                    INTEGER;
  
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
        DEC(RightInd);
        Res.NodeArray[RightInd] :=  CurrNode.ChildNodes[RIGHT_CHILD];
        DEC(NumNotProcessedNodes);
      end; // .if
      
      if CurrNode.ChildNodes[LEFT_CHILD] <> nil then begin
        CurrNode  :=  CurrNode.ChildNodes[LEFT_CHILD];
        INC(LeftInd);
        Res.NodeArray[LeftInd]  :=  CurrNode;
        DEC(NumNotProcessedNodes);
      end // .if
      else begin
        CurrNode  :=  Res.NodeArray[RightCheckInd];
        DEC(RightCheckInd);
      end; // .else
    end; // .while
    
    for i:=0 to Self.NodeCount - 1 do begin
      Res.NodeArray[i].ChildNodes[LEFT_CHILD]   :=  nil;
      Res.NodeArray[i].ChildNodes[RIGHT_CHILD]  :=  nil;
    end; // .for
    
    Self.fRoot      :=  nil;
    Self.fNodeCount :=  0;
    Alg.CustomQuickSort(POINTER(Res.NodeArray), 0, Res.NodeCount - 1, ObjArrayCompareNodes);
  end; // .if
end; // .procedure TObjArray.ConvertToLinearNodeArray

procedure TObjArray.Rebuild;
var
  LinearNodeArray:  TLinearObjNodeArray;
  NodeArray:        TObjNodeArray;
  
  procedure InsertNode (InsNode: PObjArrayNode);
  var
  {U} ParentNode: PObjArrayNode;
  {U} CurrNode:   PObjArrayNode;
    
  begin
    {!} Assert(InsNode <> nil);
    ParentNode  :=  nil;
    CurrNode    :=  Self.fRoot;
    // * * * * * //
    while CurrNode <> nil do begin
      ParentNode  :=  CurrNode;
      CurrNode    :=  CurrNode.ChildNodes[InsNode.Hash >= CurrNode.Hash];
    end; // .while
    
    ParentNode.ChildNodes[InsNode.Hash >= ParentNode.Hash]  :=  InsNode;
  end; // .procedure InsertNode
  
  procedure InsertNodeRange (MinInd, MaxInd: INTEGER);
  var
      RangeLen:   INTEGER;
      MiddleInd:  INTEGER;
  {U} InsNode:    PObjArrayNode;
    
  begin
    RangeLen  :=  MaxInd - MinInd + 1;
    {!} Assert(RangeLen > 0);
    {!} Assert((MinInd >= 0) and (MaxInd < LENGTH(NodeArray)));
    // * * * * * //
    MiddleInd :=  MinInd + (MaxInd - MinInd) shr 1;
    InsNode   :=  NodeArray[MiddleInd];
    
    if Self.fRoot = nil then begin
      Self.fRoot  :=  InsNode;
    end // .if
    else begin
      InsertNode(InsNode);
    end; // .else
    
    if RangeLen > 2 then begin
      InsertNodeRange(MinInd, MiddleInd - 1);
      InsertNodeRange(MiddleInd + 1, MaxInd);
    end // .if
    else if RangeLen = 2 then begin
      InsertNode(NodeArray[MiddleInd + 1]);
    end; // .ELSEIF
  end; // .procedure InsertNodeRange
  
begin
  {!} Assert(not Self.Locked);
  if Self.NodeCount > 2 then begin
    Self.ConvertToLinearNodeArray(LinearNodeArray);
    Self.fNodeCount :=  LinearNodeArray.NodeCount;
    NodeArray       :=  LinearNodeArray.NodeArray;
    InsertNodeRange(0, Self.NodeCount - 1);
  end; // .if
end; // .procedure TObjArray.Rebuild

function TObjArray.FindItem
(
      {n}   Key:        POINTER;
  out {ni}  ParentNode: PObjArrayNode;
  out {ni}  ItemNode:   PObjArrayNode
): BOOLEAN;

var
  Hash:             INTEGER;
  SearchDepth:      INTEGER;
  CritSearchDepth:  INTEGER;
  
begin
  {!} Assert(ParentNode = nil);
  {!} Assert(ItemNode = nil);
  result  :=  FALSE;
  
  if Self.NodeCount > 0 then begin
    Hash            :=  Self.KeyToHash(Key);
    CritSearchDepth :=  Self.CalcCritDepth;
    SearchDepth     :=  1;
    ItemNode        :=  Self.fRoot;
    
    while (ItemNode <> nil) and (ItemNode.Hash <> Hash) do begin
      INC(SearchDepth);
      ParentNode  :=  ItemNode;
      ItemNode    :=  ItemNode.ChildNodes[Hash >= ItemNode.Hash];
    end; // .while
    
    if SearchDepth > CritSearchDepth then begin
      Self.Rebuild;
      ParentNode  :=  nil;
      ItemNode    :=  nil;
      result      :=  Self.FindItem(Key, ParentNode, ItemNode);
    end; // .if
    
    result  :=  ItemNode <> nil;
  end; // .if
end; // .function TObjArray.FindItem

function TObjArray.GetValue ({n} Key: POINTER): {n} POINTER;
var
{U} ItemNode:   PObjArrayNode;
{U} ParentNode: PObjArrayNode;
  
begin
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  // * * * * * //
  if Self.FindItem(Key, ParentNode, ItemNode) then begin
    result  :=  ItemNode.Value;
  end // .if
  else begin
    result  :=  nil;
  end; // .else
end; // .function TObjArray.GetValue

function TObjArray.GetExistingValue ({n} Key: POINTER; out {Un} Res: POINTER): BOOLEAN;
var
{U} ItemNode:   PObjArrayNode;
{U} ParentNode: PObjArrayNode;
  
begin
  {!} Assert(Res = nil);
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  // * * * * * //
  result  :=  Self.FindItem(Key, ParentNode, ItemNode);
  
  if result then begin
    Res :=  ItemNode.Value;
  end; // .if
end; // .function TObjArray.GetExistingValue

procedure TObjArray.SetValue ({n} Key: POINTER; {OUn} NewValue: POINTER);
var
{U} ItemNode:   PObjArrayNode;
{U} ParentNode: PObjArrayNode;
{O} NewNode:    PObjArrayNode;
  
begin
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  NewNode     :=  nil;
  // * * * * * //
  {!} Assert(Self.IsValidValue(NewValue));
  if Self.FindItem(Key, ParentNode, ItemNode) then begin
    if ItemNode.Value <> NewValue then begin
      Self.FreeNodeValue(ItemNode);
      ItemNode.Value  :=  NewValue;
    end; // .if
  end // .if
  else begin
    NEW(NewNode);
    NewNode.Hash  :=  Self.KeyToHash(Key);
    NewNode.Value :=  NewValue;
    NewNode.ChildNodes[LEFT_CHILD]  :=  nil;
    NewNode.ChildNodes[RIGHT_CHILD] :=  nil;
    INC(Self.fNodeCount);
    
    if Self.NodeCount > 1 then begin
      ParentNode.ChildNodes[NewNode.Hash >= ParentNode.Hash]  :=  NewNode; NewNode  :=  nil;
    end // .if
    else begin
      Self.fRoot  :=  NewNode; NewNode  :=  nil;
    end; // .else
  end; // .else   
end; // .procedure TObjArray.SetValue

function TObjArray.DeleteItem ({n} Key: POINTER): BOOLEAN;
var
{U} ParentNode: PObjArrayNode;
{U} ItemNode:   PObjArrayNode;
  
begin
  {!} Assert(not Self.Locked);
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  // * * * * * //
  result  :=  Self.FindItem(Key, ParentNode, ItemNode);
  
  if result then begin
    Self.RemoveNode(ParentNode, ItemNode);
    DEC(Self.fNodeCount);
  end; // .if
end; // .function TObjArray.DeleteItem

function TObjArray.TakeValue ({n} Key: POINTER; out {OUn} Value: POINTER): BOOLEAN;
var
{U} ParentNode: PObjArrayNode;
{U} ItemNode:   PObjArrayNode;

begin
  {!} Assert(Value = nil);
  ItemNode    :=  nil;
  ParentNode  :=  nil;
  // * * * * * //
  result  :=  Self.FindItem(Key, ParentNode, ItemNode);
  
  if result then begin
    Value :=  ItemNode.Value;
    {!} Assert(Self.IsValidValue(nil));
    ItemNode.Value  :=  nil;
  end; // .if
end; // .function TObjArray.TakeValue

function TObjArray.ReplaceValue
(
      {n}   Key:      POINTER;
      {OUn} NewValue: POINTER;
  out {OUn} OldValue: POINTER
): BOOLEAN;

var
{U} ParentNode: PObjArrayNode;
{U} ItemNode:   PObjArrayNode;

begin
  {!} Assert(OldValue = nil);
  {!} Assert(Self.IsValidValue(NewValue));
  ItemNode          :=  nil;
  ParentNode        :=  nil;
  // * * * * * //
  result  :=  Self.FindItem(Key, ParentNode, ItemNode);
  
  if result then begin
    OldValue        :=  ItemNode.Value;
    ItemNode.Value  :=  NewValue;
  end; // .if
end; // .function TObjArray.ReplaceValue

procedure TObjArray.EndIterate;
begin
  {!} Assert(Self.fLocked);
  Self.fLocked  :=  FALSE;
end; // .procedure TObjArray.EndIterate

procedure TObjArray.BeginIterate;
var
  OptimalNumIterNodes:  INTEGER;
  
begin
  {!} Assert(not Self.fLocked);
  OptimalNumIterNodes :=  Self.CalcCritDepth + 1;
  
  if LENGTH(Self.fIterNodes) < OptimalNumIterNodes then begin
    SetLength(Self.fIterNodes, OptimalNumIterNodes);
  end; // .if
  
  if Self.NodeCount > 0 then begin
    Self.fIterNodeInd   :=  0;
    Self.fIterNodes[0]  :=  Self.fRoot;
  end // .if
  else begin
    Self.fIterNodeInd :=  -1;
  end; // .else
  
  Self.fLocked  :=  TRUE;
end; // .procedure TObjArray.BeginIterate

function TObjArray.IterateNext (out {Un} Key: POINTER; out {Un} Value: POINTER): BOOLEAN;
var
{U} IterNode: PObjArrayNode;  

begin
  {!} Assert(Self.Locked);
  {!} Assert(Key = nil);
  {!} Assert(Value = nil);
  IterNode  :=  nil;
  // * * * * * //
  result  :=  Self.fIterNodeInd >= 0;
  
  if result then begin
    IterNode  :=  Self.fIterNodes[Self.fIterNodeInd];
    DEC(Self.fIterNodeInd);
    
    if IterNode.ChildNodes[LEFT_CHILD] <> nil then begin
      INC(Self.fIterNodeInd);
      Self.fIterNodes[Self.fIterNodeInd]  :=  IterNode.ChildNodes[LEFT_CHILD];
    end; // .if
    
    if IterNode.ChildNodes[RIGHT_CHILD] <> nil then begin
      INC(Self.fIterNodeInd);
      Self.fIterNodes[Self.fIterNodeInd]  :=  IterNode.ChildNodes[RIGHT_CHILD];
    end; // .if
    
    Key   :=  Self.HashToKey(IterNode.Hash);
    Value :=  IterNode.Value;
  end; // .if
end; // .function TObjArray.IterateNext

function NewAssocArr
(
      HashFunc:           THashFunc;
  {n} KeyPreprocessFunc:  TKeyPreprocessFunc;
      OwnsItems:          BOOLEAN;
      ItemsAreObjects:    BOOLEAN;
      ItemType:           TClass;
      AllowNIL:           BOOLEAN
): TAssocArray;

var
{O} ItemGuard:  Utils.TDefItemGuard;

begin
  {!} Assert(ItemsAreObjects or (ItemType = Utils.NO_TYPEGUARD));
  ItemGuard :=  Utils.TDefItemGuard.Create;
  // * * * * * //
  ItemGuard.ItemType  :=  ItemType;
  ItemGuard.AllowNIL  :=  AllowNIL;
  result              :=  TAssocArray.Create
  (
    HashFunc,
    KeyPreprocessFunc,
    OwnsItems,
    ItemsAreObjects,
    @Utils.DefItemGuardProc,
    Utils.TItemGuard(ItemGuard)
  );
end; // .function NewAssocArr

function NewSimpleAssocArr
(
      HashFunc:           THashFunc;
  {n} KeyPreprocessFunc:  TKeyPreprocessFunc
): TAssocArray;

var
{O} ItemGuard:  Utils.TCloneable;

begin
  ItemGuard :=  nil;
  // * * * * * //
  result  :=  TAssocArray.Create
  (
    HashFunc,
    KeyPreprocessFunc,
    not Utils.OWNS_ITEMS,
    not Utils.ITEMS_ARE_OBJECTS,
    @Utils.NoItemGuardProc,
    ItemGuard
  );
end; // .function NewSimpleAssocArr

function NewStrictAssocArr ({n} TypeGuard: TClass; OwnsItems: BOOLEAN = TRUE): TAssocArray;
begin
  result  :=  NewAssocArr
  (
    Crypto.AnsiCRC32,
    SysUtils.AnsiLowerCase,
    OwnsItems,
    Utils.ITEMS_ARE_OBJECTS,
    TypeGuard,
    Utils.ALLOW_NIL
  );
end; // .function NewStrictAssocArr

function NewObjArr
(
  OwnsItems:        BOOLEAN;
  ItemsAreObjects:  BOOLEAN;
  ItemType:         TClass;
  AllowNIL:         BOOLEAN
): TObjArray;

var
{O} ItemGuard:  Utils.TDefItemGuard;

begin
  {!} Assert(ItemsAreObjects or (ItemType = Utils.NO_TYPEGUARD));
  ItemGuard :=  Utils.TDefItemGuard.Create;
  // * * * * * //
  ItemGuard.ItemType  :=  ItemType;
  ItemGuard.AllowNIL  :=  AllowNIL;
  result              :=  TObjArray.Create
  (
    OwnsItems,
    ItemsAreObjects,
    @Utils.DefItemGuardProc,
    Utils.TItemGuard(ItemGuard)
  );
end; // .function NewObjArr

function NewSimpleObjArr: TObjArray;
var
{O} ItemGuard:  Utils.TCloneable;

begin
  ItemGuard :=  nil;
  // * * * * * //
  result  :=  TObjArray.Create
  (
    not Utils.OWNS_ITEMS,
    not Utils.ITEMS_ARE_OBJECTS,
    @Utils.NoItemGuardProc,
    ItemGuard
  );
end; // .function NewSimpleObjArr

function NewStrictObjArr ({n} TypeGuard: TClass): TObjArray;
begin
  result  :=  NewObjArr(Utils.OWNS_ITEMS, Utils.ITEMS_ARE_OBJECTS, TypeGuard, Utils.ALLOW_NIL);
end; // .function NewStrictObjArr

end.
