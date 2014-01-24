unit Lists;
{
DESCRIPTION:  Implementation of data structure "List" in several variants.
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Windows, SysUtils, Math, Classes, Utils, Alg, StrLib;

type
  TList = class (Utils.TCloneable)
    (***) protected (***)
      const
        FIRST_ALLOC_COUNT   = 16;
        DEFAULT_GROWTH_RATE = 200;
      
      var
      (* O  *)  fData:            (* OUn *) Utils.PEndlessPtrArr;
                fCapacity:        INTEGER;
                fCount:           INTEGER;
                fGrowthRate:      INTEGER;  // in percents, ex: 120 = 1.2 growth koefficient
                fOwnsItems:       BOOLEAN;
                fItemsAreObjects: BOOLEAN;
                fItemGuardProc:   Utils.TItemGuardProc;
      (* on *)  fItemGuard:       Utils.TItemGuard;   

      procedure FreeItem (Ind: INTEGER);
      procedure Put (Ind: INTEGER; (* OUn *) Item: POINTER);
      function  Get (Ind: INTEGER): (* n *) POINTER;
      function  AddEmpty: INTEGER;
    
    (***) public (***)
      constructor Create (OwnsItems: BOOLEAN; ItemsAreObjects: BOOLEAN; ItemGuardProc: Utils.TItemGuardProc; (* n *) var (* in *) ItemGuard: Utils.TItemGuard);
      destructor  Destroy; override;
      procedure Assign (Source: Utils.TCloneable); override;
      procedure Clear;
      function  IsValidItem ((* n *) Item: POINTER): BOOLEAN;
      procedure SetGrowthRate (NewGrowthRate: INTEGER);
      procedure SetCapacity (NewCapacity: INTEGER);
      procedure SetCount (NewCount: INTEGER);
      function  Add ((* OUn *) Item: POINTER): INTEGER;
      function  Top: (* n *) POINTER;
      function  Pop: (* OUn *) POINTER;
      procedure Delete (Ind: INTEGER);
      procedure Insert ((* OUn *) Item: POINTER; Ind: INTEGER);
      procedure Exchange (SrcInd, DstInd: INTEGER);
      procedure Move (SrcInd, DstInd: INTEGER);
      procedure Shift (StartInd, Count, ShiftBy: INTEGER);
      {Returns item with specified index and NILify it in the list}
      function  Take (Ind: INTEGER): (* OUn *) POINTER;
      {Returns old item}
      function  Replace (Ind: INTEGER; (* OUn *) NewValue: POINTER): (* OUn *) POINTER;
      procedure Pack;
      function  Find ((* n *) Item: POINTER; out Ind: INTEGER): BOOLEAN;
      {Binary search assuming list is sorted}
      function  QuickFind ((* n *) Item: POINTER; out Ind: INTEGER): BOOLEAN;
      procedure Sort;
      procedure CustomSort (Compare: Alg.TCompareFunc);
      
      property  Capacity:             INTEGER READ fCapacity;
      property  Count:                INTEGER READ fCount;
      property  GrowthRate:           INTEGER READ fGrowthRate;
      property  OwnsItems:            BOOLEAN READ fOwnsItems;
      property  ItemsAreObjects:      BOOLEAN READ fItemsAreObjects;
      property  ItemGuardProc:        Utils.TItemGuardProc READ fItemGuardProc;
      property  Items[Ind: INTEGER]:  (* n *) POINTER READ Get WRITE Put; default;
  end; // .class TList
  
  TStringList = class;

  TStringListCompareFunc  = function (List: TStringList; const Str1, Str2: string): INTEGER;
  
  TStringList = class (Utils.TCloneable)
    (***) protected (***)
      const
        FIRST_ALLOC_COUNT   = 16;
        DEFAULT_GROWTH_RATE = 200;
      
      var
                fKeys:              Utils.TArrayOfString;
      (* O  *)  fValues:            (* OUn *) Utils.PEndlessPtrArr;
                fCapacity:          INTEGER;
                fCount:             INTEGER;
                fGrowthRate:        INTEGER;  // in percents, ex: 120 = 1.2 grow koefficient
                fOwnsItems:         BOOLEAN;
                fItemsAreObjects:   BOOLEAN;
                fItemGuardProc:     Utils.TItemGuardProc;
      (* on *)  fItemGuard:         Utils.TItemGuard;
                fCaseInsensitive:   BOOLEAN;
                fForbidDuplicates:  BOOLEAN;
                fSorted:            BOOLEAN;

      procedure FreeValue (Ind: INTEGER);
      function  ValidateKey (const Key: string): BOOLEAN;
      procedure PutKey (Ind: INTEGER; const Key: string);
      function  GetKey (Ind: INTEGER): string;
      procedure PutValue (Ind: INTEGER; (* OUn *) Item: POINTER);
      function  GetValue (Ind: INTEGER): (* n *) POINTER;
      function  AddEmpty: INTEGER;
      procedure QuickSort (MinInd, MaxInd: INTEGER);
      function  QuickFind (const Key: string; (* i *) out Ind: INTEGER): BOOLEAN;
      procedure SetSorted (IsSorted: BOOLEAN);
      procedure EnsureNoDuplicates;
      procedure SetCaseInsensitive (NewCaseInsensitive: BOOLEAN);
      procedure SetForbidDuplicates (NewForbidDuplicates: BOOLEAN);
      function  GetItem (const Key: string): (* n *) POINTER;
      procedure PutItem (const Key: string; (* OUn *) Value: POINTER);
    
    (***) public (***)
      constructor Create (OwnsItems: BOOLEAN; ItemsAreObjects: BOOLEAN; ItemGuardProc: Utils.TItemGuardProc; (* n *) var {IN} ItemGuard: Utils.TItemGuard);
      destructor  Destroy; override;
      procedure Assign (Source: Utils.TCloneable); override;
      procedure Clear;
      function  IsValidItem ((* n *) Item: POINTER): BOOLEAN;
      procedure SetGrowthRate (NewGrowthRate: INTEGER);
      procedure SetCapacity (NewCapacity: INTEGER);
      procedure SetCount (NewCount: INTEGER);
      function  AddObj (const Key: string; (* OUn *) Value: POINTER): INTEGER;
      function  Add (const Key: string): INTEGER;
      function  Top: string;
      function  Pop ((* OUn *) out Item: POINTER): string;
      procedure Delete (Ind: INTEGER);
      procedure InsertObj (const Key: string; Value: (* OUn *) POINTER; Ind: INTEGER);
      procedure Insert (const Key: string; Ind: INTEGER);
      procedure Exchange (SrcInd, DstInd: INTEGER);
      procedure Move (SrcInd, DstInd: INTEGER);
      procedure Shift (StartInd, Count, ShiftBy: INTEGER);
      {Returns value with specified index and NILify it in the list}
      function  TakeValue (Ind: INTEGER): (* OUn *) POINTER;
      {Returns old value}
      function  ReplaceValue (Ind: INTEGER; (* OUn *) NewValue: POINTER): (* OUn *) POINTER;
      procedure Pack;
      function  CompareStrings (const Str1, Str2: string): INTEGER;
      {If not success then returns index, where new item should be insert to keep list sorted}
      function  Find (const Key: string; (* i *) out Ind: INTEGER): BOOLEAN;
      procedure Sort;
      procedure LoadFromText (const Text, EndOfLineMarker: string);
      function  ToText (const EndOfLineMarker: string): string;
      
      property  Capacity:                 INTEGER READ fCapacity;
      property  Count:                    INTEGER READ fCount;
      property  GrowthRate:               INTEGER READ fGrowthRate { = DEFAULT_GROWTH_RATE};
      property  OwnsItems:                BOOLEAN READ fOwnsItems;
      property  ItemsAreObjects:          BOOLEAN READ fItemsAreObjects;
      property  ItemGuardProc:            Utils.TItemGuardProc READ fItemGuardProc;
      property  Keys[Ind: INTEGER]:       string READ GetKey WRITE PutKey; default;
      property  Values[Ind: INTEGER]:     (* n *) POINTER READ GetValue WRITE PutValue;
      property  CaseInsensitive:          BOOLEAN READ fCaseInsensitive WRITE SetCaseInsensitive;
      property  ForbidDuplicates:         BOOLEAN READ fForbidDuplicates WRITE SetForbidDuplicates;
      property  Sorted:                   BOOLEAN READ fSorted WRITE SetSorted;
      property  Items[const Key: string]: (* n *) POINTER READ GetItem WRITE PutItem;
  end; // .class TStringList


function  NewStrList (OwnsItems: BOOLEAN; ItemsAreObjects: BOOLEAN; ItemType: TClass; AllowNIL: BOOLEAN): TStringList;
function  NewStrictList ({n} TypeGuard: TClass): TList;
function  NewSimpleList: TList;
function  NewList (OwnsItems: BOOLEAN; ItemsAreObjects: BOOLEAN; ItemType: TClass; AllowNIL: BOOLEAN): TList;
function  NewStrictStrList ({n} TypeGuard: TClass): TStringList;
function  NewSimpleStrList: TStringList;
  

(***) implementation (***)


constructor TList.Create (OwnsItems: BOOLEAN; ItemsAreObjects: BOOLEAN; ItemGuardProc: Utils.TItemGuardProc; (* n *) var (* in *) ItemGuard: Utils.TItemGuard);
begin
  {!} Assert(@ItemGuardProc <> nil);
  Self.fGrowthRate      :=  Self.DEFAULT_GROWTH_RATE;
  Self.fOwnsItems       :=  OwnsItems;
  Self.fItemsAreObjects :=  ItemsAreObjects;
  Self.fItemGuardProc   :=  ItemGuardProc;
  Self.fItemGuard       :=  ItemGuard;
  ItemGuard             :=  nil;
end; // .constructor TList.Create

destructor TList.Destroy;
begin
  Self.Clear;
  SysUtils.FreeAndNil(Self.fItemGuard);
end; // .destructor TList.Destroy

procedure TList.Assign (Source: Utils.TCloneable);
var
(* U *) SrcList:  TList;
        i:        INTEGER;
  
begin
  {!} Assert(Source <> nil);
  SrcList :=  Source AS TList;
  // * * * * * //
  if Self <> Source then begin
    Self.Clear;
    Self.fCapacity        :=  SrcList.Capacity;
    Self.fCount           :=  SrcList.Count;
    Self.fGrowthRate      :=  SrcList.GrowthRate;
    Self.fOwnsItems       :=  SrcList.OwnsItems;
    Self.fItemsAreObjects :=  SrcList.ItemsAreObjects;
    Self.fItemGuardProc   :=  SrcList.ItemGuardProc;
    Self.fItemGuard       :=  SrcList.fItemGuard.Clone;
    GetMem(Self.fData, Self.Capacity * SIZEOF(POINTER));
    for i:=0 to SrcList.Count - 1 do begin
      if (SrcList.fData[i] = nil) or (not Self.OwnsItems) then begin
        Self.fData[i] :=  SrcList.fData[i];
      end // .if
      else begin
        {!} Assert(Self.ItemsAreObjects);
        {!} Assert(TObject(SrcList.fData[i]) IS Utils.TCloneable);
        Self.fData[i] :=  Utils.TCloneable(SrcList.fData[i]).Clone;
      end; // .else
    end; // .for
  end; // .if
end; // .procedure TList.Assign

procedure TList.FreeItem (Ind: INTEGER);
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  if Self.OwnsItems then begin
    if Self.ItemsAreObjects then begin
      SysUtils.FreeAndNil(TObject(Self.fData[Ind]));
    end // .if
    else begin
      FreeMem(Self.fData[Ind]); Self.fData[Ind] :=  nil;
    end; // .else
  end; // .if
end; // .procedure TList.FreeItem

procedure TList.Clear;
var
  i:  INTEGER;
  
begin
  if Self.OwnsItems then begin
    for i:=0 to Self.Count - 1 do begin
      Self.FreeItem(i);
    end; // .for
  end; // .if
  FreeMem(Self.fData); Self.fData :=  nil;
  Self.fCapacity  :=  0;
  Self.fCount     :=  0;
end; // .procedure TList.Clear

function TList.IsValidItem ((* n *) Item: POINTER): BOOLEAN;
begin
  result  :=  Self.ItemGuardProc(Item, Self.ItemsAreObjects, Self.fItemGuard);
end; // .function TList.IsValidItem

procedure TList.Put (Ind: INTEGER; (* OUn *) Item: POINTER);
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  {!} Assert(Self.IsValidItem(Item));
  Self.FreeItem(Ind);
  Self.fData[Ind] :=  Item;
end; // .procedure TList.Put

function TList.Get (Ind: INTEGER): (* n *) POINTER;
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  result  :=  Self.fData[Ind];
end; // .function TList.Get

procedure TList.SetGrowthRate (NewGrowthRate: INTEGER);
begin
  {!} Assert(NewGrowthRate >= 100);
  Self.fGrowthRate  :=  NewGrowthRate;
end; // .procedure TList.SetGrowthRate

procedure TList.SetCapacity (NewCapacity: INTEGER);
var
  i:  INTEGER;
  
begin
  {!} Assert(NewCapacity >= 0);
  if NewCapacity < Self.Count then begin
    for i:=NewCapacity to Self.Count - 1 do begin
      Self.FreeItem(i);
    end; // .for
  end; // .if
  Self.fCapacity  :=  NewCapacity;
  ReallocMem(Self.fData, Self.Capacity * SIZEOF(POINTER));
end; // .procedure TList.SetCapacity

procedure TList.SetCount (NewCount: INTEGER);
var
  i:  INTEGER;
  
begin
  {!} Assert(NewCount >= 0);
  if NewCount < Self.Count then begin
    for i:=NewCount to Self.Count - 1 do begin
      Self.FreeItem(i);
    end; // .for
  end // .if
  else if NewCount > Self.Count then begin
    if NewCount > Self.Capacity then begin
      Self.SetCapacity(NewCount);
    end; // .if
    for i:=Self.Count to NewCount - 1 do begin
      Self.fData[i] :=  nil;
    end; // .for
  end; // .ELSEIF
  Self.fCount :=  NewCount;
end; // .procedure TList.SetCount

function TList.AddEmpty: INTEGER;
begin
  result  :=  Self.Count;
  if Self.Count = Self.Capacity then begin
    if Self.Capacity = 0 then begin
      Self.fCapacity  :=  Self.FIRST_ALLOC_COUNT;
    end // .if
    else begin
      Self.fCapacity  :=  Math.Max(Self.Capacity + 1, INT64(Self.Capacity) * Self.GrowthRate div 100);
    end; // .else
    ReallocMem(Self.fData, Self.Capacity * SIZEOF(POINTER));
  end; // .if
  Self.fData[Self.Count]  :=  nil;
  INC(Self.fCount);
end; // .function TList.AddEmpty

function TList.Add ((* OUn *) Item: POINTER): INTEGER;
begin
  {!} Assert(Self.IsValidItem(Item));
  result              :=  Self.AddEmpty;
  Self.fData[result]  :=  Item;
end; // .function TList.Add

function TList.Top: (* n *) POINTER;
begin
  {!} Assert(Self.Count > 0);
  result  :=  Self.fData[Self.Count - 1];
end; // .function TList.Top

function TList.Pop: (* OUn *) POINTER;
begin
  result  :=  Self.Top;
  DEC(Self.fCount);
end; // .function TList.Pop

procedure TList.Delete (Ind: INTEGER);
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  Self.FreeItem(Ind);
  DEC(Self.fCount);
  if Ind < Self.Count then begin
    Utils.CopyMem((Self.Count - Ind) * SIZEOF(POINTER), @Self.fData[Ind + 1], @Self.fData[Ind]);
  end; // .if
end; // .procedure TList.Delete

procedure TList.Insert ((* OUn *) Item: POINTER; Ind: INTEGER);
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count));
  if Ind = Self.Count then begin
    Self.Add(Item);
  end // .if
  else begin
    {!} Assert(Self.IsValidItem(Item));
    Self.AddEmpty;
    Utils.CopyMem((Self.Count - Ind - 1) * SIZEOF(POINTER), @Self.fData[Ind], @Self.fData[Ind + 1]);
    Self.fData[Ind] :=  Item;
  end; // .else
end; // .procedure TList.Insert

procedure TList.Exchange (SrcInd, DstInd: INTEGER);
begin
  {!} Assert(Math.InRange(SrcInd, 0, Self.Count - 1));
  {!} Assert(Math.InRange(DstInd, 0, Self.Count - 1));
  Utils.Exchange(INTEGER(Self.fData[SrcInd]), INTEGER(Self.fData[DstInd]));
end; // .procedure TList.Exchange

procedure TList.Move (SrcInd, DstInd: INTEGER);
var
(* Un *)  SrcItem:  POINTER;
          Dist:     INTEGER;
  
begin
  {!} Assert(Math.InRange(SrcInd, 0, Self.Count - 1));
  {!} Assert(Math.InRange(DstInd, 0, Self.Count - 1));
  if SrcInd <> DstInd then begin
    Dist  :=  ABS(SrcInd - DstInd);
    if Dist = 1 then begin
      Self.Exchange(SrcInd, DstInd);
    end // .if
    else begin
      SrcItem :=  Self.fData[SrcInd];
      if DstInd > SrcInd then begin
        Utils.CopyMem(Dist * SIZEOF(POINTER), @Self.fData[SrcInd + 1],  @Self.fData[SrcInd]);
      end // .if
      else begin
        Utils.CopyMem(Dist * SIZEOF(POINTER), @Self.fData[DstInd],      @Self.fData[DstInd + 1]);
      end; // .else
      Self.fData[DstInd]  :=  SrcItem;
    end; // .else
  end; // .if
end; // .procedure TList.Move

procedure TList.Shift (StartInd, Count, ShiftBy: INTEGER);
var
  EndInd: INTEGER;
  Step:   INTEGER;
  i:      INTEGER;
  
begin
  {!} Assert(Math.InRange(StartInd, 0, Self.Count - 1));
  {!} Assert(Count >= 0);
  Count :=  Math.EnsureRange(Count, 0, Self.Count - StartInd);
  if (ShiftBy <> 0) and (Count > 0) then begin
    if ShiftBy > 0 then begin
      StartInd  :=  StartInd + Count - 1;
    end; // .if
    EndInd  :=  StartInd + ShiftBy;
    Step    :=  -SIGN(ShiftBy);
    for i:=1 to Count do begin
      if Math.InRange(EndInd, 0, Self.Count - 1) then begin
        Self.FreeItem(EndInd);
        Utils.Exchange(INTEGER(Self.fData[StartInd]), INTEGER(Self.fData[EndInd]));
        StartInd  :=  StartInd + Step;
        EndInd    :=  EndInd + Step;
      end; // .if
    end; // .for
  end; // .if
end; // .procedure TList.Shift

function TList.Take (Ind: INTEGER): (* OUn *) POINTER;
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  {!} Assert(Self.IsValidItem(nil));
  result          :=  Self.fData[Ind];
  Self.fData[Ind] :=  nil;
end; // .function TList.Take

function TList.Replace (Ind: INTEGER; (* OUn *) NewValue: POINTER): (* OUn *) POINTER;
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  {!} Assert(Self.IsValidItem(NewValue));
  result          :=  Self.fData[Ind];
  Self.fData[Ind] :=  NewValue;
end; // .function TList.Replace

procedure TList.Pack;
var
  EndInd: INTEGER;
  i:      INTEGER;
  
begin
  i :=  0;
  while (i < Self.Count) and (Self.fData[i] <> nil) do begin
    INC(i);
  end; // .while
  if i < Count then begin
    EndInd    :=  i;
    for i:=i + 1 to Self.Count - 1 do begin
      if Self.fData[i] <> nil then begin
        Self.fData[EndInd]  :=  Self.fData[i];
        INC(EndInd);
      end; // .if
    end; // .for
    Self.fCount :=  EndInd;
  end; // .if
end; // .procedure TList.Pack

function TList.Find ((* n *) Item: POINTER; out Ind: INTEGER): BOOLEAN;
begin
  Ind :=  0;
  while (Ind < Self.Count) and (Self.fData[Ind] <> Item) do begin
    INC(Ind);
  end; // .while
  result  :=  Ind < Self.Count;
end; // .function TList.Find

// !FIXME if duplicates are allowed, then what???
function TList.QuickFind ((* n *) Item: POINTER; out Ind: INTEGER): BOOLEAN;
var
  LeftInd:    INTEGER;
  RightInd:   INTEGER;
  MiddleItem: INTEGER;

begin
  result    := FALSE;
  LeftInd   := 0;
  RightInd  := Self.Count - 1;
  
  while (not result) and (LeftInd <= RightInd) do begin
    Ind        := LeftInd + (RightInd - LeftInd) shr 1;
    MiddleItem := INTEGER(Self.fData[Ind]);
    
    if INTEGER(Item) < MiddleItem then begin
      RightInd := Ind - 1;
    end // .if
    else if INTEGER(Item) > MiddleItem then begin
      LeftInd := Ind + 1;
    end // .else
    else begin
      result := TRUE;
    end; // .else
  end; // .while
  
  if not result then begin
    Ind := LeftInd;
  end // .if
  else begin
    INC(Ind);
    
    while (Ind < Self.fCount) and (Self.fData[Ind] = Item) do begin
      INC(Ind);
    end; // .while
    
    DEC(Ind);
  end; // .else
end; // .function TList.QuickFind

procedure TList.Sort;
begin
  Alg.QuickSort(@Self.fData[0], 0, Self.Count - 1);
end; // .procedure TList.Sort

procedure TList.CustomSort (Compare: Alg.TCompareFunc);
begin
  Alg.CustomQuickSort(@Self.fData[0], 0, Self.Count - 1, Compare);
end; // .procedure TList.CustomSort

constructor TStringList.Create (OwnsItems: BOOLEAN; ItemsAreObjects: BOOLEAN; ItemGuardProc: Utils.TItemGuardProc; (* n *) var {IN} ItemGuard: Utils.TItemGuard);
begin
  {!} Assert(@ItemGuardProc <> nil);
  Self.fGrowthRate      :=  Self.DEFAULT_GROWTH_RATE;
  Self.fOwnsItems       :=  OwnsItems;
  Self.fItemsAreObjects :=  ItemsAreObjects;
  Self.fItemGuardProc   :=  ItemGuardProc;
  Self.fItemGuard       :=  ItemGuard;
  ItemGuard             :=  nil;
end; // .constructor TStringList.Create

destructor TStringList.Destroy;
begin
  Self.Clear;
  SysUtils.FreeAndNil(Self.fItemGuard);
end; // .destructor TStringList.Destroy

procedure TStringList.Assign (Source: Utils.TCloneable);
var
(* U *) SrcList:  TStringList;
        i:        INTEGER;
  
begin
  {!} Assert(Source <> nil);
  SrcList   :=  Source AS TStringList;
  // * * * * * //
  if Self <> Source then begin
    Self.Clear;
    Self.fKeys              :=  System.COPY(SrcList.fKeys);
    Self.fCapacity          :=  SrcList.Capacity;
    Self.fCount             :=  SrcList.Count;
    Self.fGrowthRate        :=  SrcList.GrowthRate;
    Self.fOwnsItems         :=  SrcList.OwnsItems;
    Self.fItemsAreObjects   :=  SrcList.ItemsAreObjects;
    Self.fItemGuardProc     :=  SrcList.ItemGuardProc;
    Self.fItemGuard         :=  SrcList.fItemGuard.Clone;
    Self.fCaseInsensitive   :=  SrcList.CaseInsensitive;
    Self.fForbidDuplicates  :=  SrcList.ForbidDuplicates;
    Self.fSorted            :=  SrcList.Sorted;
    GetMem(Self.fValues, Self.Count * SIZEOF(POINTER));
    for i:=0 to SrcList.Count - 1 do begin
      if (SrcList.fValues[i] = nil) or (not Self.OwnsItems) then begin
        Self.fValues[i] :=  SrcList.fValues[i];
      end // .if
      else begin
        {!} Assert(Self.ItemsAreObjects);
        {!} Assert(TObject(SrcList.fValues[i]) IS Utils.TCloneable);
        Self.fValues[i] :=  Utils.TCloneable(SrcList.fValues[i]).Clone;
      end; // .else
    end; // .for
  end; // .if
end; // .procedure TStringList.Assign

procedure TStringList.FreeValue (Ind: INTEGER);
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  if Self.OwnsItems then begin
    if Self.ItemsAreObjects then begin
      SysUtils.FreeAndNil(TObject(Self.fValues[Ind]));
    end // .if
    else begin
      FreeMem(Self.fValues[Ind]); Self.fValues[Ind] :=  nil;
    end; // .else
  end; // .if
end; // .procedure TStringList.FreeValue

procedure TStringList.Clear;
var
  i:  INTEGER;
  
begin
  if Self.OwnsItems then begin
    for i:=0 to Self.Count - 1 do begin
      Self.FreeValue(i);
    end; // .for
  end; // .if
  Self.fKeys  :=  nil;
  FreeMem(Self.fValues); Self.fValues :=  nil;
  Self.fCapacity  :=  0;
  Self.fCount     :=  0;
end; // .procedure TStringList.Clear

function TStringList.IsValidItem ((* n *) Item: POINTER): BOOLEAN;
begin
  result  :=  Self.ItemGuardProc(Item, Self.ItemsAreObjects, Self.fItemGuard);
end; // .function TStringList.IsValidItem

function TStringList.ValidateKey (const Key: string): BOOLEAN;
var
  KeyInd: INTEGER;

begin
  result  :=  not Self.ForbidDuplicates;
  if not result then begin
    result  :=  not Self.Find(Key, KeyInd);
  end; // .if
end; // .function TStringList.ValidateKey

procedure TStringList.PutKey (Ind: INTEGER; const Key: string);
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  {!} Assert(not Self.Sorted);
  if Self.ForbidDuplicates then begin
    {!} Assert((Self.CompareStrings(Self.fKeys[Ind], Key) = 0) or Self.ValidateKey(Key));
  end; // .if
  Self.fKeys[Ind] :=  Key;
end; // .procedure TStringList.PutKey

function TStringList.GetKey (Ind: INTEGER): string;
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  result  :=  Self.fKeys[Ind];
end; // .function TStringList.GetKey

procedure TStringList.PutValue (Ind: INTEGER; (* OUn *) Item: POINTER);
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  if Item <> Self.fValues[Ind] then begin
    {!} Assert(Self.IsValidItem(Item));
    Self.FreeValue(Ind);
    Self.fValues[Ind] :=  Item;
  end; // .if
end; // .procedure TStringList.PutValue

function TStringList.GetValue (Ind: INTEGER): (* n *) POINTER;
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  result  :=  Self.fValues[Ind];
end; // .function TStringList.GetValue

function TStringList.AddEmpty: INTEGER;
begin
  result  :=  Self.Count;
  if Self.Count = Self.Capacity then begin
    if Self.Capacity = 0 then begin
      Self.fCapacity  :=  Self.FIRST_ALLOC_COUNT;
    end // .if
    else begin
      Self.fCapacity  :=  Math.Max(Self.Capacity + 1, INT64(Self.Capacity) * Self.GrowthRate div 100);
    end; // .else
    ReallocMem(Self.fValues, Self.Capacity * SIZEOF(POINTER));
    SetLength(Self.fKeys, Self.Capacity);
  end; // .if
  Self.fKeys[Self.Count]    :=  '';
  Self.fValues[Self.Count]  :=  nil;
  INC(Self.fCount);
end; // .function TStringList.AddEmpty

procedure TStringList.SetGrowthRate (NewGrowthRate: INTEGER);
begin
  {!} Assert(NewGrowthRate >= 100);
  Self.fGrowthRate  :=  NewGrowthRate;
end; // .procedure TStringList.SetGrowthRate

procedure TStringList.SetCapacity (NewCapacity: INTEGER);
var
  i:  INTEGER;
  
begin
  {!} Assert(NewCapacity >= 0);
  if NewCapacity < Self.Count then begin
    for i:=NewCapacity to Self.Count - 1 do begin
      Self.FreeValue(i);
    end; // .for
  end; // .if
  Self.fCapacity  :=  NewCapacity;
  ReallocMem(Self.fValues, Self.Capacity * SIZEOF(POINTER));
  SetLength(Self.fKeys, Self.Capacity);
end; // .procedure TStringList.SetCapacity

procedure TStringList.SetCount (NewCount: INTEGER);
var
  i:  INTEGER;
  
begin
  {!} Assert(NewCount >= 0);
  if NewCount < Self.Count then begin
    for i:=NewCount to Self.Count - 1 do begin
      Self.FreeValue(i);
    end; // .for
  end // .if
  else if NewCount > Self.Count then begin
    if NewCount > Self.Capacity then begin
      Self.SetCapacity(NewCount);
    end; // .if
    for i:=Self.Count to NewCount - 1 do begin
      Self.fKeys[i]   :=  '';
      Self.fValues[i] :=  nil;
    end; // .for
  end; // .ELSEIF
  Self.fCount :=  NewCount;
end; // .procedure TStringList.SetCount

function TStringList.AddObj (const Key: string; (* OUn *) Value: POINTER): INTEGER;
var
  KeyInd:     INTEGER;
  KeyFound:   BOOLEAN;

begin
  {!} Assert(Self.IsValidItem(Value));
  if Self.ForbidDuplicates or Self.Sorted then begin
    KeyFound  :=  Self.Find(Key, KeyInd);
    if Self.ForbidDuplicates then begin
      {!} Assert(not KeyFound);
    end; // .if
  end; // .if
  result                :=  Self.AddEmpty;
  Self.fKeys[result]    :=  Key;
  Self.fValues[result]  :=  Value;
  if Self.Sorted then begin
    Self.fSorted  :=  FALSE;
    Self.Move(result, KeyInd);
    result        :=  KeyInd;
    Self.fSorted  :=  TRUE;
  end; // .if
end; // .function TStringList.AddObj

function TStringList.Add (const Key: string): INTEGER;
begin
  result  :=  Self.AddObj(Key, nil);
end; // .function TStringList.Add

function TStringList.Top: string;
begin
  {!} Assert(Self.Count > 0);
  result  :=  Self.fKeys[Self.Count - 1];
end; // .function TStringList.Top

function TStringList.Pop ((* OUn *) out Item: POINTER): string;
begin
  {!} Assert(Item = nil);
  {!} Assert(Self.Count > 0);
  result  :=  Self.fKeys[Self.Count - 1];
  Item    :=  Self.fValues[Self.Count - 1];
  DEC(Self.fCount);
end; // .function TStringList.Pop

procedure TStringList.Delete (Ind: INTEGER);
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  Self.FreeValue(Ind);
  Self.fKeys[Ind] :=  '';
  DEC(Self.fCount);
  if Ind < Self.Count then begin
    Utils.CopyMem((Self.Count - Ind) * SIZEOF(string),  @Self.fKeys[Ind + 1],   @Self.fKeys[Ind]);
    POINTER(Self.fKeys[Self.Count]) :=  nil;
    Utils.CopyMem((Self.Count - Ind) * SIZEOF(POINTER), @Self.fValues[Ind + 1], @Self.fValues[Ind]);
  end; // .if
end; // .procedure TStringList.Delete

procedure TStringList.InsertObj (const Key: string; Value: (* OUn *) POINTER; Ind: INTEGER);
var
  LastInd:  INTEGER;

begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count));
  if Ind = Self.Count then begin
    Self.AddObj(Key, Value);
  end // .if
  else begin
    {!} Assert(not Self.Sorted);
    {!} Assert(Self.IsValidItem(Value));
    LastInd :=  Self.AddEmpty;
    Utils.CopyMem((LastInd - Ind) * SIZEOF(string),   @Self.fKeys[Ind],   @Self.fKeys[Ind + 1]);
    Utils.CopyMem((LastInd - Ind) * SIZEOF(POINTER),  @Self.fValues[Ind], @Self.fValues[Ind + 1]);
    POINTER(Self.fKeys[Ind])  :=  nil;
    Self.fKeys[Ind]           :=  Key;
    Self.fValues[Ind]         :=  Value;
  end; // .else
end; // .procedure TStringList.InsertObj

procedure TStringList.Insert ({!} const Key: string; {!} Ind: INTEGER);
begin
  {!} Self.InsertObj(Key, nil, Ind);
end; // .procedure TStringList.Insert

procedure TStringList.Exchange (SrcInd, DstInd: INTEGER);
begin
  {!} Assert(Math.InRange(SrcInd, 0, Self.Count - 1));
  {!} Assert(Math.InRange(DstInd, 0, Self.Count - 1));
  if SrcInd <> DstInd then begin
    {!} Assert(not Self.Sorted);
    Utils.Exchange(INTEGER(Self.fKeys[SrcInd]),   INTEGER(Self.fKeys[DstInd]));
    Utils.Exchange(INTEGER(Self.fValues[SrcInd]), INTEGER(Self.fValues[DstInd]));
  end; // .if
end; // .procedure TStringList.Exchange

procedure TStringList.Move (SrcInd, DstInd: INTEGER);
var
(* Un *)  SrcValue: POINTER;
          SrcKey:   POINTER;
          Dist:     INTEGER;
  
begin
  {!} Assert(Math.InRange(SrcInd, 0, Self.Count - 1));
  {!} Assert(Math.InRange(DstInd, 0, Self.Count - 1));
  SrcValue  :=  nil;
  SrcKey    :=  nil;
  // * * * * * //
  if SrcInd <> DstInd then begin
    {!} Assert(not Self.Sorted);
    Dist  :=  SrcInd - DstInd;
    if ABS(Dist) = 1 then begin
      Self.Exchange(SrcInd, DstInd);
    end // .if
    else begin
      SrcKey    :=  POINTER(Self.fKeys[SrcInd]);
      SrcValue  :=  Self.fValues[SrcInd];
      Utils.CopyMem(ABS(Dist) * SIZEOF(string),   @Self.fKeys[DstInd],    @Self.fKeys[DstInd + Math.Sign(Dist)]);
      Utils.CopyMem(ABS(Dist) * SIZEOF(POINTER),  @Self.fValues[DstInd],  @Self.fValues[DstInd + Math.Sign(Dist)]);
      POINTER(Self.fKeys[DstInd]) :=  SrcKey;
      Self.fValues[DstInd]        :=  SrcValue;
    end; // .else
  end; // .if
end; // .procedure TStringList.Move

procedure TStringList.Shift (StartInd, Count, ShiftBy: INTEGER);
var
  EndInd: INTEGER;
  Step:   INTEGER;
  i:      INTEGER;
  
begin
  {!} Assert(Math.InRange(StartInd, 0, Self.Count - 1));
  {!} Assert(Count >= 0);
  Count :=  Math.EnsureRange(Count, 0, Self.Count - StartInd);
  if (ShiftBy <> 0) and (Count > 0) then begin
    if ShiftBy > 0 then begin
      StartInd  :=  StartInd + Count - 1;
    end; // .if
    EndInd  :=  StartInd + ShiftBy;
    Step    :=  -SIGN(ShiftBy);
    for i:=1 to Count do begin
      if Math.InRange(EndInd, 0, Self.Count - 1) then begin
        Self.FreeValue(EndInd);
        Self.fKeys[EndInd]  :=  '';
        Utils.Exchange(INTEGER(Self.fKeys[StartInd]),   INTEGER(Self.fKeys[EndInd]));
        Utils.Exchange(INTEGER(Self.fValues[StartInd]), INTEGER(Self.fValues[EndInd]));
        StartInd  :=  StartInd + Step;
        EndInd    :=  EndInd + Step;
      end; // .if
    end; // .for
  end; // .if
end; // .procedure TStringList.Shift

function TStringList.TakeValue (Ind: INTEGER): (* OUn *) POINTER;
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  {!} Assert(Self.IsValidItem(nil));
  result            :=  Self.fValues[Ind];
  Self.fValues[Ind] :=  nil;
end; // .function TStringList.TakeValue

function TStringList.ReplaceValue (Ind: INTEGER; (* OUn *) NewValue: POINTER): (* OUn *) POINTER;
begin
  {!} Assert(Math.InRange(Ind, 0, Self.Count - 1));
  {!} Assert(Self.IsValidItem(NewValue));
  result            :=  Self.fValues[Ind];
  Self.fValues[Ind] :=  NewValue;
end; // .function TStringList.ReplaceValue

procedure TStringList.Pack;
var
  EndInd: INTEGER;
  i:      INTEGER;
  
begin
  if Self.Sorted then begin
    i :=  0;
    while (i < Self.Count) and (Self.fKeys[i] = '') do begin
      INC(i);
    end; // .while
    if i > 0 then begin
      Utils.CopyMem(i * SIZEOF(string), @Self.fKeys[i], @Self.fKeys[0]);
      System.FillChar(Self.fKeys[Count - i], i * SIZEOF(string), 0);
      Utils.CopyMem(i * SIZEOF(POINTER), @Self.fValues[i], @Self.fValues[0]);
      Self.fCount :=  Self.fCount - i;
    end; // .if
  end // .if
  else begin
    i :=  0;
    while (i < Self.Count) and (Self.fKeys[i] <> '') do begin
      INC(i);
    end; // .while
    if i < Count then begin
      EndInd    :=  i;
      Self.FreeValue(i);
      for i:=i + 1 to Self.Count - 1 do begin
        if Self.fKeys[i] <> '' then begin
          Utils.Exchange(INTEGER(Self.fKeys[EndInd]), INTEGER(Self.fKeys[i]));
          Self.fValues[EndInd]  :=  Self.fValues[i];
          INC(EndInd);
        end // .if
        else begin
          Self.FreeValue(i);
        end; // .else
      end; // .for
      Self.fCount :=  EndInd;
    end; // .if
  end; // .else
end; // .procedure TStringList.Pack

function TStringList.CompareStrings (const Str1, Str2: string): INTEGER;
begin
  if Self.fCaseInsensitive then begin
    result  :=  SysUtils.AnsiCompareText(Str1, Str2);
  end // .if
  else begin
    result  :=  SysUtils.AnsiCompareStr(Str1, Str2);
  end; // .else
end; // .function TStringList.CompareStrings

function TStringList.QuickFind (const Key: string; (* i *) out Ind: INTEGER): BOOLEAN;
var
  LeftInd:    INTEGER;
  RightInd:   INTEGER;
  CmpRes:     INTEGER;

begin
  result    :=  FALSE;
  LeftInd   :=  0;
  RightInd  :=  Self.Count - 1;
  while (not result) and (LeftInd <= RightInd) do begin
    Ind     :=  LeftInd + (RightInd - LeftInd + 1) shr 1;
    CmpRes  :=  Self.CompareStrings(Key, Self.fKeys[Ind]);
    if CmpRes < 0 then begin
      RightInd  :=  Ind - 1;
    end // .if
    else if CmpRes > 0 then begin
      LeftInd :=  Ind + 1;
    end // .else
    else begin
      result  :=  TRUE;
    end; // .else
  end; // .while
  
  if not result then begin
    Ind :=  LeftInd;
  end // .if
  else if not Self.fForbidDuplicates then begin
    INC(Ind);
    
    while (Ind < Self.fCount) and (Self.CompareStrings(Key, Self.fKeys[Ind]) = 0) do begin
      INC(Ind);
    end; // .while
    
    DEC(Ind);
  end; // .ELSEIF
end; // .function TStringList.QuickFind

function TStringList.Find (const Key: string; (* i *) out Ind: INTEGER): BOOLEAN;
begin
  if Self.Sorted then begin
    result  :=  Self.QuickFind(Key, Ind);
  end // .if
  else begin
    Ind :=  0;
    while (Ind < Self.Count) and (Self.CompareStrings(Self.fKeys[Ind], Key) <> 0) do begin
      INC(Ind);
    end; // .while
    result  :=  Ind < Self.Count;
    if not result then begin
      DEC(Ind);
    end; // .if
  end; // .else
end; // .function TStringList.Find

procedure TStringList.QuickSort (MinInd, MaxInd: INTEGER);
var
  LeftInd:    INTEGER;
  RightInd:   INTEGER;
  PivotItem:  string;
  
begin
  {!} Assert(Self.fKeys <> nil);
  {!} Assert(MinInd >= 0);
  {!} Assert(MaxInd >= MinInd);
  
  while MinInd < MaxInd do begin
    LeftInd   :=  MinInd;
    RightInd  :=  MaxInd;
    PivotItem :=  Self.fKeys[MinInd + (MaxInd - MinInd) div 2];
    
    while LeftInd <= RightInd do begin
      while CompareStrings(Self.fKeys[LeftInd], PivotItem) < 0 do begin
        INC(LeftInd);
      end; // .while
      
      while CompareStrings(Self.fKeys[RightInd], PivotItem) > 0 do begin
        DEC(RightInd);
      end; // .while
      
      if LeftInd <= RightInd then begin
        if CompareStrings(Self.fKeys[LeftInd], Self.fKeys[RightInd]) > 0 then begin
          Utils.Exchange(INTEGER(Self.fKeys[LeftInd]),    INTEGER(Self.fKeys[RightInd]));
          Utils.Exchange(INTEGER(Self.fValues[LeftInd]),  INTEGER(Self.fValues[RightInd]));
        end; // .if
        
        INC(LeftInd);
        DEC(RightInd);
      end; // .if
    end; // .while
    
    (* MIN__RIGHT|{PIVOT}|LEFT__MAX *)
    
    if (RightInd - MinInd) < (MaxInd - LeftInd) then begin
      if RightInd > MinInd then begin
        Self.QuickSort(MinInd, RightInd);
      end; // .if
      
      MinInd :=  LeftInd;
    end // .if
    else begin
      if MaxInd > LeftInd then begin
        Self.QuickSort(LeftInd, MaxInd);
      end; // .if
      
      MaxInd  :=  RightInd;
    end; // .else
  end; // .while
end; // .procedure TStringList.QuickSort

procedure TStringList.Sort;
begin
  if not Self.Sorted then begin
    Self.fSorted  :=  TRUE;
    
    if Self.fCount > 1 then begin
      Self.QuickSort(0, Self.Count - 1);
    end; // .if
  end; // .if
end; // .procedure TStringList.Sort

procedure TStringList.SetSorted (IsSorted: BOOLEAN);
begin
  if IsSorted then begin
    Self.Sort;
  end // .if
  else begin
    Self.fSorted  :=  FALSE;
  end; // .else
end; // .procedure TStringList.SetSorted

procedure TStringList.EnsureNoDuplicates;
var
  Etalon: string;
  i:      INTEGER;
  y:      INTEGER;
  
begin
  if Self.Sorted then begin
    for i:=1 to Self.Count - 1 do begin
      {!} Assert(Self.CompareStrings(Self.fKeys[i], Self.fKeys[i - 1]) <> 0);
    end; // .for
  end // .if
  else begin
    for i:=0 to Self.Count - 1 do begin
      Etalon  :=  Self.fKeys[i];
      for y:=i+1 to Self.Count - 1 do begin
        {!} Assert(Self.CompareStrings(Etalon, Self.fKeys[y]) <> 0);
      end; // .for
    end; // .for
  end; // .else
end; // .procedure TStringList.EnsureNoDuplicates

procedure TStringList.SetCaseInsensitive (NewCaseInsensitive: BOOLEAN);
begin
  if (not Self.CaseInsensitive) and NewCaseInsensitive then begin
    Self.fCaseInsensitive :=  NewCaseInsensitive;
    Self.EnsureNoDuplicates;
  end; // .if
  Self.fCaseInsensitive :=  NewCaseInsensitive;
end; // .procedure TStringList.SetCaseInsensitive

procedure TStringList.SetForbidDuplicates (NewForbidDuplicates: BOOLEAN);
begin
  if NewForbidDuplicates <> Self.ForbidDuplicates then begin
    if NewForbidDuplicates then begin
      Self.EnsureNoDuplicates;
    end; // .if
    Self.fForbidDuplicates  :=  NewForbidDuplicates;
  end; // .if
end; // .procedure TStringList.SetForbidDuplicates

procedure TStringList.LoadFromText (const Text, EndOfLineMarker: string);
begin
  Self.Clear;
  Self.fKeys      :=  StrLib.Explode(Text, EndOfLineMarker);
  Self.fCapacity  :=  LENGTH(Self.fKeys);
  Self.fCount     :=  Self.Capacity;
  GetMem(Self.fValues, Self.Count * SIZEOF(POINTER));
  System.FillChar(Self.fValues[0], Self.Count * SIZEOF(POINTER), 0);
  if Self.Sorted then begin
    Self.fSorted  :=  FALSE;
    Self.Sort;
  end; // .if
  if Self.ForbidDuplicates then begin
    Self.EnsureNoDuplicates;
  end; // .if
end; // .procedure TStringList.LoadFromText

function TStringList.ToText (const EndOfLineMarker: string): string;
var
  ClonedKeys: Utils.TArrayOfString;

begin
  if Self.Count = Self.Capacity then begin
    result  :=  StrLib.Join(Self.fKeys, EndOfLineMarker);
  end // .if
  else begin
    ClonedKeys  :=  Self.fKeys;
    SetLength(ClonedKeys, Self.Count);
    result  :=  StrLib.Join(ClonedKeys, EndOfLineMarker);;
  end; // .else
end; // .function TStringList.ToText

function TStringList.GetItem (const Key: string): (* n *) POINTER;
var
  Ind:  INTEGER;

begin
  if Self.Find(Key, Ind) then begin
    result  :=  Self.fValues[Ind];
  end // .if
  else begin
    result  :=  nil;
  end; // .else
end; // .function TStringList.GetItem

procedure TStringList.PutItem (const Key: string; (* OUn *) Value: POINTER);
var
  Ind:  INTEGER;

begin
  if Self.Find(Key, Ind) then begin
    Self.PutValue(Ind, Value);
  end // .if
  else begin
    Self.AddObj(Key, Value);
  end; // .else
end; // .procedure TStringList.PutItem

function NewList (OwnsItems: BOOLEAN; ItemsAreObjects: BOOLEAN; ItemType: TClass; AllowNIL: BOOLEAN): TList;
var
(* O *) ItemGuard:  Utils.TDefItemGuard;

begin
  {!} Assert(ItemsAreObjects or (ItemType = Utils.NO_TYPEGUARD));
  ItemGuard :=  Utils.TDefItemGuard.Create;
  // * * * * * //
  ItemGuard.ItemType  :=  ItemType;
  ItemGuard.AllowNIL  :=  AllowNIL;
  result              :=  TList.Create(OwnsItems, ItemsAreObjects, @Utils.DefItemGuardProc, Utils.TItemGuard(ItemGuard));
end; // .function NewList

function NewStrictList ({n} TypeGuard: TClass): TList;
begin
  result  :=  NewList(Utils.OWNS_ITEMS, Utils.ITEMS_ARE_OBJECTS, TypeGuard, Utils.ALLOW_NIL);
end; // .function NewStrictList

function NewSimpleList: TList;
var
(* n *) ItemGuard:  Utils.TCloneable; 

begin
  ItemGuard :=  nil;
  // * * * * * //
  result  :=  TList.Create(not Utils.OWNS_ITEMS, not Utils.ITEMS_ARE_OBJECTS, @Utils.NoItemGuardProc, ItemGuard);
end; // .function NewSimpleList

function NewStrList (OwnsItems: BOOLEAN; ItemsAreObjects: BOOLEAN; ItemType: TClass; AllowNIL: BOOLEAN): TStringList;
var
(* O *) ItemGuard:  Utils.TDefItemGuard;

begin
  {!} Assert(ItemsAreObjects or (ItemType = Utils.NO_TYPEGUARD));
  ItemGuard :=  Utils.TDefItemGuard.Create;
  // * * * * * //
  ItemGuard.ItemType  :=  ItemType;
  ItemGuard.AllowNIL  :=  AllowNIL;
  result              :=  TStringList.Create(OwnsItems, ItemsAreObjects, @Utils.DefItemGuardProc, Utils.TItemGuard(ItemGuard));
end; // .function NewStrList

function NewStrictStrList ({n} TypeGuard: TClass): TStringList;
begin
  result  :=  NewStrList(Utils.OWNS_ITEMS, Utils.ITEMS_ARE_OBJECTS, TypeGuard, Utils.ALLOW_NIL);
end; // .function NewStrictStrList

function NewSimpleStrList: TStringList;
var
(* n *) ItemGuard:  Utils.TCloneable; 

begin
  ItemGuard :=  nil;
  // * * * * * //
  result  :=  TStringList.Create(not Utils.OWNS_ITEMS, not Utils.ITEMS_ARE_OBJECTS, @Utils.NoItemGuardProc, ItemGuard);
end; // .function NewSimpleStrList

end.
