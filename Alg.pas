unit Alg;
{
DESCRIPTION:  Additional math/algorithmic functions
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Utils, Math;

type
  TCompareFunc = function (Value1, Value2: INTEGER): INTEGER;
  
  TQuickSortAdapter = class abstract
    function  CompareItems (Ind1, Ind2: INTEGER): INTEGER; virtual; abstract;
    procedure SwapItems (Ind1, Ind2: INTEGER); virtual; abstract;
    procedure SavePivotItem (PivotItemInd: INTEGER); virtual; abstract;
    function  CompareToPivot (Ind: INTEGER): INTEGER; virtual; abstract;
  end; // .class TQuickSortAdapter


function  IntLog2 (Num: INTEGER): INTEGER; {=> Ceil(Log2(N)), N > 0}
function  IntCompare (Int1, Int2: INTEGER): INTEGER;
function  CardCompare (Card1, Card2: CARDINAL): INTEGER;
function  PtrCompare (Ptr1, Ptr2: POINTER): INTEGER;
function  Int64To32 (Value: INT64): INTEGER; {No overflow, bounds to LOW(INT32)..HIGH(IN32)}
procedure QuickSort (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: INTEGER);
procedure CustomQuickSort
(
  Arr:          Utils.PEndlessIntArr;
  MinInd:       INTEGER;
  MaxInd:       INTEGER;
  CompareItems: TCompareFunc
);
procedure QuickSortEx (Obj: TQuickSortAdapter; MinInd, MaxInd: INTEGER);


(***)  implementation  (***)


function IntLog2 (Num: INTEGER): INTEGER;
var
  TestValue:  CARDINAL;
  
begin
  {!} Assert(Num > 0);
  result    :=  0;
  TestValue :=  1;
  
  while TestValue < CARDINAL(Num) do begin
    INC(result);
    TestValue :=  TestValue shl 1;
  end; // .while
end; // .function IntLog2

function IntCompare (Int1, Int2: INTEGER): INTEGER;
begin
  if Int1 > Int2 then begin
    result  :=  +1;
  end // .if
  else if Int1 < Int2 then begin
    result  :=  -1;
  end // .ELSEIF
  else begin
    result  :=  0;
  end; // .else
end; // .function IntCompare

function CardCompare (Card1, Card2: CARDINAL): INTEGER;
begin
  if Card1 > Card2 then begin
    result := +1;
  end // .if
  else if Card1 < Card2 then begin
    result := -1;
  end // .ELSEIF
  else begin
    result := 0;
  end; // .else
end; // .function CardCompare

function PtrCompare (Ptr1, Ptr2: POINTER): INTEGER;
begin
  if CARDINAL(Ptr1) > CARDINAL(Ptr2) then begin
    result := +1;
  end // .if
  else if CARDINAL(Ptr1) < CARDINAL(Ptr2) then begin
    result := -1;
  end // .ELSEIF
  else begin
    result := 0;
  end; // .else
end; // .function PtrCompare

function Int64To32 (Value: INT64): INTEGER;
begin
  if Value > HIGH(INTEGER) then begin
    result  :=  HIGH(INTEGER);
  end // .if
  else if Value < LOW(INTEGER) then begin
    result  :=  LOW(INTEGER);
  end // .ELSEIF
  else begin
    result  :=  Value;
  end; // .else
end; // .function Int64To32

procedure QuickSort (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: INTEGER);
begin
  CustomQuickSort(Arr, MinInd, MaxInd, IntCompare);
end; // .procedure QuickSort

procedure CustomQuickSort
(
  Arr:          Utils.PEndlessIntArr;
  MinInd:       INTEGER;
  MaxInd:       INTEGER;
  CompareItems: TCompareFunc
);

var
  RangeLen:   INTEGER;
  LeftInd:    INTEGER;
  RightInd:   INTEGER;
  PivotItem:  INTEGER;
  
  procedure ExchangeItems (Ind1, Ind2: INTEGER);
  var
    TransfValue:  INTEGER;
     
  begin
    TransfValue :=  Arr[Ind1];
    Arr[Ind1]   :=  Arr[Ind2];
    Arr[Ind2]   :=  TransfValue;
  end; // .procedure ExchangeItems
  
begin
  RangeLen := MaxInd - MinInd + 1;
  {!} Assert(RangeLen >= 0);
  {!} Assert(Utils.IsValidBuf(Arr, RangeLen));
  {!} Assert(MinInd >= 0);
  
  while MinInd < MaxInd do begin
    LeftInd   :=  MinInd;
    RightInd  :=  MaxInd;
    PivotItem :=  Arr[MinInd + (MaxInd - MinInd) div 2];
    
    while LeftInd <= RightInd do begin
      while CompareItems(Arr[LeftInd], PivotItem) < 0 do begin
        INC(LeftInd);
      end; // .while
      
      while CompareItems(Arr[RightInd], PivotItem) > 0 do begin
        DEC(RightInd);
      end; // .while
      
      if LeftInd <= RightInd then begin
        if CompareItems(Arr[LeftInd], Arr[RightInd]) > 0 then begin
          ExchangeItems(LeftInd, RightInd);
        end // .if
        else begin
          INC(LeftInd);
          DEC(RightInd);
        end; // .else
      end; // .if
    end; // .while
    
    (* MIN__RIGHT|{PIVOT}|LEFT__MAX *)
    
    if (RightInd - MinInd) < (MaxInd - LeftInd) then begin
      if RightInd > MinInd then begin
        CustomQuickSort(Arr, MinInd, RightInd, CompareItems);
      end; // .if
      
      MinInd :=  LeftInd;
    end // .if
    else begin
      if MaxInd > LeftInd then begin
        CustomQuickSort(Arr, LeftInd, MaxInd, CompareItems);
      end; // .if
      
      MaxInd  :=  RightInd;
    end; // .else
  end; // .while
end; // .procedure CustomQuickSort

procedure QuickSortEx (Obj: TQuickSortAdapter; MinInd, MaxInd: INTEGER);
var
  RangeLen: INTEGER;
  LeftInd:  INTEGER;
  RightInd: INTEGER;
  
begin
  RangeLen := MaxInd - MinInd + 1;
  {!} Assert(RangeLen >= 0);
  {!} Assert(Utils.IsValidBuf(POINTER(Obj), RangeLen));
  {!} Assert(MinInd >= 0);
  
  while MinInd < MaxInd do begin
    LeftInd  := MinInd;
    RightInd := MaxInd;
    Obj.SavePivotItem(MinInd + (MaxInd - MinInd) div 2);
    
    while LeftInd <= RightInd do begin
      while Obj.CompareToPivot(LeftInd) < 0 do begin
        INC(LeftInd);
      end; // .while
      
      while Obj.CompareToPivot(RightInd) > 0 do begin
        DEC(RightInd);
      end; // .while
      
      if LeftInd <= RightInd then begin
        if Obj.CompareItems(LeftInd, RightInd) > 0 then begin
          Obj.SwapItems(LeftInd, RightInd);
        end // .if
        else begin
          INC(LeftInd);
          DEC(RightInd);
        end; // .else
      end; // .if
    end; // .while
    
    (* MIN__RIGHT|{PIVOT}|LEFT__MAX *)
    
    if (RightInd - MinInd) < (MaxInd - LeftInd) then begin
      if RightInd > MinInd then begin
        QuickSortEx(Obj, MinInd, RightInd);
      end; // .if
      
      MinInd :=  LeftInd;
    end // .if
    else begin
      if MaxInd > LeftInd then begin
        QuickSortEx(Obj, LeftInd, MaxInd);
      end; // .if
      
      MaxInd  :=  RightInd;
    end; // .else
  end; // .while
end; // .procedure QuickSortEx

end.
