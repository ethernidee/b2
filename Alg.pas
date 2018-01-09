unit Alg;
{
DESCRIPTION:  Additional math/algorithmic functions
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Utils, Math;

type
  TCompareFunc   = function (Value1, Value2: integer): integer;
  TCompareMethod = function (Value1, Value2: integer): integer of object;
  
  TQuickSortAdapter = class abstract
    function  CompareItems (Ind1, Ind2: integer): integer; virtual; abstract;
    procedure SwapItems (Ind1, Ind2: integer); virtual; abstract;
    procedure SavePivotItem (PivotItemInd: integer); virtual; abstract;
    function  CompareToPivot (Ind: integer): integer; virtual; abstract;
  end; // .class TQuickSortAdapter


(* Rounds integer to given boundary. If ceil is true, rounding is performed to higher value, otherwise to lower one.
   Example: f(21, 4, true) = 24 *)
function IntRoundToBoundary (Value, Boundary: integer; Ceil: boolean = true): integer;

function  IntLog2 (Num: integer): integer; {=> Ceil(Log2(N)), N > 0}
function  IntCompare (Int1, Int2: integer): integer;
function  CardCompare (Card1, Card2: cardinal): integer;
function  PtrCompare (Ptr1, Ptr2: pointer): integer;
function  Int64To32 (Value: INT64): integer; {No overflow, bounds to LOW(INT32)..HIGH(IN32)}
procedure QuickSort (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer);
procedure CustomQuickSort (Arr: Utils.PEndlessIntArr; MinInd: integer; MaxInd: integer;
                           CompareItems: TCompareFunc); overload;
procedure CustomQuickSort (Arr: Utils.PEndlessIntArr; MinInd: integer; MaxInd: integer;
                           CompareItems: TCompareMethod); overload;
procedure QuickSortEx (Obj: TQuickSortAdapter; MinInd, MaxInd: integer);
function  CustomBinarySearch (Arr: PEndlessIntArr; MinInd, MaxInd: integer; Needle: integer;
                              CompareItems: TCompareFunc; out ResInd: integer): boolean; overload;
function  CustomBinarySearch (Arr: PEndlessIntArr; MinInd, MaxInd: integer; Needle: integer;
                              CompareItems: TCompareMethod; out ResInd: integer): boolean; overload;


(***)  implementation  (***)


function IntRoundToBoundary (Value, Boundary: integer; Ceil: boolean = true): integer;
begin
  {!} Assert(Boundary >= 1);
  if Ceil then begin
    result := (Value + Boundary - 1) div Boundary * Boundary;
  end else begin
    result := Value div Boundary * Boundary;
  end;
end;

function IntLog2 (Num: integer): integer;
var
  TestValue:  cardinal;
  
begin
  {!} Assert(Num > 0);
  result    :=  0;
  TestValue :=  1;
  
  while TestValue < cardinal(Num) do begin
    Inc(result);
    TestValue :=  TestValue shl 1;
  end; // .while
end; // .function IntLog2

function IntCompare (Int1, Int2: integer): integer;
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

function CardCompare (Card1, Card2: cardinal): integer;
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

function PtrCompare (Ptr1, Ptr2: pointer): integer;
begin
  if cardinal(Ptr1) > cardinal(Ptr2) then begin
    result := +1;
  end // .if
  else if cardinal(Ptr1) < cardinal(Ptr2) then begin
    result := -1;
  end // .ELSEIF
  else begin
    result := 0;
  end; // .else
end; // .function PtrCompare

function Int64To32 (Value: INT64): integer;
begin
  if Value > High(integer) then begin
    result  :=  High(integer);
  end // .if
  else if Value < Low(integer) then begin
    result  :=  Low(integer);
  end // .ELSEIF
  else begin
    result  :=  Value;
  end; // .else
end; // .function Int64To32

procedure QuickSort (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer);
begin
  CustomQuickSort(Arr, MinInd, MaxInd, IntCompare);
end; // .procedure QuickSort

procedure CustomQuickSort (Arr: Utils.PEndlessIntArr; MinInd: integer; MaxInd: integer;
                           CompareItems: TCompareFunc); overload;
var
  RangeLen:  integer;
  LeftInd:   integer;
  RightInd:  integer;
  PivotItem: integer;
  
  procedure ExchangeItems (Ind1, Ind2: integer);
  var
    TransfValue: integer;
     
  begin
    TransfValue := Arr[Ind1];
    Arr[Ind1]   := Arr[Ind2];
    Arr[Ind2]   := TransfValue;
  end; // .procedure ExchangeItems
  
begin
  RangeLen := MaxInd - MinInd + 1;
  {!} Assert(RangeLen >= 0);
  {!} Assert(Utils.IsValidBuf(Arr, RangeLen));
  {!} Assert(MinInd >= 0);
  
  while MinInd < MaxInd do begin
    LeftInd   := MinInd;
    RightInd  := MaxInd;
    PivotItem := Arr[MinInd + (MaxInd - MinInd) div 2];
    
    while LeftInd <= RightInd do begin
      while CompareItems(Arr[LeftInd], PivotItem) < 0 do begin
        Inc(LeftInd);
      end; // .while
      
      while CompareItems(Arr[RightInd], PivotItem) > 0 do begin
        Dec(RightInd);
      end; // .while
      
      if LeftInd <= RightInd then begin
        if CompareItems(Arr[LeftInd], Arr[RightInd]) > 0 then begin
          ExchangeItems(LeftInd, RightInd);
        end // .if
        else begin
          Inc(LeftInd);
          Dec(RightInd);
        end; // .else
      end; // .if
    end; // .while
    
    (* MIN__RIGHT|{PIVOT}|LEFT__MAX *)
    
    if (RightInd - MinInd) < (MaxInd - LeftInd) then begin
      if RightInd > MinInd then begin
        CustomQuickSort(Arr, MinInd, RightInd, CompareItems);
      end; // .if
      
      MinInd := LeftInd;
    end // .if
    else begin
      if MaxInd > LeftInd then begin
        CustomQuickSort(Arr, LeftInd, MaxInd, CompareItems);
      end; // .if
      
      MaxInd := RightInd;
    end; // .else
  end; // .while
end; // .procedure CustomQuickSort

procedure CustomQuickSort (Arr: Utils.PEndlessIntArr; MinInd: integer; MaxInd: integer;
                           CompareItems: TCompareMethod); overload;
var
  RangeLen:  integer;
  LeftInd:   integer;
  RightInd:  integer;
  PivotItem: integer;
  
  procedure ExchangeItems (Ind1, Ind2: integer);
  var
    TransfValue: integer;
     
  begin
    TransfValue := Arr[Ind1];
    Arr[Ind1]   := Arr[Ind2];
    Arr[Ind2]   := TransfValue;
  end; // .procedure ExchangeItems
  
begin
  RangeLen := MaxInd - MinInd + 1;
  {!} Assert(RangeLen >= 0);
  {!} Assert(Utils.IsValidBuf(Arr, RangeLen));
  {!} Assert(MinInd >= 0);
  
  while MinInd < MaxInd do begin
    LeftInd   := MinInd;
    RightInd  := MaxInd;
    PivotItem := Arr[MinInd + (MaxInd - MinInd) div 2];
    
    while LeftInd <= RightInd do begin
      while CompareItems(Arr[LeftInd], PivotItem) < 0 do begin
        Inc(LeftInd);
      end; // .while
      
      while CompareItems(Arr[RightInd], PivotItem) > 0 do begin
        Dec(RightInd);
      end; // .while
      
      if LeftInd <= RightInd then begin
        if CompareItems(Arr[LeftInd], Arr[RightInd]) > 0 then begin
          ExchangeItems(LeftInd, RightInd);
        end // .if
        else begin
          Inc(LeftInd);
          Dec(RightInd);
        end; // .else
      end; // .if
    end; // .while
    
    (* MIN__RIGHT|{PIVOT}|LEFT__MAX *)
    
    if (RightInd - MinInd) < (MaxInd - LeftInd) then begin
      if RightInd > MinInd then begin
        CustomQuickSort(Arr, MinInd, RightInd, CompareItems);
      end; // .if
      
      MinInd := LeftInd;
    end // .if
    else begin
      if MaxInd > LeftInd then begin
        CustomQuickSort(Arr, LeftInd, MaxInd, CompareItems);
      end; // .if
      
      MaxInd := RightInd;
    end; // .else
  end; // .while
end; // .procedure CustomQuickSort

procedure QuickSortEx (Obj: TQuickSortAdapter; MinInd, MaxInd: integer);
var
  RangeLen: integer;
  LeftInd:  integer;
  RightInd: integer;
  
begin
  RangeLen := MaxInd - MinInd + 1;
  {!} Assert(RangeLen >= 0);
  {!} Assert(Utils.IsValidBuf(pointer(Obj), RangeLen));
  {!} Assert(MinInd >= 0);
  
  while MinInd < MaxInd do begin
    LeftInd  := MinInd;
    RightInd := MaxInd;
    Obj.SavePivotItem(MinInd + (MaxInd - MinInd) div 2);
    
    while LeftInd <= RightInd do begin
      while Obj.CompareToPivot(LeftInd) < 0 do begin
        Inc(LeftInd);
      end; // .while
      
      while Obj.CompareToPivot(RightInd) > 0 do begin
        Dec(RightInd);
      end; // .while
      
      if LeftInd <= RightInd then begin
        if Obj.CompareItems(LeftInd, RightInd) > 0 then begin
          Obj.SwapItems(LeftInd, RightInd);
        end // .if
        else begin
          Inc(LeftInd);
          Dec(RightInd);
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

function CustomBinarySearch (Arr: PEndlessIntArr; MinInd, MaxInd: integer; Needle: integer;
                             CompareItems: TCompareFunc; out ResInd: integer): boolean; overload;
var
  PivotInd:   integer;
  CompareRes: integer;

begin
  result := false;

  while not result and (MinInd <= MaxInd) do begin
    PivotInd   := MinInd + (MaxInd - MinInd) div 2;
    CompareRes := CompareItems(PivotInd, Needle);

    if CompareRes < 0 then begin
      MaxInd := PivotInd - 1;
    end else if CompareRes > 0 then begin
      MinInd := PivotInd + 1;
    end else begin
      ResInd := PivotInd;
      result := true;
    end; // .else
  end; // .while
end; // .function CustomBinarySearch

function CustomBinarySearch (Arr: PEndlessIntArr; MinInd, MaxInd: integer; Needle: integer;
                             CompareItems: TCompareMethod; out ResInd: integer): boolean; overload;
var
  PivotInd:   integer;
  CompareRes: integer;

begin
  result := false;

  while not result and (MinInd <= MaxInd) do begin
    PivotInd   := MinInd + (MaxInd - MinInd) div 2;
    CompareRes := CompareItems(PivotInd, Needle);

    if CompareRes < 0 then begin
      MaxInd := PivotInd - 1;
    end else if CompareRes > 0 then begin
      MinInd := PivotInd + 1;
    end else begin
      ResInd := PivotInd;
      result := true;
    end; // .else
  end; // .while
end; // .function CustomBinarySearch

end.
