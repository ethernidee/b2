unit Alg;
{
DESCRIPTION:  Additional math/algorithmic functions
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Utils, Math;

type
  TOnRangeMinMaxConflict = (FORCE_MIN_VALUE, FORCE_MAX_VALUE);

  TCompareFunc   = function (Value1, Value2: integer): integer;
  TCompareMethod = function (Value1, Value2: integer): integer of object;
  
  TQuickSortAdapter = class abstract
    function  CompareItems (Ind1, Ind2: integer): integer; virtual; abstract;
    procedure SwapItems (Ind1, Ind2: integer); virtual; abstract;
    procedure SavePivotItem (PivotItemInd: integer); virtual; abstract;
    function  CompareToPivot (Ind: integer): integer; virtual; abstract;
  end;


(* Rounds integer to given boundary. If ceil is true, rounding is performed to higher value, otherwise to lower one.
   Example: f(21, 4, true) = 24 *)
function IntRoundToBoundary (Value, Boundary: integer; Ceil: boolean = true): integer;

function  ToRange (Value, MinValue, MaxValue: integer; OnRangeMinMaxConflict: TOnRangeMinMaxConflict = FORCE_MIN_VALUE): integer;
function  InRange (Value, MinValue, MaxValue: cardinal): boolean; overload; inline;
function  InRange (Value, MinValue, MaxValue: integer): boolean; overload; inline;
function  IntLog2 (Num: integer): integer; {=> Ceil(Log2(N)), N > 0}
function  IntCompare (Int1, Int2: integer): integer;
function  CardCompare (Card1, Card2: cardinal): integer;
function  PtrCompare (Ptr1, Ptr2: pointer): integer;
function  Int64To32 (Value: INT64): integer; {No overflow, bounds to LOW(INT32)..HIGH(IN32)}
function  IsSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer): boolean; { Ascendantly }
function  IsCustomSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer; CompareItems: TCompareFunc): boolean; overload;
function  IsCustomSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer; CompareItems: TCompareMethod): boolean; overload;
function  IsDescSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer): boolean;
function  IsCustomDescSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer; CompareItems: TCompareFunc): boolean; overload;
function  IsCustomDescSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer; CompareItems: TCompareMethod): boolean; overload;
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
  result := (Value + (Boundary - 1)) and not (Boundary - 1);
end;

function ToRange (Value, MinValue, MaxValue: integer; OnRangeMinMaxConflict: TOnRangeMinMaxConflict = FORCE_MIN_VALUE): integer;
begin
  result := Value;

  if OnRangeMinMaxConflict = FORCE_MIN_VALUE then begin
    if result > MaxValue then begin
      result := MaxValue;
    end;

    if result < MinValue then begin
      result := MinValue;
    end;
  end else begin
    if result < MinValue then begin
      result := MinValue;
    end;

    if result > MaxValue then begin
      result := MaxValue;
    end;
  end; // .else
end; // .function ToRange

function InRange (Value, MinValue, MaxValue: cardinal): boolean; overload;
begin
  result := (Value >= MinValue) and (Value <= MaxValue);
end;

function InRange (Value, MinValue, MaxValue: integer): boolean; overload;
begin
  result := (Value >= MinValue) and (Value <= MaxValue);
end;

function IntLog2 (Num: integer): integer;
var
  TestValue:  cardinal;
  
begin
  {!} Assert(Num > 0);
  result    := 0;
  TestValue := 1;
  
  while TestValue < cardinal(Num) do begin
    Inc(result);
    TestValue := TestValue shl 1;
  end;
end; // .function IntLog2

function IntCompare (Int1, Int2: integer): integer;
begin
  if Int1 > Int2 then begin
    result := +1;
  end else if Int1 < Int2 then begin
    result := -1;
  end else begin
    result := 0;
  end;
end;

function CardCompare (Card1, Card2: cardinal): integer;
begin
  if Card1 > Card2 then begin
    result := +1;
  end else if Card1 < Card2 then begin
    result := -1;
  end else begin
    result := 0;
  end;
end;

function PtrCompare (Ptr1, Ptr2: pointer): integer;
begin
  if cardinal(Ptr1) > cardinal(Ptr2) then begin
    result := +1;
  end else if cardinal(Ptr1) < cardinal(Ptr2) then begin
    result := -1;
  end else begin
    result := 0;
  end;
end;

function Int64To32 (Value: INT64): integer;
begin
  if Value > High(integer) then begin
    result  :=  High(integer);
  end else if Value < Low(integer) then begin
    result  :=  Low(integer);
  end else begin
    result  :=  Value;
  end;
end;

function IsSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer): boolean;
var
  i: integer;

begin
  result := true;

  for i := MinInd + 1 to MaxInd do begin
    if Arr[i - 1] > Arr[i] then begin
      result := false;
      break;
    end;
  end;
end; // .function IsSortedArr

function IsCustomSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer; CompareItems: TCompareFunc): boolean;
var
  i: integer;

begin
  result := true;

  for i := MinInd + 1 to MaxInd do begin
    if CompareItems(Arr[i - 1], Arr[i]) > 0 then begin
      result := false;
      break;
    end;
  end;
end; // .function IsCustomSortedArr

function IsCustomSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer; CompareItems: TCompareMethod): boolean;
var
  i: integer;

begin
  result := true;

  for i := MinInd + 1 to MaxInd do begin
    if CompareItems(Arr[i - 1], Arr[i]) > 0 then begin
      result := false;
      break;
    end;
  end;
end; // .function IsCustomSortedArr

function IsDescSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer): boolean;
var
  i: integer;

begin
  result := true;

  for i := MinInd + 1 to MaxInd do begin
    if Arr[i - 1] > Arr[i] then begin
      result := false;
      break;
    end;
  end;
end; // .function IsDescSortedArr

function IsCustomDescSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer; CompareItems: TCompareFunc): boolean;
var
  i: integer;

begin
  result := true;

  for i := MinInd + 1 to MaxInd do begin
    if CompareItems(Arr[i - 1], Arr[i]) > 0 then begin
      result := false;
      break;
    end;
  end;
end; // .function IsCustomDescSortedArr

function IsCustomDescSortedArr (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer; CompareItems: TCompareMethod): boolean;
var
  i: integer;

begin
  result := true;

  for i := MinInd + 1 to MaxInd do begin
    if CompareItems(Arr[i - 1], Arr[i]) > 0 then begin
      result := false;
      break;
    end;
  end;
end; // .function IsCustomDescSortedArr

procedure QuickSort (Arr: Utils.PEndlessIntArr; MinInd, MaxInd: integer);
begin
  CustomQuickSort(Arr, MinInd, MaxInd, IntCompare);
end;

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
  end;
  
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
      end;
      
      while CompareItems(Arr[RightInd], PivotItem) > 0 do begin
        Dec(RightInd);
      end;
      
      if LeftInd <= RightInd then begin
        if CompareItems(Arr[LeftInd], Arr[RightInd]) > 0 then begin
          ExchangeItems(LeftInd, RightInd);
        end else begin
          Inc(LeftInd);
          Dec(RightInd);
        end;
      end;
    end; // .while
    
    (* MIN__RIGHT|{PIVOT}|LEFT__MAX *)
    
    if (RightInd - MinInd) < (MaxInd - LeftInd) then begin
      if RightInd > MinInd then begin
        CustomQuickSort(Arr, MinInd, RightInd, CompareItems);
      end;
      
      MinInd := LeftInd;
    end else begin
      if MaxInd > LeftInd then begin
        CustomQuickSort(Arr, LeftInd, MaxInd, CompareItems);
      end;
      
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
  end;
  
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
      end;
      
      while CompareItems(Arr[RightInd], PivotItem) > 0 do begin
        Dec(RightInd);
      end;
      
      if LeftInd <= RightInd then begin
        if CompareItems(Arr[LeftInd], Arr[RightInd]) > 0 then begin
          ExchangeItems(LeftInd, RightInd);
        end else begin
          Inc(LeftInd);
          Dec(RightInd);
        end;
      end;
    end; // .while
    
    (* MIN__RIGHT|{PIVOT}|LEFT__MAX *)
    
    if (RightInd - MinInd) < (MaxInd - LeftInd) then begin
      if RightInd > MinInd then begin
        CustomQuickSort(Arr, MinInd, RightInd, CompareItems);
      end;
      
      MinInd := LeftInd;
    end else begin
      if MaxInd > LeftInd then begin
        CustomQuickSort(Arr, LeftInd, MaxInd, CompareItems);
      end;
      
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
      end;
      
      while Obj.CompareToPivot(RightInd) > 0 do begin
        Dec(RightInd);
      end;
      
      if LeftInd <= RightInd then begin
        if Obj.CompareItems(LeftInd, RightInd) > 0 then begin
          Obj.SwapItems(LeftInd, RightInd);
        end else begin
          Inc(LeftInd);
          Dec(RightInd);
        end;
      end;
    end; // .while
    
    (* MIN__RIGHT|{PIVOT}|LEFT__MAX *)
    
    if (RightInd - MinInd) < (MaxInd - LeftInd) then begin
      if RightInd > MinInd then begin
        QuickSortEx(Obj, MinInd, RightInd);
      end;
      
      MinInd :=  LeftInd;
    end else begin
      if MaxInd > LeftInd then begin
        QuickSortEx(Obj, LeftInd, MaxInd);
      end;
      
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
    end;
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
    end;
  end; // .while
end; // .function CustomBinarySearch

end.
