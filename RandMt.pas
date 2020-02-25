{ ******************************************************************
    Mersenne Twister Random Number Generator for pascal by Alex Vergara Gil
    Ported and improved by Alexander Shostak.
  ******************************************************************}

unit RandMt;

interface
uses Utils;

const
  N          = 624;
  M          = 397;
  MATRIX_A   = integer($9908b0df);  { constant vector a }
  UPPER_MASK = integer($80000000);  { most significant w-r bits }
  LOWER_MASK = integer($7fffffff);  { least significant r bits }

type
  TRngState = Utils.TArrayOfByte;

procedure InitMt (Seed: integer);
{ Initializes MT generator with a seed }

procedure InitMtbyArray (InitKey: array of integer; KeyLength: word);
{ Initialize MT generator with an array InitKey[0..(KeyLength - 1)] }

procedure InitMtByTime;
{ Initializes MT generator with current timestamp in milliseconds. Used by default on first number generation,
  if not initialized yet }

function RandomMt: integer;
{ Generates a Random number }

function RandomRangeMt (Min, Max: integer): integer; overload;
{ Generates Random number in specified range }

(* Returns generator state *)
function GetState: TRngState;

(* Replaces generator state *)
procedure SetState (const State: TRngState);

implementation
uses Windows, Concur;

type
  TRngStateVector = array [0..(N - 1)] of integer;

  PRngStateStruct = ^TRngStateStruct;
  TRngStateStruct = packed record
    mt:  TRngStateVector;
    mti: word;
    _1:  word;
  end;

const
  mag01: array [0..1] of integer = (0, MATRIX_A);

var
  mt:          TRngStateVector;
  mti:         word;
  IsInited:    boolean;
  CritSection: Concur.TCritSection;

procedure InitMt (Seed: integer);
var
  i: word;

begin
  with CritSection do begin
    Enter;

    mt[0] := Seed and $ffffffff;
    
    for i := 1 to N-1 do begin
      mt[i] := (1812433253 * (mt[i-1] xor (mt[i-1] shr 30)) + i);
        { See Knuth TAOCP Vol2. 3rd Ed. P.106 For multiplier.
          In the previous versions, MSBs of the seed affect
          only MSBs of the array mt[].
          2002/01/09 modified by Makoto Matsumoto }
      mt[i] := mt[i] and $ffffffff;
        { For >32 Bit machines }
    end;
    
    mti      := N;
    IsInited := true;

    Leave;
  end;
end; // .procedure InitMt

procedure InitMTbyArray (InitKey: array of integer; KeyLength: word);
var
  i, j, k, k1 : word;
begin
  with CritSection do begin
    Enter;

    InitMT(19650218);

    i := 1;
    j := 0;

    if N > KeyLength then k1 := N else k1 := KeyLength;

    for k := k1 downto 1 do
      begin
        mt[i] := (mt[i] xor ((mt[i-1] xor (mt[i-1] shr 30)) * 1664525)) + InitKey[j] + j; { non linear }
        mt[i] := mt[i] and $ffffffff; { for WORDSIZE > 32 machines }
        i     := i + 1;
        j     := j + 1;
        
        if i >= N then begin
          mt[0] := mt[N-1];
          i := 1;
        end;
        
        if j >= KeyLength then j := 0;
      end;

    for k := N-1 downto 1 do begin
      mt[i] := (mt[i] xor ((mt[i-1] xor (mt[i-1] shr 30)) * 1566083941)) - i; { non linear }
      mt[i] := mt[i] and $ffffffff; { for WORDSIZE > 32 machines }
      i     := i + 1;
      
      if i >= N then begin
        mt[0] := mt[N-1];
        i := 1;
      end;
    end;

    mt[0] := integer($80000000); { MSB is 1; assuring non-zero initial array }

    Leave;
  end; // .with
end; // .procedure InitMTbyArray

procedure InitMtByTime;
var
  SysTime:  Windows.TSystemTime;
  FileTime: Windows.TFileTime;

begin
  Windows.GetSystemTime(SysTime);
  Windows.SystemTimeToFileTime(SysTime, FileTime);
  InitMt(FileTime.dwLowDateTime);
end;

function RandomMt: integer;
var
  y : integer;
  k : word;
begin
  with CritSection do begin
    Enter;

    if not IsInited then begin
      InitMtByTime;
    end;

    if mti >= N then begin
      for k := 0 to (N-M)-1 do begin
        y     := (mt[k] and UPPER_MASK) or (mt[k+1] and LOWER_MASK);
        mt[k] := mt[k+M] xor (y shr 1) xor mag01[y and $1];
      end;

      for k := (N-M) to (N-2) do begin
        y     := (mt[k] and UPPER_MASK) or (mt[k+1] and LOWER_MASK);
        mt[k] := mt[k - (N - M)] xor (y shr 1) xor mag01[y and $1];
      end;

      y       := (mt[N-1] and UPPER_MASK) or (mt[0] and LOWER_MASK);
      mt[N-1] := mt[M-1] xor (y shr 1) xor mag01[y and $1];

      mti := 0;
    end;

    y   := mt[mti];
    mti := mti + 1;

    { Tempering }
    y := y xor (y shr 11);
    y := y xor ((y shl  7) and integer($9d2c5680));
    y := y xor ((y shl 15) and integer($efc60000));
    y := y xor (y shr 18);

    result := y;

    Leave;
  end; // .with
end; // .function RandomMt

function RandomRangeMt (Min, Max: integer): integer; overload;
var
  Interval:    cardinal;
  IntervalCap: cardinal;
  Mask:        cardinal;

begin
  if Min >= Max then begin
    result := Min;
    exit;
  end;

  if (Min = Low(integer)) and (Max = High(integer)) then begin
    result := RandomMt();
    exit;
  end;

  Interval := cardinal(Max - Min) + 1;
  Mask     := $ffffffff;

  if Interval < (cardinal(1) shl 31) then begin
    IntervalCap := 1;

    while IntervalCap < Interval do begin
      IntervalCap := IntervalCap shl 1;
    end;

    Mask := IntervalCap - 1;
  end;  

  repeat
    result := RandomMt() and Mask;
  until cardinal(result) < Interval;

  Inc(result, Min);
end; // .function RandomRangeMt

function GetState: TRngState;
var
  State: PRngStateStruct;

begin
  CritSection.Enter;

  if not IsInited then begin
    InitMtByTime;
  end;

  SetLength(result, sizeof(TRngStateStruct));
  State     := @result[0];
  State.mt  := mt;
  State.mti := mti;
  State._1  := 0;

  CritSection.Leave;
end; // .function GetState

procedure SetState (const State: TRngState); 
var
  StateStruct: PRngStateStruct;

begin
  CritSection.Enter;

  IsInited    := true;
  StateStruct := @State[0];
  mt          := StateStruct.mt;
  mti         := StateStruct.mti;

  CritSection.Leave;
end;

initialization
  CritSection.Init;
finalization
  CritSection.Delete;
end.