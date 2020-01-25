{ ******************************************************************
    Mersenne Twister Random Number Generator for pascal
    by Alex Vergara Gil
  ******************************************************************}

unit RandMt;

interface

procedure InitMt (Seed: integer);
{ Initializes MT generator with a seed }

procedure InitMtbyArray (InitKey: array of integer; KeyLength: Word);
{ Initialize MT generator with an array InitKey[0..(KeyLength - 1)] }

function RandomMt: integer;
{ Generates a Random number on [-231 .. 231 - 1] interval }

implementation
uses Windows;

const
  N          = 624;
  M          = 397;
  MATRIX_A   = integer($9908b0df);  { constant vector a }
  UPPER_MASK = integer($80000000);  { most significant w-r bits }
  LOWER_MASK = integer($7fffffff);  { least significant r bits }

  mag01 : array[0..1] of integer{integer} = (0, MATRIX_A);

var
  mt  : array[0..(N-1)] of integer{integer};  { the array for the state vector }
  mti : Word;                        { mti == N+1 means mt[N] is not initialized }

procedure InitMT (Seed: integer);
var
  i : Word;
begin
  mt[0] := Seed and $ffffffff;
  for i := 1 to N-1 do
    begin
      mt[i] := (1812433253 * (mt[i-1] Xor (mt[i-1] shr 30)) + i);
        { See Knuth TAOCP Vol2. 3rd Ed. P.106 For multiplier.
          In the previous versions, MSBs of the seed affect
          only MSBs of the array mt[].
          2002/01/09 modified by Makoto Matsumoto }
      mt[i] := mt[i] and $ffffffff;
        { For >32 Bit machines }
    end;
  mti := N;
end;

procedure InitMTbyArray (InitKey: array of integer; KeyLength: Word);
var
  i, j, k, k1 : Word;
begin
  InitMT(19650218);

  i := 1;
  j := 0;

  if N > KeyLength then k1 := N else k1 := KeyLength;

  for k := k1 downto 1 do
    begin
      mt[i] := (mt[i] Xor ((mt[i-1] Xor (mt[i-1] shr 30)) * 1664525)) + InitKey[j] + j; { non linear }
      mt[i] := mt[i] and $ffffffff; { for WORDSIZE > 32 machines }
      i := i + 1;
      j := j + 1;
      if i >= N then
        begin
          mt[0] := mt[N-1];
          i := 1;
        end;
      if j >= KeyLength then j := 0;
    end;

  for k := N-1 downto 1 do
    begin
      mt[i] := (mt[i] Xor ((mt[i-1] Xor (mt[i-1] shr 30)) * 1566083941)) - i; { non linear }
      mt[i] := mt[i] and $ffffffff; { for WORDSIZE > 32 machines }
      i := i + 1;
      if i >= N then
        begin
          mt[0] := mt[N-1];
          i := 1;
        end;
    end;

    mt[0] := integer($80000000); { MSB is 1; assuring non-zero initial array }
end;

procedure InitMtByTime;
var
  Time: Windows.TSystemTime;

begin
  Windows.GetSystemTime(Time);
  InitMT(Time.wHour * 3600000 + Time.wMinute * 60000 + Time.wSecond * 1000 + Time.wMilliseconds);
end;

function RandomMt: integer;
var
  y : integer;
  k : Word;
begin
  if mti >= N then  { generate N words at one Time }
    begin
      { If IRanMT() has not been called, a default initial seed is used }
      if mti = N + 1 then InitMT(5489);

      for k := 0 to (N-M)-1 do
        begin
          y := (mt[k] and UPPER_MASK) or (mt[k+1] and LOWER_MASK);
          mt[k] := mt[k+M] xor (y shr 1) xor mag01[y and $1];
        end;

      for k := (N-M) to (N-2) do
        begin
          y := (mt[k] and UPPER_MASK) or (mt[k+1] and LOWER_MASK);
          mt[k] := mt[k - (N - M)] xor (y shr 1) xor mag01[y and $1];
        end;

      y := (mt[N-1] and UPPER_MASK) or (mt[0] and LOWER_MASK);
      mt[N-1] := mt[M-1] xor (y shr 1) xor mag01[y and $1];

      mti := 0;
    end;

  y := mt[mti];
  mti := mti + 1;

  { Tempering }
  y := y xor (y shr 11);
  y := y xor ((y shl  7) and integer($9d2c5680));
  y := y xor ((y shl 15) and integer($efc60000));
  y := y xor (y shr 18);

  result := y
end;

const
  init: array[0..3] of integer = ($123, $234, $345, $456);

begin
  InitMtByTime;
end.