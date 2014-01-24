unit CFiles;
{
DESCRIPTION:  Abstract interface of virtual device with sequential access
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Math, Utils;

const
  MODE_OFF        = 0;
  MODE_READ       = 1;
  MODE_WRITE      = 2;
  MODE_READWRITE  = 3;

type
  TDeviceMode = MODE_OFF..MODE_READWRITE;

  (*
  Note about detecting Input/Output (IO) errors.
  if an error occurs during any operation functions will return FALSE.
  But FALSE is also returned if end of file (EOF) is reached.
  The solution is to check EOF flag after FALSE result.
  if EOF is true, then nothing more can be read/written, otherwise IO error occured.
  
  Example:
  while File.ReadByte(Arr[File.Pos]) do begin end;
  if not File.EOF then begin /* ERROR! */ end;
  *)
  
  TAbstractFile = class abstract
    (***) protected (***)
      const
        MIN_BUF_SIZE  = 64 * 1024;
        MAX_BUF_SIZE  = 1024 * 1024;
    
      var
        fMode:          TDeviceMode;
        fHasKnownSize:  BOOLEAN;
        fSizeIsConst:   BOOLEAN;
        fSize:          INTEGER;
        fPos:           INTEGER;
        fEOF:           BOOLEAN;
  
    (***) public (***)
      (* Core *)
      
      // Reads 1..Count bytes
      function  ReadUpTo
      (
                Count:      INTEGER;
            {n} Buf:        POINTER;
        out     BytesRead:  INTEGER
      ): BOOLEAN; virtual; abstract;
      
      // Writes 1..Count bytes
      function  WriteUpTo
      (
                Count:        INTEGER;
            {n} Buf:          POINTER;
        out     ByteWritten:  INTEGER
      ): BOOLEAN; virtual; abstract;

      function  Seek (NewPos: INTEGER): BOOLEAN; virtual; abstract;
    
      (* Reading *)
      function  Read (Count: INTEGER; {n} Buf: POINTER): BOOLEAN;
      function  ReadByte (out Res: BYTE): BOOLEAN;
      function  ReadInt (out Res: INTEGER): BOOLEAN;
      function  ReadStr (Count: INTEGER; out Res: string): BOOLEAN;
      function  ReadAllToBuf (out Buf: POINTER; out Size: INTEGER): BOOLEAN;
      function  ReadAllToStr (out Str: string): BOOLEAN;

      (* Writing *)
      function  Write (Count: INTEGER; {n} Buf: POINTER): BOOLEAN;
      function  WriteByte (Data: BYTE): BOOLEAN;
      function  WriteWord (Data: WORD): BOOLEAN;
      function  WriteInt (Data: INTEGER): BOOLEAN;
      function  WriteStr (Data: string): BOOLEAN;
      function  WriteFrom (Count: INTEGER; Source: TAbstractFile): BOOLEAN;
      function  WriteAllFrom (Source: TAbstractFile): BOOLEAN;

      property  Mode:         TDeviceMode READ fMode;
      property  HasKnownSize: BOOLEAN READ fHasKnownSize;
      property  SizeIsConst:  BOOLEAN READ fSizeIsConst;
      property  Size:         INTEGER READ fSize;
      property  Pos:          INTEGER READ fPos;
      property  EOF:          BOOLEAN READ fEOF;
  end; // .class TAbstractFile
  
  TItemInfo = class
    IsDir:        BOOLEAN;
    HasKnownSize: BOOLEAN;
    FileSize:     INTEGER;
  end; // .class TItemInfo
  
  TAbstractLocator  = class
    (***) protected (***)
      fNotEnd:      BOOLEAN;
      fSearchMask:  string;
      
    (***) public (***)
      destructor  Destroy; override;
      procedure FinitSearch; virtual; abstract;
      procedure InitSearch (const Mask: string); virtual; abstract;
      function  GetNextItem (out ItemInfo: TItemInfo): string; virtual; abstract;
      function  GetItemInfo
      (
        const ItemName: string;
        out   ItemInfo: TItemInfo
      ): BOOLEAN; virtual; abstract;
      
      property  NotEnd:     BOOLEAN READ fNotEnd;
      property  SearchMask: string READ fSearchMask;
  end; // .class TAbstractLocator


(***) implementation (***)


function TAbstractFile.Read (Count: INTEGER; {n} Buf: POINTER): BOOLEAN;
var
  TotalBytesRead: INTEGER;
  BytesRead:      INTEGER;

begin
  {!} Assert(Count >= 0);
  TotalBytesRead  :=  0;
  
  while
    (TotalBytesRead < Count)  and
    Self.ReadUpTo(Count - TotalBytesRead, Utils.PtrOfs(Buf, TotalBytesRead), BytesRead)
  do begin
    TotalBytesRead  :=  TotalBytesRead + BytesRead;
  end; // .while
  
  result  :=  TotalBytesRead = Count;
end; // .function TAbstractFile.Read

function TAbstractFile.ReadByte (out Res: BYTE): BOOLEAN;
var
  BytesRead:  INTEGER;

begin
  result  :=  Self.ReadUpTo(SIZEOF(Res), @Res, BytesRead);
end; // .function TAbstractFile.ReadByte

function TAbstractFile.ReadInt (out Res: INTEGER): BOOLEAN;
begin
  result  :=  Self.Read(SIZEOF(Res), @Res);
end; // .function TAbstractFile.ReadInt

function TAbstractFile.ReadStr (Count: INTEGER; out Res: string): BOOLEAN;
begin
  SetLength(Res, Count);
  result  :=  Self.Read(Count, POINTER(Res));
  
  if not result then begin
    Res :=  '';
  end; // .if
end; // .function TAbstractFile.ReadStr

function TAbstractFile.ReadAllToBuf (out Buf: POINTER; out Size: INTEGER): BOOLEAN;
var
  TotalBytesRead: INTEGER;
  BytesRead:      INTEGER;
  BufSize:        INTEGER;

begin
  {!} Assert(Buf = nil);
  if Self.HasKnownSize then begin
    Size  :=  Self.Size;
    GetMem(Buf, Size);
    result  :=  Self.Read(Size, Buf);
  end // .if
  else begin
    BufSize :=  Self.MIN_BUF_SIZE;
    GetMem(Buf, BufSize);
    TotalBytesRead  :=  0;
    
    while
      Self.ReadUpTo(BufSize - TotalBytesRead, Utils.PtrOfs(Buf, TotalBytesRead), BytesRead) and
      not Self.EOF
    do begin
      TotalBytesRead  :=  TotalBytesRead + BytesRead;
      
      if TotalBytesRead = BufSize then begin
        BufSize :=  BufSize * 2;
        ReallocMem(Buf, BufSize);
      end; // .if
    end; // .while
    
    result  :=  Self.EOF;
    
    if result and (BufSize > TotalBytesRead) then begin
      ReallocMem(Buf, TotalBytesRead);
    end; // .if
  end; // .else
  // * * * * * //
  if not result then begin
    FreeMem(Buf); Buf :=  nil;
  end; // .if
end; // .function TAbstractFile.ReadAllToBuf

function TAbstractFile.ReadAllToStr (out Str: string): BOOLEAN;
var
  TotalBytesRead: INTEGER;
  BytesRead:      INTEGER;
  StrLen:         INTEGER;

begin
  if Self.HasKnownSize then begin
    result  :=  Self.ReadStr(Self.Size, Str);
  end // .if
  else begin
    StrLen  :=  Self.MIN_BUF_SIZE;
    SetLength(Str, StrLen);
    TotalBytesRead  :=  0;
    
    while
      Self.ReadUpTo(StrLen - TotalBytesRead, @Str[1 + TotalBytesRead], BytesRead) and
      not Self.EOF
    do begin
      TotalBytesRead  :=  TotalBytesRead + BytesRead;
      
      if TotalBytesRead = StrLen then begin
        StrLen  :=  StrLen * 2;
        SetLength(Str, StrLen);
      end; // .if
    end; // .while
    
    result  :=  Self.EOF;
    
    if result and (StrLen > TotalBytesRead) then begin
      SetLength(Str, TotalBytesRead);
    end; // .if
  end; // .else
  // * * * * * //
  if not result then begin
    Str :=  '';
  end; // .if
end; // .function TAbstractFile.ReadAllToStr

function TAbstractFile.Write (Count: INTEGER; {n} Buf: POINTER): BOOLEAN;
var
  TotalBytesWritten:  INTEGER;
  BytesWritten:       INTEGER;

begin
  {!} Assert(Count >= 0);
  TotalBytesWritten :=  0;
  
  while
    (TotalBytesWritten < Count) and
    Self.WriteUpTo(Count - TotalBytesWritten, Utils.PtrOfs(Buf, TotalBytesWritten), BytesWritten)
  do begin
    TotalBytesWritten :=  TotalBytesWritten + BytesWritten;
  end; // .while
  
  result  :=  TotalBytesWritten = Count;
end; // .function TAbstractFile.Write

function TAbstractFile.WriteByte (Data: BYTE): BOOLEAN;
var
  BytesWritten: INTEGER;

begin
  result  :=  Self.WriteUpTo(SIZEOF(Data), @Data, BytesWritten);
end; // .function TAbstractFile.WriteByte

function TAbstractFile.WriteWord (Data: WORD): BOOLEAN;
begin
  result  :=  Self.Write(SIZEOF(Data), @Data);
end; // .function TAbstractFile.WriteByte

function TAbstractFile.WriteInt (Data: INTEGER): BOOLEAN;
begin
  result  :=  Self.Write(SIZEOF(Data), @Data);
end; // .function TAbstractFile.WriteInt

function TAbstractFile.WriteStr (Data: string): BOOLEAN;
begin
  result  :=  Self.Write(LENGTH(Data), POINTER(Data));
end; // .function TAbstractFile.WriteStr

function TAbstractFile.WriteFrom (Count: INTEGER; Source: TAbstractFile): BOOLEAN;
var
  StrBuf:           string;
  NumWriteOpers:    INTEGER;
  NumBytesToWrite:  INTEGER;
  i:                INTEGER;

begin
  {!} Assert(Count >= 0);
  {!} Assert(Source <> nil);
  result  :=  FALSE;
  SetLength(StrBuf, Math.Min(Count, Self.MAX_BUF_SIZE));
  
  if Count <= MAX_BUF_SIZE then begin
    result  :=
      Source.Read(Count, POINTER(StrBuf)) and
      Self.Write(Count, POINTER(StrBuf));
  end // .if
  else begin
    NumWriteOpers   :=  Math.Ceil(Count / MAX_BUF_SIZE);
    NumBytesToWrite :=  MAX_BUF_SIZE;
    i               :=  1;
    
    while (i <= NumWriteOpers) and result do begin
      if i = NumWriteOpers then begin
        NumBytesToWrite :=  Count - (MAX_BUF_SIZE * (NumWriteOpers - 1));
      end; // .if
      
      result  :=
        Source.Read(NumBytesToWrite, POINTER(StrBuf)) and
        Self.Write(NumBytesToWrite, POINTER(StrBuf));
      
      INC(i);
    end; // .while
  end; // .else
end; // .function TAbstractFile.WriteFrom

function TAbstractFile.WriteAllFrom (Source: TAbstractFile): BOOLEAN;
var
  StrBuf:     string;
  BytesRead:  INTEGER;

begin
  {!} Assert(Source <> nil);
  result  :=  TRUE;
  
  if Source.HasKnownSize then begin
    result  :=  Self.WriteFrom(Source.Size, Source);
  end // .if
  else begin
    SetLength(StrBuf, Self.MAX_BUF_SIZE);
    
    while result and Source.ReadUpTo(Self.MAX_BUF_SIZE, POINTER(StrBuf), BytesRead) do begin
      result  :=  Self.Write(BytesRead, POINTER(StrBuf));
    end; // .while
    
    result  :=  result and Source.EOF;
  end; // .else
end; // .function TAbstractFile.WriteAllFrom

destructor TAbstractLocator.Destroy;
begin
  Self.FinitSearch;
end; // .destructor TAbstractLocator.Destroy

end.
