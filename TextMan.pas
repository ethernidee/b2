unit Textman;
{
DESCRIPTION:  Text manager provides high-level functions-wrappers over AText
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Windows, SysUtils, Utils, Crypto, StrLib, CFiles, Files, ATexts;

const
  (* FindStr *)
  IGNORE_CASE = TRUE;


type
  TCharPreprocessFunc = function (c: CHAR): CHAR;


function  FindStr (Str: string; IgnoreCase: BOOLEAN; Text: ATexts.AText): BOOLEAN;
procedure ReplaceStr (Str: string; ReplWith: string; IgnoreCase: BOOLEAN; Text: ATexts.AText);
function  LoadFromFile (const FilePath: string; {n} Settings: Utils.TCloneable; Text: ATexts.AText): BOOLEAN;
function  SaveToFile (const FilePath: string; Text: ATexts.AText): BOOLEAN;


(***) implementation (***)


function FindStr (Str: string; IgnoreCase: BOOLEAN; Text: ATexts.AText): BOOLEAN;
var
  StrLen:     INTEGER;
  StrHash:    INTEGER;
  MatchHash:  INTEGER;
  HashInd:    INTEGER;
  StartPos:   INTEGER;
  c:          CHAR;
  i:          INTEGER;
  
  function IsMatch: BOOLEAN;
  begin
    Text.GotoPos(StartPos);
    result  :=  TRUE;
    i       :=  1;
    while result and (i <= StrLen) do begin
      Text.GetCurrChar(c);
      if IgnoreCase then begin
        c :=  StrLib.CharToLower(c);
      end; // .if
      result  :=  c = Str[i];
      Text.GotoNextPos;
      INC(i);
    end; // .while
  end; // .function IsMatch
  
begin
  {!} Assert(Text <> nil);
  StrLen  :=  LENGTH(Str);
  result  :=  FALSE;
  if (Text.Len - Text.Pos + 1) >= StrLen then begin
    if IgnoreCase then begin
      Str :=  SysUtils.AnsiLowerCase(Str);
    end; // .if
    StartPos  :=  Text.Pos;
    StrHash   :=  0;
    HashInd   :=  0;
    for i:=1 to StrLen do begin
      c :=  Str[i];
      if IgnoreCase then begin
        c :=  StrLib.CharToLower(c);
      end; // .if
      INC(Crypto.TInt32(StrHash)[HashInd], Crypto.ByteRedirTable[ORD(c)]);
      HashInd :=  (HashInd + 1) and 3;
    end; // .for
    MatchHash :=  0;
    HashInd   :=  0;
    for i:=1 to StrLen do begin
      Text.GetCurrChar(c);
      if IgnoreCase then begin
        c :=  StrLib.CharToLower(c);
      end; // .if
      INC(Crypto.TInt32(MatchHash)[HashInd], Crypto.ByteRedirTable[ORD(c)]);
      HashInd :=  (HashInd + 1) and 3;
      Text.GotoNextPos;
    end; // .for
    result  :=  (MatchHash = StrHash) and IsMatch;
    HashInd :=  (StrLen - 1) and 3;
    while (not result) and (not Text.TextEnd) do begin
      Text.GotoPos(StartPos);
      Text.GetCurrChar(c);
      if IgnoreCase then begin
        c :=  StrLib.CharToLower(c);
      end; // .if
      DEC(Crypto.TInt32(MatchHash)[0], Crypto.ByteRedirTable[ORD(c)]);
      MatchHash :=  ((MatchHash and $FF) shl 24) or (MatchHash shr 8);
      Text.GotoPos(StartPos + StrLen);
      Text.GetCurrChar(c);
      if IgnoreCase then begin
        c :=  StrLib.CharToLower(c);
      end; // .if
      INC(Crypto.TInt32(MatchHash)[HashInd], Crypto.ByteRedirTable[ORD(c)]);
      INC(StartPos);
      result  :=  (MatchHash = StrHash) and IsMatch;
      Text.GotoNextPos;
    end; // .while
    if result then begin
      Text.GotoPos(StartPos);
    end; // .if
  end; // .if
end; // .function FindStr

procedure ReplaceStr (Str: string; ReplWith: string; IgnoreCase: BOOLEAN; Text: ATexts.AText);
begin
  {!} Assert(Text <> nil);
  while FindStr(Str, IgnoreCase, Text) do begin
    Text.Replace(LENGTH(Str), ReplWith);
  end; // .while
end; // .procedure ReplaceStr

function LoadFromFile (const FilePath: string; {n} Settings: Utils.TCloneable; Text: ATexts.AText): BOOLEAN;
var
{O} DataFile:     Files.TFile;
    FileContents: string;

begin
  {!} Assert(Text <> nil);
  DataFile  :=  Files.TFile.Create;
  // * * * * * //
  result  :=  DataFile.Open(FilePath, CFiles.MODE_READ) and DataFile.ReadAllToStr(FileContents);
  if result then begin
    Text.Connect(FileContents, Settings);
  end; // .if
  SysUtils.FreeAndNil(DataFile);
end; // .function LoadFromFile

function SaveToFile (const FilePath: string; Text: ATexts.AText): BOOLEAN;
var
{O} DataFile: Files.TFile;
    SavedPos: INTEGER;

begin
  {!} Assert(Text <> nil);
  DataFile  :=  Files.TFile.Create;
  // * * * * * //
  SavedPos  :=  Text.Pos;
  Text.GotoPos(1);
  Windows.DeleteFile(PCHAR(FilePath));
  result  :=  DataFile.CreateNew(FilePath) and DataFile.WriteStr(Text.GetStr(Text.Len));
  Text.GotoPos(SavedPos);
  SysUtils.FreeAndNil(DataFile);
end; // .function SaveToFile

end.
