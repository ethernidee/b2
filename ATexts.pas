unit ATexts;
{
DESCRIPTION:  Text model interface
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses Utils;

const
  NO_SETTINGS = nil;


type
  {
    First character position is 1.
  }
  AText = class (Utils.TCloneable)
    (***) protected (***)
      fLen:     INTEGER;
      fPos:     INTEGER;
      fTextEnd: BOOLEAN;
    
    (***) public (***)
      (* Character navigation *)
      function  GotoNextPos: BOOLEAN; virtual; abstract;
      function  GotoPrevPos: BOOLEAN; virtual; abstract;
      function  GotoPos (NewPos: INTEGER): BOOLEAN; virtual; abstract;
      
      (* Reading operations *)
      function  GetCurrChar (out c: CHAR): BOOLEAN; virtual; abstract;
      function  GetStr (StrLen: INTEGER): string; virtual; abstract;
      
      (* Writing operations *)
      function  SetCurrChar (c: CHAR): BOOLEAN; virtual; abstract;
      procedure Insert (const Str: string); virtual; abstract;
      procedure Delete (DelCount: INTEGER); virtual; abstract;
      {The same as Delete + Insert}
      procedure Replace (ReplCount: INTEGER; const ReplWith: string); virtual; abstract;
      
      (* Generic *)
      procedure Connect (const Source: string; {IN} var {n} Settings: TObject); virtual; abstract;
      procedure Clear; virtual; abstract;
      
      property  Pos:      INTEGER READ fPos;
      property  Len:      INTEGER READ fLen;
      property  TextEnd:  BOOLEAN READ fTextEnd;
  end; // .class AText
  
  {
    "Text over lines".
    Position of first character in line is 1.
    First line number is 1.
    Minimal line count is 1.
  }
  ATextLines  = class (AText)
    (***) protected (***)
      fLineN:         INTEGER;
      fLinePos:       INTEGER;
      fNumLines:      INTEGER;
      fLineEndMarker: CHAR;
    
    (***) public (***)
      (* Line navigation *)
      function  GotoNextLine: BOOLEAN; virtual; abstract;
      function  GotoPrevLine: BOOLEAN; virtual; abstract;
      function  GotoLine (NewLineN: INTEGER): BOOLEAN; virtual; abstract;
      
      (* Position conversions *)
      function  PosToLinePos (Pos: INTEGER; out LineN, LinePos: INTEGER): BOOLEAN; virtual; abstract;
      function  LinePosToPos (LineN, LinePos: INTEGER; out Pos: INTEGER): BOOLEAN; virtual; abstract;
      
      (* Getting metrics *)
      function  GetLineLen (LineN: INTEGER; out LineLen: INTEGER): BOOLEAN; virtual; abstract;
      
      property  LineN:          INTEGER READ fLineN;
      property  LinePos:        INTEGER READ fLinePos;
      property  NumLines:       INTEGER READ fNumLines;
      property  LineEndMarker:  CHAR READ fLineEndMarker;
  end; // .class ATextLines


(***) implementation (***)

end.
