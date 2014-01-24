unit FilesEx;
{
DESCRIPTION:  High-level finctions for working with files
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  interface  (***)
uses SysUtils, Utils, Files, DataLib;

type
  (* IMPORT *)
  TStrList = DataLib.TStrList;
  TDict = DataLib.TDict;
  

// List does not own its objects. It simply contains case insensitive file names
function  GetFileList (const MaskedPath: string; SearchSubj: TSearchSubj): {O} TStrList;
procedure MergeFileLists (MainList, DependantList: TStrList);


(***) implementation (***)


function GetFileList (const MaskedPath: string; SearchSubj: TSearchSubj): {O} TStrList;
begin
  result := DataLib.NewStrList(not Utils.OWNS_ITEMS, DataLib.CASE_INSENSITIVE);
  
  with Files.Locate(MaskedPath, SearchSubj) do begin
    while FindNext do begin
      result.Add(FoundName);
    end; // .while
  end; // .with 
end; // .procedure GetFileList

procedure MergeFileLists (MainList, DependantList: TStrList);
var
{O} NamesDict: TDict {OF FileName: STRING => Ptr(1)};
    i: INTEGER;
   
begin
  {!} Assert(MainList <> nil);
  {!} Assert(DependantList <> nil);
  NamesDict := DataLib.NewDict(not Utils.OWNS_ITEMS, DataLib.CASE_INSENSITIVE);
  // * * * * * //
  for i := 0 to MainList.Count - 1 do begin
    NamesDict[MainList[i]] := Ptr(1);
  end; // .for
  
  for i := 0 to DependantList.Count - 1 do begin
    if NamesDict[DependantList[i]] = nil then begin
      NamesDict[DependantList[i]] := Ptr(1);
      MainList.Add(DependantList[i]);
    end; // .if
  end; // .for
  // * * * * * //
  SysUtils.FreeAndNil(NamesDict);
end; // .procedure MergeFileLists

end.
