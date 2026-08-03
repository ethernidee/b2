unit CmdApp;
{
DESCRIPTION:  Provides new-style command line handling and some inter-process functions
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(*
New style command line: ArgName=ArgValue "Arg Name"="Arg Value"
Simple "ArgName" means ArgName:1.
Argument names are case-insensitive.
Duplicate arguments override the previous ones.
*)

(***)  interface  (***)
uses Windows, SysUtils, Utils, TypeWrappers, Crypto, TextScan, AssocArrays, Lists;

const
  (* RunProcess *)
  WAIT_PROCESS_END  = TRUE;


type
  (* IMPORT *)
  TString = TypeWrappers.TString;


function  ArgExists (const ArgName: string): boolean;
function  GetArg (const ArgName: string): string;
procedure SetArg (const ArgName, NewArgValue: string);

(* If ProcessInfo address is passed, hProcess and hThread handles are not closed automatically *)
function  RunProcess (const ExeFilePath, ExeArgs, ExeCurrentDir: string; WaitEnd: boolean; {n} ProcessInfo: Windows.PProcessInformation = nil): boolean;


var
{O} Args: {O} AssocArrays.TAssocArray {of TString};


(***)  implementation  (***)


function ArgExists (const ArgName: string): boolean;
begin
  result := Args[ArgName] <> nil;
end;

function GetArg (const ArgName: string): string;
var
{U} ArgValue: TString;

begin
  ArgValue := Args[ArgName];
  // * * * * * //
  if ArgValue <> nil then begin
    result := ArgValue.Value;
  end else begin
    result := '';
  end;
end;

procedure SetArg (const ArgName, NewArgValue: string);
var
{U} ArgValue: TString;

begin
  ArgValue := Args[ArgName];
  // * * * * * //
  if ArgValue <> nil then begin
    ArgValue.Value := NewArgValue;
  end else begin
    Args[ArgName] := TString.Create(NewArgValue);
  end;
end;

procedure ProcessArgs;
var
{O} Scanner:  TextScan.TTextScanner;
    Arg:      string;
    ArgName:  string;
    ArgValue: string;
    i:        integer;

begin
  Scanner := TextScan.TTextScanner.Create;
  // * * * * * //
  for i := 1 to ParamCount do begin
    Arg := ParamStr(i);
    Scanner.Connect(Arg, #0);

    Scanner.ReadTokenTillDelim(['='], ArgName);
    ArgValue := '';

    if (Scanner.c = '=') and Scanner.GotoNextChar then begin
      Scanner.ReadTokenTillDelim([], ArgValue);
    end;

    SetArg(ArgName, ArgValue);
  end;
  // * * * * * //
  SysUtils.FreeAndNil(Scanner);
end;

function RunProcess (const ExeFilePath, ExeArgs, ExeCurrentDir: string; WaitEnd: boolean; {n} ProcessInfo: Windows.PProcessInformation = nil): boolean;
const
  NO_APPLICATION_NAME        = nil;
  DEFAULT_PROCESS_ATTRIBUTES = nil;
  DEFAULT_THREAD_ATTRIBUTES  = nil;
  INHERIT_HANDLES            = TRUE;
  NO_CREATION_FLAGS          = 0;
  INHERIT_ENVIROMENT         = nil;

var
  StartupInfo:    Windows.TStartupInfo;
  ProcessInfoRec: Windows.TProcessInformation;

begin
  FillChar(StartupInfo, sizeof(StartupInfo), #0);
  StartupInfo.cb  :=  sizeof(StartupInfo);
  result          :=  Windows.CreateProcess
  (
    NO_APPLICATION_NAME,
    pchar('"' + ExeFilePath + '" ' + ExeArgs),
    DEFAULT_PROCESS_ATTRIBUTES,
    DEFAULT_THREAD_ATTRIBUTES,
    not INHERIT_HANDLES,
    NO_CREATION_FLAGS,
    INHERIT_ENVIROMENT,
    pointer(ExeCurrentDir),
    StartupInfo,
    ProcessInfoRec
  );

  if ProcessInfo <> nil then begin
    ProcessInfo^ := ProcessInfoRec;
  end else begin
    Windows.CloseHandle(ProcessInfoRec.hProcess);
    Windows.CloseHandle(ProcessInfoRec.hThread);
  end;

  if result and WaitEnd then begin
    Windows.WaitForSingleObject(ProcessInfo.hProcess, Windows.INFINITE);
  end;
end; // .function RunProcess

begin
  Args := AssocArrays.NewStrictAssocArr(TString);
  ProcessArgs;
end.
