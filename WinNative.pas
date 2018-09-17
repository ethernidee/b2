unit WinNative;
(*
  Windows native API (ntdll, etc).
  Adapted from: https://github.com/magicmonty/delphi-code-coverage/
*)

(***)  interface  (***)

uses
  Windows, SysUtils, Utils;


////////////////////////////////////////////////////////////////////////
//                                                                    //
//                      COMMON TYPES AND CONSTS                       //
//                                                                    //
////////////////////////////////////////////////////////////////////////

type
  NTSTATUS  = Longword;
  USHORT    = word;
  PCSZ      = PAnsiChar;
  PWSTR     = PWideChar;
  HANDLE    = THandle;
  PVOID     = pointer;
  ULONG_PTR = pointer;

  _STRING = packed record
    Length:        USHORT;
    MaximumLength: USHORT;
    Buffer:        PAnsiChar;
  end;

  ANSI_STRING  = _STRING;
  PANSI_STRING = ^ANSI_STRING;
  OEM_STRING   = _STRING;
  POEM_STRING  = ^OEM_STRING;
  PCOEM_STRING = POEM_STRING;

  _UNICODE_STRING = packed record
    Length: USHORT;
    MaximumLength: USHORT;
    Buffer: PWSTR;
  end;
  
  UNICODE_STRING   = _UNICODE_STRING;
  PUNICODE_STRING  = ^UNICODE_STRING;
  PCUNICODE_STRING = PUNICODE_STRING;

  _OBJECT_ATTRIBUTES = packed record
    Length:                   ULONG;
    RootDirectory:            HANDLE;
    ObjectName:               PUNICODE_STRING;
    Attributes:               ULONG;
    SecurityDescriptor:       PVOID;
    SecurityQualityOfService: PVOID;
  end;
  
  OBJECT_ATTRIBUTES  = _OBJECT_ATTRIBUTES;
  POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;
  TObjectAttributes  = OBJECT_ATTRIBUTES;
  PObjectAttributes  = ^TObjectAttributes;


  _IO_STATUS_BLOCK = packed record
    Status: packed record
      case byte of
        0: (Status:  NTSTATUS);
        1: (Pointer: ULONG_PTR);
    end;
    
    Information: ULONG_PTR;
  end;
  
  IO_STATUS_BLOCK  = _IO_STATUS_BLOCK;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;
  TIoStatusBlock   = IO_STATUS_BLOCK;
  PIoStatusBlock   = ^TIoStatusBlock;


////////////////////////////////////////////////////////////////////////
//                                                                    //
//                             ACCESS TYPES                           //
//                                                                    //
////////////////////////////////////////////////////////////////////////

type
  ACCESS_MASK  = DWORD;
  PACCESS_MASK = ^ACCESS_MASK;
  TAccessMask  = ACCESS_MASK;
  PAccessMask  = PACCESS_MASK; 

//
//  The following are masks for the predefined standard access types
//

const
  DELETE                   = $00010000;
  READ_CONTROL             = $00020000;
  WRITE_DAC                = $00040000;
  WRITE_OWNER              = $00080000;
  SYNCHRONIZE              = $00100000;

  STANDARD_RIGHTS_REQUIRED = $000F0000;
  STANDARD_RIGHTS_READ     = READ_CONTROL;
  STANDARD_RIGHTS_WRITE    = READ_CONTROL;
  STANDARD_RIGHTS_EXECUTE  = READ_CONTROL;
  STANDARD_RIGHTS_ALL      = $001F0000;
  SPECIFIC_RIGHTS_ALL      = $0000FFFF;

  //
  // AccessSystemAcl access type
  //
  ACCESS_SYSTEM_SECURITY = $01000000;

  //
  // MaximumAllowed access type
  //
  MAXIMUM_ALLOWED = $02000000;

  //
  //  These are the generic rights.
  //
  GENERIC_READ    = DWORD($80000000);
  GENERIC_WRITE   = $40000000;
  GENERIC_EXECUTE = $20000000;
  GENERIC_ALL     = $10000000;

//
//  Define the generic mapping array.  This is used to denote the
//  mapping of each generic access right to a specific access mask.
//

type
  PGENERIC_MAPPING = ^GENERIC_MAPPING;
  _GENERIC_MAPPING = packed record
    GenericRead: ACCESS_MASK;
    GenericWrite: ACCESS_MASK;
    GenericExecute: ACCESS_MASK;
    GenericAll: ACCESS_MASK;
  end;
  
  GENERIC_MAPPING = _GENERIC_MAPPING;
  TGenericMapping = GENERIC_MAPPING;
  PGenericMapping = PGENERIC_MAPPING;


////////////////////////////////////////////////////////////////////////
//                                                                    //
//                         EXPORTED FUNCTIONS                         //
//                                                                    //
////////////////////////////////////////////////////////////////////////

type
  TNtOpenFile = function (FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK;  ShareAccess: ULONG; OpenOptions: ULONG): NTSTATUS; stdcall;


(***)  implementation  (***)



end.