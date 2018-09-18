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
    Length: USHORT; // in bytes
    MaximumLength: USHORT; // in bytes
    Buffer: PWSTR;

    function GetLength (): integer; inline; // in characters
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

type
  _FILE_INFORMATION_CLASS = (
    FileDirectoryInformation = 1              ,
    FileFullDirectoryInformation              ,
    FileBothDirectoryInformation              ,
    FileBasicInformation                      ,
    FileStandardInformation                   ,
    FileInternalInformation                   ,
    FileEaInformation                         ,
    FileAccessInformation                     ,
    FileNameInformation                       ,
    FileRenameInformation                     ,
    FileLinkInformation                       ,
    FileNamesInformation                      ,
    FileDispositionInformation                ,
    FilePositionInformation                   ,
    FileFullEaInformation                     ,
    FileModeInformation                       ,
    FileAlignmentInformation                  ,
    FileAllInformation                        ,
    FileAllocationInformation                 ,
    FileEndOfFileInformation                  ,
    FileAlternateNameInformation              ,
    FileStreamInformation                     ,
    FilePipeInformation                       ,
    FilePipeLocalInformation                  ,
    FilePipeRemoteInformation                 ,
    FileMailslotQueryInformation              ,
    FileMailslotSetInformation                ,
    FileCompressionInformation                ,
    FileObjectIdInformation                   ,
    FileCompletionInformation                 ,
    FileMoveClusterInformation                ,
    FileQuotaInformation                      ,
    FileReparsePointInformation               ,
    FileNetworkOpenInformation                ,
    FileAttributeTagInformation               ,
    FileTrackingInformation                   ,
    FileIdBothDirectoryInformation            ,
    FileIdFullDirectoryInformation            ,
    FileValidDataLengthInformation            ,
    FileShortNameInformation                  ,
    FileIoCompletionNotificationInformation   ,
    FileIoStatusBlockRangeInformation         ,
    FileIoPriorityHintInformation             ,
    FileSfioReserveInformation                ,
    FileSfioVolumeInformation                 ,
    FileHardLinkInformation                   ,
    FileProcessIdsUsingFileInformation        ,
    FileNormalizedNameInformation             ,
    FileNetworkPhysicalNameInformation        ,
    FileIdGlobalTxDirectoryInformation        ,
    FileIsRemoteDeviceInformation             ,
    FileUnusedInformation                     ,
    FileNumaNodeInformation                   ,
    FileStandardLinkInformation               ,
    FileRemoteProtocolInformation             ,
    FileRenameInformationBypassAccessCheck    ,
    FileLinkInformationBypassAccessCheck      ,
    FileVolumeNameInformation                 ,
    FileIdInformation                         ,
    FileIdExtdDirectoryInformation            ,
    FileReplaceCompletionInformation          ,
    FileHardLinkFullIdInformation             ,
    FileIdExtdBothDirectoryInformation        ,
    FileDispositionInformationEx              ,
    FileRenameInformationEx                   ,
    FileRenameInformationExBypassAccessCheck  ,
    FileDesiredStorageClassInformation        ,
    FileStatInformation                       ,
    FileMemoryPartitionInformation            ,
    FileStatLxInformation                     ,
    FileCaseSensitiveInformation              ,
    FileMaximumInformation
  ); // _FILE_INFORMATION_CLASS
  
  FILE_INFORMATION_CLASS  = _FILE_INFORMATION_CLASS;
  TFileInformationClass   = FILE_INFORMATION_CLASS;

//
// Large (64-bit) integer types and operations
//

type
  LPLARGE_INTEGER = ^LARGE_INTEGER;
  _LARGE_INTEGER  = Windows._LARGE_INTEGER;
  LARGE_INTEGER   = Windows.LARGE_INTEGER;
  TLargeInteger   = Windows.TLargeInteger;

  PLARGE_INTEGER = ^LARGE_INTEGER;
  PLargeInteger  = LPLARGE_INTEGER;

  LPULARGE_INTEGER = ^ULARGE_INTEGER;

  ULARGE_INTEGER = Windows.ULARGE_INTEGER;
  TULargeInteger = Windows.TULargeInteger;
  PULargeInteger = Windows.PULargeInteger;

  PULARGE_INTEGER = ^ULARGE_INTEGER;

  TIME  = LARGE_INTEGER;
  _TIME = _LARGE_INTEGER;
  PTIME = PLARGE_INTEGER;


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
  TNtOpenFile = function (FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; ShareAccess: ULONG; OpenOptions: ULONG): NTSTATUS; stdcall;
  
  TNtCreateFile = function(FileHandle: PHANDLE; DesiredAccess: ACCESS_MASK; ObjectAttributes: POBJECT_ATTRIBUTES; IoStatusBlock: PIO_STATUS_BLOCK; AllocationSize: PLARGE_INTEGER;
                           FileAttributes: ULONG; ShareAccess: ULONG; CreateDisposition: ULONG; CreateOptions: ULONG; EaBuffer: PVOID; EaLength: ULONG): NTSTATUS; stdcall;

  TNtQueryInformationFile = function (FileHandle: HANDLE; PIO_STATUS_BLOCK: TIoStatusBlock; FileInformation: PVOID; Length: ULONG; FileInformationClass: FILE_INFORMATION_CLASS): NTSTATUS; stdcall;


function NT_SUCCESS     (Status: NTSTATUS): boolean; inline;
function NT_INFORMATION (Status: NTSTATUS): boolean; inline;
function NT_WARNING     (Status: NTSTATUS): boolean; inline;
function NT_ERROR       (Status: NTSTATUS): boolean; inline;


(***)  implementation  (***)


function _UNICODE_STRING.GetLength: integer;
begin
  result := Self.Length shr 1;
end;

function NT_SUCCESS (Status: NTSTATUS): boolean; inline;
begin
  result := Status >= 0;
end;

function NT_INFORMATION (Status: NTSTATUS): boolean; inline;
begin
  result := (Status shr 30) = 1;
end;

function NT_WARNING (Status: NTSTATUS): boolean; inline;
begin
  result := (Status shr 30) = 2;
end;

function NT_ERROR (Status: NTSTATUS): boolean; inline;
begin
  result := (Status shr 30) = 3;
end;

end.