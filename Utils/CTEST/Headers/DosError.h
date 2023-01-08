//
// Define the severity codes
//

#ifndef DOSERROR_H
#define DOSERROR_H


//
// MessageId: ERROR_SUCCESS
//
// MessageText:
//
// The operation completed successfully.
//
#define ERROR_SUCCESS   0L
#define NO_ERROR        0L

//
// MessageId: ERROR_INVALID_FUNCTION
//
// MessageText:
//
// Incorrect function.
//
#define ERROR_INVALID_FUNCTION           1L    // dderror

//
// MessageId: ERROR_FILE_NOT_FOUND
//
// MessageText:
//
// The system cannot find the file specified.
//
#define ERROR_FILE_NOT_FOUND             2L

//
// MessageId: ERROR_PATH_NOT_FOUND
//
// MessageText:
//
// The system cannot find the path specified.
//
#define ERROR_PATH_NOT_FOUND             3L

//
// MessageId: ERROR_TOO_MANY_OPEN_FILES
//
// MessageText:
//
// The system cannot open the file.
//
#define ERROR_TOO_MANY_OPEN_FILES        4L

//
// MessageId: ERROR_ACCESS_DENIED
//
// MessageText:
//
// Access is denied.
//
#define ERROR_ACCESS_DENIED              5L

//
// MessageId: ERROR_INVALID_HANDLE
//
// MessageText:
//
// The handle is invalid.
//
#define ERROR_INVALID_HANDLE             6L

//
// MessageId: ERROR_ARENA_TRASHED
//
// MessageText:
//
// The storage control blocks were destroyed.
//
#define ERROR_ARENA_TRASHED              7L

//
// MessageId: ERROR_NOT_ENOUGH_MEMORY
//
// MessageText:
//
// Not enough memory resources are available to process this command.
//
#define ERROR_NOT_ENOUGH_MEMORY          8L    // dderror

//
// MessageId: ERROR_INVALID_BLOCK
//
// MessageText:
//
// The storage control block address is invalid.
//
#define ERROR_INVALID_BLOCK              9L

//
// MessageId: ERROR_BAD_ENVIRONMENT
//
// MessageText:
//
// The environment is incorrect.
//
#define ERROR_BAD_ENVIRONMENT            10L

//
// MessageId: ERROR_BAD_FORMAT
//
// MessageText:
//
// An attempt was made to load a program with an incorrect format.
//
#define ERROR_BAD_FORMAT                 11L

//
// MessageId: ERROR_INVALID_ACCESS
//
// MessageText:
//
// The access code is invalid.
//
#define ERROR_INVALID_ACCESS             12L

//
// MessageId: ERROR_INVALID_DATA
//
// MessageText:
//
// The data is invalid.
//
#define ERROR_INVALID_DATA               13L

//
// MessageId: ERROR_OUTOFMEMORY
//
// MessageText:
//
// Not enough memory resources are available to complete this operation.
//
#define ERROR_OUTOFMEMORY                14L

//
// MessageId: ERROR_INVALID_DRIVE
//
// MessageText:
//
// The system cannot find the drive specified.
//
#define ERROR_INVALID_DRIVE              15L

//
// MessageId: ERROR_CURRENT_DIRECTORY
//
// MessageText:
//
// The directory cannot be removed.
//
#define ERROR_CURRENT_DIRECTORY          16L

//
// MessageId: ERROR_NOT_SAME_DEVICE
//
// MessageText:
//
// The system cannot move the file to a different disk drive.
//
#define ERROR_NOT_SAME_DEVICE            17L

//
// MessageId: ERROR_NO_MORE_FILES
//
// MessageText:
//
// There are no more files.
//
#define ERROR_NO_MORE_FILES              18L

//
// MessageId: ERROR_WRITE_PROTECT
//
// MessageText:
//
// The media is write protected.
//
#define ERROR_WRITE_PROTECT              19L

//
// MessageId: ERROR_BAD_UNIT
//
// MessageText:
//
// The system cannot find the device specified.
//
#define ERROR_BAD_UNIT                   20L

//
// MessageId: ERROR_NOT_READY
//
// MessageText:
//
// The device is not ready.
//
#define ERROR_NOT_READY                  21L

//
// MessageId: ERROR_BAD_COMMAND
//
// MessageText:
//
// The device does not recognize the command.
//
#define ERROR_BAD_COMMAND                22L

//
// MessageId: ERROR_CRC
//
// MessageText:
//
// Data error (cyclic redundancy check).
//
#define ERROR_CRC                        23L

//
// MessageId: ERROR_BAD_LENGTH
//
// MessageText:
//
// The program issued a command but the command length is incorrect.
//
#define ERROR_BAD_LENGTH                 24L

//
// MessageId: ERROR_SEEK
//
// MessageText:
//
// The drive cannot locate a specific area or track on the disk.
//
#define ERROR_SEEK                       25L

//
// MessageId: ERROR_NOT_DOS_DISK
//
// MessageText:
//
// The specified disk or diskette cannot be accessed.
//
#define ERROR_NOT_DOS_DISK               26L

//
// MessageId: ERROR_SECTOR_NOT_FOUND
//
// MessageText:
//
// The drive cannot find the sector requested.
//
#define ERROR_SECTOR_NOT_FOUND           27L

//
// MessageId: ERROR_OUT_OF_PAPER
//
// MessageText:
//
// The printer is out of paper.
//
#define ERROR_OUT_OF_PAPER               28L

//
// MessageId: ERROR_WRITE_FAULT
//
// MessageText:
//
// The system cannot write to the specified device.
//
#define ERROR_WRITE_FAULT                29L

//
// MessageId: ERROR_READ_FAULT
//
// MessageText:
//
// The system cannot read from the specified device.
//
#define ERROR_READ_FAULT                 30L

//
// MessageId: ERROR_GEN_FAILURE
//
// MessageText:
//
// A device attached to the system is not functioning.
//
#define ERROR_GEN_FAILURE                31L

//
// MessageId: ERROR_SHARING_VIOLATION
//
// MessageText:
//
// The process cannot access the file because it is being used by another process.
//
#define ERROR_SHARING_VIOLATION          32L

//
// MessageId: ERROR_LOCK_VIOLATION
//
// MessageText:
//
// The process cannot access the file because another process has locked a portion of the file.
//
#define ERROR_LOCK_VIOLATION             33L

//
// MessageId: ERROR_WRONG_DISK
//
// MessageText:
//
//
#define ERROR_WRONG_DISK                 34L

//
// MessageId: ERROR_SHARING_BUFFER_EXCEEDED
//
// MessageText:
//
// Too many files opened for sharing.
//
#define ERROR_SHARING_BUFFER_EXCEEDED    36L

//
// MessageId: ERROR_HANDLE_EOF
//
// MessageText:
//
// Reached the end of the file.
//
//#define ERROR_HANDLE_EOF                 38L

//
// MessageId: ERROR_HANDLE_DISK_FULL
//
// MessageText:
//
// The disk is full.
//
//#define ERROR_HANDLE_DISK_FULL           39L

//
// MessageId: ERROR_NOT_SUPPORTED
//
// MessageText:
//
// The request is not supported.
//
#define ERROR_NOT_SUPPORTED              50L

//
// MessageId: ERROR_REM_NOT_LIST
//
// MessageText:
//
#define ERROR_REM_NOT_LIST               51L

//
// MessageId: ERROR_DUP_NAME
//
// MessageText:
//
#define ERROR_DUP_NAME                   52L

//
// MessageId: ERROR_BAD_NETPATH
//
// MessageText:
//
// The network path was not found.
//
#define ERROR_BAD_NETPATH                53L

//
// MessageId: ERROR_NETWORK_BUSY
//
// MessageText:
//
// The network is busy.
//
#define ERROR_NETWORK_BUSY               54L

//
// MessageId: ERROR_DEV_NOT_EXIST
//
// MessageText:
//
// The specified network resource or device is no longer available.
//
#define ERROR_DEV_NOT_EXIST              55L    // dderror

//
// MessageId: ERROR_TOO_MANY_CMDS
//
// MessageText:
//
// The network BIOS command limit has been reached.
//
#define ERROR_TOO_MANY_CMDS              56L

//
// MessageId: ERROR_ADAP_HDW_ERR
//
// MessageText:
//
// A network adapter hardware error occurred.
//
#define ERROR_ADAP_HDW_ERR               57L

//
// MessageId: ERROR_BAD_NET_RESP
//
// MessageText:
//
// The specified server cannot perform the requested operation.
//
#define ERROR_BAD_NET_RESP               58L

//
// MessageId: ERROR_UNEXP_NET_ERR
//
// MessageText:
//
// An unexpected network error occurred.
//
#define ERROR_UNEXP_NET_ERR              59L

//
// MessageId: ERROR_BAD_REM_ADAP
//
// MessageText:
//
// The remote adapter is not compatible.
//
#define ERROR_BAD_REM_ADAP               60L

//
// MessageId: ERROR_PRINTQ_FULL
//
// MessageText:
//
// The printer queue is full.
//
#define ERROR_PRINTQ_FULL                61L

//
// MessageId: ERROR_NO_SPOOL_SPACE
//
// MessageText:
//
// Space to store the file waiting to be printed is not available on the server.
//
#define ERROR_NO_SPOOL_SPACE             62L

//
// MessageId: ERROR_PRINT_CANCELLED
//
// MessageText:
//
// Your file waiting to be printed was deleted.
//
#define ERROR_PRINT_CANCELLED            63L

//
// MessageId: ERROR_NETNAME_DELETED
//
// MessageText:
//
// The specified network name is no longer available.
//
#define ERROR_NETNAME_DELETED            64L

//
// MessageId: ERROR_NETWORK_ACCESS_DENIED
//
// MessageText:
//
// Network access is denied.
//
#define ERROR_NETWORK_ACCESS_DENIED      65L

//
// MessageId: ERROR_BAD_DEV_TYPE
//
// MessageText:
//
// The network resource type is not correct.
//
#define ERROR_BAD_DEV_TYPE               66L

//
// MessageId: ERROR_BAD_NET_NAME
//
// MessageText:
//
// The network name cannot be found.
//
#define ERROR_BAD_NET_NAME               67L

//
// MessageId: ERROR_TOO_MANY_NAMES
//
// MessageText:
//
// The name limit for the local computer network adapter card was exceeded.
//
#define ERROR_TOO_MANY_NAMES             68L

//
// MessageId: ERROR_TOO_MANY_SESS
//
// MessageText:
//
// The network BIOS session limit was exceeded.
//
#define ERROR_TOO_MANY_SESS              69L

//
// MessageId: ERROR_SHARING_PAUSED
//
// MessageText:
//
// The remote server has been paused or is in the process of being started.
//
#define ERROR_SHARING_PAUSED             70L

//
// MessageId: ERROR_REQ_NOT_ACCEP
//
// MessageText:
//
// No more connections can be made to this remote computer at this time because there are already as many connections as the computer can accept.
//
#define ERROR_REQ_NOT_ACCEP              71L

//
// MessageId: ERROR_REDIR_PAUSED
//
// MessageText:
//
// The specified printer or disk device has been paused.
//
#define ERROR_REDIR_PAUSED               72L

//
// MessageId: ERROR_FILE_EXISTS
//
// MessageText:
//
// The file exists.
//
#define ERROR_FILE_EXISTS                80L

//
// MessageId: ERROR_CANNOT_MAKE
//
// MessageText:
//
// The directory or file cannot be created.
//
#define ERROR_CANNOT_MAKE                82L

//
// MessageId: ERROR_FAIL_I44
//
// MessageText:
//
// Fail on INT 44.
//
#define ERROR_FAIL_I44                   83L

//
// MessageId: ERROR_OUT_OF_STRUCTURES
//
// MessageText:
//
// Storage to process this request is not available.
//
#define ERROR_OUT_OF_STRUCTURES          84L

//
// MessageId: ERROR_ALREADY_ASSIGNED
//
// MessageText:
//
// The local device name is already in use.
//
#define ERROR_ALREADY_ASSIGNED           85L

//
// MessageId: ERROR_INVALID_PASSWORD
//
// MessageText:
//
// The specified network password is not correct.
//
#define ERROR_INVALID_PASSWORD           86L

//
// MessageId: ERROR_INVALID_PARAMETER
//
// MessageText:
//
// The parameter is incorrect.
//
#define ERROR_INVALID_PARAMETER          87L    // dderror

//
// MessageId: ERROR_NET_WRITE_FAULT
//
// MessageText:
//
// A write fault occurred on the network.
//
#define ERROR_NET_WRITE_FAULT            88L

//
//
// Error Loci 
//
//

//
// Error location within DOS unknown
//
#define ERROR_LOCUS_UNKNOWN              01L

//
// Error was related to disk device
//
#define ERROR_LOCUS_DISK                 02L

//
// Error was related to network software
//
#define ERROR_LOCUS_NETWORK              03L

//
// Error was related to character device
//
#define ERROR_LOCUS_SERIAL               04L

//
// Error was related to system memory issues
// 
#define ERROR_LOCUS_MEMORY               05L

//
//
// Error Actions to be taken after error
//
//

//
// Retry the request
// 
#define ERROR_ACTION_RETRY               01L

//
// Retry the request after a delay
// 
#define ERROR_ACTION_DELAY_RETRY         02L

//
// Retry the request after data has been reinput
// 
#define ERROR_ACTION_RETRY_INPUT         03L

//
// Exit the program with resource cleanup
// 
#define ERROR_ACTION_ABORT_CLEANUP       04L

//
// Exit the program immediately without cleanup
// 
#define ERROR_ACTION_ABORT_NO_CLEANUP    05L

//
// Safe to ignore the error
// 
#define ERROR_ACTION_IGNORE              06L

//
// Retry the request after some intervention, like swapping removable devices
// 
#define ERROR_ACTION_RETRY_AFTER_INTERVENTION   07L


//
//
// Error Classes, indicating the severity of the error
//
//


//
// Out of resoureces, i.e. no more memory or free handles
// 
#define ERROR_CLASS_OUT_OF_RESOURCES    01L

//
// This error is temporary and should go away shortly
// 
#define ERROR_CLASS_TEMPORARY           02L

//
// Authorisation error, e.g. file permission related error
// 
#define ERROR_CLASS_AUTHORISATION       03L

//
// DOS has encountered an internal error or bug
// 
#define ERROR_CLASS_INTERNAL_DOS        04L

//
// The hardware is failing, shutdown immediately
// 
#define ERROR_CLASS_HARDWARE_FAULT      05L

//
// System failure class, e.g. missing configuration files
// 
#define ERROR_CLASS_SYSTEM_FAILURE      06L

//
// An application program error, i.e. inconsistent request made
// 
#define ERROR_CLASS_APPLICATION_ERROR   07L

//
// Resource not found (e.g. No free SFT handle)
// 
#define ERROR_CLASS_RESOURCE_NOT_FOUND  08L

//
// Data is in a bad format for processing
// 
#define ERROR_CLASS_BAD_DATA_FORMAT     09L

//
// The resource is currently locked
// 
#define ERROR_CLASS_RESOURCE_LOCKED     10L

//
// The error is due to an error on the medium being accessed
// 
#define ERROR_CLASS_ERROR_ON_MEDIA      11L

//
// The resource already exists!
// 
#define ERROR_CLASS_RESOURCE_EXISTS    12L

//
// Unknown Error class
// 
#define ERROR_CLASS_UNKNOWN    13L


#endif
