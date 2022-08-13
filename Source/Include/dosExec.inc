;This include file contains structures related to the EXE format
; and the exec frame used when setting up the EXE in memory by 41h/4Bh

;Int 41h/4Bh stack frame, used to store values whilst processing a EXE or COM
; file.

struc execFrame
    .bSubFunc   resb 1  ;Subfunction number
    .wProgHdl   resb 2  ;File handle number for executable file
    .wEnvSize   resb 2  ;Size of the environment block
    .wNameLen   resb 2  ;Needs to be less than 64 but welp
                resb 1  ;Alignment byte
    .rbx:
    .pParam     resq 1  ;Parameter list pointer
    .rdx:
    .pProgname  resq 1  ;Ptr to ASCIIZ program name
    .dNewSize   resd 1  ;Size of the allocation for the new program
                resb 4  ;Alignment dword
    .pEnvBase   resq 1  ;Ptr to new Environment block 
    .pProgMCB   resq 1  ;Ptr to the program MCB
    .pProgBase  resq 1  ;Ptr to new program base address

endstruc

;---------------------------
;EXE header structures
;---------------------------

;DOS MZ .EXE header. Functionally useless except for e_magic and e_lfanew

struc imageDosHdr
    .e_magic    resw 1  ;      // Magic number
    .e_cblp     resw 1  ;      // Bytes on last page of file
    .e_cp       resw 1  ;      // Pages in file
    .e_crlc     resw 1  ;      // Relocations
    .e_cparhdr  resw 1  ;      // Size of header in paragraphs
    .e_minalloc resw 1  ;      // Minimum extra paragraphs needed
    .e_maxalloc resw 1  ;      // Maximum extra paragraphs needed
    .e_ss       resw 1  ;      // Initial (relative) SS value
    .e_sp       resw 1  ;      // Initial SP value
    .e_csum     resw 1  ;      // Checksum
    .e_ip       resw 1  ;      // Initial IP value
    .e_cs       resw 1  ;      // Initial (relative) CS value
    .e_lfarlc   resw 1  ;      // File address of relocation table
    .e_ovno     resw 1  ;      // Overlay number
    .e_res      resw 4  ;      // Reserved words
    .e_oemid    resw 1  ;      // OEM identifier (for e_oeminfo)
    .e_oeminfo  resw 1  ;      // OEM information; e_oemid specific
    .e_res2     resw 10 ;      // Reserved words
    .e_lfanew   resd 1  ;      // File address of new exe header
endstruc

;Pssobile signatures pointed to by e_lfanew
imageDosSignature   equ 05A4Dh      ; MZ, REJECT
imageOS2Signature   equ 0454Eh      ; NE, REJECT
imageOS2SignatureLE equ 0454Ch      ; LE, REJECt
imagePESignature    equ 00004550h   ; PE00, ACCEPT

;COFF File header, immadiately follows the signature so @ [e_lfanew + 4]

struc imageFileHeader
    .machineType        resw 1  ;Machine Type, must be imageFileMachineAMD64
    .numberOfSections   resw 1  ;Number of sections in file
    .timeDateStamp      resd 1  ;Unix datestamp
    .ptrToSymbolTbl     resd 1  ;Offset in the file to the symbol table
    .numberOfSymbols    resd 1  ;Number of symbols in the table
    .sizeOfOptionalHdr  resw 1  ;Size of the optional header
    .characteristics    resw 1  ;Bitfield characteristics
endstruc

;The only valid values in the .machineType field

imageFileMachineUnknown equ 0
imageFileMachineAMD64   equ 08664h 

;Optional header, immediately follows COFF header

struc imageFileOptionalHeader
	.mMagic                     resw 1 ;0x010b - PE32, 0x020b - PE32+ (64 bit)
	.mMajorLinkerVersion        resb 1
	.mMinorLinkerVersion        resb 1
	.mSizeOfCode                resd 1
	.mSizeOfInitializedData     resd 1
	.mSizeOfUninitializedData   resd 1
	.mAddressOfEntryPoint       resd 1
	.mBaseOfCode                resq 1
	.mSectionAlignment          resd 1
	.mFileAlignment             resd 1
	.mMajOSVer                  resw 1
	.mMinOSVer                  resw 1
	.mMajorImageVersion         resw 1
	.mMinorImageVersion         resw 1
	.mMajorSubsystemVersion     resw 1
	.mMinorSubsystemVersion     resw 1
	.mWin32VersionValue         resd 1
	.mSizeOfImage               resd 1
	.mSizeOfHeaders             resd 1
	.mCheckSum                  resd 1
	.mSubsystem                 resw 1
	.mDllCharacteristics        resw 1
	.mSizeOfStackReserve        resd 1
	.mSizeOfStackCommit         resd 1
	.mSizeOfHeapReserve         resd 1
	.mSizeOfHeapCommit          resd 1
	.mLoaderFlags               resd 1
	.mNumberOfRvaAndSizes       resd 1
endstruc

;Section header, start of every section as pointed to by the directories

struc imageSectionHdr       ;size 40 bytes
    .name                   resb 8
	.mVirtualSize           resd 1
	.mVirtualAddress        resd 1
	.mSizeOfRawData         resd 1
	.mPointerToRawData      resd 1
	.mPointerToRelocations  resd 1
	.mPointerToLinenumbers  resd 1
	.mNumberOfRelocations   resw 1
	.mNumberOfLinenumbers   resw 1
	.mCharacteristics       resd 1
endstruc
