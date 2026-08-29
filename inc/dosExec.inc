;This include file contains structures related to the EXE format
; and the exec frame used when setting up the EXE in memory by 21h/4Bh

;Int 21h/4Bh stack frame, used to store values whilst processing a EXE or COM
; file.
execLoadGo  equ 0
execLoad    equ 1
execInvld   equ 2
execOverlay equ 3
execBkgrnd  equ 4

struc execFrame
    .oldRBP     dq ?  
    .bSubFunc   dq ?  ;Subfunction number
    .wProgHdl   dq ?  ;File handle number for executable file
    .wNameLen   dq ?  ;Needs to be less than 64 but welp
    .pParam     dq ?  ;Parameter list pointer
    .pProgname  dq ?  ;Ptr to ASCIIZ program name

    .pEnvBase   dq ?  ;Ptr to new Environment block, NOT THE MCB FOR ENV BLK
    .pPSPBase   dq ?  ;Ptr to new PSP base address
    .pProgBase  dq ?  ;Ptr to Prog. Base addr., ALIGNED FOR FIRST EXE SECTION
    .dProgSize  dq ?  ;Program size is saved here, PSP included.
    .dCOFFhdr   dq ?  ;Offset in file to COFF header. Set to 0 for COM
    .bSegCount  dq ?  ;Count of segments loaded into memory
    .wCOFFChars dq ?  ;Save the COFF characteristics in here
.pProgEP:   ;Program EP, recycle this qword
    .wNumSeg    dq ?  ;Number of segments, consider reusing NameLen
    .dFilePtr   dq ?  ;Keep of track of where in file we are
    .qRelocVal  dq ?  ;Used when computing relocations, Rebasing factor
endstruc

;Possible blocks to be pointed to by rbx

struc execProg 
    .pEnv       dq ?  ;Ptr to environment block (or 0 => copy parent env)
    .pCmdLine   dq ?  ;Ptr to the command line to be placed at PSP + 80h
    .pfcb1      dq ?  ;Ptr to the first FCB (parsed argument 1)
    .pfcb2      dq ?  ;Ptr to the second FCB  (parsed argument 2)
endstruc

struc loadProg
    .pEnv       dq ?  ;Ptr to environment block (or 0 => copy parent env)
    .pCmdLine   dq ?  ;Ptr to the command line to be placed at PSP + 80h
    .pfcb1      dq ?  ;Ptr to the first FCB (parsed argument 1)
    .pfcb2      dq ?  ;Ptr to the second FCB  (parsed argument 2)
    .initRSP    dq ?  ;Return the start value for rsp
    .initRIP    dq ?  ;Return the entry point for the task
endstruc

struc loadOvly
    .pLoadLoc   dq ?  ;Address in memory to load the overlay
    .qRelocFct  dq ?  ;Relocation address for relocatable items (0 for COM files)
endstruc

;----------------------------
;   EXE header structures   :
;----------------------------

;DOS MZ .EXE header. Functionally useless except for e_magic and e_lfanew
dosMagicSignature   equ "MZ"
dosMagicSignature2  equ "ZM"
struc imageDosHdr
    .e_magic    dw ?  ;      // Magic number
    .e_cblp     dw ?  ;      // Bytes on last page of file
    .e_cp       dw ?  ;      // Pages in file
    .e_crlc     dw ?  ;      // Relocations
    .e_cparhdr  dw ?  ;      // Size of header in paragraphs
    .e_minalloc dw ?  ;      // Minimum extra paragraphs needed
    .e_maxalloc dw ?  ;      // Maximum extra paragraphs needed
    .e_ss       dw ?  ;      // Initial (relative) SS value
    .e_sp       dw ?  ;      // Initial SP value
    .e_csum     dw ?  ;      // Checksum
    .e_ip       dw ?  ;      // Initial IP value
    .e_cs       dw ?  ;      // Initial (relative) CS value
    .e_lfarlc   dw ?  ;      // File address of relocation table
    .e_ovno     dw ?  ;      // Overlay number
    .e_res      dw 4 dup (?)    ;      // Reserved words
    .e_oemid    dw ?  ;      // OEM identifier (for e_oeminfo)
    .e_oeminfo  dw ?  ;      // OEM information; e_oemid specific
    .e_res2     dw 10 dup (?)   ;      // Reserved words
    .e_lfanew   dd ?  ;      // File address of new exe header
endstruc

;Pssobile signatures pointed to by e_lfanew
imageDosSignature   equ 05A4Dh      ; MZ, REJECT
imageOS2Signature   equ 0454Eh      ; NE, REJECT
imageOS2SignatureLE equ 0454Ch      ; LE, REJECT
imagePESignature    equ 00004550h   ; PE00, ACCEPT

;COFF File header @ [e_lfanew]

struc imageFileHeader
    .dPESignature        dd ?  ;PESignature, must be PE00
    .wMachineType        dw ?  ;Machine Type, must be imageFileMachineAMD64
    .wNumberOfSections   dw ?  ;Number of sections in file
    .dTimeDateStamp      dd ?  ;Unix datestamp
    .dPtrToSymbolTbl     dd ?  ;Pointer to the symbol table,      NOT USED
    .dNumberOfSymbols    dd ?  ;Number of symbols in the table,   NOT USED
    .wSizeOfOptionalHdr  dw ?  ;Size of the optional header
    .wCharacteristics    dw ?  ;Bitfield characteristics
endstruc

;The only valid values in the .machineType field

imageFileMachineUnknown equ 0
imageFileMachineAMD64   equ 08664h 

;Characteristics to check for
imageFileRelocsStripped equ 0001h   ;If set, base addr MUST equal load addr
imageFileExecutable     equ 0002h   ;Must always be set else badfmterr

;Optional header, immediately follows COFF header

struc imageFileOptionalHeader
	.wMagic                     dw ? ;0x010b - PE32, 0x020b - PE32+ (64 bit)
	.bMajorLinkerVersion        db ?
	.bMinorLinkerVersion        db ?
	.dSizeOfCode                dd ?
	.dSizeOfInitializedData     dd ?
	.dSizeOfUninitializedData   dd ?
	.dAddressOfEntryPoint       dd ?  ;RVA of the EP for program
	.dBaseOfCode                dd ?
    .qImageBase                 dq ?  ;Preferred load address  
	.dSectionAlignment          dd ?
	.dFileAlignment             dd ?
	.wMajOSVer                  dw ?
	.wMinOSVer                  dw ?
	.wMajorImageVersion         dw ?
	.wMinorImageVersion         dw ?
	.wMajorSubsystemVersion     dw ?
	.wMinorSubsystemVersion     dw ?
	.dWin32VersionValue         dd ?
	.dSizeOfImage               dd ?  ;Size of ctg mem to hold program
	.dSizeOfHeaders             dd ?
	.dCheckSum                  dd ?
	.wSubsystem                 dw ?
	.wDllCharacteristics        dw ?
	.qSizeOfStackReserve        dq ?
	.qSizeOfStackCommit         dq ?  ;Default stack allocation is 256Kb
	.qSizeOfHeapReserve         dq ?
	.qSizeOfHeapCommit          dq ?
	.dLoaderFlags               dd ?
	.dNumberOfRvaAndSizes       dd ?  ;Number of data dirs entries in header
endstruc

romMagicNum     equ 0107h
pe32MagicNum    equ 010Bh
pe64MagicNum    equ 020Bh

;Data directory header structure

struc imageDataDirectory
    .virtualAddress  dd ?
    .size    dd ?
endstruc

struc baseRelocBlock
    .pageRVA    dd ?  ;Offset from the load address
    .size       dd ?  ;Number of bytes in this block
    ;Each entry is a word. High 4 bits are type of reloc (all must be 0Ah)
endstruc
;Section headers that form the Section Table.
;There are wNumberOfSections many entries in this table.
;Each entry is one imageSectionHdr

struc imageSectionHdr       ;size 40 bytes
    .qName                  db 8 dup (?)
	.dVirtualSize           dd ?  ;If this is bigger than RawData, 0 pad
	.dVirtualAddress        dd ?  ;Offset from ImageBase
	.dSizeOfRawData         dd ?
	.dPointerToRawData      dd ?  ;File pointer location
	.dPointerToRelocations  dd ?
	.dPointerToLinenumbers  dd ?
	.wNumberOfRelocations   dw ?
	.wNumberOfLinenumbers   dw ?
	.dCharacteristics       dd ?
endstruc

;Section Hdr characteristics flags 
imgScnCntCode   equ 20h   ;The section contains executable code.
imgScnCntData   equ 40h   ;The section contains initialized data.
imgScnCntBSS    equ 80h   ;The section contains uninitialized data. 