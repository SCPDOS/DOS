;This include file contains structures related to the EXE format and the 
; structures related to the EXEC (21h/4Bh) system call.

;EXEC valid subfunctions
execLoadGo  equ 0
execLoad    equ 1
execInvld   equ 2
execOverlay equ 3
execBkgrnd  equ 4

;Possible blocks to be pointed to by rbx on entry to EXEC system call.

struc execProg 
    .pEnv       dq ?  ; Ptr to environment block (or 0 => copy parent env)
    .pCmdLine   dq ?  ; Ptr to the command line to be placed at PSP + 80h
    .pfcb1      dq ?  ; Ptr to the first FCB (parsed argument 1)
    .pfcb2      dq ?  ; Ptr to the second FCB  (parsed argument 2)
endstruc

struc loadProg
    .pEnv       dq ?  ; Ptr to environment block (or 0 => copy parent env)
    .pCmdLine   dq ?  ; Ptr to the command line to be placed at PSP + 80h
    .pfcb1      dq ?  ; Ptr to the first FCB (parsed argument 1)
    .pfcb2      dq ?  ; Ptr to the second FCB  (parsed argument 2)
    .initRSP    dq ?  ; Return the start value for rsp
    .initRIP    dq ?  ; Return the entry point for the task
endstruc

struc loadOvly
    .pLoadLoc   dq ?  ; Address in memory to load the overlay
    .qRelocFct  dq ?  ; Relocation address for relocatable items (0 for COM files)
endstruc

;----------------------------
;   EXE header structures   :
;----------------------------

;PE Headers have the following structure:
; 1) MZ Header
; 2) MZ executable
; 3) COFF Header
; 4) COFF Optional Header, if COFF Header.wSizeOfOptionalHdr ! = 0
; 5) Data directories, if COFF Optional Header.wSizeOfOptionalHdr ! = 0
; 6) Section Table, if COFF Header.wNumberOfSections != 0
; 7) Padding, if COFF Optional Header(.dSizeOfHeaders != .dSectionAlignment)

;The COFF headers + the data directories form the magical ``PE'' header.

;DOS MZ .EXE header. Functionally useless except for e_magic and e_lfanew
dosMagicSignature   equ "MZ"
dosMagicSignature2  equ "ZM"
struc imageDosHdr
    .e_magic    dw ?            ;Magic number
    .e_cblp     dw ?            ;Bytes on last page of file
    .e_cp       dw ?            ;Pages in file (page = 512 bytes in MSDOS)
    .e_crlc     dw ?            ;Relocations
    .e_cparhdr  dw ?            ;Size of header in paragraphs
    .e_minalloc dw ?            ;Minimum extra paragraphs needed
    .e_maxalloc dw ?            ;Maximum extra paragraphs needed
    .e_ss       dw ?            ;Initial (relative) SS value
    .e_sp       dw ?            ;Initial SP value
    .e_csum     dw ?            ;Checksum
    .e_ip       dw ?            ;Initial IP value
    .e_cs       dw ?            ;Initial (relative) CS value
    .e_lfarlc   dw ?            ;File address of relocation table
    .e_ovno     dw ?            ;Overlay number
    .e_res      dw 4 dup (?)    ;Reserved words
    .e_oemid    dw ?            ;OEM identifier (for e_oeminfo)
    .e_oeminfo  dw ?            ;OEM information; e_oemid specific
    .e_res2     dw 10 dup (?)   ;Reserved words
    .e_lfanew   dd ?            ;File address of new exe header
endstruc

;Possible signatures pointed to by e_lfanew
imageDosSignature   equ 05A4Dh      ;MZ, REJECT
imageOS2Signature   equ 0454Eh      ;NE, REJECT
imageOS2SignatureLE equ 0454Ch      ;LE, REJECT
imagePESignature    equ 00004550h   ;PE00, ACCEPT

;COFF File header @ [e_lfanew]

struc imageFileHeader
    .dPESignature        dd ?   ;PESignature, must be PE00
    .wMachineType        dw ?   ;Machine Type, must be imageFileMachineAMD64
    .wNumberOfSections   dw ?   ;Number of sections in file
    .dTimeDateStamp      dd ?   ;Unix datestamp,                 NOT USED
    .dPtrToSymbolTbl     dd ?   ;Pointer to the symbol table,    NOT USED
    .dNumberOfSymbols    dd ?   ;Number of symbols in the table, NOT USED
    .wSizeOfOptionalHdr  dw ?   ;Size of the optional header
    .wCharacteristics    dw ?   ;Bitfield characteristics
endstruc
coffHdr_size equ imageFileHeader_size
;The only valid values in the .machineType field
imageFileMachineUnknown equ 0
imageFileMachineAMD64   equ 08664h 

;Characteristics to check for
imageFileRelocsStripped equ 0001h   ;If set, base addr MUST equal load addr
imageFileExecutable     equ 0002h   ;Must always be set else badfmterr

;COFF Optional header

struc imageFileOptionalHeader
	.wMagic                     dw ?    ;peMagicNumber (must be 020Bh).
	.bMajorLinkerVersion        db ?    ;Not used.
	.bMinorLinkerVersion        db ?    ;Not used.
	.dSizeOfCode                dd ?    ;Size of Code.
	.dSizeOfInitializedData     dd ?    ;Size of DATA.
	.dSizeOfUninitializedData   dd ?    ;Size of BSS.
	.dAddressOfEntryPoint       dd ?    ;Relative address of code EP.
	.dBaseOfCode                dd ?    ;Distance from qImageBase to code seg.
    .qImageBase                 dq ?    ;Preferred load address.
	.dSectionAlignment          dd ?    ;Alignment of start of segs in mem.
	.dFileAlignment             dd ?    ;Alignment of start of segs in file.
	.wMajOSVer                  dw ?    ;Not used.
	.wMinOSVer                  dw ?    ;Not used.
	.wMajorImageVersion         dw ?    ;Not used.
	.wMinorImageVersion         dw ?    ;Not used.
	.wMajorSubsystemVersion     dw ?    ;Not used.
	.wMinorSubsystemVersion     dw ?    ;Not used.
	.dWin32VersionValue         dd ?    ;Not used.
	.dSizeOfImage               dd ?    ;Total aligned in memory alloc size.
	.dSizeOfHeaders             dd ?    ;Rounded to file alignment.
	.dCheckSum                  dd ?    ;Not used.
	.wSubsystem                 dw ?    ;Not used. For VMEMD.
	.wDllCharacteristics        dw ?
	.qSizeOfStackReserve        dq ?    ;Not used by default. For VMEMD.
	.qSizeOfStackCommit         dq ?    ;Default stack allocation is 256Kb
	.qSizeOfHeapReserve         dq ?    ;Not used by default. For VMEMD.
	.qSizeOfHeapCommit          dq ?    ;Not used by default. For VMEMD.
	.dLoaderFlags               dd ?
	.dNumberOfRvaAndSizes       dd ?    ;Number of data dirs entries in header
endstruc

optCoffHdr_size equ imageFileOptionalHeader_size
coffHdrs_size equ coffHdr_size + optCoffHdr_size

romMagicNum     equ 0107h
pe32MagicNum    equ 010Bh
pe64MagicNum    equ 020Bh

;Data directory header structure
struc imageDataDirectory
    .dVAddr dd ?
    .dSize  dd ?    ;Directory size
endstruc

dataDir_eData   equ 1   ;Export Table directory entry
dataDir_iData   equ 2   ;Import Table directory entry
dataDir_rsrc    equ 3   ;Resource Table directory entry
dataDir_pData   equ 4   ;Exception Table directory entry
dataDir_reloc   equ 6   ;Base relocation Table directory entry
dataDir_debug   equ 7   ;Debug Table directory entry

struc baseRelocBlock    
;Struct for entries in the base reloc table. 
; Each table entry points to a word. 
; High 4 bits are type of reloc (all must be 0Ah)
    .dPageRVA   dd ?    ;Offset from the load address
    .dSize      dd ?    ;Number of bytes in this block
endstruc

;Section headers that form the Section Table after the data dir tables.
;There are wNumberOfSections many entries in this table.
;Each entry is one imageSectionHdr
struc imageSectionHdr       ;size 40 bytes
    .qName                  db 8 dup (?)
	.dVirtualSize           dd ?    ;Size of data in section
	.dVirtualAddress        dd ?    ;Offset of section from load address
	.dSizeOfRawData         dd ?    ;Size of section in image (File Aligned)
	.dPointerToRawData      dd ?    ;Offset of section from head of image
	.dPointerToRelocations  dd ?
	.dPointerToLinenumbers  dd ?
	.wNumberOfRelocations   dw ?
	.wNumberOfLinenumbers   dw ?
	.dCharacteristics       dd ?
endstruc

;Section Hdr characteristics flags 
imgScnCntCode   equ 20h   ; The section contains executable code.
imgScnCntData   equ 40h   ; The section contains initialized data.
imgScnCntBSS    equ 80h   ; The section contains uninitialized data. 
