#!/bin/sh
#############################################################################
# A WSL makefile to assemble my file, and if successful, write it  
# to sector 88 (where bootsector is sector 0). We declare the space before 
# this file as reserved in the FAT, for SCPBIOS!
#############################################################################
# Assemble the file
# -Zp1 forces byte alignment of structures, -Sp1 forces segment byte alignment
assemble:
	cmd.exe /c uasm -Sa -Fl=scpdos.lst -bin scpdos.asm
	mv ./scpdos.BIN ./scpdos.sys
# io.sys should start at sector 88
	dd if=./scpdos.sys of=./MyDiskDOS.ima bs=512 seek=88 conv=notrunc
# Copy to make a fake USB device
	cp ./MyDiskDOS.ima ./MyDiskDOSMSD.ima