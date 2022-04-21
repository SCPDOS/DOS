#!/bin/sh
#############################################################################
# A WSL makefile to assemble my file, and if successful, write it  
# to sector 100 (where bootsector is sector 0). We declare the space before 
# this file as reserved in the FAT, for SCPBIOS!
#############################################################################
# Assemble the file
# -Zp1 forces byte alignment of structures, -Sp1 forces segment byte alignment
#	cmd.exe /c uasm -Sa -Fl=scpdos.lst -bin scpdos.asm
assemble:
	nasm scpdos.asm -o scpdos.bin -f bin -l scpdos.lst -O0
	mv ./scpdos.BIN ./scpdos.sys
# io.sys should start at sector 88
	dd if=./scpdos.sys of=./MyDiskDOS.ima bs=512 seek=100 conv=notrunc
# Copy to make a fake USB device
	cp ./MyDiskDOS.ima ./MyDiskDOSMSD.ima