#!/bin/sh
#############################################################################
# A WSL makefile to assemble my file, and if successful, write it  
# to sector 88 (where bootsector is sector 0). We declare the space before 
# this file as reserved in the FAT, for SCPBIOS!
#############################################################################
# Assemble the file
assemble:
	cmd.exe /c jwasm -Sa -Fl=io.lst -bin io.asm
	mv ./io.BIN ./io.sys
# io.sys should start at sector 88
	dd if=./io.sys of=./MyDiskDOS.ima bs=512 seek=88 conv=notrunc
# Copy to make a fake USB device
	cp ./MyDiskDOS.ima ./MyDiskDOSMSD.ima