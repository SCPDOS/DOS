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
	dd if=./scpdos.sys of=./MyDiskDOS.ima bs=512 seek=100 conv=notrunc
	cp ./MyDiskDOS.ima ./MyDiskDOSMSD.ima

#Add a new boot sector to current image
loader:
	nasm loader.asm -o loader.bin -f bin -l loader.lst -O0
	dd if=loader.bin of=./MyDiskDOS.ima bs=512 count=1 conv=notrunc
	cp ./MyDiskDOS.ima ./MyDiskDOSMSD.ima

#Create a fresh disk image
fresh:
	dd if=/dev/zero of=./MyDiskDOS.IMA bs=512 count=2880 conv=notrunc

	nasm loader.asm -o loader.bin -f bin -l loader.lst -O0
	dd if=loader.bin of=./MyDiskDOS.ima bs=512 count=1 conv=notrunc

	dd if=./scpbios.bin of=./MyDiskDOS.ima bs=512 seek=33 conv=notrunc

	nasm scpdos.asm -o scpdos.bin -f bin -l scpdos.lst -O0
	mv ./scpdos.BIN ./scpdos.sys
	dd if=./scpdos.sys of=./MyDiskDOS.ima bs=512 seek=100 conv=notrunc
	cp ./MyDiskDOS.ima ./MyDiskDOSMSD.ima