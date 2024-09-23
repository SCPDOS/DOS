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
# Build four modules, then link them together, then strip headers.
# Build with all alignment of 1. Export nothing.
	nasm 
#############################################################################
# OLD FUNCTIONS BELOW
#############################################################################
Xassemble:
	nasm ./src/scpdos.asm -o ./bin/scpdos.sys -f bin -l ./lst/SCPDOS/scpdos.lst -O0v
	dd if=./bin/scpdos.sys of=./img/MyDiskDOS.ima bs=512 seek=91 conv=notrunc
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

#Add a new boot sector to current image
loader:
	nasm ./src/Boot/loader.asm -o ./bin/loader.bin -f bin -l ./lst/Boot/loader.lst -O0v
	dd if=./bin/loader.bin of=./img/MyDiskDOS.ima bs=512 count=1 conv=notrunc
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

loader32:
	nasm ./src/Boot/loader32.asm -o ./bin/loader32.bin -f bin -l ./lst/Boot/loader32.lst -O0v
	dd if=/dev/zero of=./img/MyDiskDOS32.ima bs=512 count=2880 conv=notrunc
	dd if=./bin/loader32.bin of=./img/MyDiskDOS32.ima bs=512 count=1 conv=notrunc

mbr:
	nasm ./src/Boot/mbr.asm -o ./bin/mbr.bin -f bin -l ./lst/Boot/mbr.lst -O0v

#Create a fresh disk image
fresh:
	dd if=/dev/zero of=./img/MyDiskDOS.IMA bs=512 count=2880 conv=notrunc

	nasm ./src/Boot/loader.asm -o ./bin/loader.bin -f bin -l ./lst/Boot/loader.lst -O0v
	dd if=./bin/loader.bin of=./img/MyDiskDOS.ima bs=512 count=1 conv=notrunc

	dd if=./bin/scpbios.sys of=./img/MyDiskDOS.ima bs=512 seek=33 conv=notrunc

	nasm ./src/scpdos.asm -o ./bin/scpdos.sys -f bin -l ./lst/SCPDOS/scpdos.lst -O0v
	dd if=./bin/scpdos.sys of=./img/MyDiskDOS.ima bs=512 seek=91 conv=notrunc
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima
#Replace BIOS
bios:
	dd if=./bin/scpbios.sys of=./img/MyDiskDOS.ima bs=512 seek=33 conv=notrunc
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

#Simply copies MyDiskDOS to the USB image
copy:
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

# Temp, to test Executable files
tst:
	nasm ./Utils/EXETEST/tst.asm -o ./Utils/EXETEST/bin/tst.obj -f win64 -l ./Utils/EXETEST/lst/tst.lst -O0v
#Link using VS Dev Console using good old MASM/VC LINK with 
#link tst.obj /entry:main /machine:x64
