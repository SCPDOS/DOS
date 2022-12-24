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
	nasm ./Source/scpdos.asm -o ./Binaries/scpdos.sys -f bin -l ./Listings/SCPDOS/scpdos.lst -O0v
	dd if=./Binaries/scpdos.sys of=./Images/MyDiskDOS.ima bs=512 seek=91 conv=notrunc
	cp ./Images/MyDiskDOS.ima ./Images/MyDiskDOSMSD.ima

#Add a new boot sector to current image
loader:
	nasm ./Source/Boot/loader.asm -o ./Binaries/loader.bin -f bin -l ./Listings/Boot/loader.lst -O0v
	dd if=./Binaries/loader.bin of=./Images/MyDiskDOS.ima bs=512 count=1 conv=notrunc
	cp ./Images/MyDiskDOS.ima ./Images/MyDiskDOSMSD.ima

#Create a fresh disk image
fresh:
	dd if=/dev/zero of=./Images/MyDiskDOS.IMA bs=512 count=2880 conv=notrunc

	nasm ./Source/Boot/loader.asm -o ./Binaries/loader.bin -f bin -l ./Listings/Boot/loader.lst -O0v
	dd if=./Binaries/loader.bin of=./Images/MyDiskDOS.ima bs=512 count=1 conv=notrunc

	dd if=./Binaries/scpbios.bin of=./Images/MyDiskDOS.ima bs=512 seek=33 conv=notrunc

	nasm ./Source/scpdos.asm -o ./Binaries/scpdos.sys -f bin -l ./Listings/SCPDOS/scpdos.lst -O0v
	dd if=./Binaries/scpdos.sys of=./Images/MyDiskDOS.ima bs=512 seek=91 conv=notrunc
	cp ./Images/MyDiskDOS.ima ./Images/MyDiskDOSMSD.ima
#Replace BIOS
bios:
	dd if=./Binaries/scpbios.bin of=./Images/MyDiskDOS.ima bs=512 seek=33 conv=notrunc
	cp ./Images/MyDiskDOS.ima ./Images/MyDiskDOSMSD.ima

#Build COMMAND.COM
command:
	nasm ./Source/Utils/COMMAND/command.asm -o ./Source/Utils/COMMAND/Binaries/command.com -f bin -l ./Source/Utils/COMMAND/Listings/command.lst -O0v

format:
	nasm ./Source/Utils/FORMAT/format.asm -o ./Source/Utils/FORMAT/Binaries/FORMAT.COM -f bin -l ./Source/Utils/FORMAT/Listings/format.lst -O0v
# Temp, to test Executable files
tst:
	nasm ./Source/Utils/EXETEST/tst.asm -o ./Source/Utils/EXETEST/Binaries/tst.obj -f win64 -l ./Source/Utils/EXETEST/Listings/tst.lst -O0v
#Link using VS Dev Console using good old MASM/VC LINK with 
#link tst.obj /entry:main /machine:x64