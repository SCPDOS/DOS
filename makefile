#!/bin/sh

ASM     := nasm
BINUTIL := x86_64-w64-mingw32
LINKER  := ${BINUTIL}-ld 

#LD_FLAGS := -T ./src/scpdos.ld -r -n -N -nostdlib --section-alignment=1 --file-alignment=1 --image-base=0x0 --enable-reloc-section -Map=./lst/SCPDOS/tmp/dos.map
LD_FLAGS := -T ./src/scpdos.ld --section-alignment=1 --file-alignment=1 --image-base=0x0 --disable-reloc-section -Map=./lst/SCPDOS/tmp/dos.map

all:
	$(MAKE) assemble
	$(MAKE) link
	$(MAKE) dos
#	$(MAKE) disk

assemble:
# Build four modules, then link them together, then strip headers.
# Build with all alignment of 1. Export nothing.
	${ASM} ./src/Oeminit/oembuild.asm -o ./bin/tmp/oem.obj -f win64 -l ./lst/SCPDOS/tmp/oem.lst -O0v
	${ASM} ./src/Sysinit/sysbuild.asm -o ./bin/tmp/sys.obj -f win64 -l ./lst/SCPDOS/tmp/sys.lst -O0v
	${ASM} ./src/Kernel/dosbuild.asm -o ./bin/tmp/krn.obj -f win64 -l ./lst/SCPDOS/tmp/krn.lst -O0v
	${ASM} ./src/Drivers/drvbuild.asm -o ./bin/tmp/drv.obj -f win64 -l ./lst/SCPDOS/tmp/drv.lst -O0v

link:
	${LINKER} ${LD_FLAGS} -o ./bin/tmp/dos.exe

dos:
	${BINUTIL}-objcopy --dump-section oem$$=./bin/tmp/oem.bin ./bin/tmp/dos.exe 
	${BINUTIL}-objcopy --dump-section sys$$=./bin/tmp/sys.bin ./bin/tmp/dos.exe 
	${BINUTIL}-objcopy --dump-section dos$$=./bin/tmp/krn.bin ./bin/tmp/dos.exe 
	${BINUTIL}-objcopy --dump-section drv$$=./bin/tmp/drv.bin ./bin/tmp/dos.exe 
	cat ./bin/tmp/oem.bin ./bin/tmp/sys.bin ./bin/tmp/krn.bin ./bin/tmp/drv.bin > ./bin/scpdos.sys 

clean:
	rm ./bin/tmp/*.bin ./bin/tmp/*.obj ./bin/tmp/dos.exe

disk:
	dd if=./bin/scpdos.sys of=./img/MyDiskDOS.ima bs=512 seek=91 conv=notrunc
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

#############################################################################
# OLD UTILITY FUNCTIONS BELOW
#############################################################################

#Add a new boot sector to current image
loader:
	nasm ./src/Boot/loader.asm -o ./bin/loader.bin -f bin -l ./lst/Boot/loader.lst -O0v
	dd if=./bin/loader.bin of=./img/MyDiskDOS.ima bs=512 count=1 conv=notrunc
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

mbr:
	nasm ./src/Boot/mbr.asm -o ./bin/mbr.bin -f bin -l ./lst/Boot/mbr.lst -O0v

#Create a fresh disk image
fresh:
	dd if=/dev/zero of=./img/MyDiskDOS.IMA bs=512 count=2880 conv=notrunc

	nasm ./src/Boot/loader.asm -o ./bin/loader.bin -f bin -l ./lst/Boot/loader.lst -O0v
	dd if=./bin/loader.bin of=./img/MyDiskDOS.ima bs=512 count=1 conv=notrunc

	dd if=./bin/scpbios.sys of=./img/MyDiskDOS.ima bs=512 seek=33 conv=notrunc
	$(MAKE) all

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
