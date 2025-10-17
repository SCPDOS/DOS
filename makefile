#!/bin/sh

ASM     := nasm
BINUTIL := x86_64-w64-mingw32
#BINUTIL := 
LINKER  := ${BINUTIL}-ld 
OBJCOPY := ${BINUTIL}-objcopy

LD_FLAGS := -T ./src/dos/scpdos.ld --no-check-sections --section-alignment=1 --file-alignment=1 --image-base=0x0 --disable-reloc-section -Map=./lst/SCPDOS/scpdos.map
#LD_FLAGS := -T ./src/dos/scpdos.ld --no-check-sections -Map=./lst/SCPDOS/scpdos.map
OC_FLAGS := --dump-section


all:
	$(MAKE) assemble
	$(MAKE) link
	$(MAKE) dos
#	$(MAKE) disk

world:
	$(MAKE) assemble
	$(MAKE) link
	$(MAKE) dos
	$(MAKE) disk
	$(MAKE) clean
	./build


assemble:
# Build four modules, then link them together, then strip headers.
# Build with all alignment of 1. Export nothing.
	${ASM} ./src/dos/Oeminit/oembuild.asm -o ./bin/oem.obj -f win64 -l ./lst/SCPDOS/oem.lst -O0v
	${ASM} ./src/dos/Sysinit/sysbuild.asm -o ./bin/sys.obj -f win64 -l ./lst/SCPDOS/sys.lst -O0v
	${ASM} ./src/dos/Kernel/dosbuild.asm -o ./bin/krn.obj -f win64 -l ./lst/SCPDOS/krn.lst -O0v
	${ASM} ./src/dos/Drivers/drvbuild.asm -o ./bin/drv.obj -f win64 -l ./lst/SCPDOS/drv.lst -O0v

link:
	${LINKER} ${LD_FLAGS} -o ./bin/dos.exe

dos:
	${OBJCOPY} ${OC_FLAGS} oem$$=./bin/oem.bin ./bin/dos.exe 
	${OBJCOPY} ${OC_FLAGS} sys$$=./bin/sys.bin ./bin/dos.exe 
	${OBJCOPY} ${OC_FLAGS} dos$$=./bin/krn.bin ./bin/dos.exe 
	${OBJCOPY} ${OC_FLAGS} drv$$=./bin/drv.bin ./bin/dos.exe 
	cat ./bin/oem.bin ./bin/sys.bin ./bin/krn.bin ./bin/drv.bin > ./bin/scpdos.sys 

clean:
	rm ./bin/*.bin ./bin/*.obj ./bin/dos.exe

disk:
	dd if=./bin/scpdos.sys of=./img/MyDiskDOS.ima bs=512 seek=91 conv=notrunc
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

#############################################################################
# OLD UTILITY FUNCTIONS BELOW
#############################################################################

#Add a new boot sector to current image
loader:
	${ASM} ./src/dos/Boot/loader.asm -o ./bin/loader.bin -f bin -l ./lst/Boot/loader.lst -O0v
	dd if=./bin/loader.bin of=./img/MyDiskDOS.ima bs=512 count=1 conv=notrunc
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

mbr:
	${ASM} ./src/dos/Boot/mbr.asm -o ./bin/mbr.bin -f bin -l ./lst/Boot/mbr.lst -O0v

#Create a fresh disk image
fresh:
	dd if=/dev/zero of=./img/MyDiskDOS.IMA bs=512 count=2880 conv=notrunc

	${ASM} ./src/dos/Boot/loader.asm -o ./bin/loader.bin -f bin -l ./lst/Boot/loader.lst -O0v
	dd if=./bin/loader.bin of=./img/MyDiskDOS.ima bs=512 count=1 conv=notrunc

	dd if=./bin/scpbios.sys of=./img/MyDiskDOS.ima bs=512 seek=33 conv=notrunc
	$(MAKE) world

#Replace BIOS
bios:
	dd if=./bin/scpbios.sys of=./img/MyDiskDOS.ima bs=512 seek=33 conv=notrunc
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

#Simply copies MyDiskDOS to the USB image
copy:
	cp ./img/MyDiskDOS.ima ./img/MyDiskDOSMSD.ima

# Temp, to test Executable files
tst:
	${ASM} ./Utils/EXETEST/tst.asm -o ./Utils/EXETEST/bin/tst.obj -f win64 -l ./Utils/EXETEST/lst/tst.lst -O0v
#Link using VS Dev Console using good old MASM/VC LINK with 
#link tst.obj /entry:main /machine:x64
