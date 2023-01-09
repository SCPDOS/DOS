build:
	x86_64-elf-gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c main.c -o main.obj -Wall -Wextra -fpie

	x86_64-pe-ld main.obj ./Lib/dos.lib ./Lib/bochs.lib

libraries:
	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c ./Lib/Source/dos.c -o ./Lib/scpdos.lib -Wall -Wextra -fpie

	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c ./Lib/Source/bochs.c -o ./Lib/bochs.lib -Wall -Wextra -fpie

all:
	x86_64-elf-gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c main.c -o main.obj -Wall -Wextra -fpie

	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c ./Lib/Source/dos.c -o ./Lib/dos.lib -Wall -Wextra -fpie

	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c ./Lib/Source/bochs.c -o ./Lib/bochs.lib -Wall -Wextra -fpie

	x86_64-pe-ld main.obj ./Lib/dos.lib ./Lib/bochs.lib