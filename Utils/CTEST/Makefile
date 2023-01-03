build:
	x86_64-elf-gcc -m64 -masm=intel -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c main.c -o main.obj -Wall -Wextra -fpie

	x86_64-pe-ld main.obj