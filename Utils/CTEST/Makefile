build:
	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c main.c -o main.obj -Wall -Wextra -fpie

	ld -emain --oformat=pei-x86-64 main.obj ./Lib/scpdos.lib ./Lib/bochs.lib

libraries:
	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c ./Lib/Source/scpdos.c -o ./Lib/scpdos.lib -Wall -Wextra -fpie

	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c ./Lib/Source/bochs.c -o ./Lib/bochs.lib -Wall -Wextra -fpie

all:
	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c main.c -o main.obj -Wall -Wextra -fpie

	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c ./Lib/Source/scpdos.c -o ./Lib/scpdos.lib -Wall -Wextra -fpie

	gcc -m64 -masm=intel -mabi=ms -ffreestanding -mno-mmx -mno-sse -mno-sse2 -mno-red-zone -c ./Lib/Source/bochs.c -o ./Lib/bochs.lib -Wall -Wextra -fpie

	ld -emain --oformat=pei-x86-64 main.obj ./Lib/scpdos.lib ./Lib/bochs.lib