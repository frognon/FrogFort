all:
	nasm -O0 -f bin boot.asm -o boot.bin
	nasm -O0 -f bin frogfort.asm -o frogfort.bin
	cat boot.bin frogfort.bin > ff.img
