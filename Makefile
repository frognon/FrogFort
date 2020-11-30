all:
	nasm -f bin boot.asm -o boot.bin
	nasm -f bin frogfort.asm -o frogfort.bin
	cat boot.bin frogfort.bin > ff.bin