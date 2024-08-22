make:
	nasm -f elf64 out.asm -o out.o
	gcc -o out out.o -nostartfiles -no-pie
	./out
