#!/bin/sh
# racket x86-example.rkt >/dev/null || exit 1
nasm -ggdb test.s -f elf -o test.o || exit 1
nasm -ggdb test-self-run.s -f elf -o test-self-run.o || exit 1
gcc -ggdb -m32 test-self-run.o test.o -o test || exit 1
./test || exit
