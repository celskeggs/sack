#!/bin/sh
racket x86-example.rkt >/dev/null || exit 1
nasm -ggdb test.s -f elf -o test.o || exit 1
gcc -ggdb -m32 test-run.c -S -o test-run.s || exit 1
gcc -ggdb -m32 test-run.s -c -o test-run.o || exit 1
gcc -ggdb -m32 test-run.o test.o -o test || exit 1
./test || exit
