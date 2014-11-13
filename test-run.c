#include <stdio.h>
#include <stdint.h>

uint32_t fib(uint32_t n);

int main() {
	int i;
	for (i=0; i<10; i++) {
		printf("Fib %d\n", i);
		printf("is %d\n", fib(i));
	}
	return 0;
}
