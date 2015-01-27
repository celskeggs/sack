#include <stdio.h>
#include <stdint.h>

extern uint32_t fib(uint32_t n);

int main() {
	int i;
	for (i=0; i<10; i++) {
		printf("fib(%d) = %d\n", i, fib(i));
	}
	return 0;
}
