#include <stdio.h>
#include <stdint.h>

uint32_t ack(uint32_t m, uint32_t n);

int main() {
	int i, j;
	for (i=0; i<=3; i++) {
		for (j=0; j<=3; j++) {
			printf("Ack %d %d\n", i, j);
			printf("is %d\n", ack(i, j));
		}
	}
	return 0;
}
