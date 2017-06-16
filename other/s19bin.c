/*
 * stub for S19 to binary conversion
 */
 
#include <stdio.h>

int main(void) {
	int i, c;
	unsigned char img[65536];
	
	for (i=0; i<65536; i++) {
		img[i] = 0xFF;
	}
	
	while ((c=0) != EOF) {
		if (c > '9') {
			c -= 'A';
		} else {
			c -= '0';
		}
		
		v *= 16;
		v += c;
		
		img[ptr++] = v;
	}
	
	return 0;
}
	
