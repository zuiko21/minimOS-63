/*
 * RPN expression evaluator
 * (c) 2017 Carlos J. Santisteban
 * last modified 20170618-2209
 */

#include <stdio.h>

int main(void) {
	int i;

	while(ram[ptr] != '}') {
		x = getval(&ptr, &err);
		if (!err) {
			push(x); /* was a number */
		} else {
			switch (ram[ptr++]) {
			case '+':
				x = ();=();}
}
