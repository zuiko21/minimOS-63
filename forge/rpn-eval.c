/*
 * RPN expression evaluator
 * (c) 2017 Carlos J. Santisteban
 * last modified 20170618-2255
 */

#include <stdio.h>

int main(void) {
	int ptr, x, y;

	while(ram[ptr] != '}') {
		x = getval(&ptr, &err);
		if (!err) {
			push(x); /* was a number */
		} else {
			switch (ram[ptr++]) {
			case '+':
				x = pop();
				x += pop();
				break;
			case '-':
				y = pop();
				x = pop() - y;
				break;
			case '*':
				x = pop();
				x *= pop();
				break;
			case '/':
				y = pop();
				x = pop() / y;
				break;
			case '&':
				x = pop();
				x &= pop();
				break;
			case '|':
				x = pop();
				x |= pop();
				break;
			case '%':
				y = pop();
				x = pop() % y;
				break;
			default:
				abort();
			}
			push(x); /* computed result */
		}
	}
	x = pop(); /* final result on top of stack */
	/* might check for an empty stack, otherwise error */
}
