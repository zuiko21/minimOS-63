/*
 * RPN expression evaluator
 * (c) 2017-2020 Carlos J. Santisteban
 * last modified 20170619-1445
 */

#include <stdio.h>

/* Global variables */

unsigned char ram[65536];
/* stack structure */
int stack[256];
int sp = 0;

void push(int x) {
	if (sp<256)	stack[sp++] = x;
	else		printf("\n*** FULL STACK ***\n");
}

int pop(void) {
	if (sp>0)	return stack[--sp];
	else		printf("\n*** EMPTY STACK ***\n");
	return 0;
}

bool isvalid(int *p) {
	bool result = FALSE;	/* bad by default */
	
	while (ram[*p]
	       
//**********************************************	       
int getval(int *p, bool *e) {
	int value = 0;
	*e = TRUE;	/* error unless some number detected */
	
	while (isvalid(*p)) {	/* valid cipher */
		*e = FALSE;	/* no longer an error */
		value *= 10;	/* previous cipher */
		value += ram[p]-'0';	/* add numeric value */
		*p++;			/* go for next, if possible */
	}
	
	return value;
}

int main(void) {
	int ptr = 0, x, y;
	
	scanf(">%s ", &(ram[ptr]));	/* input string */

	while (ram[ptr] != '}') {
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
