---
title: Building a brainfuck to c compiler (in c)
date: January 11, 2013
tags: brainfuck, c
---

Brainfuck is a simple turing machine language. It's also  a turing tarpit - a language that is turing complete but just about impossible to write useful programs in.
It's operation is incredibly simple - imagine you have an infinite length of tape where each square contains a number. You have a "head" that sits on one of the squares.
The commands are as follows:

	< : move the head left one square
	> : move the head right one square
	+ : increment the head square
	- : decrement the head square
	. : print the head square
	\ : get a character from standard input
	[ : while the head square is not zero do
	] : end [ block

and that's all. The awesome part about this simplicity is that each command can be mapped exactly to a piece of c code. We need some memory so create an array of some fixed size. We also need a number to represent the head.

~~~~~{.c}
int memory[size];
int p = size/2;
~~~~~

Then the transformations are

~~~~~{.c}
< : if(p>0)p--;
> : if(p<size-1)p++;
+ : memory[p]++;
- : memory[p]--;
. : putchar(memory[p]);
\ : memory[p] = getchar();
[ : while(memory[p]!=0){
] : }
~~~~~

We need to limit the head movements so it doesn't go out of the memory. To compile a brainfuck file we just need to go through the characters and emit the right c code.
The completed code is on [github](https://github.com/WarwickMasson/brainfuck-to-c). This version properly formats the c code by adding indents.
