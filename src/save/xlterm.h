/* xlterm.h - definitions for xlterm.c */

#ifndef __XLTERM_H__
#define __XLTERM_H__

#include "xlisp.h"
#include "xledit.h"


/* prototypes */
int ostgetc(void);
void ostputs(char *str);
void ostputc(int ch);
int ostatbol(void);
void ostflush(void);
int ostcheck(void);
void osflushoutput(void);

#endif