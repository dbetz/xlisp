/* xlisp.c - a simple xlisp main program */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* main - the main routine */
int main(int argc,char *argv[])
{
    xlCallbacks *callbacks = xlDefaultCallbacks(argv[0]);
    xlInit(callbacks,argc,argv,NULL);
    xlInfo("%s",xlBanner());
    xlLoadFile("xlisp.lsp");
    xlCallFunctionByName(NULL,0,"*TOPLEVEL*",0);
    return 0;
}
