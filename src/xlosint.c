/* xlosint.c - operating system interface routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* global variables */
static xlCallbacks *callbacks = NULL;

/* xlSetCallbacks - initialize xlisp */
void xlSetCallbacks(xlCallbacks *cb)
{
    /* save the pointer to the callbacks */
    callbacks = cb;
}

/* xlosLoadPath - return the load path */
xlEXPORT char *xlosLoadPath(void)
{
    return callbacks->loadPath ? (*callbacks->loadPath)() : NULL;
}

/* xlosParsePath - return the load path */
xlEXPORT char *xlosParsePath(char **pp)
{
    return callbacks->parsePath ? (*callbacks->parsePath)(pp) : NULL;
}

/* xlosDirectorySeparator - return the directory separator character */
xlEXPORT int xlosDirectorySeparator(void)
{
    return callbacks->directorySeparator ? (*callbacks->directorySeparator)() : '\\';
}

/* xlosEnter - enter o/s specific functions */
void xlosEnter(void)
{
    xlSubrDef *sdp;
    xlXSubrDef *xsdp;
    for (sdp = xlosSubrTab; sdp->name != NULL; ++sdp)
        xlSubr(sdp->name,sdp->subr);
    for (xsdp = xlosXSubrTab; xsdp->name != NULL; ++xsdp)
        xlXSubr(xsdp->name,xsdp->subr);
}

/* xlosFindSubr - find an os specific function */
xlEXPORT xlValue (*xlosFindSubr(char *name))(void)
{
    xlSubrDef *sdp;
    xlXSubrDef *xsdp;

    /* find the built-in function */
    for (sdp = xlosSubrTab; sdp->name != NULL; ++sdp)
        if (strcmp(sdp->name,name) == 0)
            return sdp->subr;
    for (xsdp = xlosXSubrTab; xsdp->name != NULL; ++xsdp)
        if (strcmp(xsdp->name,name) == 0)
            return (xlValue (*)(void))xsdp->subr;

    /* call the user handler */
    return callbacks->findSubr ? (*callbacks->findSubr)(name) : NULL;
}

/* xlosError - print an error message */
xlEXPORT void xlosError(char *msg)
{
    if (callbacks->error)
        (*callbacks->error)(msg);
}

/* xlosFileModTime - return the modification time of a file */
xlEXPORT int xlosFileModTime(char *fname,xlFIXTYPE *pModTime)
{                        
    return callbacks->fileModTime ? (*callbacks->fileModTime)(fname,pModTime) : FALSE;
}

/* xlosConsoleGetC - get a character from the terminal */
xlEXPORT int xlosConsoleGetC(void)
{
    int ch;
    
    /* get the next character */
    ch = callbacks->consoleGetC ? (*callbacks->consoleGetC)() : EOF;

    /* output the character to the transcript file */
    if (xlTranscriptFP && ch != EOF)
        putc(ch,xlTranscriptFP);

    /* return the character */
    return ch;
}

/* xlosConsolePutC - put a character to the terminal */
xlEXPORT void xlosConsolePutC(int ch)
{
    /* check for control characters */
    xlosCheck();

    /* output the character */
    if (callbacks->consolePutC)
        (*callbacks->consolePutC)(ch);

    /* output the character to the transcript file */
    if (xlTranscriptFP)
        putc(ch,xlTranscriptFP);
}

/* xlosConsolePutS - output a string to the terminal */
xlEXPORT void xlosConsolePutS(char *str)
{
    while (*str)
        xlosConsolePutC(*str++);
}

/* xlosConsoleAtBOLP - are we at the beginning of a line? */
xlEXPORT int xlosConsoleAtBOLP(void)
{
    return callbacks->consoleAtBOLP ? (*callbacks->consoleAtBOLP)() : FALSE;
}

/* xlosConsoleFlush - flush the terminal input buffer */
xlEXPORT void xlosConsoleFlush(void)
{
    if (callbacks->consoleFlushInput)
        (*callbacks->consoleFlushInput)();
}

/* xlosConsoleCheck - check for control characters during execution */
xlEXPORT int xlosConsoleCheck(void)
{
    return callbacks->consoleCheck ? (*callbacks->consoleCheck)() : 0;
}

/* xlosFlushOutput - flush the output buffer */
xlEXPORT void xlosFlushOutput(void)
{
    if (callbacks->consoleFlushOutput)
        (*callbacks->consoleFlushOutput)();
}

/* xlosExit - exit from XLISP */
xlEXPORT void xlosExit(int sts)
{
    if (callbacks->exit)
        (*callbacks->exit)(sts);
}
