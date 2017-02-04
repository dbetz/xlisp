/* xlio - xlisp i/o routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* global variables */
xlFIXTYPE xlfsize;

/* external variables */
extern xlValue s_stdin,s_stdout,s_stderr,xlUnboundObject;

/* forward declarations */
static int fstream_getc(xlValue fptr);
static int ustream_getc(xlValue fptr);
static int ostream_getc(xlValue fptr);
static void fstream_putc(xlValue fptr,int ch);
static void ustream_putc(xlValue fptr,int ch);
static void ostream_putc(xlValue fptr,int ch);

/* xlGetC - get a character from a file or stream */
xlEXPORT int xlGetC(xlValue fptr)
{
    int flags,ch;

    /* check for input from nil */
    if (fptr == xlNil)
        return EOF;

    /* check for a buffered character */
    if ((ch = xlGetSaveCh(fptr)) != '\0') {
        xlSetSaveCh(fptr,'\0');
        return ch;
    }

    /* otherwise, dispatch on stream type */
    switch (xlNodeType(fptr)) {
    case xlFSTREAM:
        ch = fstream_getc(fptr);
        break;
    case xlUSTREAM:
        ch = ustream_getc(fptr);
        break;
    case xlOSTREAM:
        ch = ostream_getc(fptr);
        break;
    default:
        xlError("expecting stream",fptr);
        break;
    }

    /* set the beginning of line flag */
    if ((flags = xlGetPFlags(fptr)) & xlpfTERMINAL)
        xlSetPFlags(fptr,xlosConsoleAtBOLP() ? flags | xlpfBOL : flags & ~xlpfBOL);
    else
        xlSetPFlags(fptr,ch == '\n' ? flags | xlpfBOL : flags & ~xlpfBOL);

    /* return the character */
    return ch;
}

/* xlAtBOLP - check to see if a stream is at the beginning of a line */
xlEXPORT int xlAtBOLP(xlValue fptr)
{
    int flags;

    /* check for input from nil */
    if (fptr == xlNil)
        return TRUE;

    /* check for the console */
    if ((flags = xlGetPFlags(fptr)) & xlpfTERMINAL)
        return xlosConsoleAtBOLP();

    /* handle a normal stream */
    return (flags & xlpfBOL) != 0;
}

/* fstream_getc - port getc routine */
static int fstream_getc(xlValue fptr)
{    
    int flags,ch;
    
    /* check for terminal input or file input */
    if ((flags = xlGetPFlags(fptr)) & xlpfTERMINAL)
        ch = xlosConsoleGetC();
    else
        ch = getc(xlGetFile(fptr));
    
    /* return the character */
    return ch;
}

/* ustream_getc - unnamed stream getc routine */
static int ustream_getc(xlValue fptr)
{
    xlValue head = xlGetStrHead(fptr);
    xlFIXTYPE iptr;
    xlValue buf;
    int ch;
    
    /* check for no buffer */
    if (head == xlNil)
        ch = EOF;
    else {
        buf = xlCar(head);
        iptr = xlGetFixnum(xlGetStrIPtr(fptr));
        ch = xlGetString(buf)[iptr++];
        if (head == xlGetStrTail(fptr)) {
            if (iptr >= xlGetFixnum(xlGetStrOPtr(fptr))) {
                xlSetStrHead(fptr,xlNil);
                xlSetStrTail(fptr,xlNil);
                xlSetStrOPtr(fptr,xlMakeSmallFixnum(0));
                iptr = 0;
            }
        }
        else if (iptr >= xlGetSLength(buf)) {
            xlSetStrHead(fptr,xlCdr(head));
            iptr = 0;
        }
        xlSetStrIPtr(fptr,xlMakeFixnum(iptr));
    }
    return ch;
}

/* ostream_getc - object stream getc routine */
static int ostream_getc(xlValue fptr)
{
    xlValue val;
    xlInternalCall(&val,1,xlGetSObject(fptr),1,xlEnter("GETC"));
    if (!xlCharacterP(val))
        xlError("expecting a character",val);
    return xlGetChCode(val);
}

/* xlUngetC - unget a character */
xlEXPORT void xlUngetC(xlValue fptr,int ch)
{
    /* check for ungetc from nil */
    if (fptr == xlNil)
        return;
        
    /* otherwise, store the character */
    switch (xlNodeType(fptr)) {
    case xlFSTREAM:
    case xlUSTREAM:
    case xlOSTREAM:
        xlSetSaveCh(fptr,ch);
        break;
    default:
        xlError("expecting stream",fptr);
        break;
    }
}

/* xlPeek - peek at a character from a file or stream */
xlEXPORT int xlPeek(xlValue fptr)
{
    int ch = 0;

    /* check for input from nil */
    if (fptr == xlNil)
        return EOF;

    /* otherwise, dispatch on stream type */
    switch (xlNodeType(fptr)) {
    case xlFSTREAM:
    case xlUSTREAM:
    case xlOSTREAM:
        ch = xlGetC(fptr);
        xlSetSaveCh(fptr,ch);
        break;
    default:
        xlError("expecting stream",fptr);
        break;
    }

    /* return the character */
    return ch;
}

/* xlInputReadyP - check for a character from a file or stream */
xlEXPORT int xlInputReadyP(xlValue fptr)
{
    int sts = 0;

    /* check for input from nil */
    if (fptr == xlNil)
        return FALSE;

    /* otherwise, dispatch on stream type */
    switch (xlNodeType(fptr)) {
    case xlFSTREAM:
    case xlUSTREAM:
    case xlOSTREAM:
        if (xlGetSaveCh(fptr) != '\0')
            sts = TRUE;
        else if (xlGetPFlags(fptr) & xlpfTERMINAL)
            sts = xlosConsoleCheck() != 0;
        else
            sts = xlPeek(fptr) != EOF;
        break;
    default:
        xlError("expecting stream",fptr);
        break;
    }

    /* return the status */
    return sts;
}

/* xlFlushInput - flush buffered input from a file or stream */
xlEXPORT void xlFlushInput(xlValue fptr)
{
    xlValue tmp;
    
    /* check for input from nil */
    if (fptr == xlNil)
        return;

    /* otherwise, dispatch on stream type */
    switch (xlNodeType(fptr)) {
    case xlFSTREAM:
        xlSetSaveCh(fptr,'\0');
        if (xlGetPFlags(fptr) & xlpfTERMINAL)
            xlosConsoleFlush();
        break;
    case xlUSTREAM:
        xlSetSaveCh(fptr,'\0');
        xlSetStrHead(fptr,xlNil);
        xlSetStrTail(fptr,xlNil);
        xlSetStrIPtr(fptr,xlMakeSmallFixnum(0));
        xlSetStrOPtr(fptr,xlMakeSmallFixnum(0));
        break;
    case xlOSTREAM:
        xlSetSaveCh(fptr,'\0');
        xlInternalCall(&tmp,1,xlGetSObject(fptr),1,xlEnter("FLUSH"));
        break;
    default:
        xlError("expecting stream",fptr);
        break;
    }
}

/* xlPutC - put a character to a file or stream */
xlEXPORT void xlPutC(xlValue fptr,int ch)
{
    int flags;
    
    /* count the character */
    ++xlfsize;

    /* check for output to nil */
    if (fptr == xlNil)
        return;

    /* otherwise, check for output to an unnamed stream */
    switch (xlNodeType(fptr)) {
    case xlFSTREAM:
        fstream_putc(fptr,ch);
        break;
    case xlUSTREAM:
        ustream_putc(fptr,ch);
        break;
    case xlOSTREAM:
        ostream_putc(fptr,ch);
        break;
    default:
        xlError("expecting stream",fptr);
        break;
    }

    /* set the beginning of line flag */
    flags = xlGetPFlags(fptr);
    if (ch == '\n')
        flags |= xlpfBOL;
    else
        flags &= ~xlpfBOL;
    xlSetPFlags(fptr,flags);
}

/* fstream_putc - port putc */
static void fstream_putc(xlValue fptr,int ch)
{
    int flags;
    if ((flags = xlGetPFlags(fptr)) & xlpfTERMINAL)
        xlosConsolePutC(ch);
    else
        putc(ch,xlGetFile(fptr));
}

/* ustream_putc - unnamed stream putc */
static void ustream_putc(xlValue fptr,int ch)
{
    xlValue tail = xlGetStrTail(fptr);
    xlFIXTYPE optr;
    xlValue buf;
        
    /* check for no buffer */
    if (tail == xlNil) {
        xlCPush(fptr);
        buf = xlNewString(xlUSTRBUFSIZE);
        xlSetStrHead(fptr,xlCons(buf,xlNil));
        xlSetStrTail(fptr,xlGetStrHead(fptr));
        xlSetStrIPtr(fptr,xlMakeSmallFixnum(0));
        optr = 0;
        xlDrop(1);
    }
    else {
        buf = xlCar(tail);
        optr = xlGetFixnum(xlGetStrOPtr(fptr));
        if (optr >= xlGetSLength(buf)) {
            xlCPush(fptr);
            buf = xlNewString(xlUSTRBUFSIZE);
            xlSetCdr(tail,xlCons(buf,xlNil));
            xlSetStrTail(fptr,xlCdr(tail));
            optr = 0;
            xlDrop(1);
        }
    }
    
    /* put the next character in the stream */
    xlGetString(buf)[optr++] = ch;
    xlSetStrOPtr(fptr,xlMakeFixnum(optr));    
}

/* ostream_putc - object stream putc */
static void ostream_putc(xlValue fptr,int ch)
{
    xlValue tmp;
    xlInternalCall(&tmp,1,xlGetSObject(fptr),2,xlEnter("PUTC"),xlMakeChar(ch));
}

/* xlFlush - flush the input buffer */
xlEXPORT void xlFlush(void)
{
    xlValue port = xlCurInput();
    if (xlPortP(port))
        xlSetSaveCh(port,'\0');
    xlosConsoleFlush();
}

/* xlGetStrLength - get the length of an output stream */
xlEXPORT xlFIXTYPE xlGetStrLength(xlValue stream)
{
    xlFIXTYPE len,bufcount;
    if ((bufcount = xlLength(xlGetStrHead(stream))) == 0)
        len = 0;
    else if (bufcount == 1)
        len = xlGetFixnum(xlGetStrOPtr(stream))
            - xlGetFixnum(xlGetStrIPtr(stream));
    else
        len = (xlUSTRBUFSIZE - xlGetFixnum(xlGetStrIPtr(stream)))
            + (bufcount - 2) * xlUSTRBUFSIZE
            + xlGetFixnum(xlGetStrOPtr(stream));
    if (xlGetSaveCh(stream) != '\0')
        ++len;
    return len;
}

/* xlGetStrOutput - get the output stream string */
xlEXPORT xlValue xlGetStrOutput(xlValue stream)
{
    xlFIXTYPE length;
    char *dst;
    xlValue val;

    /* compute the length of the stream */
    length = xlGetStrLength(stream);
    
    /* create a new string */
    xlCPush(stream);
    val = xlNewString(length);
    xlDrop(1);
    
    /* copy the characters into the new string */
    dst = xlGetString(val);
    while (--length >= 0)
        *dst++ = xlGetC(stream);

    /* return the string */
    return val;
}

/* xlStdPrint - print to *standard-output */
xlEXPORT void xlStdPrint(xlValue expr)
{
    xlWrite(expr,xlCurOutput());
}

/* xlStdPutStr - print a string to *standard-output* */
xlEXPORT void xlStdPutStr(char *str)
{
    xlPutStr(xlCurOutput(),str);
}

/* xlErrPrint - print to *error-output* */
xlEXPORT void xlErrPrint(xlValue expr)
{
    xlWrite(expr,xlCurError());
}

/* xlErrPutStr - print a string to *error-output* */
xlEXPORT void xlErrPutStr(char *str)
{
    xlPutStr(xlCurError(),str);
}

