/* xlfasl.c - fast load file handler */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* external variables */
extern xlValue xlEofObject;

/* prototypes */
static int faslwritecode(xlValue fptr,xlValue code);
static xlValue faslreadprocedure(xlValue fptr);
static xlValue faslreadexpr(xlValue fptr);
static xlValue faslread(xlValue fptr);
static xlValue faslreadcode(xlValue fptr);
static int gethexbyte(xlValue fptr);
static int faslskip(xlValue fptr);
static int faslgetc(xlValue fptr);
static void fasleof(void);

/* xloadfaslfile - built-in function 'load-fasl-file' */
xlValue xloadfaslfile(void)
{
    FILE *fp;

    /* get the name of the file to load */
    xlVal = xlGetArgString();
    xlLastArg();

    /* create a file object */
    xlCPush(xlMakeFileStream(NULL,xlpfINPUT));
    if ((fp = xlLoadOpen(xlGetString(xlVal),"r","*LOAD-PATH*",NULL)) == NULL) {
        xlDrop(1);
        return xlNil;
    }
    xlSetSData(xlTop(),fp);

    /* load and evaluate each expression in the file */
    while ((xlVal = faslreadprocedure(xlTop())) != xlEofObject)
        xlCallFunction(NULL,0,xlVal,0);

    /* close the fasl file */
    xlosClose(fp);
    xlSetSData(xlPop(),NULL);

    /* return successfully */
    return xlTrue;
}

/* xfaslwriteprocedure - write a procedure to a fasl file */
xlValue xfaslwriteprocedure(void)
{
    xlValue fun,fptr;

    /* parse the argument list */
    fun = xlGetArgClosure();
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();

    /* write out the code portion of the closure */
    return faslwritecode(fptr,xlGetCode(fun)) ? xlTrue : xlFalse;
}

/* faslwritecode - write a code object to a fasl file */
static int faslwritecode(xlValue fptr,xlValue code)
{
    xlValue bytecodes = xlGetBCode(code);
    long nbytes = xlGetSLength(bytecodes);
    long nlits = xlGetSize(code);
    char buf[100];
    long i;
    
    /* write the function name */
    xlPutStr(fptr,"C ");
    xlWrite(xlGetCName(code),fptr);
    xlPutStr(fptr," ");

    /* write the required argument names */
    xlWrite(xlGetVNames(code),fptr);
    xlNewline(fptr);

    /* write the code object size and number of bytecodes */
    sprintf(buf,"%ld %ld\n",nlits - xlFIRSTLIT,nbytes);
    xlPutStr(fptr,buf);

    /* write the bytecodes */
    for (i = 0; i < nbytes; ) {
        sprintf(buf,"%02x ",xlGetString(bytecodes)[i]);
        xlPutStr(fptr,buf);
        if ((++i % 16) == 0)
            xlNewline(fptr);
    }
    if ((i % 16) != 0)
        xlNewline(fptr);

    /* write the literals */
    for (i = xlFIRSTLIT; i < nlits; ++i) {
        xlValue lit = xlGetElement(code,i);
        if (xlCodeP(lit))
            faslwritecode(fptr,lit);
        else {
            xlPutStr(fptr,"D ");
            xlWrite(lit,fptr);
            xlNewline(fptr);
        }
    }

    /* return successfully */
    return TRUE;
}

/* xfaslreadprocedure - read a procedure from a fasl file */
xlValue xfaslreadprocedure(void)
{
    xlValue fptr;
    
    /* parse the argument list */
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();

    /* read the procedure */
    return faslreadprocedure(fptr);
}

/* faslreadprocedure - read a procedure from a fasl file */
static xlValue faslreadprocedure(xlValue fptr)
{
    int ch;

    /* check for eof */
    if ((ch = faslskip(fptr)) == EOF)
        return xlEofObject;

    /* make sure the next expression is a code object */
    else if (ch != 'C')
        xlFmtError("bad fasl file ~A",xlMakeChar(ch));
    xlUngetC(fptr,'C');

    /* read the code portion of the closure */
    return xlMakeClosure(faslreadexpr(fptr),xlNil);
}

/* faslreadexpr - read the next expression from a fasl file */
static xlValue faslreadexpr(xlValue fptr)
{
    int ch;

    /* dispatch on the expression type */
    switch (ch = faslskip(fptr)) {
    case 'D':
        return faslread(fptr);
    case 'C':
        return faslreadcode(fptr);
        break;
    default:
        xlFmtError("unknown fasl expression type ~A",xlMakeChar(ch));
        break;
    }
    return xlNil; /* never reached */
}

/* faslread - read an expression from a fasl file */
xlValue faslread(xlValue fptr)
{
    xlValue val;
    if (!xlRead(fptr,&val))
        fasleof();
    return val;
}

/* faslreadcode - read a code object from a fasl file */
static xlValue faslreadcode(xlValue fptr)
{
    long nlits,nbytes,i;
    xlValue val;
    
    /* read the function name and argument names */
    xlCheck(2);
    xlPush(faslread(fptr));
    xlPush(faslread(fptr));

    /* get the code object size and number of bytecodes */
    val = faslread(fptr); nlits = xlGetFixnum(val) + xlFIRSTLIT;
    val = faslread(fptr); nbytes = xlGetFixnum(val);
    
    /* allocate the code object */
    val = xlNewCode(nlits);
    xlSetVNames(val,xlPop());
    xlSetCName(val,xlPop());
    xlPush(val);

    /* allocate the bytecode array */
    val = xlNewString(nbytes);
    xlSetBCode(xlTop(),val);
    
    /* read the bytecodes */
    for (i = 0; i < nbytes; ++i)
        xlGetString(val)[i] = gethexbyte(fptr);

    /* read the literals */
    for (i = xlFIRSTLIT; i < nlits; ++i)
        xlSetElement(xlTop(),i,faslreadexpr(fptr));

    /* return the code object */
    return xlPop();
}

/* gethexbyte - get a hex byte from a fasl file */
static int gethexbyte(xlValue fptr)
{
    int ch1,ch2;

    /* read the first hex digit */
    if ((ch1 = faslskip(fptr)) == EOF)
        fasleof();
    ch1 = toupper(ch1);
    if ((ch1 -= '0') > 9)
        ch1 += '0' - 'A' + 10;

    /* read the second hex digit */
    ch2 = faslgetc(fptr);
    ch2 = toupper(ch2);
    if ((ch2 -= '0') > 9)
        ch2 += '0' - 'A' + 10;

    /* return the byte */
    return (ch1 << 4) | ch2;
}
    
/* faslskip - read the next non-space character from a fasl file */
static int faslskip(xlValue fptr)
{
    int ch;
    while ((ch = xlGetC(fptr)) != EOF && isspace(ch))
        ;
    return ch;
}

/* faslgetc - read the next character from a fasl file */
static int faslgetc(xlValue fptr)
{
    int ch;
    if ((ch = xlGetC(fptr)) == EOF)
        fasleof();
    return ch;
}

/* fasleof - unexpected eof in fasl file */
static void fasleof(void)
{
    xlFmtError("unexpected eof in fasl file");
}
