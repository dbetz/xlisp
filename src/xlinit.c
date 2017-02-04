/* xlinit.c - xlisp initialization routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"
#include "xlbcode.h"

/* macro to store a byte into a bytecode vector */
#define pb(x)   (*bcode++ = (x))

/* shorthand for xlFIRSTENV */
#define FE      xlFIRSTENV

/* global variables */
xlEXPORT xlValue xlSymConst,xlSymMEscape,xlSymSEscape,xlSymWSpace;
xlEXPORT xlValue xlSymTMacro,xlSymNMacro,xlSymReadTable;
xlValue lk_optional,lk_rest,lk_key,lk_aux,lk_allow_other_keys,slk_optional,slk_rest;
xlValue xlEofObject,xlDefaultObject,s_unassigned,s_error;
xlValue s_quote,s_function,s_quasiquote,s_unquote,s_unquotesplicing,s_dot;
xlValue s_package,s_eval,s_load,xlUnboundObject,s_stdin,s_stdout,s_stderr;
xlValue s_print,s_printcase,k_upcase,k_downcase,s_eql;
xlValue s_stackpointer;
xlValue k_internal,k_external,k_inherited,k_key,k_uses,k_test,k_testnot;
xlValue k_start,k_end,k_1start,k_1end,k_2start,k_2end,k_count,k_fromend;
xlValue s_fixfmt,s_hexfmt,s_flofmt,s_freeptr,s_backtrace;

/* external variables */
extern xlValue xlLispPackage;

/* local functions */
static xlValue getloadpath(void);

/* xlInitWorkspace - create an initial workspace */
void xlInitWorkspace(int ssize)
{
    unsigned char *bcode;
    xlValue code;

    /* allocate memory for the workspace */
    xlInitMemory(ssize);

    /* initialize the interpreter */
    xlInitInterpreter();

    /* initialize the packages */
    xlInitPackages();

    /* enter the eof object */
    xlEofObject = xlCons(xlInternAndExport("**EOF**",xlLispPackage),xlNil);
    
    /* enter the default object */
    xlDefaultObject = xlCons(xlInternAndExport("**DEFAULT**",xlLispPackage),xlNil);

    /* initialize the error handlers */
    xlSetValue(xlInternAndExport("*ERROR-HANDLER*",xlLispPackage),xlNil);
    xlSetValue(xlInternAndExport("*UNBOUND-HANDLER*",xlLispPackage),xlNil);
    
    /* install the built-in functions and objects */
    xlInitFunctions();
    xlInitObjects();
    
    /* enter all of the symbols used by the runtime system */
    xlEnterSymbols();
    
    /* initialize the reader */
    xlInitReader();
    
    /* set the initial values of the symbols T and NIL */
    xlSetValue(xlInternAndExport("T",xlLispPackage),xlTrue);
    xlSetValue(xlInternAndExport("NIL",xlLispPackage),xlNil);

    /* default to lowercase output of symbols */
    xlSetValue(s_printcase,k_downcase);

    /* setup the print formats for numbers */
    xlSetValue(s_fixfmt,xlMakeCString(xlIFMT));
    xlSetValue(s_flofmt,xlMakeCString(xlFFMT));
    xlSetValue(s_hexfmt,xlMakeCString(xlXFMT));

    /* disable backtrace */
    xlSetValue(s_backtrace,xlNil);
    
    /* build the 'eval' function */
    code = xlNewCode(xlFIRSTLIT+1); xlCPush(code);
    xlSetBCode(code,xlNewString(0x0c));
    xlSetCName(code,xlEnter("EVAL"));
    xlSetVNames(code,xlCons(xlEnter("X"),xlNil));
    xlSetElement(code,xlFIRSTLIT,xlEnter("COMPILE"));
    xlDrop(1);

    /* store the byte codes */
    bcode = xlGetCodeStr(code);

pb(xlopARGSEQ);pb(0x01);        /* 0000 ARGSEQ 01               */
pb(xlopEREF);pb(0x00);pb(FE+0); /* 0002 EREF 00 +0 ; x          */
pb(xlopPUSH);                   /* 0005 PUSH                    */
pb(xlopGREF);pb(0x03);          /* 0006 GREF 03 ; compile       */
pb(xlopCALL);pb(0x01);          /* 0008 CALL 01                 */
pb(xlopTCALL);pb(0x00);         /* 000a TCALL 00                */

    xlSetValue(xlGetElement(code,1),xlMakeClosure(code,xlNil));

    /* setup the main loop code */
    code = xlNewCode(xlFIRSTLIT+6); xlCPush(code);
    xlSetBCode(code,xlNewString(0x1b));
    xlSetCName(code,xlInternAndExport("*TOPLEVEL*",xlLispPackage));
    xlSetVNames(code,xlNil);
    xlSetElement(code,xlFIRSTLIT,xlMakeCString("\n\n> "));
    xlSetElement(code,xlFIRSTLIT+1,xlEnter("DISPLAY"));
    xlSetElement(code,xlFIRSTLIT+2,xlEnter("READ"));
    xlSetElement(code,xlFIRSTLIT+3,xlEnter("EVAL"));
    xlSetElement(code,xlFIRSTLIT+4,xlEnter("PRINT"));
    xlSetElement(code,xlFIRSTLIT+5,xlInternAndExport("*TOPLEVEL*",xlLispPackage));
    xlDrop(1);

    /* store the byte codes */
    bcode = xlGetCodeStr(code);

pb(xlopARGSEQ);pb(0x00);        /* 0000 ARGSEQ 00               */
pb(xlopLIT);pb(0x03);           /* 0002 LIT 03 ; "\n> "         */
pb(xlopPUSH);                   /* 0004 PUSH                    */
pb(xlopGREF);pb(0x04);          /* 0005 GREF 04 ; display       */
pb(xlopCALL);pb(0x01);          /* 0007 CALL 01                 */
pb(xlopGREF);pb(0x05);          /* 0009 GREF 05 ; read          */
pb(xlopCALL);pb(0x00);          /* 000b CALL 00                 */
pb(xlopPUSH);                   /* 000d PUSH                    */
pb(xlopGREF);pb(0x06);          /* 000e GREF 06 ; eval          */
pb(xlopCALL);pb(0x01);          /* 0010 CALL 01                 */
pb(xlopPUSH);                   /* 0012 PUSH                    */
pb(xlopGREF);pb(0x07);          /* 0013 GREF 07 ; print         */
pb(xlopCALL);pb(0x01);          /* 0015 CALL 01                 */
pb(xlopGREF);pb(0x08);          /* 0017 GREF 08 ; *toplevel*    */
pb(xlopTCALL);pb(0x00);         /* 0019 TCALL 00                */

    xlSetValue(xlGetElement(code,1),xlMakeClosure(code,xlNil));

    /* path to executable file */
    xlSetValue(xlEnter("*LOAD-PATH*"),getloadpath());

    /* enter o/s specific functions */
    xlosEnter();
}

/* getloadpath - get the load path */
static xlValue getloadpath(void)
{
    char *entry,*path;
    xlValue this,last;

    /* get the load path */
    if ((path = xlosLoadPath()) == NULL)
        return xlNil;

    /* append each directory to the path */
    for (xlVal = xlNil; (entry = xlosParsePath(&path)) != NULL; ) {
        this = xlCons(xlMakeCString(entry),xlNil);
        if (xlVal == xlNil)
            xlVal = this;
        else
            xlSetCdr(last,this);
        last = this;
    }
    return xlVal;
}

/* xlEnterSymbols - lookup/enter all symbols used by the runtime system */
void xlEnterSymbols(void)
{
    short portflags;
    xlValue sym;
    
    /* enter the unbound indicator */
    xlUnboundObject = xlInternAndExport("*UNBOUND*",xlLispPackage);
    xlSetValue(xlUnboundObject,xlUnboundObject);

    /* enter the #T symbol and set its value */
    xlTrue = xlInternAndExport("#T",xlLispPackage);
    xlSetValue(xlTrue,xlTrue);

    /* enter the symbols used by the system */
    s_eval = xlInternAndExport("EVAL",xlLispPackage);
    s_load = xlInternAndExport("LOAD",xlLispPackage);
    s_unassigned = xlInternAndExport("#!UNASSIGNED",xlLispPackage);
    s_package = xlInternAndExport("*PACKAGE*",xlLispPackage);
    s_error = xlInternAndExport("ERROR",xlLispPackage);
    s_stackpointer = xlInternAndExport("%STACK-POINTER",xlLispPackage);

    /* enter the i/o symbols */
    s_stdin  = xlInternAndExport("*STANDARD-INPUT*",xlLispPackage);
    s_stdout = xlInternAndExport("*STANDARD-OUTPUT*",xlLispPackage);
    s_stderr = xlInternAndExport("*ERROR-OUTPUT*",xlLispPackage);
    
    /* enter the symbols used by the printer */
    s_fixfmt = xlInternAndExport("*FIXNUM-FORMAT*",xlLispPackage);
    s_hexfmt = xlInternAndExport("*HEXNUM-FORMAT*",xlLispPackage);
    s_flofmt = xlInternAndExport("*FLONUM-FORMAT*",xlLispPackage);

    /* property tag for foreign pointer free function */
    s_freeptr = xlInternAndExport("%FREE-POINTER",xlLispPackage);

    /* scheme keywords */
    slk_optional        = xlInternAndExport("#!OPTIONAL",xlLispPackage);
    slk_rest            = xlInternAndExport("#!REST",xlLispPackage);

    /* enter the lambda list keywords */
    lk_optional         = xlInternAndExport("&OPTIONAL",xlLispPackage);
    lk_rest             = xlInternAndExport("&REST",xlLispPackage);
    lk_key              = xlInternAndExport("&KEY",xlLispPackage);
    lk_aux              = xlInternAndExport("&AUX",xlLispPackage);
    lk_allow_other_keys = xlInternAndExport("&ALLOW-OTHER-KEYS",xlLispPackage);

    /* enter symbols needed by the reader */
    xlSymReadTable      = xlInternAndExport("*READTABLE*",xlLispPackage);
    xlSymNMacro         = xlInternAndExport("NON-TERMINATING-MACRO",xlLispPackage);
    xlSymTMacro         = xlInternAndExport("TERMINATING-MACRO",xlLispPackage);
    xlSymWSpace         = xlInternAndExport("WHITE-SPACE",xlLispPackage);
    xlSymConst          = xlInternAndExport("CONSTITUENT",xlLispPackage);
    xlSymSEscape        = xlInternAndExport("SINGLE-ESCAPE",xlLispPackage);
    xlSymMEscape        = xlInternAndExport("MULTIPLE-ESCAPE",xlLispPackage);
    s_quote             = xlInternAndExport("QUOTE",xlLispPackage);
    s_function          = xlInternAndExport("FUNCTION",xlLispPackage);
    s_quasiquote        = xlInternAndExport("QUASIQUOTE",xlLispPackage);
    s_unquote           = xlInternAndExport("UNQUOTE",xlLispPackage);
    s_unquotesplicing   = xlInternAndExport("UNQUOTE-SPLICING",xlLispPackage);
    s_dot               = xlInternAndExport(".",xlLispPackage);

    /* 'else' is a useful synonym for #t in cond clauses */
    sym = xlInternAndExport("ELSE",xlLispPackage);
    xlSetValue(sym,xlTrue);

    /* setup stdin/stdout/stderr */
    portflags = xlpfINPUT | xlpfOUTPUT | xlpfTERMINAL;
    xlSetValue(s_stdin,xlMakeFileStream(NULL,portflags));
    xlSetValue(s_stdout,xlGetValue(s_stdin));
    xlSetValue(s_stderr,xlGetValue(s_stdin));

    /* enter print, *print-case* and its keywords */
    s_print     = xlInternAndExport("PRINT",xlLispPackage);
    s_printcase = xlInternAndExport("*PRINT-CASE*",xlLispPackage);
    k_upcase    = xlInternAndExport("UPCASE",xlLispPackage);
    k_downcase  = xlInternAndExport("DOWNCASE",xlLispPackage);
    
    /* keywords used by intern and find-symbol */
    k_internal  = xlEnterKeyword("INTERNAL");
    k_external  = xlEnterKeyword("EXTERNAL");
    k_inherited = xlEnterKeyword("INHERITED");
    k_uses      = xlEnterKeyword("USES");
    
    /* other useful symbols */
    s_eql       = xlInternAndExport("EQL",xlLispPackage);
    s_backtrace = xlInternAndExport("*BACKTRACE*",xlLispPackage);

    /* other useful keywords */
    k_key       = xlEnterKeyword("KEY");
    k_test      = xlEnterKeyword("TEST");
    k_testnot   = xlEnterKeyword("TEST-NOT");
    k_start     = xlEnterKeyword("START");
    k_end       = xlEnterKeyword("END");
    k_1start    = xlEnterKeyword("START1");
    k_1end      = xlEnterKeyword("END1");
    k_2start    = xlEnterKeyword("START2");
    k_2end      = xlEnterKeyword("END2");
    k_count     = xlEnterKeyword("COUNT");
    k_fromend   = xlEnterKeyword("FROM-END?");
    
    /* initialize the object system symbols */
    xlObSymbols();
}
