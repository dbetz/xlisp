/* xlfun3.c - xlisp built-in functions - part 3 */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* external variables */
extern xlValue s_package;

/* forward declarations */
static const char *showstring(const char *str,int bch);
static void withfile_continuation(void);
static void withfile_unwind(void);
static void do_withfile(short flags,const char *mode);
static void do_load(xlValue print);
static void load_continuation(void);
static void load_unwind(void);
static void do_loadloop(xlValue print,xlValue oldpack);
static void force_continuation(void);
static void pushccontinuation(xlCContinuation *);

/* xapply - built-in function 'apply' */
void xapply(void)
{
    xlValue args,*p;

    /* get the function and argument list */
    xlVal = xlGetArg();
    args = xlGetArgList();
    xlLastArg();

    /* get the argument count and make space on the stack */
    xlArgC = (int)xlLength(args);
    xlCheck(xlArgC);

    /* copy the arguments onto the stack */
    for (xlSP -= xlArgC, p = xlSP; xlConsP(args); args = xlCdr(args))
        *p++ = xlCar(args);

    /* apply the function to the arguments */
    xlNext = xlApply;
}

/* xvalues - return multiple values */
void xvalues(void)
{
    xlVal = xlArgC > 0 ? xlTop() : xlNil;
    xlDrop(xlArgC);
    xlCDRestore();
}

/* xvalueslist - return a list as multiple values */
void xvalueslist(void)
{
    xlValue p;
    xlVal = p = xlGetArgList();
    xlLastArg();
    xlArgC = (int)xlLength(xlVal);
    xlCheck(xlArgC);
    for (xlSP -= xlArgC; xlConsP(p); p = xlCdr(p))
        *xlSP++ = xlCar(p);
    xlVal = xlVal ? xlCar(xlVal) : xlNil;
    xlCDRestore();
}

/* xcallcc - built-in function 'call-with-current-continuation' */
void xcallcc(void)
{
    xlValue xlMakeContinuation();

    /* get the function to call */
    xlVal = xlGetArg();
    xlLastArg();

    /* create a continuation object */
    xlCPush(xlMakeContinuation());
    xlArgC = 1;

    /* apply the function */
    xlNext = xlApply;
}

/* ccode_restore - restore a C continuation */
static void ccode_restore(void)
{
    xlCContinuation *cc;
    cc = (xlCContinuation *)xlCtlPop();
    (*cc->cc_cont)();
}

/* ccode_mark - mark a C continuation */
static xlValue *ccode_mark(xlValue *p)
{
    xlCContinuation *cc;
    int cnt;
    cc = (xlCContinuation *)*--p;
    for (cnt = cc->cc_cnt; --cnt >= 0; )
        xlMark(*--p);
    return p;
}

/* ccode_skipit - skip a C continuation */
static xlValue *ccode_skipit(xlValue *p)
{
    xlCContinuation *cc;
    cc = (xlCContinuation *)*--p;
    return p - cc->cc_cnt;
}

/* ccode_unwind - unwind past a C continuation (just skip over it) */
static void ccode_unwind(void)
{
    xlCContinuation *cc;
    cc = (xlCContinuation *)xlCtlPop();
    if (cc->cc_unwind)
        (*cc->cc_unwind)();
    else
        xlCtlDrop(cc->cc_cnt);
}

/* ccode_print - print a C continuation */
static xlValue *ccode_print(xlValue *p)
{
    xlCContinuation *cc;
    const char *str;
    int cnt;
    cc = (xlCContinuation *)*--p;
    xlErrPutStr("\n ");
    str = showstring(cc->cc_names,':');
    for (cnt = cc->cc_cnt; --cnt >= 0; ) {
        xlErrPutStr("\n  ");
        str = showstring(str,',');
        xlErrPutStr(": ");
        xlErrPrint(*--p);
    }
    return p;
}

/* showstring - show the next string up to a given delimiter */
static const char *showstring(const char *str,int bch)
{
    char buf[20],*p;
    int ch;
    for (p = buf; *str != '\0' && (ch = *str++) != bch; )
        *p++ = ch;
    *p = '\0';
    xlErrPutStr(buf);
    return str;
}

/* C continuation dispatch table */
static xlCDispatch cd_ccode = {
    ccode_restore,
    ccode_mark,
    ccode_skipit,
    ccode_unwind,
    ccode_skipit,
    ccode_print
};

/* pushccontinuation - push a C continuation on the control stack */
static void pushccontinuation(xlCContinuation *cc)
{
    xlCtlCheck(2);
    xlCtlPush((xlValue)cc);
    xlCtlPush((xlValue)&cd_ccode);
}

/* xcallwi - built-in function 'call-with-input-file' */
void xcallwi(void)
{
    do_withfile(xlpfINPUT,"r");
}

/* xcallwo - built-in function 'call-with-output-file' */
void xcallwo(void)
{
    do_withfile(xlpfOUTPUT | xlpfBOL,"w");
}

/* withfile_continuation - withfile continuation */
static void withfile_continuation(void)
{
    xlValue file;
    file = xlCtlPop();
    xlosClose(xlGetFile(file));
    xlSetSData(file,NULL);
    xlSVReturn();
}

/* withfile_unwind - withfile unwind handler */
static void withfile_unwind(void)
{
    xlVal = xlCtlPop();
    xlosClose(xlGetFile(xlVal));
}

/* withfile continuation structure */
xlCContinuation withfile_cc = { withfile_continuation,withfile_unwind,1,"Withfile:file" };

/* do_withfile - handle the 'call-with-xxx-file' functions */
static void do_withfile(short flags,const char *mode)
{
    xlValue name,file;
    FILE *fp;

    /* get the function to call */
    name = xlGetArgString();
    xlVal = xlGetArg();
    xlLastArg();

    /* create a file object */
    file = xlMakeFileStream(NULL,flags);
    if ((fp = xlosOpenText(xlGetString(name),mode)) == NULL)
        xlError("can't open file",name);
    xlSetSData(file,fp);

    /* save a continuation */
    xlCtlCheck(1);
    xlCtlPush(file);
    pushccontinuation(&withfile_cc);

    /* setup the argument list */
    xlCPush(file);
    xlArgC = 1;

    /* apply the function */
    xlNext = xlApply;
}

/* xload - built-in function 'load' */
void xload(void)
{
    do_load(xlFalse);
}

/* xloadnoisily - built-in function 'load-noisily' */
void xloadnoisily(void)
{
    do_load(xlTrue);
}

/* do_load - open the file and setup the load loop */
static void do_load(xlValue print)
{
    FILE *fp;

    /* get the name of the file to load */
    xlVal = xlGetArgString();
    xlLastArg();

    /* create a file object */
    xlCPush(xlMakeFileStream(NULL,xlpfINPUT));
    if ((fp = xlLoadOpen(xlGetString(xlVal),"r","*LOAD-PATH*",NULL)) == NULL) {
        xlDrop(1);
        xlVal = xlNil;
        xlSVReturn();
        return;
    }
    xlVal = xlPop();
    xlSetSData(xlVal,fp);

    /* do the first read */
    do_loadloop(print,xlGetValue(s_package));
}

/* load_continuation - load continuation */
static void load_continuation(void)
{
    xlValue print;
    xlCheck(2);
    xlPush(xlCtlPop());
    xlEnv = xlCtlPop();
    xlPush(xlCtlPop());
    if ((print = xlCtlPop()) != xlFalse) {
        xlNewline(xlCurOutput());
        xlWrite(xlVal,xlCurOutput());
    }
    xlVal = xlPop();
    do_loadloop(print,xlPop());
}

/* load_unwind - load unwind handler */
static void load_unwind(void)
{
    xlSetValue(s_package,xlCtlPop());
    xlCtlDrop(1);
    xlVal = xlCtlPop();
    xlosClose(xlGetFile(xlVal));
    xlCtlDrop(1);
}

/* load continuation structure */
xlCContinuation load_cc = { load_continuation,load_unwind,4,"Load:package,env,file,print" };

/* do_loadloop - read the next expression and setup to evaluate it */
static void do_loadloop(xlValue print,xlValue oldpack)
{
    extern xlValue s_eval;
    xlValue expr;
    
    /* try to read the next expression from the file */
    xlCheck(3);
    xlPush(oldpack);
    xlPush(print);
    if (xlRead(xlVal,&expr)) {

        /* save a continuation */
        xlPush(expr);
        xlEnv = xlUnstackEnvironment(xlEnv);
        expr = xlPop();
        xlCtlCheck(4);
        xlCtlPush(xlPop());
        xlCtlPush(xlVal);
        xlCtlPush(xlEnv);
        xlCtlPush(xlPop());
        pushccontinuation(&load_cc);

        /* setup the argument list */
        xlVal = xlGetValue(s_eval);
        xlCPush(expr);
        xlArgC = 1;

        /* apply the function */
        xlNext = xlApply;
    }
    else {
        xlDrop(1);
        xlSetValue(s_package,xlPop());
        xlosClose(xlGetFile(xlVal));
        xlSetSData(xlVal,NULL);
        xlVal = xlTrue;
        xlSVReturn();
    }
}

/* force_continuation - force continuation */
static void force_continuation(void)
{
    xlValue promise;
    promise = xlCtlPop();
    if (xlGetPProc(promise)) {
        xlSetPValue(promise,xlVal);
        xlSetPProc(promise,xlNil);
    }
    else
        xlVal = xlGetPValue(promise);
    xlSVReturn();
}

/* force continuation structure */
xlCContinuation force_cc = { force_continuation,NULL,1,"Force:promise" };

/* xforce - built-in function 'force' */
void xforce(void)
{
    /* get the promise */
    xlVal = xlGetArg();
    xlLastArg();

    /* check for a promise */
    if (xlPromiseP(xlVal)) {

        /* force the promise the first time */
        if ((xlFun = xlGetPProc(xlVal)) != xlNil) {
            xlCtlCheck(1);
            xlCtlPush(xlVal);
            pushccontinuation(&force_cc);
            xlVal = xlFun;
            xlArgC = 0;
            xlNext = xlApply;
        }

        /* return the saved value if the promise has already been forced */
        else {
            xlVal = xlGetPValue(xlVal);
            xlSVReturn();
        }
        
    }
    
    /* otherwise, just return the argument */
    else
        xlSVReturn();
}
