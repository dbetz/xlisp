/* xlint.c - xlisp bytecode interpreter */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"
#include "xlbcode.h"

/* macro to call a xlSUBR */
#define callsubr(x,c)   (xlArgC = (c), (x)())

/* globals */
xlErrorTarget *xlerrtarget = NULL;  /* error target */
xlValue *xlcatch = NULL;            /* catch frame pointer */
int xlTraceBytecodes = FALSE;       /* trace enable */
xlEXPORT int xlArgC;                /* number of arguments remaining */
xlEXPORT void (*xlNext)(void);      /* next function to call (xlApply or NULL) */

/* external variables */
extern xlValue s_package,s_stdin,s_stdout,xlUnboundObject;
extern xlValue s_unassigned,xlDefaultObject,s_error;
extern xlValue s_stackpointer;

/* error target (and bytecode dispatch target) */
#define BCD_START       0       /* must be zero */
#define BCD_RETURN      1
#define BCD_NEXT        2
#define BCD_ABORT       3

/* local variables */
static unsigned char *base,*pc;
static xlValue *xltarget;               /* current throw target */
static int sample=xlSRATE;

/* local prototypes */
static void show_call(xlValue code,xlValue frame);
static void throwtotag(xlValue tag);
static xlValue *findmatchingcatch(xlValue tag);
static void throwtoreturn(void);
static void throwtotarget(xlValue *target);
static void call(void),tcall(void);
static void restore_continuation(void);
static void stkframe(int,int,xlValue);
static void badfuntype(xlValue arg);
static void badargtype(xlValue arg);

/* xtraceon - built-in function 'trace-on' */
xlValue xtraceon(void)
{
    xlLastArg();
    xlTraceBytecodes = TRUE;
    return xlNil;
}

/* xtraceoff - built-in function 'trace-off' */
xlValue xtraceoff(void)
{
    xlLastArg();
    xlTraceBytecodes = FALSE;
    return xlNil;
}

/* code_restore - restore a code continuation */
static void code_restore(void)
{
    xlFun = xlCtlPop();
    base = xlGetCodeStr(xlFun);
    pc = base + (xlFIXTYPE)xlCtlPop();
    xlEnv = xlCtlPop();
    if (xlMovedStackEnvironmentP(xlEnv))
        xlEnv = xlGetForwardingAddr(xlEnv);
}

/* code_mark - mark a code continuation */
static xlValue *code_mark(xlValue *p)
{
    xlMark(*--p);   /* xlFun */
    p -= 1;         /* pc */
    xlMark(*--p);   /* xlEnv */
    return p;
}

/* code_unmark - unmark a code continuation */
static xlValue *code_unmark(xlValue *p)
{
    return p - 3;
}

/* code_unwind - unwind a code continuation */
static void code_unwind(void)
{
    xlCtlDrop(3);
}

/* code_unstack - unstack a code continuation */
static xlValue *code_unstack(xlValue *p)
{
    p -= 3;
    *p = xlUnstackEnvironment(*p);
    return p;
}

/* code_print - print a code continuation */
static xlValue *code_print(xlValue *p)
{
    xlFIXTYPE val;
    xlErrPutStr("\n Code ");
    xlErrPrint(*--p);
    xlErrPutStr("\n  pc: ");
    val = (xlFIXTYPE)*--p;
    xlErrPrint(xlMakeFixnum(val));
    xlErrPutStr("\n  env: ");
    xlErrPrint(*--p);
    return p;
}

/* code continuation dispatch table */
static xlCDispatch cd_code = {
    code_restore,
    code_mark,
    code_unmark,
    code_unwind,
    code_unstack,
    code_print
};

/* frame_restore - pop a stack environment frame */
static void frame_restore(void)
{
    xlEnv = xlGetNextFrame(xlEnv);
    xlCtlDrop(xlNODEWORDS);
    xlSP = xlGetVector((xlValue)xlCSP) + xlGetSize((xlValue)xlCSP);
}

/* frame_mark - mark a stack environment frame */
static xlValue *frame_mark(xlValue *p)
{
    p -= xlNODEWORDS;   /* environment frame */
    return p;
}

/* frame_unmark - unmark a stack environment frame */
static xlValue *frame_unmark(xlValue *p)
{
    p -= xlNODEWORDS;
    ((xlValue)p)->flags = 0;
    return p;
}

/* frame_unwind - unwind a stack environment frame */
static void frame_unwind(void)
{
    xlCtlDrop(xlNODEWORDS);
}

/* frame_unstack - unstack a stack environment frame */
static xlValue *frame_unstack(xlValue *p)
{
    p -= xlNODEWORDS;
    return p;
}

/* frame_print - print a stack environment frame */
static xlValue *frame_print(xlValue *p)
{
    xlValue frame,np;
    int i;
    xlErrPutStr("\n Frame ");
    p -= xlNODEWORDS;
    frame = (xlValue)p;
    xlErrPrint(frame);
    if (xlMovedStackEnvironmentP(frame))
        frame = xlGetForwardingAddr(frame);
    np = xlGetEnvNames(frame);
    for (i = xlFIRSTENV; np != xlNil; ++i, np = xlCdr(np)) {
        xlErrPutStr("\n  ");
        xlErrPrint(xlCar(np));
        xlErrPutStr(" = ");
        xlErrPrint(xlGetEnvElement(frame,i));
    }
    return p;
}

/* stack environment frame dispatch table */
static xlCDispatch cd_frame = {
    frame_restore,
    frame_mark,
    frame_unmark,
    frame_unwind,
    frame_unstack,
    frame_print
};

/* xlFindTopProcedure - find the top procedure on the call stack */
xlValue xlFindTopProcedure(void)
{
    xlValue *p;
    for (p = xlCSP; p > xlStkBase; p = xlCDUnmark(p))
        if ((xlCDispatch *)p[-1] == &cd_code)
            return p[-2];
    return xlNil;
}

/* show_call - show a single call on the call stack */
static void show_call(xlValue code,xlValue frame)
{
    xlValue name = xlGetCName(code);
    
    /* start the function */
    xlErrPutStr("\n  (");
    
    /* print the function name */
    if (name == xlNil)
        xlErrPrint(code);
    else
        xlErrPrint(name);
    
    /* print the function arguments */
    if (frame != xlNil) {
        xlFIXTYPE i,max;
        for (i = xlFIRSTENV, max = xlGetSize(frame); i < max; ++i) {
            xlErrPutStr(" ");
            xlErrPrint(xlGetElement(frame,i));
        }
    }
    
    /* end the function */
    xlErrPutStr(")");
}

/* xlShowCallStack - display the call stack */
void xlShowCallStack(int cmax)
{
    xlValue lastcode = xlNil,lastframe = xlNil,*p;
    for (p = xlCSP; cmax > 0 && p > xlStkBase; p = xlCDUnmark(p)) {
        xlCDispatch *d = (xlCDispatch *)p[-1];
        if (d == &cd_code) {
            if (lastcode != xlNil)
                show_call(lastcode,lastframe);
            lastcode = p[-2];
            lastframe = xlNil;
            --cmax;
        }
        else if (d == &cd_frame) {
            lastframe = (xlValue)&p[-((int)xlNODEWORDS + 1)];
            if (xlMovedStackEnvironmentP(lastframe))
                lastframe = xlGetForwardingAddr(lastframe);
        }
    }
    if (lastcode != xlNil)
        show_call(lastcode,lastframe);
}

/* unwind_restore - restore an unwind frame after invoking an unwind-protect cleanup */
static void unwind_restore(void)
{
    xlValue *target,*p;
    int cnt;
    
    /* restore the throw target */
    target = (xlValue *)xlCtlPop();
    xlArgC = (int)(xlFIXTYPE)xlCtlPop();
    
    /* move the results back onto the stack */
    if (xlArgC > 0) {
        xlCheck(xlArgC);
        for (xlSP -= xlArgC, p = xlSP, cnt = xlArgC; --cnt >= 0; )
            *p++ = xlCtlPop();
    }

    /* continue throwing to the target */
    throwtotarget(target);
}

/* unwind_mark - mark an unwind frame */
static xlValue *unwind_mark(xlValue *p)
{
    int argc;
    p -= 1;                     /* xltarget */
    argc = (int)(xlFIXTYPE)*--p;        /* xlArgC */
    if (argc > 0)
        while (--argc >= 0)     /* results */
            xlMark(*--p);
    return p;
}

/* unwind_unmark - unmark an unwind frame */
static xlValue *unwind_unmark(xlValue *p)
{
    int argc;
    p -= 1;                     /* xltarget */
    argc = (int)(xlFIXTYPE)*--p;        /* xlArgC */
    return argc > 0 ? p - argc : p;
}

/* unwind_unwind - unwind an unwind frame */
static void unwind_unwind(void)
{
    int argc;
    xlCtlDrop(1);                       /* xltarget */
    argc = (int)(xlFIXTYPE)xlCtlPop();/* xlArgC */
    if (argc > 0)
        xlCtlDrop(argc);
}

/* unwind_unstack - unstack an unwind frame */
static xlValue *unwind_unstack(xlValue *p)
{
    int argc;
    p -= 1;                     /* xltarget */
    argc = (int)(xlFIXTYPE)*--p;        /* xlArgC */
    return argc > 0 ? p - argc : p;
}

/* unwind_print - print a unwind frame */
static xlValue *unwind_print(xlValue *p)
{
    xlFIXTYPE val;
    xlErrPutStr("\n Unwind");
    xlErrPutStr("\n  target: ");
    val = (xlFIXTYPE)*--p;
    xlErrPrint(xlMakeFixnum(val));
    xlErrPutStr("\n  argc: ");
    val = (xlFIXTYPE)*--p;
    xlErrPrint(xlMakeFixnum(val));
    xlErrPutStr("\n  results:");
    if (val > 0)
        while (--val >= 0) {
            xlErrPutStr("\n   ");
            xlErrPrint(*--p);
        }
    return p;
}

/* unwind frame dispatch table */
static xlCDispatch cd_unwind = {
    unwind_restore,
    unwind_mark,
    unwind_unmark,
    unwind_unwind,
    unwind_unstack,
    unwind_print
};

/* protect_restore - restore a protect frame after a successful throw */
static void protect_restore(void)
{
    xlCtlDrop(3);
}

/* protect_mark - mark a protect frame */
static xlValue *protect_mark(xlValue *p)
{
    xlMark(*--p);               /* cleanup */
    p -= 1;             /* xlSP */
    xlMark(*--p);       /* xlEnv */
    return p;
}

/* protect_unmark - unmark a protect frame */
static xlValue *protect_unmark(xlValue *p)
{
    return p - 3;
}

/* protect_unwind - unwind a protect frame */
static void protect_unwind(void)
{
    xlValue *p = xlSP + xlArgC;
    int cnt = xlArgC;
    xlVal = xlCtlPop();
    xlSP = (xlValue *)xlCtlPop();
    xlEnv = xlCtlPop();
    if (xlMovedStackEnvironmentP(xlEnv))
        xlEnv = xlGetForwardingAddr(xlEnv);
    if (xlArgC > 0) {
        xlCtlCheck(xlArgC + 3);
        while (--cnt >= 0)
            xlCtlPush(*--p);
    }
    else
        xlCtlCheck(3);
    xlCtlPush((xlValue)(xlOFFTYPE)xlArgC);
    xlCtlPush((xlValue)xltarget);
    xlCtlPush((xlValue)&cd_unwind);
    xlArgC = 0;
    xlNext = xlApply;
    longjmp(xlerrtarget->target,BCD_NEXT);
}

/* protect_unstack - unstack a protect frame */
static xlValue *protect_unstack(xlValue *p)
{
    return p - 3;
    *p = xlUnstackEnvironment(*p);
}

/* protect_print - print a protect frame */
static xlValue *protect_print(xlValue *p)
{
    xlFIXTYPE val;
    xlErrPutStr("\n Unwind-protect");
    xlErrPutStr("\n  cleanup: ");
    xlErrPrint(*--p);
    xlErrPutStr("\n  sp: ");
    val = (xlFIXTYPE)*--p;
    xlErrPrint(xlMakeFixnum(val));
    xlErrPutStr("\n  env: ");
    xlErrPrint(*--p);
    return p;
}

/* protect frame dispatch table */
static xlCDispatch cd_protect = {
    protect_restore,
    protect_mark,
    protect_unmark,
    protect_unwind,
    protect_unstack,
    protect_print
};

/* return_restore - pop a return frame */
static void return_restore(void)
{
    longjmp(xlerrtarget->target,BCD_RETURN);
}

/* return_mark - mark a return frame */
static xlValue *return_mark(xlValue *p)
{
    return p;
}

/* return_unmark - unmark a return frame */
static xlValue *return_unmark(xlValue *p)
{
    return p;
}

/* return_unwind - unwind a return frame */
static void return_unwind(void)
{
}

/* return_unstack - unstack a return frame */
static xlValue *return_unstack(xlValue *p)
{
    return p;
}

/* return_print - print a return frame */
static xlValue *return_print(xlValue *p)
{
    xlErrPutStr("\n Return");
    return p;
}

/* return frame dispatch table */
static xlCDispatch cd_return = {
    return_restore,
    return_mark,
    return_unmark,
    return_unwind,
    return_unstack,
    return_print
};

/* catch_restore - restore a catch frame after a successful throw */
static void catch_restore(void)
{
    xlCtlDrop(1);
    xlcatch = (xlValue *)xlCtlPop();
    xlSP = (xlValue *)xlCtlPop();
    xlFun = xlCtlPop();
    base = xlGetCodeStr(xlFun);
    pc = base + (xlFIXTYPE)xlCtlPop();
    xlEnv = xlCtlPop();
    if (xlMovedStackEnvironmentP(xlEnv))
        xlEnv = xlGetForwardingAddr(xlEnv);
}

/* catch_mark - mark a catch frame */
static xlValue *catch_mark(xlValue *p)
{
    xlMark(*--p);   /* tag */
    p -= 2;         /* xlcatch, xlSP */
    xlMark(*--p);   /* xlFun */
    p -= 1;         /* pc */
    xlMark(*--p);   /* xlEnv */
    return p;
}

/* catch_unmark - unmark a catch frame */
static xlValue *catch_unmark(xlValue *p)
{
    return p - 6;
}

/* catch_unwind - unwind a catch frame */
static void catch_unwind(void)
{
    xlCtlDrop(1);
    xlcatch = (xlValue *)xlCtlPop();
    xlCtlDrop(4);
}

/* catch_unstack - unstack a catch frame */
static xlValue *catch_unstack(xlValue *p)
{
    return p - 6;
    *p = xlUnstackEnvironment(*p);
}

/* catch_print - print a catch frame */
static xlValue *catch_print(xlValue *p)
{
    xlFIXTYPE val;
    xlErrPutStr("\n Catch");
    xlErrPutStr("\n  tag: ");
    xlErrPrint(*--p);
    xlErrPutStr("\n  catch: ");
    val = (xlFIXTYPE)*--p;
    xlErrPrint(xlMakeFixnum(val));
    xlErrPutStr("\n  sp: ");
    val = (xlFIXTYPE)*--p;
    xlErrPrint(xlMakeFixnum(val));
    xlErrPutStr("\n  fun: ");
    xlErrPrint(*--p);
    xlErrPutStr("\n  pc: ");
    val = (xlFIXTYPE)*--p;
    xlErrPrint(xlMakeFixnum(val));
    xlErrPutStr("\n  env: ");
    xlErrPrint(*--p);
    return p;
}

/* catch frame dispatch table */
static xlCDispatch cd_catch = {
    catch_restore,
    catch_mark,
    catch_unmark,
    catch_unwind,
    catch_unstack,
    catch_print
};

/* opNOP - handler for opcode NOP */
static void opNOP(void)
{
}

/* opBRT - handler for opcode BRT */
static void opBRT(void)
{
    register unsigned int i;
    if (xlVal != xlFalse) {
        i = *pc++ << 8;
        pc = base + (i | *pc);
    }
    else
        pc += 2;
}

/* opBRF - handler for opcode BRF */
static void opBRF(void)
{
    register unsigned int i;
    if (xlVal == xlFalse) {
        i = *pc++ << 8;
        pc = base + (i | *pc);
    }
    else
        pc += 2;
}

/* opBR - handler for opcode BR */
static void opBR(void)
{
    register unsigned int i;
    i = *pc++ << 8;
    pc = base + (i | *pc);
}

/* opLIT - handler for opcode LIT */
static void opLIT(void)
{
    xlVal = xlGetElement(xlFun,*pc++);
    xlArgC = 1;
}

/* opGREF - handler for opcode GREF */
static void opGREF(void)
{
    extern xlValue s_package;
    register xlValue tmp;
    xlValue key;
    tmp = xlGetElement(xlFun,*pc++);
    if ((xlVal = xlGetValue(tmp)) == xlUnboundObject) {
        xlVal = xlFindSymbol("*UNBOUND-HANDLER*",xlGetValue(s_package),&key);
        if ((xlVal = xlGetValue(xlVal)) != xlNil) {
            xlFIXTYPE offset;
            pc -= 2; /* backup the pc */
            offset = pc - base;
            xlosCheck();
            xlCtlCheck(4);
            xlCtlPush(xlEnv);
            xlCtlPush((xlValue)offset);
            xlCtlPush(xlFun);
            xlCtlPush((xlValue)&cd_code);
            tmp = xlMakeContinuation();
            xlCheck(2);
            xlPush(tmp);
            xlPush(xlGetElement(xlFun,pc[1]));
            xlArgC = 2;
            xlNext = xlApply;
        }
        else
            xlError("unbound variable",tmp);
    }
    else
        xlArgC = 1;
}

/* opGSET - handler for opcode GSET */
static void opGSET(void)
{
    xlSetValue(xlGetElement(xlFun,*pc++),xlVal);
    xlArgC = 1;
}

/* opEREF - handler for opcode EREF */
static void opEREF(void)
{
    register xlValue tmp;
    register int i;
    i = *pc++;
    tmp = xlEnv;
    while (--i >= 0) tmp = xlGetNextFrame(tmp);
    xlVal = xlGetEnvElement(tmp,*pc++);
    xlArgC = 1;
}

/* opESET - handler for opcode ESET */
static void opESET(void)
{
    register xlValue tmp;
    register int i;
    i = *pc++;
    tmp = xlEnv;
    while (--i >= 0) tmp = xlGetNextFrame(tmp);
    xlSetEnvElement(tmp,*pc++,xlVal);
    xlArgC = 1;
}

/* opIREF - handler for opcode IREF */
static void opIREF(void)
{
    register xlValue tmp;
    register int i;
    i = *pc++;
    tmp = xlEnv;
    while (--i >= 0) tmp = xlGetNextFrame(tmp);
    xlVal = xlGetIVar(xlGetEnvElement(tmp,xlFIRSTENV),*pc++);
    xlArgC = 1;
}

/* opISET - handler for opcode ISET */
static void opISET(void)
{
    register xlValue tmp;
    register int i;
    i = *pc++;
    tmp = xlEnv;
    while (--i >= 0) tmp = xlGetNextFrame(tmp);
    xlSetIVar(xlGetEnvElement(tmp,xlFIRSTENV),*pc++,xlVal);
    xlArgC = 1;
}

/* opCALL - handler for opcode CALL */
static void opCALL(void)
{
    xlArgC = *pc++;     /* get argument count */
    call();
}

/* opMVCALL - handler for opcode MVCALL */
static void opMVCALL(void)
{
    xlValue argc = xlPop();
    xlArgC = (int)xlGetSmallFixnum(argc);
    call();
}

/* call - common code for CALL and MVCALL */
static void call(void)
{
    register xlFIXTYPE offset = pc - base;
    xlCtlCheck(4);
    xlCtlPush(xlEnv);
    xlCtlPush((xlValue)offset);
    xlCtlPush(xlFun);
    xlCtlPush((xlValue)&cd_code);
    xlApply();
}

/* opTCALL - handler for opcode TCALL */
static void opTCALL(void)
{
    xlArgC = *pc++;     /* get argument count */
    tcall();
}

/* opMVTCALL - handler for opcode MVTCALL */
static void opMVTCALL(void)
{
    register xlValue argc = xlPop();
    xlArgC = (int)xlGetSmallFixnum(argc);
    tcall();
}

/* tcall - common code for TCALL and MVTCALL */
static void tcall(void)
{
    register xlCDispatch *cd;
    register xlValue *src;
    register int cnt;
    src = xlSP + xlArgC;
    while ((cd = (xlCDispatch *)xlCtlPop()) == &cd_frame)
        (*cd->cd_restore)();
    xlCtlPush((xlValue)cd);
    for (cnt = xlArgC; --cnt >= 0; )
        xlPush(*--src);
    xlApply();
}

/* opRETURN - handler for opcode RETURN */
static void opRETURN(void)
{
    xlValue *src,*dst;
    xlCDispatch *cd;
    int cnt;
    if (xlArgC > 1) {
        src = xlSP;
        do {
            cd = (xlCDispatch *)xlCtlPop();
            (*cd->cd_restore)();
        } while (cd == &cd_frame);
        for (dst = xlSP, cnt = xlArgC; --cnt >= 0; )
            *--dst = *--src;
    }
    else {
        do {
            cd = (xlCDispatch *)xlCtlPop();
            (*cd->cd_restore)();
        } while (cd == &cd_frame);
    }
}

/* opARGSEQ - handler for opcode ARGSEQ */
static void opARGSEQ(void)
{
    register int n;
    n = *pc++;

    /* check the argument count */
    if (xlArgC < n)
        xlTooFew();
    else if (xlArgC > n)
        xlTooMany();

    /* setup the environment stack frame */
    stkframe(xlArgC,xlFIRSTENV,xlGetVNames(xlFun));
}

/* opARGSGE - handler for opcode ARGSGE */
static void opARGSGE(void)
{
    register int min;
    min = *pc++;

    /* check the argument count */
    if (xlArgC < min)
        xlTooFew();

    /* setup the environment stack frame */
    stkframe(xlArgC,xlFIRSTENV,xlGetVNames(xlFun));
}

/* opARGSBT - handler for opcode ARGSBT */
static void opARGSBT(void)
{
    register int min,max;
    min = *pc++;
    max = *pc++;

    /* check the argument count */
    if (xlArgC < min)
        xlTooFew();
    else if (xlArgC > max)
        xlTooMany();

    /* setup the environment stack frame */
    stkframe(xlArgC,xlFIRSTENV,xlGetVNames(xlFun));
}

/* opMETHOD - handler for opcode METHOD */
static void opMETHOD(void)
{
    xlSetFrameType(xlEnv,xlSMENV);
}

/* opOPTARG - handler for opcode OPTARG */
static void opOPTARG(void)
{
    register xlValue pframe = xlGetNextFrame(xlEnv);
    register int n,arg;
    n = *pc++;
    arg = *pc++;
    if (xlGetEnvSize(pframe) > n) {
        xlSetEnvElement(xlEnv,arg,xlGetEnvElement(pframe,n));
        xlVal = xlTrue;
    }
    else
        xlVal = xlNil;
}

/* opKEYARG - handler for the opcode KEYARG */
static void opKEYARG(void)
{
    register xlValue pframe = xlGetNextFrame(xlEnv);
    register int n,arg;
    register xlValue key;
    key = xlGetElement(xlFun,*pc++);
    n = *pc++;
    arg = *pc++;
    for (; n < xlGetEnvSize(pframe); n += 2) {
        if (xlGetEnvElement(pframe,n) == key) {
            if (++n >= xlGetEnvSize(pframe))
                xlError("no value following keyword",key);
            xlSetEnvElement(xlEnv,arg,xlGetEnvElement(pframe,n));
            xlVal = xlTrue;
            return;
        }
    }
    xlVal = xlNil;
}

/* opREST - handler for opcode REST */
static void opREST(void)
{
    register xlValue pframe = xlGetNextFrame(xlEnv);
    register xlFIXTYPE n,arg,i;
    n = *pc++;  /* get the slot number */
    arg = *pc++;
    for (xlVal = xlNil, i = xlGetEnvSize(pframe); --i >= n; )
        xlVal = xlCons(xlGetEnvElement(pframe,i),xlVal);
    xlSetEnvElement(xlEnv,arg,xlVal);
}

/* opFRAME - handler for the opcode FRAME */
static void opFRAME(void)
{
    register int rargc,extra,lit;
    rargc = *pc++;
    extra = *pc++;
    lit = *pc++;

    /* create the environment frame */
    stkframe(rargc,extra,xlGetElement(xlFun,lit));
}

/* opMVFRAME - handler for the opcode MVFRAME */
static void opMVFRAME(void)
{
    register int size,lit,lit2;
    size = *pc++;
    lit = *pc++;
    lit2 = *pc++;
    
    /* push the multiple values onto the stack */
    xlCheck(xlArgC);
    xlSP -= xlArgC;
    xlSetTop(xlVal);

    /* create the environment frame */
    stkframe(xlArgC,xlFIRSTENV,xlGetElement(xlFun,lit));
    stkframe(0,size,xlGetElement(xlFun,lit2));
}

/* opMVPUSH - handler for the opcode MVPUSH */
static void opMVPUSH(void)
{
    if (xlArgC > 0) {
        xlCheck(xlArgC);
        xlSP -= xlArgC;
        xlSetTop(xlVal);
    }
    xlCPush(xlMakeSmallFixnum((xlFIXTYPE)xlArgC));
}

/* opMVPOP - handler for opcode MVPOP */
static void opMVPOP(void)
{
    xlVal = xlPop();
    xlArgC = (int)xlGetFixnum(xlVal);
    if (xlArgC <= 0)
        xlVal = xlNil;
    else {
        xlVal = xlTop();
        xlSP += xlArgC;
    }
}

/* opUNFRAME - handler for the opcode UNFRAME */
static void opUNFRAME(void)
{
    xlValue *src,*dst;
    int cnt;
    if (xlArgC > 1) {
        src = xlSP;
        xlCDRestore();
        for (dst = xlSP, cnt = xlArgC; --cnt >= 0; )
            *--dst = *--src;
    }
    else
        xlCDRestore();
}

/* opT - handler for opcode T */
static void opT(void)
{
    xlVal = xlTrue;
    xlArgC = 1;
}

/* opNIL - handler for opcode NIL */
static void opNIL(void)
{
    xlVal = xlNil;
    xlArgC = 1;
}

/* opPUSH - handler for opcode PUSH */
static void opPUSH(void)
{
    xlCPush(xlVal);
}

/* opCLOSE - handler for opcode CLOSE */
static void opCLOSE(void)
{
    if (!xlCodeP(xlVal)) badargtype(xlVal);
    xlEnv = xlUnstackEnvironment(xlEnv);
    xlVal = xlMakeClosure(xlVal,xlEnv);
    xlArgC = 1;
}

/* opDELAY - handler for opcode DELAY */
static void opDELAY(void)
{
    if (!xlCodeP(xlVal)) badargtype(xlVal);
    xlEnv = xlUnstackEnvironment(xlEnv);
    xlVal = xlMakePromise(xlVal,xlEnv);
    xlArgC = 1;
}

/* opATOM - handler for opcode ATOM */
static void opATOM(void)
{
    xlVal = (xlAtomP(xlVal) ? xlTrue : xlFalse);
    xlArgC = 1;
}

/* opEQ - handler for opcode EQ */
static void opEQ(void)
{
    xlVal = (xlPop() == xlVal ? xlTrue : xlFalse);
    xlArgC = 1;
}

/* opNULL - handler for opcode NULL */
static void opNULL(void)
{
    xlVal = (xlVal ? xlFalse : xlTrue);
    xlArgC = 1;
}

/* opCONS - handler for opcode xlCONS */
static void opCONS(void)
{
    xlVal = xlCons(xlVal,xlPop());
}

/* opCAR - handler for opcode CAR */
static void opCAR(void)
{
    if (!xlListP(xlVal)) badargtype(xlVal);
    xlVal = (xlVal ? xlCar(xlVal) : xlNil);
    xlArgC = 1;
}

/* opCDR - handler for opcode CDR */
static void opCDR(void)
{
    if (!xlListP(xlVal)) badargtype(xlVal);
    xlVal = (xlVal ? xlCdr(xlVal) : xlNil);
    xlArgC = 1;
}

/* opSETCAR - handler for opcode SETCAR */
static void opSETCAR(void)
{
    register xlValue tmp;
    tmp = xlPop();
    if (!xlConsP(xlVal)) badargtype(xlVal);
    xlSetCar(xlVal,tmp);
    xlArgC = 1;
}

/* opSETCDR - handler for opcode SETCDR */
static void opSETCDR(void)
{
    register xlValue tmp;
    tmp = xlPop();
    if (!xlConsP(xlVal)) badargtype(xlVal);
    xlSetCdr(xlVal,tmp);
    xlArgC = 1;
}

/* opADD - handler for opcode ADD */
static void opADD(void)
{
    register xlFIXTYPE val;
    register xlValue tmp;
    tmp = xlPop();
    if (xlFixnumP(tmp) && xlFixnumP(xlVal)) {
        val = xlGetFixnum(xlVal) + xlGetFixnum(tmp);
        xlVal = xlFastMakeFixnum(val);
    }
    else {
        xlPush(tmp); xlCPush(xlVal);
        xlVal = callsubr(xadd,2);
    }
    xlArgC = 1;
}

/* opSUB - handler for opcode SUB */
static void opSUB(void)
{
    register xlFIXTYPE val;
    register xlValue tmp;
    tmp = xlPop();
    if (xlFixnumP(tmp) && xlFixnumP(xlVal)) {
        val = xlGetFixnum(xlVal) - xlGetFixnum(tmp);
        xlVal = xlFastMakeFixnum(val);
    }
    else {
        xlPush(tmp); xlCPush(xlVal);
        xlVal = callsubr(xsub,2);
    }
    xlArgC = 1;
}

/* opMUL - handler for opcode MUL */
static void opMUL(void)
{
    register xlFIXTYPE val;
    register xlValue tmp;
    tmp = xlPop();
    if (xlFixnumP(tmp) && xlFixnumP(xlVal)) {
        val = xlGetFixnum(xlVal) * xlGetFixnum(tmp);
        xlVal = xlFastMakeFixnum(val);
    }
    else {
        xlPush(tmp); xlCPush(xlVal);
        xlVal = callsubr(xmul,2);
    }
    xlArgC = 1;
}

/* opQUO - handler for opcode QUO */
static void opQUO(void)
{
    register xlFIXTYPE val;
    register xlValue tmp;
    tmp = xlPop();
    if (xlFixnumP(tmp) && xlFixnumP(xlVal)) {
        if ((val = xlGetFixnum(tmp)) == (xlFIXTYPE)0)
            xlFmtError("division by zero");
        val = xlGetFixnum(xlVal) / val;
        xlVal = xlFastMakeFixnum(val);
    }
    else if (xlFixnumP(tmp))
        badargtype(xlVal);
    else
        badargtype(tmp);
    xlArgC = 1;
}

/* opLSS - handler for opcode LSS */
static void opLSS(void)
{
    register xlValue tmp;
    tmp = xlPop();
    if (xlFixnumP(tmp) && xlFixnumP(xlVal))
        xlVal = (xlGetFixnum(xlVal) < xlGetFixnum(tmp) ? xlTrue : xlFalse);
    else {
        xlPush(tmp); xlCPush(xlVal);
        xlVal = callsubr(xlss,2);
    }
    xlArgC = 1;
}

/* opEQL - handler for opcode EQL */
static void opEQL(void)
{
    register xlValue tmp;
    tmp = xlPop();
    if (xlFixnumP(tmp) && xlFixnumP(xlVal))
        xlVal = (xlGetFixnum(xlVal) == xlGetFixnum(tmp) ? xlTrue : xlFalse);
    else {
        xlPush(tmp); xlCPush(xlVal);
        xlVal = callsubr(xeql,2);
    }
    xlArgC = 1;
}

/* opGTR - handler for opcode GTR */
static void opGTR(void)
{
    register xlValue tmp;
    tmp = xlPop();
    if (xlFixnumP(tmp) && xlFixnumP(xlVal))
        xlVal = (xlGetFixnum(xlVal) > xlGetFixnum(tmp) ? xlTrue : xlFalse);
    else {
        xlPush(tmp); xlCPush(xlVal);
        xlVal = callsubr(xgtr,2);
    }
    xlArgC = 1;
}

/* opCATCH - handler for opcode CATCH */
static void opCATCH(void)
{
    register unsigned int offset;
    
    /* get the target offset */
    offset = *pc++ << 8;
    offset |= *pc++;

    /* push a catch frame */
    xlCtlCheck(7);
    xlCtlPush(xlEnv);
    xlCtlPush((xlValue)(xlOFFTYPE)offset);
    xlCtlPush(xlFun);
    xlCtlPush((xlValue)xlSP);
    xlCtlPush((xlValue)xlcatch);
    xlCtlPush(xlVal);
    xlCtlPush((xlValue)&cd_catch);
    xlcatch = xlCSP;
}

/* opUNCATCH - handler for opcode UNCATCH */
static void opUNCATCH(void)
{
    xlCtlDrop(2);
    xlcatch = (xlValue *)xlCtlPop();
    xlCtlDrop(4);
}

/* opPROTECT - handler for opcode PROTECT */
static void opPROTECT(void)
{
    xlCtlCheck(4);
    xlCtlPush(xlEnv);
    xlCtlPush((xlValue)xlSP);
    xlCtlPush(xlVal);
    xlCtlPush((xlValue)&cd_protect);
}

/* opUNPROTECT - handler for opcode UNPROTECT */
static void opUNPROTECT(void)
{
    xlCtlDrop(1);
    xlVal = xlCtlPop();
    xlCtlDrop(2);
}

/* opENV - handler for opcode ENV */
static void opENV(void)
{
    xlEnv = xlUnstackEnvironment(xlEnv);
    xlVal = xlEnv;
    xlArgC = 1;
}

/* opBAD - handler for all bad opcodes */
static void opBAD(void)
{
    xlError("bad opcode",xlMakeSmallFixnum((xlFIXTYPE)*--pc));
}

/* opcode dispatch table */
static void (*optab[256])(void);

/* xlInitInterpreter - initialize the interpreter */
void xlInitInterpreter(void)
{
    int i;
    
    /* first fill in the default opcode handler */
    for (i = 0; i < 256; ++i)
        optab[i] = opBAD;

    /* now fill in the real handlers */
    optab[xlopNOP] = opNOP;
    optab[xlopBRT] = opBRT;
    optab[xlopBRF] = opBRF;
    optab[xlopBR] = opBR;
    optab[xlopLIT] = opLIT;
    optab[xlopGREF] = opGREF;
    optab[xlopGSET] = opGSET;
    optab[xlopEREF] = opEREF;
    optab[xlopIREF] = opIREF;
    optab[xlopESET] = opESET;
    optab[xlopISET] = opISET;
    optab[xlopCALL] = opCALL;
    optab[xlopTCALL] = opTCALL;
    optab[xlopRETURN] = opRETURN;
    optab[xlopT] = opT;
    optab[xlopNIL] = opNIL;
    optab[xlopPUSH] = opPUSH;
    optab[xlopCLOSE] = opCLOSE;
    optab[xlopARGSEQ] = opARGSEQ;
    optab[xlopARGSGE] = opARGSGE;
    optab[xlopARGSBT] = opARGSBT;
    optab[xlopOPTARG] = opOPTARG;
    optab[xlopREST] = opREST;
    optab[xlopKEYARG] = opKEYARG;
    optab[xlopDELAY] = opDELAY;
    optab[xlopATOM] = opATOM;
    optab[xlopEQ] = opEQ;
    optab[xlopNULL] = opNULL;
    optab[xlopCONS] = opCONS;
    optab[xlopCAR] = opCAR;
    optab[xlopCDR] = opCDR;
    optab[xlopSETCAR] = opSETCAR;
    optab[xlopSETCDR] = opSETCDR;
    optab[xlopADD] = opADD;
    optab[xlopSUB] = opSUB;
    optab[xlopMUL] = opMUL;
    optab[xlopQUO] = opQUO;
    optab[xlopLSS] = opLSS;
    optab[xlopEQL] = opEQL;
    optab[xlopGTR] = opGTR;
    optab[xlopFRAME] = opFRAME;
    optab[xlopUNFRAME] = opUNFRAME;
    optab[xlopMETHOD] = opMETHOD;
    optab[xlopMVFRAME] = opMVFRAME;
    optab[xlopMVPUSH] = opMVPUSH;
    optab[xlopMVCALL] = opMVCALL;
    optab[xlopMVTCALL] = opMVTCALL;
    optab[xlopMVPOP] = opMVPOP;
    optab[xlopCATCH] = opCATCH;
    optab[xlopUNCATCH] = opUNCATCH;
    optab[xlopPROTECT] = opPROTECT;
    optab[xlopUNPROTECT] = opUNPROTECT;
    optab[xlopENV] = opENV;
}

/* xlInternalCall - call a function */
int xlInternalCall(xlValue *values,int vmax,xlValue fun,int argc,...)
{
    va_list ap;
    int valc;
    
    /* execute the function call */
    va_start(ap,argc);
    valc = xlInvokeInterpreter(values,vmax,fun,xlNil,argc,ap);
    va_end(ap);
    
    /* return the value count */
    return valc;
}

/* xlInvokeInterpreter - invoke the bytecode interpreter */
int xlInvokeInterpreter(xlValue *values,int vmax,xlValue fun,xlValue sel,int argc,va_list ap)
{
    xlErrorTarget target;
    void (*next)(void);
    xlValue *save_csp,*save_sp,*p;
    void (*save_next)(void);
    int save_pcoff,cnt;
    
    /* save the interpreter state */
    xlCheck(3);
    save_next = xlNext;
    save_pcoff = pc - base;
    save_csp = xlCSP;
    xlPush(xlVal);
    xlPush(xlFun);
    xlPush(xlEnv);
    save_sp = xlSP;
    
    /* initialize the registers */
    xlNext = NULL;

    /* push the return continuation */
    xlCtlCheck(1);
    xlCtlPush((xlValue)&cd_return);

    /* message sends have a negative argument count */
    if (argc < 0) {
        argc = -argc;
        xlArgC = argc;
        xlCheck(argc);
        xlSP -= argc;
        p = xlSP;
        *p++ = sel;
        --argc;
    }
    else {
        xlArgC = argc;
        xlCheck(argc);
        xlSP -= argc;
        p = xlSP;
    }
    
    /* setup the function and arguments */
    xlVal = fun;
    while (--argc >= 0)
        *p++ = va_arg(ap,xlValue);
    
    /* setup a target for the error handler */
    xlPushTarget(&target);
    switch (setjmp(target.target)) {
    case BCD_START:
        xlApply();
        break;
    case BCD_ABORT:
        throwtoreturn();
        break;
    case BCD_RETURN:
        xlPopTarget();
        
        /* store the return value */
        if (vmax > 0) {
            xlValue *p = xlSP - xlArgC;
            *values++ = xlVal;
            for (cnt = xlArgC; --cnt > 0 && --vmax > 0; )
                *values++ = *++p;
        }
        
        /* restore the interpreter state */
        xlNext = save_next;
        xlCSP = save_csp;
        xlSP = save_sp;
        xlEnv = xlPop();
        xlFun = xlPop();
        xlVal = xlPop();
        if (xlCodeP(xlFun)) {
            base = xlGetCodeStr(xlFun);
            pc = base + save_pcoff;
        }
        
        /* return value count */
        return xlArgC;
    }
    
    /* execute the code */
    for (;;) {

        /* check for control codes */
        if (--sample <= 0) {
            sample = xlSRATE;
            xlosCheck();
        }

        /* execute the next bytecode instruction */
        if (xlNext) {
            next = xlNext;
            xlNext = NULL;
            (*next)();
        }
        else {
            if (xlTraceBytecodes)
                xlDecodeInstruction(xlCurOutput(),
                                    xlFun,
                                    (int)(pc - base),
                                    xlEnv);
            (*optab[*pc++])();
        }
    }
}

/* xthrow - built-in function 'throw' */
void xthrow(void)
{
    throwtotag(xlGetArg());
}

/* xthrowerror - built-in function 'throw-error' */
void xthrowerror(void)
{
    xlVal = xlGetArg();
    xlLastArg();
    xlThrowError(xlVal);
}

/* xlThrowError - throw an error */
void xlThrowError(xlValue type)
{
    xlValue *target;

    /* setup the arguments */
    xlCPush(type);
    xlArgC = 1;

    /* find the throw target */
    if ((target = findmatchingcatch(s_error)) == NULL)
        xlJumpToTarget(-1);

    /* throw to the target */
    throwtotarget(target);
}

/* throwtotag - throw to a catch tag */
static void throwtotag(xlValue tag)
{
    xlValue *target;
    
    /* find the throw target */
    if ((target = findmatchingcatch(tag)) == NULL)
        xlError("no target for throw",tag);

    /* throw to the target */
    throwtotarget(target);
}

/* findmatchingcatch - find the catch frame matching a tag */
static xlValue *findmatchingcatch(xlValue tag)
{
    xlValue *p;
    for (p = xlcatch; p != NULL; p = (xlValue *)p[-3])
        if (tag == p[-2])
            return p;
    return NULL;
}

/* throwtoreturn - throw to a return frame */
static void throwtoreturn(void)
{
    xlValue *target,*p;
    
    /* find the return target */
    for (p = xlCSP, target = NULL; p > xlStkBase; p = xlCDUnmark(p))
        if ((xlCDispatch *)p[-1] == &cd_return) {
            target = p;
            break;
        }
    
    /* make sure we found a target */
    if (target == NULL)
        xlFmtAbort("no target for return");

    /* throw to the target */
    throwtotarget(target);
}
        
/* throwtotarget - throw to the target in xltarget */
static void throwtotarget(xlValue *target)
{        
    xlValue *dst,*p;
    int cnt;

    /* save the target */
    xltarget = target;
    
    /* save the position of the return values */
    p = xlSP + xlArgC;
    
    /* unwind to the target */
    while (xlCSP > xltarget)
        xlCDUnwind();
    xlCDRestore();
    
    /* move the arguments to the new stack position */
    for (dst = xlSP, cnt = xlArgC; --cnt >= 0; )
        *--dst = *--p;
    xlVal = *p;

    /* jump back the the bytecode interpreter */
    longjmp(xlerrtarget->target,BCD_NEXT);
}

/* xlJumpToTarget - jump to the current target */
void xlJumpToTarget(int sts)
{
    xlArgC = sts;
    longjmp(xlerrtarget->target,BCD_ABORT);
}

/* xlPushTarget - push a new target */
void xlPushTarget(xlErrorTarget *target)
{
    target->next = xlerrtarget;
    xlerrtarget = target;
}

/* xlPopTarget - pop a target */
void xlPopTarget(void)
{
    xlerrtarget = xlerrtarget->next;
}

/* xlFindVar - find a variable in an environment */
xlValue xlFindVar(xlValue env,xlValue var,int *poff)
{
    xlValue names;
    for (; env != xlNil; env = xlGetNextFrame(env)) {
        names = xlGetEnvNames(env);
        for (*poff = xlFIRSTENV; names != xlNil; ++(*poff), names = xlCdr(names))
            if (var == xlCar(names))
                return env;
        if (xlMethodEnvironmentP(env)) {
            names = xlGetIVar(xlGetEnvElement(xlGetNextFrame(env),xlFIRSTENV),xlivIVARS);
            for (*poff = xlFIRSTIVAR; names != xlNil; ++(*poff), names = xlCdr(names))
                if (var == xlCar(names))
                    return xlGetEnvElement(env,xlFIRSTENV);
        }
    }
    return xlNil;
}

/* xlApply - apply a function to arguments */
/*      The function should be in xlVal and the arguments should
        be on the stack.  The number of arguments should be in xlArgC.
*/
void xlApply(void)
{
    xlValue tmp;

    /* check for null function */
    if (xlNullP(xlVal))
        badfuntype(xlVal);

    /* dispatch on function type */
    switch (xlNodeType(xlVal)) {
    case xlSUBR:
        xlFun = xlVal;
        xlVal = (*xlGetSubr(xlFun))();
        xlSVReturn();
        break;
    case xlXSUBR:
        xlFun = xlVal;
        (*xlGetSubr(xlFun))();
        break;
    case xlCLOSURE:
        xlFun = xlGetCode(xlVal);
        xlEnv = xlGetEnvironment(xlVal);
        base = pc = xlGetCodeStr(xlFun);
        break;
    case xlOBJECT:
        xlSend(xlVal,xlGetArgSymbol());
        break;
    case xlCONTINUATION:
        tmp = xlGetArg();
        xlLastArg();
        restore_continuation();
        xlVal = tmp;
        xlSVReturn();
        break;
    default:
        badfuntype(xlVal);
    }
}

/* xlMakeContinuation - make a continuation */
xlValue xlMakeContinuation(void)
{
    xlValue cont,*dst,*p;
    xlFIXTYPE vsize,csize;

    /* unstack all the environments saved on the control stack */
    for (p = xlCSP; p > xlStkBase; )
        p = xlCDUnstack(p);
 
    /* create a continuation object */
    vsize = xlStkTop - xlSP;
    csize = xlCSP - xlStkBase;
    cont = xlNewContinuation(vsize + csize + 1);

    /* setup the destination pointer */
    dst = xlGetVector(cont);

    /* save the size of the value stack */
    *dst++ = xlMakeFixnum((xlFIXTYPE)vsize);

    /* copy the value stack */
    for (p = xlStkTop; --vsize >= 0; )
        *dst++ = *--p;

    /* copy the control stack */
    for (p = xlStkBase; --csize >= 0; )
        *dst++ = *p++;

    /* return the new continuation */
    return cont;
}

/* restore_continuation - restore a continuation to the stack */
/*      The continuation should be in xlVal.
*/
static void restore_continuation(void)
{
    xlFIXTYPE vsize,csize;
    xlValue *src;

    /* setup the source pointer */
    src = xlGetVector(xlVal) + 1;

    /* get the stack sizes */
    vsize = xlGetFixnum(xlGetElement(xlVal,0));
    csize = xlGetSize(xlVal) - vsize - 1;

    /* restore the value stack */
    for (xlSP = xlStkTop; --vsize >= 0; )
        *--xlSP = *src++;

    /* restore the control stack */
    for (xlCSP = xlStkBase; --csize >= 0; )
        *xlCSP++ = *src++;
}

/* xlUnstackEnvironment - move stack frames to the heap */
xlValue xlUnstackEnvironment(xlValue env)
{
    xlValue last,old,new,*src,*dst;
    xlFIXTYPE size;

    /* initialize */
    xlCPush(xlNil);
    last = xlNil;

    /* copy each stack environment frame to the heap */
    while (xlStackEnvironmentP(env)) {

        /* move ahead to the next frame */
        old = env;
        env = xlGetNextFrame(env);

        /* allocate a new frame and copy the data */
        size = xlGetEnvSize(old);
        new = xlNewFrame(xlGetFrameType(old) - 1,xlNil,size);
        src = xlGetVector(old);
        dst = xlGetVector(new);
        while (--size >= 0)
            *dst++ = *src++;

        /* link the new frame into the new environment */
        if (last == xlNil)
            xlSetTop(new);
        else
            xlSetNextFrame(last,new);
        last = new;
        
        /* store the forwarding address */
        xlSetFrameType(old,xlMSENV);
        xlSetForwardingAddr(old,new);
    }

    /* link the first heap frame into the new environment */
    if (last == xlNil)
        xlSetTop(env);
    else
        xlSetNextFrame(last,env);

    /* return the new environment */
    return xlPop();
}

/* stkframe - create an environment frame on the control stack */
static void stkframe(int argc,int extra,xlValue vnames)
{
    xlValue env;
    int n;

    /* expand the argument frame if necessary */
    if (extra > 0) {
        xlCheck(extra);
        for (n = extra; --n >= 0; )
            xlPush(xlNil);
    }

    /* make sure we have enough space on the control stack */
    xlCtlCheck(xlNODEWORDS + 1);

    /* make the environment frame */
    env = (xlValue)xlCSP;
    env->type = xlSENV;
    xlSetSize(env,argc + extra);
    xlSetVector(env,xlSP);
    xlCSP += xlNODEWORDS;

    /* store the environment variable names */
    xlSetNextFrame(env,xlEnv);
    xlSetEnvNames(env,vnames);

    /* establish the new frame */
    xlCtlPush((xlValue)&cd_frame);
    xlEnv = env;
}

/* xlCallErrorHandler - call the error handler */
void xlCallErrorHandler(void)
{
    xlVal = xlEnter("*ERROR-HANDLER*");
    if ((xlVal = xlGetValue(xlVal)) != xlNil)
        xlInternalCall(NULL,0,xlVal,3,xlFun,xlEnv);
    xlThrowError(s_error);
}

/* xlGCProtect - protect the state of the interpreter from the collector */
void xlGCProtect(void (*protected_fcn)(void))
{
    int pcoff;
    pcoff = pc - base;
    (*protected_fcn)();
    if (xlCodeP(xlFun)) {
        base = xlGetCodeStr(xlFun);
        pc = base + pcoff;
    }
}

/* xlShowControlStack - display some of the control stack */
void xlShowControlStack(int cmax)
{
    xlValue *p;
    xlErrPutStr("\nControl Stack Top:");
    for (p = xlCSP; --cmax >= 0 && p > xlStkBase; )
        p = xlCDPrint(p);
}

/* xlShowValueStack - display some of the value stack */
void xlShowValueStack(int vmax)
{
    xlValue *p;
    xlErrPutStr("\nValue Stack Top:");
    for (p = xlSP; --vmax >= 0 && p < xlStkTop; ) {
        xlErrPutStr("\n  ");
        xlErrPrint(*p++);
    }
}

/* badfuntype - bad function error */
static void badfuntype(xlValue arg)
{
    xlError("bad function type",arg);
}

/* badargtype - bad argument type error */
static void badargtype(xlValue arg)
{
    xlBadType(arg);
}

/* xlStkOver - value stack overflow */
xlEXPORT void xlStkOver(void)
{
    xlFmtAbort("value stack overflow");
}

/* xlCStkOver - control stack overflow */
xlEXPORT void xlCStkOver(void)
{
    xlFmtAbort("control stack overflow");
}

/* xlTooFew - too few arguments to this function */
xlEXPORT xlValue xlTooFew(void)
{
    xlFmtError("too few arguments");
    return xlNil; /* never reached */
}

/* xlTooMany - too many arguments to this function */
xlEXPORT void xlTooMany(void)
{
    xlFmtError("too many arguments");
}

/* xlBadType - incorrect argument type */
xlEXPORT xlValue xlBadType(xlValue val)
{
    xlError("incorrect type",val);
    return xlNil; /* never reached */
}
