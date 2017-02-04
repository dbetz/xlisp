/* xldbg.c - debugging routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"
#include "xlbcode.h"

/* instruction output formats */
#define FMT_NONE        0
#define FMT_BYTE        1
#define FMT_2BYTE       2
#define FMT_LOFF        3
#define FMT_WORD        4
#define FMT_EOFF        5
#define FMT_IOFF        6
#define FMT_OPTARG      7
#define FMT_KEYARG      8
#define FMT_FRAME       9
#define FMT_MVFRAME     10
#define FMT_FOFF        11
#define FMT_ARGS        12
#define FMT_ARGS2       13

typedef struct { int ot_code; char *ot_name; int ot_fmt; } OTDEF;
static OTDEF otab[] = {
{       xlopBRT,        "BRT",              FMT_WORD    },
{       xlopBRF,        "BRF",              FMT_WORD    },
{       xlopBR,         "BR",               FMT_WORD    },
{       xlopLIT,        "LIT",              FMT_LOFF    },
{       xlopGREF,       "GREF",             FMT_LOFF    },
{       xlopGSET,       "GSET",             FMT_LOFF    },
{       xlopEREF,       "EREF",             FMT_EOFF    },
{       xlopIREF,       "IREF",             FMT_IOFF    },
{       xlopESET,       "ESET",             FMT_EOFF    },
{       xlopISET,       "ISET",             FMT_IOFF    },
{       xlopCALL,       "CALL",             FMT_BYTE    },
{       xlopMVCALL,     "MVCALL",           FMT_NONE    },
{       xlopTCALL,      "TCALL",            FMT_BYTE    },
{       xlopMVTCALL,    "MVTCALL",          FMT_NONE    },
{       xlopRETURN,     "RETURN",           FMT_NONE    },
{       xlopFRAME,      "FRAME",            FMT_FRAME   },
{       xlopMVFRAME,    "MVFRAME",          FMT_MVFRAME },
{       xlopUNFRAME,    "UNFRAME",          FMT_NONE    },
{       xlopMVPUSH,     "MVPUSH",           FMT_NONE    },
{       xlopMVPOP,      "MVPOP",            FMT_NONE    },
{       xlopT,          "T",                FMT_NONE    },
{       xlopNIL,        "NIL",              FMT_NONE    },
{       xlopPUSH,       "PUSH",             FMT_NONE    },
{       xlopCLOSE,      "CLOSE",            FMT_NONE    },
{       xlopDELAY,      "DELAY",            FMT_NONE    },

{       xlopARGSEQ,     "ARGSEQ",           FMT_ARGS    },
{       xlopARGSGE,     "ARGSGE",           FMT_ARGS    },
{       xlopARGSBT,     "ARGSBT",           FMT_ARGS2   },
{       xlopOPTARG,     "OPTARG",           FMT_OPTARG  },
{       xlopKEYARG,     "KEYARG",           FMT_KEYARG  },
{       xlopREST,       "REST",             FMT_2BYTE   },

{       xlopMETHOD,     "METHOD",           FMT_NONE    },

{       xlopCATCH,      "CATCH",            FMT_WORD    },
{       xlopUNCATCH,    "UNCATCH",          FMT_NONE    },
{       xlopPROTECT,    "PROTECT",          FMT_NONE    },
{       xlopUNPROTECT,  "UNPROTECT",        FMT_NONE    },

/* integrable functions */
{       xlopATOM,       "ATOM",             FMT_NONE    },
{       xlopEQ,         "EQ?",              FMT_NONE    },
{       xlopNULL,       "NULL?",            FMT_NONE    },
{       xlopNULL,       "NOT",              FMT_NONE    },
{       xlopCONS,       "CONS",             FMT_NONE    },
{       xlopCAR,        "CAR",              FMT_NONE    },
{       xlopCDR,        "CDR",              FMT_NONE    },
{       xlopSETCAR,     "SET-CAR!",         FMT_NONE    },
{       xlopSETCDR,     "SET-CDR!",         FMT_NONE    },
{       xlopADD,        "+",                FMT_NONE    },
{       xlopSUB,        "-",                FMT_NONE    },
{       xlopMUL,        "*",                FMT_NONE    },
{       xlopQUO,        "QUOTIENT",         FMT_NONE    },
{       xlopLSS,        "<",                FMT_NONE    },
{       xlopEQL,        "=",                FMT_NONE    },
{       xlopGTR,        ">",                FMT_NONE    },
{       xlopENV,        "THE-ENVIRONMENT",  FMT_NONE    },

{0,0,0}
};

/* forward declarations */
static xlValue findenvname(xlValue env,int lev,int off);
static xlValue findivarname(xlValue env,int lev,int off);

/* xlDecodeProcedure - decode the instructions in a code object */
void xlDecodeProcedure(xlValue fptr,xlValue code,xlValue env)
{
    unsigned char *cp;
    xlFIXTYPE len,lc;
    int n;
    xlCPush(env);
    xlSetTop(xlNewFrame(xlENV,xlTop(),xlFIRSTENV));
    xlSetEnvNames(xlTop(),xlGetVNames(code));
    len = xlGetSLength(xlGetBCode(code));
    for (lc = 0; lc < len; lc += n) {
        n = xlDecodeInstruction(fptr,code,lc,xlTop());
        cp = xlGetCodeStr(code) + lc;
        switch (*cp) {
        case xlopFRAME:
            xlSetTop(xlNewFrame(xlENV,xlTop(),xlFIRSTENV));
            xlSetEnvNames(xlTop(),xlGetElement(code,cp[3]));
            break;
        case xlopMVFRAME:
            xlSetTop(xlNewFrame(xlENV,xlTop(),xlFIRSTENV));
            xlSetEnvNames(xlTop(),xlGetElement(code,cp[2]));
            xlSetTop(xlNewFrame(xlENV,xlTop(),xlFIRSTENV));
            xlSetEnvNames(xlTop(),xlGetElement(code,cp[3]));
            break;
        case xlopUNFRAME:
            xlSetTop(xlGetNextFrame(xlTop()));
            break;
        }
    }
    xlDrop(1);
}

/* xlDecodeInstruction - decode a single bytecode instruction */
int xlDecodeInstruction(xlValue fptr,xlValue code,xlFIXTYPE lc,xlValue env)
{
    unsigned char *cp;
    char buf[100];
    OTDEF *op;
    xlValue tmp;
    int n=1;

    /* start on a new line */
    xlNewline(fptr);

    /* get a pointer to the bytecodes for this instruction */
    cp = xlGetCodeStr(code) + lc;

    /* show the address and opcode */
    if ((tmp = xlGetCName(code)) == xlNil) {
        sprintf(buf,xlAFMT,code); xlPutStr(fptr,buf);
        sprintf(buf,":%04lx %02x ",lc,*cp);
    }
    else
        sprintf(buf,"%s:%04lx %02x ",xlGetString(xlGetPName(tmp)),lc,*cp);
    xlPutStr(fptr,buf);

    /* display the operands */
    for (op = otab; op->ot_name; ++op)
        if (*cp == op->ot_code) {
            switch (op->ot_fmt) {
            case FMT_NONE:
                sprintf(buf,"         %s",
                        op->ot_name);
                xlPutStr(fptr,buf);
                break;
            case FMT_BYTE:
                sprintf(buf,"%02x       %s %02x",
                        cp[1],
                        op->ot_name,cp[1]);
                xlPutStr(fptr,buf);
                n += 1;
                break;
            case FMT_2BYTE:
                sprintf(buf,"%02x %02x    %s %02x %02x",
                        cp[1],cp[2],
                        op->ot_name,cp[1],cp[2]);
                xlPutStr(fptr,buf);
                n += 2;
                break;
            case FMT_LOFF:
                sprintf(buf,"%02x       %s %02x ; ",
                        cp[1],
                        op->ot_name,cp[1]);
                xlPutStr(fptr,buf);
                xlWrite(xlGetElement(code,cp[1]),fptr);
                n += 1;
                break;
            case FMT_WORD:
                sprintf(buf,"%02x %02x    %s %02x%02x",
                        cp[1],cp[2],
                        op->ot_name,cp[1],cp[2]);
                xlPutStr(fptr,buf);
                n += 2;
                break;
            case FMT_EOFF:
                sprintf(buf,"%02x %02x    %s %02x %02x ; ",
                        cp[1],cp[2],
                        op->ot_name,cp[1],cp[2]);
                xlPutStr(fptr,buf);
                xlWrite(findenvname(env,cp[1],cp[2]),fptr);
                n += 2;
                break;
            case FMT_FOFF:
                sprintf(buf,"%02x       %s %02x ; ",
                        cp[1],
                        op->ot_name,cp[1]);
                xlPutStr(fptr,buf);
                xlWrite(findenvname(env,0,cp[1]),fptr);
                n += 1;
                break;
            case FMT_IOFF:
                sprintf(buf,"%02x %02x    %s %02x %02x ; ",
                        cp[1],cp[2],
                        op->ot_name,cp[1],cp[2]);
                xlPutStr(fptr,buf);
                xlWrite(findivarname(env,cp[1],cp[2]),fptr);
                n += 2;
                break;
            case FMT_OPTARG:
                sprintf(buf,"%02x %02x    %s %02x %02x",
                        cp[1],cp[2],
                        op->ot_name,cp[1],cp[2]);
                xlPutStr(fptr,buf);
                n += 2;
                break;
            case FMT_KEYARG:
                sprintf(buf,"%02x %02x %02x %s %02x %02x %02x ; ",
                        cp[1],cp[2],cp[3],
                        op->ot_name,cp[1],cp[2],cp[3]);
                xlPutStr(fptr,buf);
                xlWrite(xlGetElement(code,cp[1]),fptr);
                n += 3;
                break;
            case FMT_ARGS:
                sprintf(buf,"%02x       %s %02x ; ",
                        cp[1],
                        op->ot_name,cp[1]);
                xlPutStr(fptr,buf);
                xlWrite(xlGetVNames(code),fptr);
                n += 1;
                break;
            case FMT_ARGS2:
                sprintf(buf,"%02x %02x    %s %02x %02x ; ",
                        cp[1],cp[2],
                        op->ot_name,cp[1],cp[2]);
                xlPutStr(fptr,buf);
                xlWrite(xlGetVNames(code),fptr);
                n += 2;
                break;
            case FMT_FRAME:
                sprintf(buf,"%02x %02x %02x %s %02x %02x %02x ; ",
                        cp[1],cp[2],cp[3],
                        op->ot_name,cp[1],cp[2],cp[3]);
                xlPutStr(fptr,buf);
                xlWrite(xlGetElement(code,cp[3]),fptr);
                n += 3;
                break;
            case FMT_MVFRAME:
                sprintf(buf,"%02x %02x %02x %s %02x %02x %02x ; ",
                        cp[1],cp[2],cp[3],
                        op->ot_name,cp[1],cp[2],cp[3]);
                xlPutStr(fptr,buf);
                xlWrite(xlGetElement(code,cp[2]),fptr);
                xlPutStr(fptr," ");
                xlWrite(xlGetElement(code,cp[3]),fptr);
                n += 3;
                break;
            }
            return n;
        }
    
    /* unknown opcode */
    sprintf(buf,"      <UNKNOWN>");
    xlPutStr(fptr,buf);
    return n;
}

/* findenvname - find the name of an environment variable */
static xlValue findenvname(xlValue env,int lev,int off)
{
    xlValue names;
    
    /* find the frame */
    while (env != xlNil && --lev >= 0)
        env = xlGetNextFrame(env);
    if (env == xlNil)
        return xlNil;
    
    /* get the variable names for this frame */
    names = xlGetEnvNames(env);

    /* find the variable within the frame */
    while (names != xlNil && --off >= xlFIRSTENV)
        names = xlCdr(names);
    return names == xlNil ? xlNil : xlCar(names);
}

/* findivarname - find the name of an instance variable */
static xlValue findivarname(xlValue env,int lev,int off)
{
    xlValue names,cls;
    xlFIXTYPE ivstart;

    /* find the frame */
    while (env != xlNil && --lev >= 0)
        env = xlGetNextFrame(env);
    if (env == xlNil)
        return xlNil;
    
    /* get the method's class */
    cls = xlGetEnvElement(xlGetNextFrame(env),xlFIRSTENV);

    /* get the maximum instance variable offset */
    ivstart = xlFIRSTIVAR + xlGetFixnum(xlGetIVar(cls,xlivIVARTOTAL));

    /* find the instance variable */
    while (cls != xlNil) {

        /* compute the start of variables for this class */
        ivstart -= xlGetFixnum(xlGetIVar(cls,xlivIVARCNT));

        /* check to see if the variable is in this class */
        if (off >= ivstart) {

            /* get the instance variable names */
            names = xlGetIVar(cls,xlivIVARS);
    
            /* compute the local offset */
            off -= ivstart;

            /* find the instance variable */
            while (names != xlNil && --off >= 0)
                names = xlCdr(names);
            return names == xlNil ? xlNil : xlCar(names);
        }

        /* try the superclass */
        cls = xlGetIVar(cls,xlivSUPERCLASS);
    }
    
    /* not found */
    return xlNil;
}
