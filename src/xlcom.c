/* xlcom.c - the xlisp bytecode compiler */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"
#include "xlbcode.h"

/* size of code buffer */
#define CMAX    40000

/* continuation types */
#define C_RETURN        -1
#define C_NEXT          -2

/* macro to check for a lambda list keyword */
#define lambdakey(x)    ((x) == lk_optional \
                      || (x) == lk_rest \
                      || (x) == lk_key \
                      || (x) == lk_allow_other_keys \
                      || (x) == lk_aux \
                      || slambdakey(x))

/* macro to check for a scheme lambda list keyword */
#define slambdakey(x)   ((x) == slk_optional \
                      || (x) == slk_rest)

/* global variables */
xlEXPORT int xlDebugModeP = FALSE;

/* external variables */
extern xlValue lk_optional,lk_rest,lk_key,lk_allow_other_keys,lk_aux;
extern xlValue slk_optional,slk_rest;

/* local variables */
static xlValue info;            /* compiler info */

/* code buffer */
static unsigned char *cbuff = NULL;     /* base of code buffer */
static int cbase;                       /* base for current function */
static int cptr;                        /* code buffer pointer */

/* forward declarations */
static void do_expr(xlValue expr,int cont);
static int in_ntab(xlValue expr,int cont);
static int in_ftab(xlValue expr,int cont);
static void do_define(xlValue form,int cont);
static void define1(xlValue list,xlValue body,int cont);
static void do_setq(xlValue form,int cont);
static void do_setvar(xlValue form,int cont);
static void do_quote(xlValue form,int cont);
static void do_lambda(xlValue form,int cont);
static void do_namedlambda(xlValue form,int cont);
static void do_method(xlValue form,int cont);
static void cd_fundefinition(xlValue fun,xlValue fargs,xlValue body);
static void parse_lambda_expr(xlValue fargs,xlValue body,int mflag);
static int count_arguments(xlValue fargs,int *prargc,int *poargc,xlValue *prestarg,int *pkargc);
static void add_extra_arguments(xlValue fargs);
static void parse_optional_arguments(xlValue key,xlValue *pfargs,int base);
static void parse_optional_argument(xlValue form,xlValue *parg,xlValue *pdef,xlValue *psvar);
static void parse_key_arguments(xlValue *pfargs,int base);
static void parse_key_argument(xlValue form,xlValue *parg,xlValue *pkey,xlValue *pdef,xlValue *psvar);
static void parse_aux_arguments(xlValue *pfargs);
static void parse_aux_argument(xlValue form,xlValue *parg,xlValue *pdef);
static void add_argument_name(xlValue name);
static void patch_argument_name(xlValue name);
static int get_argument_offset(xlValue name);
static void do_delay(xlValue form,int cont);
static void do_let(xlValue form,int cont);
static void do_named_let(xlValue form,int cont);
static xlValue extract_let_variables(xlValue bindings,int *pcnt);
static void do_unnamed_let(xlValue form,int cont);
static void do_letrec(xlValue form,int cont);
static void do_letstar(xlValue form,int cont);
static void letstar1(xlValue blist,xlValue body,int cont);
static int push_dummy_values(xlValue blist);
static int push_init_expressions(xlValue blist);
static void generate_let_setup_code(xlValue blist);
static void parse_let_variables(xlValue blist,int *pcnt,int *pextra);
static void set_bound_variables(xlValue blist);
static void do_mvbind(xlValue form,int cont);
static void do_mvcall(xlValue form,int cont);
static xlValue make_code_object(xlValue fun);
static void do_cond(xlValue form,int cont);
static void do_and(xlValue form,int cont);
static void do_or(xlValue form,int cont);
static void do_if(xlValue form,int cont);
static void do_begin(xlValue form,int cont);
static void do_while(xlValue form,int cont);
static void do_catch(xlValue form,int cont);
static void do_unwindprotect(xlValue form,int cont);
static void do_call(xlValue form,int cont);
static int push_args(xlValue form);
static void do_nary(int op,int n,xlValue form,int cont);
static void push_nargs(xlValue form,int n);
static void do_literal(xlValue lit,int cont);
static void do_identifier(xlValue sym,int cont);
static void do_continuation(int cont);
static void add_frame(void);
static void remove_frame(void);
static int add_level(void);
static void remove_level(int oldcbase);
static int findvariable(int opcode,xlValue sym,int *plev,int *poff);
static int findcvariable(int opcode,xlValue frame,xlValue sym,int *poff);
static int findliteral(xlValue lit);
static void cd_variable(int op,xlValue sym);
static void cd_evariable(int op,int lev,int off);
static void cd_literal(xlValue lit);
static int nextcaddr(void);
static int putcbyte(int b);
static int putcword(int w);
static void fixup(int chn);

/* integrable function table */
typedef struct { char *nt_name; int nt_code,nt_args; } NTDEF;
static NTDEF ntab[] = {
{       "ATOM",                 xlopATOM,       1       },
{       "EQ?",                  xlopEQ,         2       },
{       "NULL?",                xlopNULL,       1       },
{       "NOT",                  xlopNULL,       1       },
{       "CONS",                 xlopCONS,       2       },
{       "CAR",                  xlopCAR,        1       },
{       "CDR",                  xlopCDR,        1       },
{       "SET-CAR!",             xlopSETCAR,     2       },
{       "SET-CDR!",             xlopSETCDR,     2       },
{       "+",                    xlopADD,        -2      },
{       "-",                    xlopSUB,        -2      },
{       "*",                    xlopMUL,        -2      },
{       "QUOTIENT",             xlopQUO,        -2      },
{       "<",                    xlopLSS,        -2      },
{       "=",                    xlopEQL,        -2      },
{       ">",                    xlopGTR,        -2      },
{0,0,0}
};

/* integrable functions that shouldn't be disabled for debugging */
static NTDEF ntab2[] = {
{       "THE-ENVIRONMENT",      xlopENV,        0       },
{0,0,0}
};

/* special form table */
typedef struct { char *ft_name; void (*ft_fcn)(xlValue,int); } FTDEF;
static FTDEF ftab[] = {
{       "QUOTE",                do_quote                },
{       "LAMBDA",               do_lambda               },
{       "NAMED-LAMBDA",         do_namedlambda          },
{       "METHOD",               do_method               },
{       "DELAY",                do_delay                },
{       "LET",                  do_let                  },
{       "LET*",                 do_letstar              },
{       "LETREC",               do_letrec               },
{       "MULTIPLE-VALUE-BIND",  do_mvbind               },
{       "MULTIPLE-VALUE-CALL",  do_mvcall               },
{       "DEFINE",               do_define               },
{       "SET!",                 do_setq                 },
{       "IF",                   do_if                   },
{       "COND",                 do_cond                 },
{       "BEGIN",                do_begin                },
{       "SEQUENCE",             do_begin                },
{       "AND",                  do_and                  },
{       "OR",                   do_or                   },
{       "WHILE",                do_while                },
{       "CATCH",                do_catch                },
{       "UNWIND-PROTECT",       do_unwindprotect        },
{0,0}
};

/* xlCompile - compile an expression */
xlValue xlCompile(xlValue expr,xlValue ctenv)
{
    /* allocate the code buffer on the first call */
    if (cbuff == NULL) {
        if ((cbuff = xlosAlloc(CMAX)) == NULL)
            xlFatal("insufficient memory");
    }

    /* initialize the compile time environment */
    info = xlCons(xlNil,xlNil); xlCPush(info);
    xlSetCar(info,xlNewFrame(xlENV,ctenv,xlFIRSTENV));
    xlSetCdr(info,xlCons(xlNil,xlNil));

    /* setup the base of the code for this function */
    cbase = cptr = 0;

    /* setup the entry code */
    putcbyte(xlopARGSEQ);
    putcbyte(0);

    /* compile the expression */
    do_expr(expr,xlDebugModeP ? C_NEXT : C_RETURN);
    if (xlDebugModeP)
        putcbyte(xlopRETURN);

    /* build the code object */
    xlSetTop(make_code_object(xlNil));
    return xlPop();
}

/* xlCompileMethod - compile a method */
xlValue xlCompileMethod(xlValue fun,xlValue fargs,xlValue body,xlValue ctenv)
{
    /* initialize the compile time environment */
    info = xlCons(xlNil,xlNil); xlCPush(info);
    xlSetCar(info,xlNewFrame(xlMENV,ctenv,xlFIRSTENV));
    xlSetCdr(info,xlCons(xlNil,xlNil));

    /* setup the base of the code for this function */
    cbase = cptr = 0;

    /* add 'self' to the argument list */
    xlCPush(xlCons(xlEnter("SELF"),fargs));

    /* compile the lambda list and the function body */
    parse_lambda_expr(xlTop(),body,TRUE);
    
    /* build the code object */
    xlSetTop(make_code_object(fun));
    return xlPop();
}

/* do_expr - compile an expression */
static void do_expr(xlValue expr,int cont)
{
    xlValue fun;
    xlCPush(expr);
    if (xlConsP(expr)) {
        fun = xlCar(expr);
        if (!xlSymbolP(fun) || (!in_ntab(expr,cont) && !in_ftab(expr,cont)))
            do_call(expr,cont);
    }
    else if (xlSymbolP(expr))
        do_identifier(expr,cont);
    else
        do_literal(expr,cont);
    xlDrop(1);
}

/* in_ntab - check for a function in ntab */
static int in_ntab(xlValue expr,int cont)
{
    char *pname = xlGetString(xlGetPName(xlCar(expr)));
    NTDEF *nptr;
    if (!xlDebugModeP) {
        for (nptr = ntab; nptr->nt_name; ++nptr)
            if (strcmp(pname,nptr->nt_name) == 0) {
                do_nary(nptr->nt_code,nptr->nt_args,expr,cont);
                return TRUE;
            }
    }
    for (nptr = ntab2; nptr->nt_name; ++nptr)
        if (strcmp(pname,nptr->nt_name) == 0) {
            do_nary(nptr->nt_code,nptr->nt_args,expr,cont);
            return TRUE;
        }
    return FALSE;
}

/* in_ftab - check for a function in ftab */
static int in_ftab(xlValue expr,int cont)
{
    FTDEF *fptr;
    char *pname = xlGetString(xlGetPName(xlCar(expr)));
    for (fptr = ftab; fptr->ft_name; ++fptr)
        if (strcmp(pname,fptr->ft_name) == 0) {
            (*fptr->ft_fcn)(xlCdr(expr),cont);
            return TRUE;
        }
    return FALSE;
}

/* do_define - handle the (DEFINE ... ) expression */
static void do_define(xlValue form,int cont)
{
    if (xlAtomP(form))
        xlError("expecting symbol or function template",form);
    define1(xlCar(form),xlCdr(form),cont);
}

/* define1 - helper routine for do_define */
static void define1(xlValue list,xlValue body,int cont)
{
    int opcode,off;

    /* check for procedure definition */
    if (xlConsP(list)) {
        xlCPush(xlCar(list));
        if (!xlSymbolP(xlTop()))
            xlError("expecting function name",xlTop());
        else if (xlAtomP(xlCdr(list)))
            xlError("expecting argument list",xlCdr(list));
        cd_fundefinition(xlCar(list),xlCdr(list),body);
    }
    else {
        xlCPush(list);
        do_begin(body,C_NEXT);
    }
        
    /* define the variable value */
    if ((opcode = findcvariable(xlopESET,xlCar(info),xlTop(),&off)) != 0)
        cd_evariable(opcode,0,off);
    else
        cd_variable(xlopGSET,xlTop());
    do_literal(xlTop(),cont);
    xlDrop(1);
}

/* do_setq - compile the (SET! ... ) expression */
static void do_setq(xlValue form,int cont)
{
    xlCheck(1);
    while (xlConsP(form) && xlConsP(xlCdr(form))) {
        xlPush(xlCdr(xlCdr(form)));
        if (xlAtomP(form))
            xlError("expecting symbol",form);
        else if (xlSymbolP(xlCar(form)))
            do_setvar(form,xlTop() ? C_NEXT : cont);
        else
            xlError("expecting symbol",form);
        form = xlPop();
    }
    if (form != xlNil)
        xlFmtError("bad syntax in set! ~S",form);
}

/* do_setvar - compile the (SET! var value) expression */
static void do_setvar(xlValue form,int cont)
{
    int opcode,lev,off;

    /* get the variable name */
    xlCPush(xlCar(form));

    /* compile the value expression */
    form = xlCdr(form);
    if (xlAtomP(form))
        xlError("expecting value expression",form);
    do_expr(xlCar(form),C_NEXT);

    /* set the variable value */
    if ((opcode = findvariable(xlopESET,xlTop(),&lev,&off)) != 0)
        cd_evariable(opcode,lev,off);
    else
        cd_variable(xlopGSET,xlTop());
    do_continuation(cont);
    xlDrop(1);
}

/* do_quote - compile the (QUOTE ... ) expression */
static void do_quote(xlValue form,int cont)
{
    if (xlAtomP(form))
        xlError("expecting quoted expression",form);
    do_literal(xlCar(form),cont);
}

/* do_lambda - compile the (LAMBDA ... ) expression */
static void do_lambda(xlValue form,int cont)
{
    if (xlAtomP(form))
        xlError("expecting argument list",form);
    cd_fundefinition(xlNil,xlCar(form),xlCdr(form));
    do_continuation(cont);
}

/* do_namedlambda - compile the (NAMED-LAMBDA ... ) expression */
static void do_namedlambda(xlValue form,int cont)
{
    if (xlAtomP(form) || !xlSymbolP(xlCar(form)))
        xlError("expecting function name",form);
    else if (xlAtomP(xlCdr(form)))
        xlError("expecting argument list",form);
    cd_fundefinition(xlCar(form),xlCar(xlCdr(form)),xlCdr(xlCdr(form)));
    do_continuation(cont);
}

/* do_method - compile the (METHOD ... ) expression */
static void do_method(xlValue form,int cont)
{
    /* check syntax */
    if (xlAtomP(form) || !xlSymbolP(xlCar(form)))
        xlError("expecting class",form);
    else if (xlAtomP(xlCdr(form)))
        xlError("expecting selector",form);
    else if (xlAtomP(xlCdr(xlCdr(form))))
        xlError("expecting argument list",form);

    /* get the arguments */
    (void)xlCar(form);
    (void)xlCar(xlCdr(form));
    (void)xlCar(xlCdr(xlCdr(form)));
    (void)xlCdr(xlCdr(xlCdr(form)));

    cd_fundefinition(xlCar(form),xlCar(xlCdr(form)),xlCdr(xlCdr(form)));

    /* compile the continuation */
    do_continuation(cont);
}

/* cd_fundefinition - compile the function */
static void cd_fundefinition(xlValue fun,xlValue fargs,xlValue body)
{
    int oldcbase;

    /* establish a new environment frame */
    oldcbase = add_level();

    /* compile the lambda list and the function body */
    parse_lambda_expr(fargs,body,FALSE);

    /* build the code object */
    xlCPush(make_code_object(fun));
    
    /* restore the previous environment */
    remove_level(oldcbase);

    /* compile code to create a closure */
    do_literal(xlPop(),C_NEXT);
    putcbyte(xlopCLOSE);
}

/* parse_lambda_expr - parse a lambda expression */
static void parse_lambda_expr(xlValue fargs,xlValue body,int mflag)
{
    int rargc,oargc,kargc,extra;
    xlValue arg,key,restarg;
    
    /* count the arguments */
    extra = count_arguments(fargs,&rargc,&oargc,&restarg,&kargc);
    
    /* output the entry code */
    if (kargc == 0 && restarg == xlNil) {
        if (oargc == 0) {
            putcbyte(xlopARGSEQ);
            putcbyte(rargc);
        }
        else {
            putcbyte(xlopARGSBT);
            putcbyte(rargc);
            putcbyte(rargc + oargc);
        }
    }
    else {
        putcbyte(xlopARGSGE);
        putcbyte(rargc);
    }
    
    /* handle each required argument */
    while (xlConsP(fargs)
    &&     (arg = xlCar(fargs)) != xlNil
    &&     !lambdakey(arg)) {
        add_argument_name(arg);
        fargs = xlCdr(fargs);
    }

    /* insert the code to mark this frame as a method frame */
    if (mflag)
        putcbyte(xlopMETHOD);

    /* handle extra variables */
    if (extra > xlFIRSTENV) {

        /* add an extra frame for optional, key, rest and aux variables */
        add_frame();
        add_extra_arguments(fargs);
        putcbyte(xlopFRAME);
        putcbyte(0);
        putcbyte(extra);
        putcbyte(findliteral(xlGetEnvNames(xlCar(info))));

        /* check for &optional or #!optional arguments */
        if (xlConsP(fargs) && (xlCar(fargs) == lk_optional || xlCar(fargs) == slk_optional)) {
            key = xlCar(fargs);
            fargs = xlCdr(fargs);
            parse_optional_arguments(key,&fargs,xlFIRSTENV + rargc);
        }

        /* check for the &rest argument */
        if (xlConsP(fargs) && (xlCar(fargs) == lk_rest || xlCar(fargs) == slk_rest))
            fargs = xlCdr(xlCdr(fargs));
        if (restarg)
            patch_argument_name(restarg);

        /* check for &key arguments */
        if (xlConsP(fargs) && xlCar(fargs) == lk_key) {
            fargs = xlCdr(fargs);
            parse_key_arguments(&fargs,xlFIRSTENV + rargc + oargc);
            if (xlConsP(fargs) && xlCar(fargs) == lk_allow_other_keys)
                fargs = xlCdr(fargs);
        }

        /* check for &aux arguments */
        if (xlConsP(fargs) && xlCar(fargs) == lk_aux) {
            fargs = xlCdr(fargs);
            parse_aux_arguments(&fargs);
        }

        /* output instruction to build the '&rest' argument list */
        if (restarg) {
            putcbyte(xlopREST);
            putcbyte(xlFIRSTENV + rargc + oargc);
            putcbyte(get_argument_offset(restarg));
        }

    }
    
    /* compile the function body */
    do_begin(body,xlDebugModeP ? C_NEXT : C_RETURN);
    if (xlDebugModeP)
        putcbyte(xlopRETURN);

    /* remove the extra variable frame */
    if (extra > xlFIRSTENV)
        remove_frame();
}

/* count_arguments - count the arguments */
static int count_arguments(xlValue fargs,int *prargc,int *poargc,xlValue *prestarg,int *pkargc)
{
    int extra=xlFIRSTENV,rargc=0,oargc=0,kargc=0;
    xlValue arg,restarg=NULL,key,def,svar;
    
    /* skip each required argument */
    while (xlConsP(fargs)
    &&     (arg = xlCar(fargs)) != xlNil
    &&     !lambdakey(arg)) {
        if (!xlSymbolP(arg))
            xlError("variable must be a symbol",arg);
        fargs = xlCdr(fargs);
        ++rargc;
    }

    /* check for '&optional and #!optional arguments */
    if (xlConsP(fargs) && (xlCar(fargs) == lk_optional || xlCar(fargs) == slk_optional)) {
        key = xlCar(fargs);
        fargs = xlCdr(fargs);
        while (xlConsP(fargs)
        &&     (arg = xlCar(fargs)) != xlNil
        &&     !lambdakey(arg)) {
            if (key == lk_optional)
                parse_optional_argument(arg,&arg,&def,&svar);
            else {
                if (!xlSymbolP(arg))
                    xlError("#!optional argument must be a symbol",arg);
                svar = xlNil;
            }
            if (svar)
                ++extra;
            fargs = xlCdr(fargs);
            ++extra;
            ++oargc;
        }
    }

    /* check for the &rest or #!rest argument */
    if (xlConsP(fargs) && (xlCar(fargs) == lk_rest || xlCar(fargs) == slk_rest)) {
        fargs = xlCdr(fargs);
        if (xlConsP(fargs)
        &&  (arg = xlCar(fargs)) != xlNil
        &&  !lambdakey(arg)) {
            if (!xlSymbolP(arg))
                xlError("&rest variable must be a symbol",arg);
            fargs = xlCdr(fargs);
            restarg = arg;
        }
        else
            xlError("expecting the &rest variable",fargs);
    }

    /* check for &key arguments */
    if (xlConsP(fargs) && xlCar(fargs) == lk_key) {
        fargs = xlCdr(fargs);
        while (xlConsP(fargs)
        &&     (arg = xlCar(fargs)) != xlNil
        &&     !lambdakey(arg)) {
            parse_key_argument(arg,&arg,&key,&def,&svar);
            if (svar)
                ++extra;
            fargs = xlCdr(fargs);
            ++extra;
            ++kargc;
        }
        if (xlConsP(fargs) && xlCar(fargs) == lk_allow_other_keys)
            fargs = xlCdr(fargs);
    }

    /* check for &aux arguments */
    if (xlConsP(fargs) && xlCar(fargs) == lk_aux) {
        fargs = xlCdr(fargs);
        while (xlConsP(fargs)
        &&     (arg = xlCar(fargs)) != xlNil
        &&     !lambdakey(arg)) {
            parse_aux_argument(arg,&arg,&def);
            fargs = xlCdr(fargs);
            ++extra;
        }
    }

    /* check for the a dotted tail */
    if (restarg == xlNil && xlSymbolP(fargs)) {
        restarg = fargs;
        fargs = xlNil;
    }

    /* add the &rest argument */
    if (restarg)
        ++extra;
    
    /* check for the end of the argument list */
    if (fargs != xlNil)
        xlError("bad argument list tail",fargs);

    /* return the argument counts */
    *prargc = rargc;
    *poargc = oargc;
    *prestarg = restarg;
    *pkargc = kargc;
    return extra;
}

/* add_extra_arguments - add extra (optional, key, rest, aux) arguments */
static void add_extra_arguments(xlValue fargs)
{
    xlValue arg,restarg=NULL,key,def,svar;
    
    /* skip each required argument */
    while (xlConsP(fargs)
    &&     (arg = xlCar(fargs)) != xlNil
    &&     !lambdakey(arg)) {
        if (!xlSymbolP(arg))
            xlError("variable must be a symbol",arg);
        fargs = xlCdr(fargs);
    }

    /* check for '&optional and #!optional arguments */
    if (xlConsP(fargs) && (xlCar(fargs) == lk_optional || xlCar(fargs) == slk_optional)) {
        key = xlCar(fargs);
        fargs = xlCdr(fargs);
        while (xlConsP(fargs)
        &&     (arg = xlCar(fargs)) != xlNil
        &&     !lambdakey(arg)) {
            if (key == lk_optional)
                parse_optional_argument(arg,&arg,&def,&svar);
            else {
                if (!xlSymbolP(arg))
                    xlError("#!optional argument must be a symbol",arg);
                svar = xlNil;
            }
            add_argument_name(xlNil);   /* arg */
            if (svar)
                add_argument_name(xlNil);       /* svar */
            fargs = xlCdr(fargs);
        }
    }

    /* check for the &rest or #!rest argument */
    if (xlConsP(fargs) && (xlCar(fargs) == lk_rest || xlCar(fargs) == slk_rest)) {
        fargs = xlCdr(fargs);
        if (xlConsP(fargs)
        &&  (arg = xlCar(fargs)) != xlNil
        &&  !lambdakey(arg)) {
            if (!xlSymbolP(arg))
                xlError("&rest variable must be a symbol",arg);
            fargs = xlCdr(fargs);
            restarg = arg;
        }
        else
            xlError("expecting the &rest variable",fargs);
    }

    /* check for &key arguments */
    if (xlConsP(fargs) && xlCar(fargs) == lk_key) {
        fargs = xlCdr(fargs);
        while (xlConsP(fargs)
        &&     (arg = xlCar(fargs)) != xlNil
        &&     !lambdakey(arg)) {
            parse_key_argument(arg,&arg,&key,&def,&svar);
            add_argument_name(xlNil);   /* arg */
            if (svar)
                add_argument_name(xlNil);       /* svar */
            fargs = xlCdr(fargs);
        }
        if (xlConsP(fargs) && xlCar(fargs) == lk_allow_other_keys)
            fargs = xlCdr(fargs);
    }

    /* check for &aux arguments */
    if (xlConsP(fargs) && xlCar(fargs) == lk_aux) {
        fargs = xlCdr(fargs);
        while (xlConsP(fargs)
        &&     (arg = xlCar(fargs)) != xlNil
        &&     !lambdakey(arg)) {
            parse_aux_argument(arg,&arg,&def);
            add_argument_name(xlNil);   /* arg */
            fargs = xlCdr(fargs);
        }
    }

    /* check for the a dotted tail */
    if (restarg == xlNil && xlSymbolP(fargs)) {
        restarg = fargs;
        fargs = xlNil;
    }

    /* add the &rest argument */
    if (restarg)
        add_argument_name(xlNil);               /* rest */
    
    /* check for the end of the argument list */
    if (fargs != xlNil)
        xlError("bad argument list tail",fargs);
}

/* parse_optional_arguments - parse the &optional arguments */
static void parse_optional_arguments(xlValue key,xlValue *pfargs,int base)
{
    extern xlValue xlDefaultObject;
    int patch,patch2,chain,off,oargc=0;
    xlValue fargs,arg,def,svar;

    /* generate the conditional branches */
    fargs = *pfargs;
    while (xlConsP(fargs)
    &&     (arg = xlCar(fargs)) != xlNil
    &&     !lambdakey(arg)) {
        
        /* parse the argument form */
        if (key == lk_optional)
            parse_optional_argument(arg,&arg,&def,&svar);
        else {
            def = xlDefaultObject;
            svar = xlNil;
        }

        /* output code to check for the optional argument */
        patch = putcbyte(xlopOPTARG);
        putcbyte(base + oargc);
        putcbyte(0);

        /* set the supplied-p variable if present */
        if (svar)
            cd_evariable(xlopESET,0,0);

        /* compile the default value expression */
        if (def) {
            putcbyte(xlopBRT);
            chain = putcword(0);
            if (def == xlDefaultObject)
                do_literal(def,C_NEXT);
            else
                do_expr(def,C_NEXT);
            patch2 = nextcaddr();
            cd_evariable(xlopESET,0,0);
            fixup(chain);
        }

        /* add the argument name to the name list */
        patch_argument_name(arg);
        off = get_argument_offset(arg);
        cbuff[cbase+patch+2] = off;

        /* check for a supplied-p variable */
        if (svar) {
            patch_argument_name(svar);
            cbuff[cbase+patch+5] = get_argument_offset(svar);
        }

        /* patch the setting of the default value */
        if (def)
            cbuff[cbase+patch2+2] = off;

        /* move the formal argument list pointer ahead */
        fargs = xlCdr(fargs);
        ++oargc;
    }

    /* update the reference parameters */
    *pfargs = fargs;
}

/* parse_optional_argument - parse a single &optional argument */
static void parse_optional_argument(xlValue form,xlValue *parg,xlValue *pdef,xlValue *psvar)
{
    *pdef = *psvar = xlNil;
    if (xlConsP(form)) {
        if ((*pdef = xlCdr(form)) != xlNil) {
            if (xlConsP(*pdef)) {
                if ((*psvar = xlCdr(*pdef)) != xlNil) {
                    if (xlConsP(*psvar)) {
                        *psvar = xlCar(*psvar);
                        if (!xlSymbolP(*psvar))
                            xlFmtError("supplied-p variable must be a symbol");
                    }
                    else
                        xlFmtError("expecting supplied-p variable");
                }
                *pdef = xlCar(*pdef);
            }
            else
                xlFmtError("expecting init expression");
        }
        *parg = xlCar(form);
    }
    else
        *parg = form;
    if (!xlSymbolP(*parg))
        xlError("&optional variable must be a symbol",*parg);
}

/* parse_key_arguments - parse the &key arguments */
static void parse_key_arguments(xlValue *pfargs,int base)
{
    xlValue fargs,arg,key,def,svar;
    int patch,patch2,chain,off;

    /* generate the conditional branches */
    fargs = *pfargs;
    while (xlConsP(fargs)
    &&     (arg = xlCar(fargs)) != xlNil
    &&     !lambdakey(arg)) {
        
        /* parse the argument form */
        parse_key_argument(arg,&arg,&key,&def,&svar);
        
        /* check for the &key argument */
        patch = putcbyte(xlopKEYARG);
        putcbyte(findliteral(key));
        putcbyte(base);
        putcbyte(0);

        /* set the supplied-p variable if present */
        if (svar)
            cd_evariable(xlopESET,0,0);

        /* compile the default value expression */
        if (def) {
            putcbyte(xlopBRT);
            chain = putcword(0);
            do_expr(def,C_NEXT);
            patch2 = nextcaddr();
            cd_evariable(xlopESET,0,0);
            fixup(chain);
        }

        /* add the argument name to the name list */
        patch_argument_name(arg);
        off = get_argument_offset(arg);
        cbuff[cbase+patch+3] = off;

        /* set the supplied-p variable */
        if (svar) {
            patch_argument_name(svar);
            cbuff[cbase+patch+6] = get_argument_offset(svar);
        }

        /* patch the setting of the default value */
        if (def)
            cbuff[cbase+patch2+2] = off;

        /* move the formal argument list pointer ahead */
        fargs = xlCdr(fargs);
    }

    /* update the reference parameters */
    *pfargs = fargs;
}

/* parse_key_argument - parse a single &key argument */
static void parse_key_argument(xlValue form,xlValue *parg,xlValue *pkey,xlValue *pdef,xlValue *psvar)
{
    extern xlValue xlKeywordPackage;
    xlValue key;
    *pkey = *pdef = *psvar = xlNil;
    if (xlConsP(form)) {
        if ((*pdef = xlCdr(form)) != xlNil) {
            if (xlConsP(*pdef)) {
                if ((*psvar = xlCdr(*pdef)) != xlNil) {
                    if (xlConsP(*psvar)) {
                        *psvar = xlCar(*psvar);
                        if (!xlSymbolP(*psvar))
                            xlError("supplied-p variable must be a symbol",*psvar);
                    }
                    else
                        xlFmtError("expecting supplied-p variable");
                }
                *pdef = xlCar(*pdef);
            }
            else
                xlFmtError("expecting init expression");
        }
        if ((*parg = xlCar(form)) != xlNil) {
            if (xlConsP(*parg)) {
                *pkey = xlCar(*parg);
                if (!xlSymbolP(*pkey))
                    xlError("&key keyword must be a symbol",*pkey);
                if ((*parg = xlCdr(*parg)) != xlNil)
                    *parg = xlCar(*parg);
                else
                    xlFmtError("expecting keyword variable");
            }
        }
        else
            xlFmtError("expecting keyword variable");
    }
    else
        *parg = form;
    if (!xlSymbolP(*parg))
        xlError("&key variable must be a symbol",*parg);
    if (*pkey == xlNil) {
        *pkey = xlIntern(xlGetPName(*parg),xlKeywordPackage,&key);
        xlExport(*pkey,xlKeywordPackage);
    }
}

/* parse_aux_arguments - parse the &aux arguments */
static void parse_aux_arguments(xlValue *pfargs)
{
    xlValue fargs,arg,def;
    fargs = *pfargs;
    while (xlConsP(fargs)
    &&     (arg = xlCar(fargs)) != xlNil
    &&     !lambdakey(arg)) {
        
        /* parse the argument form */
        parse_aux_argument(arg,&arg,&def);

        /* compile the initialization expression */
        if (def)
            do_expr(def,C_NEXT);

        /* add the argument name */
        patch_argument_name(arg);
        
        /* store the initialization value */
        if (def)
            cd_evariable(xlopESET,0,get_argument_offset(arg));
        
        /* move the formal argument list pointer ahead */
        fargs = xlCdr(fargs);
    }

    /* update the reference parameters */
    *pfargs = fargs;
}

/* parse_aux_argument - parse a single &aux argument */
static void parse_aux_argument(xlValue form,xlValue *parg,xlValue *pdef)
{
    *pdef = xlNil;
    if (xlConsP(form)) {
        if ((*pdef = xlCdr(form)) != xlNil) {
            if (xlConsP(*pdef))
                *pdef = xlCar(*pdef);
            else
                xlFmtError("expecting init expression");
        }
        *parg = xlCar(form);
    }
    else
        *parg = form;
    if (!xlSymbolP(*parg))
        xlError("&aux variable must be a symbol",*parg);
}

/* add_argument_name - add an argument name to the argument list */
static void add_argument_name(xlValue name)
{
    int level = xlFIRSTENV;
    xlValue last;
    if ((last = xlGetEnvNames(xlCar(info))) != xlNil) {
        for (;;) {
            if (name && name == xlCar(last))
                xlError("duplicate argument name",name);
            if (xlCdr(last) == xlNil)
                break;
            last = xlCdr(last);
            ++level;
        }
        xlSetCdr(last,xlCons(name,xlNil));
    }
    else {
        if (level < 255)
            xlSetEnvNames(xlCar(info),xlCons(name,xlNil));
        else
            xlFmtError("too many environment variables");
    }
}

/* patch_argument_name - patch a nil argument name in the argument list */
static void patch_argument_name(xlValue name)
{
    int found=FALSE;
    xlValue next;
    if ((next = xlGetEnvNames(xlCar(info))) != xlNil) {
        while (next) {
            if (name == xlCar(next))
                xlError("duplicate argument name",name);
            if (!found && xlCar(next) == xlNil) {
                xlSetCar(next,name);
                found = TRUE;
            }
            next = xlCdr(next);
        }
    }
    if (!found)
        xlError("trouble patching argument",name);
}

/* get_argument_offset - get the offset to an argument in the environment frame */
static int get_argument_offset(xlValue arg)
{
    int off;
    findcvariable(xlopEREF,xlCar(info),arg,&off);
    return off;
}

/* do_delay - compile the (DELAY ... ) expression */
static void do_delay(xlValue form,int cont)
{
    int oldcbase;

    /* check argument list */
    if (xlAtomP(form))
        xlError("expecting delay expression",form);

    /* establish a new environment frame */
    oldcbase = add_level();

    /* setup the entry code */
    putcbyte(xlopARGSEQ);
    putcbyte(0);

    /* compile the expression */
    do_expr(xlCar(form),xlDebugModeP ? C_NEXT : C_RETURN);
    if (xlDebugModeP)
        putcbyte(xlopRETURN);

    /* build the code object */
    xlCPush(make_code_object(xlNil));
    
    /* restore the previous environment */
    remove_level(oldcbase);

    /* compile code to create a closure */
    do_literal(xlPop(),C_NEXT);
    putcbyte(xlopDELAY);
    do_continuation(cont);
}

/* do_let - compile the (LET ... ) expression */
static void do_let(xlValue form,int cont)
{
    /* handle named let */
    if (xlConsP(form) && xlSymbolP(xlCar(form)))
        do_named_let(form,cont);
    
    /* handle unnamed let */
    else
        do_unnamed_let(form,cont);
}

/* do_named_let - compile the (LET name (...) ... ) expression */
static void do_named_let(xlValue form,int cont)
{
    int oldcbase,rargc,opcode,lev,off;

    /* push the procedure */
    putcbyte(xlopNIL);
    putcbyte(xlopPUSH);
    
    /* establish a new environment frame */
    add_frame();
    add_argument_name(xlCar(form));

    /* push a new environment frame */
    putcbyte(xlopFRAME);
    putcbyte(1);
    putcbyte(xlFIRSTENV);
    putcbyte(findliteral(xlGetEnvNames(xlCar(info))));

    /* make sure there is a binding list */
    if (xlAtomP(xlCdr(form)) || !xlListP(xlCar(xlCdr(form))))
        xlError("expecting binding list",form);
    
    /* push the initialization expressions */
    push_init_expressions(xlCar(xlCdr(form)));
    
    /* establish a new environment frame */
    oldcbase = add_level();
    
    /* build a function */
    xlCPush(extract_let_variables(xlCar(xlCdr(form)),&rargc));
    parse_lambda_expr(xlTop(),xlCdr(xlCdr(form)),FALSE);
    
    /* build the code object */
    xlSetTop(make_code_object(xlCar(form)));
    
    /* restore the previous environment */
    remove_level(oldcbase);

    /* compile code to create a closure */
    do_literal(xlPop(),C_NEXT);
    putcbyte(xlopCLOSE);

    /* store the procedure */
    opcode = findvariable(xlopESET,xlCar(form),&lev,&off);
    cd_evariable(opcode,lev,off);
    
    /* apply the function */
    putcbyte(cont == C_RETURN ? xlopTCALL : xlopCALL);
    putcbyte(rargc);
    if (cont == C_NEXT) {
        putcbyte(xlopUNFRAME);
        do_continuation(cont);
    }

    /* restore the previous environment */
    remove_frame();
}

/* extract_let_variables - extract a list of variable names from a let binding list */
static xlValue extract_let_variables(xlValue bindings,int *pcnt)
{
    xlValue this,last;
    xlCheck(2);
    xlPush(bindings);
    xlPush(xlNil);
    for (*pcnt = 0; xlConsP(bindings); bindings = xlCdr(bindings), ++(*pcnt)) {
        xlValue def = xlCar(bindings), sym = NULL;
        if (xlSymbolP(def))
            sym = def;
        else if (xlConsP(def) && xlSymbolP(xlCar(def)))
            sym = xlCar(def);
        else
            xlError("invalid binding",def);
        this = xlCons(sym,xlNil);
        if (xlTop() == xlNil)
            xlSetTop(this);
        else
            xlSetCdr(last,this);
        last = this;
    }
    this = xlPop();
    xlDrop(1);
    return this;
}

/* do_unnamed_let - compile the (LET (...) ... ) expression */
static void do_unnamed_let(xlValue form,int cont)
{
    /* make sure there is a binding list */
    if (xlAtomP(form) || !xlListP(xlCar(form)))
        xlError("expecting binding list",form);

    /* push the initialization expressions */
    push_init_expressions(xlCar(form));

    /* establish a new environment frame */
    add_frame();

    /* compile the binding list */
    generate_let_setup_code(xlCar(form));

    /* compile the body of the let */
    do_begin(xlCdr(form),cont);
    if (cont == C_NEXT) {
        putcbyte(xlopUNFRAME);
        do_continuation(cont);
    }

    /* restore the previous environment */
    remove_frame();
}

/* do_letrec - compile the (LETREC ... ) expression */
static void do_letrec(xlValue form,int cont)
{
    /* make sure there is a binding list */
    if (xlAtomP(form) || !xlListP(xlCar(form)))
        xlError("expecting binding list",form);

    /* push the initialization expressions */
    push_dummy_values(xlCar(form));

    /* establish a new environment frame */
    add_frame();

    /* compile the binding list */
    generate_let_setup_code(xlCar(form));

    /* compile instructions to set the bound variables */
    set_bound_variables(xlCar(form));
    
    /* compile the body of the letrec */
    do_begin(xlCdr(form),cont);
    if (cont == C_NEXT) {
        putcbyte(xlopUNFRAME);
        do_continuation(cont);
    }

    /* restore the previous environment */
    remove_frame();
}

/* do_letstar - compile the (LET* ... ) expression */
static void do_letstar(xlValue form,int cont)
{
    /* make sure there is a binding list */
    if (xlAtomP(form) || !xlListP(xlCar(form)))
        xlError("expecting binding list",form);

    /* build the nested lambda expressions */
    if (xlConsP(xlCar(form)))
        letstar1(xlCar(form),xlCdr(form),cont);
    
    /* handle the case where there are no bindings */
    else
        do_begin(xlCdr(form),cont);
}

/* letstar1 - helper routine for let* */
static void letstar1(xlValue blist,xlValue body,int cont)
{
    /* push the next initialization expressions */
    xlCPush(xlCons(xlCar(blist),xlNil));
    push_init_expressions(xlTop());

    /* establish a new environment frame */
    add_frame();

    /* handle the case where there are more bindings */
    if (xlConsP(xlCdr(blist))) {
        generate_let_setup_code(xlTop());
        letstar1(xlCdr(blist),body,cont);
    }
    
    /* handle the last binding */
    else {
        generate_let_setup_code(xlTop());
        do_begin(body,cont);
    }

    /* remove the frame if more code follows */
    if (cont == C_NEXT) {
        putcbyte(xlopUNFRAME);
        do_continuation(cont);
    }
    xlDrop(1);
        
    /* restore the previous environment */
    remove_frame();
    do_continuation(cont);
}

/* push_dummy_values - push dummy values for a 'letrec' expression */
static int push_dummy_values(xlValue blist)
{
    int n=0;
    if (xlConsP(blist)) {
        putcbyte(xlopNIL);
        for (; xlConsP(blist); blist = xlCdr(blist), ++n)
            putcbyte(xlopPUSH);
    }
    return n;
}

/* push_init_expressions - push init expressions for a 'let' expression */
static int push_init_expressions(xlValue blist)
{
    int argc;
    if (xlConsP(blist)) {
        argc = push_init_expressions(xlCdr(blist));
        if (xlConsP(xlCar(blist)) && xlConsP(xlCdr(xlCar(blist))))
            do_expr(xlCar(xlCdr(xlCar(blist))),C_NEXT);
        else
            putcbyte(xlopNIL);
        putcbyte(xlopPUSH);
        return argc + 1;
    }
    return 0;
}

/* generate_let_setup_code - generate the setup code for an in-line let */
static void generate_let_setup_code(xlValue blist)
{
    int rargc,extra;
    parse_let_variables(blist,&rargc,&extra);    
    putcbyte(xlopFRAME);
    putcbyte(rargc);
    putcbyte(extra);
    putcbyte(findliteral(xlGetEnvNames(xlCar(info))));
}

/* parse_let_variables - parse the binding list */
static void parse_let_variables(xlValue blist,int *prargc,int *pextra)
{
    int rargc;
    xlValue arg;
    
    /* initialize the argument name list and slot number */
    rargc = 0;
    
    /* handle each required argument */
    while (xlConsP(blist) && (arg = xlCar(blist)) != xlNil) {

        /* make sure the argument is a symbol */
        if (xlSymbolP(arg))
            ;
        else if (xlConsP(arg) && xlSymbolP(xlCar(arg)))
            arg = xlCar(arg);
        else
            xlError("invalid binding",arg);

        /* add the argument name to the name list */
        add_argument_name(arg);
        
        /* move the formal argument list pointer ahead */
        blist = xlCdr(blist);
        ++rargc;
    }

    /* return the binding count and extra slot count */
    *prargc = rargc;
    *pextra = xlFIRSTENV;
}

/* set_bound_variables - set bound variables in a 'letrec' expression */
static void set_bound_variables(xlValue blist)
{
    int opcode,lev,off;
    for (; xlConsP(blist); blist = xlCdr(blist)) {
        if (xlConsP(xlCar(blist)) && xlConsP(xlCdr(xlCar(blist)))) {
            do_expr(xlCar(xlCdr(xlCar(blist))),C_NEXT);
            if ((opcode = findvariable(xlopESET,xlCar(xlCar(blist)),&lev,&off)) != 0)
                cd_evariable(opcode,lev,off);
            else
                xlError("compiler error -- can't find",xlCar(xlCar(blist)));
        }
    }
}

/* do_mvbind - compile the (MULTIPLE-VALUE-BIND (...) ... ) expression */
static void do_mvbind(xlValue form,int cont)
{
    xlValue blist,arg,p;
    int size,n;
    
    /* get the binding list */
    if (xlAtomP(form) || !xlListP(xlCar(form)))
        xlError("expecting binding list",form);
    blist = xlCar(form);

    /* compile the multiple value expression */
    if (!xlConsP(xlCdr(form)))
        xlError("expecting expression",xlCdr(form));
    do_expr(xlCar(xlCdr(form)),C_NEXT);

    /* establish a new environment frame */
    add_frame();

    /* handle each symbol to bind */
    for (p = blist; xlConsP(p) && (arg = xlCar(p)) != xlNil; p = xlCdr(p)) {
        if (!xlSymbolP(arg))
            xlError("invalid binding",arg);
        add_argument_name(arg);
    }

    /* establish a new environment frame */
    add_frame();
    size = xlFIRSTENV;

    /* handle each symbol to bind */
    for (p = blist; xlConsP(p) && (arg = xlCar(p)) != xlNil; p = xlCdr(p)) {
        if (!xlSymbolP(arg))
            xlError("invalid binding",arg);
        add_argument_name(arg);
        ++size;
    }

    /* compile the binding list */
    putcbyte(xlopMVFRAME);
    putcbyte(size);
    putcbyte(findliteral(xlGetEnvNames(xlGetNextFrame(xlCar(info)))));
    putcbyte(findliteral(xlGetEnvNames(xlCar(info))));

    /* move the arguments into the correct stack frame slots */
    for (p = blist, n = xlFIRSTENV; xlConsP(p); p = xlCdr(p), ++n) {
        putcbyte(xlopOPTARG);
        putcbyte(n);
        putcbyte(get_argument_offset(xlCar(p)));
    }

    /* compile the body of the multiple-value-bind */
    do_begin(xlCdr(xlCdr(form)),cont);
    if (cont == C_NEXT) {
        putcbyte(xlopUNFRAME);
        putcbyte(xlopUNFRAME);
        do_continuation(cont);
    }

    /* restore the previous environment */
    remove_frame();
    remove_frame();
}

/* make_code_object - build a code object */
static xlValue make_code_object(xlValue fun)
{
    unsigned char *src,*dst,*end;
    xlValue code,p;
    int i;

    /* create a code object */
    code = xlNewCode(xlFIRSTLIT + xlLength(xlCar(xlCdr(info)))); xlCPush(code);
    xlSetBCode(code,xlNewString(cptr - cbase));
    xlSetCName(code,fun);                               /* function name */
    xlSetVNames(code,xlGetEnvNames(xlCar(info)));       /* lambda list variables */

    /* copy the literals into the code object */
    for (i = xlFIRSTLIT, p = xlCar(xlCdr(info)); xlConsP(p); p = xlCdr(p), ++i)
        xlSetElement(code,i,xlCar(p));

    /* copy the byte codes */
    for (src = &cbuff[cbase], end = &cbuff[cptr], dst = xlGetCodeStr(code); src < end; )
        *dst++ = *src++;

    /* return the new code object */
    return xlPop();
}

/* do_cond - compile the (COND ... ) expression */
static void do_cond(xlValue form,int cont)
{
    int nxt,end;
    if (xlConsP(form)) {
        for (end = 0; xlConsP(form); form = xlCdr(form)) {
            if (xlAtomP(xlCar(form)))
                xlError("expecting a cond clause",form);
            do_expr(xlCar(xlCar(form)),C_NEXT);
            putcbyte(xlopBRF);
            nxt = putcword(0);
            if (xlCdr(xlCar(form)))
                do_begin(xlCdr(xlCar(form)),cont);
            else
                do_continuation(cont);
            if (cont == C_NEXT) {
                putcbyte(xlopBR);
                end = putcword(end);
            }
            fixup(nxt);
        }
        fixup(end);
    }
    else
        putcbyte(xlopNIL);
    do_continuation(cont);
}

/* do_and - compile the (AND ... ) expression */
static void do_and(xlValue form,int cont)
{
    int end;
    if (xlConsP(form)) {
        for (end = 0; xlConsP(form); form = xlCdr(form)) {
            if (xlCdr(form)) {
                do_expr(xlCar(form),C_NEXT);
                putcbyte(xlopBRF);
                end = putcword(end);
            }
            else
                do_expr(xlCar(form),cont);
        }
        fixup(end);
    }
    else
        putcbyte(xlopT);
    do_continuation(cont);
}

/* do_or - compile the (OR ... ) expression */
static void do_or(xlValue form,int cont)
{
    int end;
    if (xlConsP(form)) {
        for (end = 0; xlConsP(form); form = xlCdr(form)) {
            if (xlCdr(form)) {
                do_expr(xlCar(form),C_NEXT);
                putcbyte(xlopBRT);
                end = putcword(end);
            }
            else
                do_expr(xlCar(form),cont);
        }
        fixup(end);
    }
    else
        putcbyte(xlopNIL);
    do_continuation(cont);
}

/* do_if - compile the (IF ... ) expression */
static void do_if(xlValue form,int cont)
{
    int nxt,end;

    /* compile the test expression */
    if (xlAtomP(form))
        xlError("expecting test expression",form);
    do_expr(xlCar(form),C_NEXT);

    /* skip around the 'then' clause if the expression is false */
    putcbyte(xlopBRF);
    nxt = putcword(0);

    /* skip to the 'then' clause */
    form = xlCdr(form);
    if (xlAtomP(form))
        xlError("expecting then clause",form);

    /* compile the 'then' and 'else' clauses */
    if (xlConsP(xlCdr(form))) {
        if (cont == C_NEXT) {
            do_expr(xlCar(form),C_NEXT);
            putcbyte(xlopBR);
            end = putcword(0);
        }
        else {
            do_expr(xlCar(form),cont);
            end = -1;
        }
        fixup(nxt);
        do_expr(xlCar(xlCdr(form)),cont);
        nxt = end;
    }

    /* compile just a 'then' clause */
    else
        do_expr(xlCar(form),cont);

    /* handle the end of the statement */
    if (nxt >= 0) {
        fixup(nxt);
        do_continuation(cont);
    }
}

/* do_begin - compile the (BEGIN ... ) expression */
static void do_begin(xlValue form,int cont)
{
    if (xlConsP(form))
        for (; xlConsP(form); form = xlCdr(form))
            if (xlConsP(xlCdr(form)))
                do_expr(xlCar(form),C_NEXT);
            else
                do_expr(xlCar(form),cont);
    else {
        putcbyte(xlopNIL);
        do_continuation(cont);
    }
}

/* do_while - compile the (WHILE ... ) expression */
static void do_while(xlValue form,int cont)
{
    int loop,nxt;

    /* make sure there is a test expression */
    if (xlAtomP(form))
        xlError("expecting test expression",form);

    /* skip around the 'body' to the test expression */
    putcbyte(xlopBR);
    nxt = putcword(0);

    /* compile the loop body */
    loop = cptr - cbase;
    do_begin(xlCdr(form),C_NEXT);

    /* label for the first iteration */
    fixup(nxt);

    /* compile the test expression */
    nxt = cptr - cbase;
    do_expr(xlCar(form),C_NEXT);

    /* skip around the 'body' if the expression is false */
    putcbyte(xlopBRT);
    putcword(loop);

    /* compile the continuation */
    do_continuation(cont);
}

/* do_catch - compile the (CATCH ... ) expression */
static void do_catch(xlValue form,int cont)
{
    int nxt;
    
    /* make sure there is a tag expression */
    if (xlAtomP(form))
        xlError("expecting tag expression",form);

    /* compile the catch tag expression */
    do_expr(xlCar(form),C_NEXT);
    
    /* output a catch instruction to push a catch frame */
    putcbyte(xlopCATCH);
    nxt = putcword(0);

    /* compile the catch body */
    do_begin(xlCdr(form),C_NEXT);
    
    /* pop the catch frame */
    putcbyte(xlopUNCATCH);
    fixup(nxt);
    
    /* compile the continuation */
    do_continuation(cont);
}

/* do_unwindprotect - compile the (UNWIND-PROTECT ... ) expression */
static void do_unwindprotect(xlValue form,int cont)
{
    /* make sure there is a protected expression */
    if (xlAtomP(form))
        xlError("expecting protected expression",form);

    /* check for cleanup forms */
    if (xlCdr(form) == xlNil)
        do_expr(xlCar(form),cont);
    else {
        cd_fundefinition(xlNil,xlNil,xlCdr(form));
        putcbyte(xlopPROTECT);
        do_expr(xlCar(form),C_NEXT);
        putcbyte(xlopMVPUSH);
        putcbyte(xlopUNPROTECT);
        putcbyte(xlopCALL);
        putcbyte(0);
        putcbyte(xlopMVPOP);
        do_continuation(cont);
    }
}

/* do_call - compile a function call */
static void do_call(xlValue form,int cont)
{
    int n;
    
    /* compile each argument expression */
    n = push_args(xlCdr(form));

    /* compile the function itself */
    do_expr(xlCar(form),C_NEXT);

    /* apply the function */
    putcbyte(cont == C_RETURN ? xlopTCALL : xlopCALL);
    putcbyte(n);
}

/* do_mvcall - compile a multiple value function call */
static void do_mvcall(xlValue form,int cont)
{
    /* check the syntax */
    if (!xlConsP(xlCdr(form)))
        xlError("expecting multiple value expression",xlCdr(form));
    else if (!xlNullP(xlCdr(xlCdr(form))))
        xlError("only one multiple value expression allowed",xlCdr(form));

    /* compile each argument expression */
    do_expr(xlCar(xlCdr(form)),C_NEXT);
    putcbyte(xlopMVPUSH);

    /* compile the function itself */
    do_expr(xlCar(form),C_NEXT);

    /* apply the function */
    putcbyte(cont == C_RETURN ? xlopMVTCALL : xlopMVCALL);
}

/* push_args - compile the arguments for a function call */
static int push_args(xlValue form)
{
    int argc;
    if (xlConsP(form)) {
        argc = push_args(xlCdr(form));
        do_expr(xlCar(form),C_NEXT);
        putcbyte(xlopPUSH);
        return argc + 1;
    }
    return 0;
}

/* do_nary - compile nary operator expressions */
static void do_nary(int op,int n,xlValue form,int cont)
{
    if (n < 0 && (n = (-n)) != xlLength(xlCdr(form)))
        do_call(form,cont);
    else {
        push_nargs(form,n);
        putcbyte(op);
        do_continuation(cont);
    }
}

/* push_nargs - compile the arguments for an inline function call */
static void push_nargs(xlValue form,int n)
{
    int cnt=0;
    if (xlConsP(xlCdr(form))) {
        cnt = push_args(xlCdr(xlCdr(form))) + 1;
        do_expr(xlCar(xlCdr(form)),C_NEXT);
    }
    if (cnt > n)
        xlError("too many arguments",form);
    else if (cnt < n)
        xlError("too few arguments",form);
}

/* do_literal - compile a literal */
static void do_literal(xlValue lit,int cont)
{
    cd_literal(lit);
    do_continuation(cont);
}

/* do_identifier - compile an identifier */
static void do_identifier(xlValue sym,int cont)
{
    int opcode,lev,off;
    if (sym == xlTrue)
        putcbyte(xlopT);
    else if ((opcode = findvariable(xlopEREF,sym,&lev,&off)) != 0)
        cd_evariable(opcode,lev,off);
    else
        cd_variable(xlopGREF,sym);
    do_continuation(cont);
}

/* do_continuation - compile a continuation */
static void do_continuation(int cont)
{
    switch (cont) {
    case C_RETURN:
        putcbyte(xlopRETURN);
        break;
    case C_NEXT:
        break;
    }
}

/* add_frame - add a new environment frame */
static void add_frame(void)
{
    xlSetCar(info,xlNewFrame(xlENV,xlCar(info),xlFIRSTENV));
}

/* remove_frame - remove an environment frame */
static void remove_frame(void)
{
    xlSetCar(info,xlGetNextFrame(xlCar(info)));
}

/* add_level - add a nesting level */
static int add_level(void)
{
    int oldcbase;
    
    /* add a new environment frame */
    add_frame();
    
    /* add a new literal list */
    xlSetCdr(info,xlCons(xlNil,xlCdr(info)));
    
    /* setup the base of the code for this function */
    oldcbase = cbase;
    cbase = cptr;

    /* return the old code base */
    return oldcbase;
}

/* remove_level - remove a nesting level */
static void remove_level(int oldcbase)
{
    /* restore the previous environment */
    remove_frame();
    
    /* remove the previous literal list */
    xlSetCdr(info,xlCdr(xlCdr(info)));

    /* restore the base and code pointer */
    cptr = cbase;
    cbase = oldcbase;
}

/* findvariable - find an environment variable */
static int findvariable(int opcode,xlValue sym,int *plev,int *poff)
{
    xlValue frame = xlCar(info);
    int newop;
    for (*plev = 0; frame != xlNil; frame = xlGetNextFrame(frame), ++(*plev))
        if ((newop = findcvariable(opcode,frame,sym,poff)) != 0)
            return newop;
    return 0;
}

/* findcvariable - find an environment variable in the current frame */
static int findcvariable(int opcode,xlValue frame,xlValue sym,int *poff)
{
    xlValue names = xlGetEnvNames(frame);
    for (*poff = xlFIRSTENV; names != xlNil; ++(*poff), names = xlCdr(names))
        if (sym == xlCar(names))
            return opcode;
    if (xlMethodEnvironmentP(frame)) {
        xlValue class = xlGetEnvElement(xlGetNextFrame(frame),xlFIRSTENV);
        if (xlFindIVarOffset(class,sym,poff))
            return opcode + 1;
    }
    return 0;
}

/* findliteral - find a literal in the literal frame */
static int findliteral(xlValue lit)
{
    int o = xlFIRSTLIT;
    xlValue t,p;

    /* first check to see if the literal already exists */
    if ((t = xlCar(xlCdr(info))) != xlNil) {
        for (p = xlNil; xlConsP(t); p = t, t = xlCdr(t), ++o)
            if (xlEq(lit,xlCar(t)))
                return o;
    }

    /* make sure there aren't too many literals */
    if (o > 255)
        xlFmtError("too many literals");

    /* add the new literal */
    if (xlCar(xlCdr(info)) == xlNil)
        xlSetCar(xlCdr(info),xlCons(lit,xlNil));
    else
        xlSetCdr(p,xlCons(lit,xlNil));
    return o;
}

/* cd_variable - compile a variable reference */
static void cd_variable(int op,xlValue sym)
{
    putcbyte(op);
    putcbyte(findliteral(sym));
}

/* cd_evariable - compile an environment variable reference */
static void cd_evariable(int op,int lev,int off)
{
    putcbyte(op);
    putcbyte(lev);
    putcbyte(off);
}

/* cd_literal - compile a literal reference */
static void cd_literal(xlValue lit)
{
    if (lit == xlNil)
        putcbyte(xlopNIL);
    else if (lit == xlTrue)
        putcbyte(xlopT);
    else {
        putcbyte(xlopLIT);
        putcbyte(findliteral(lit));
    }
}

/* nextcaddr - get the next code address */
static int nextcaddr(void)
{
    return cptr - cbase;
}

/* putcbyte - put a code byte into data space */
static int putcbyte(int b)
{
    int adr;
    if (cptr >= CMAX)
        xlFmtAbort("insufficient code space");
    adr = (cptr - cbase);
    cbuff[cptr++] = b;
    return adr;
}

/* putcword - put a code word into data space */
static int putcword(int w)
{
    int adr;
    adr = putcbyte(w >> 8);
    putcbyte(w);
    return adr;
}

/* fixup - fixup a reference chain */
static void fixup(int chn)
{
    int val,hval,nxt;

    /* store the value into each location in the chain */
    val = cptr - cbase; hval = val >> 8;
    for (; chn; chn = nxt) {
        nxt = (cbuff[cbase+chn] << 8) | (cbuff[cbase+chn+1]);
        cbuff[cbase+chn] = hval;
        cbuff[cbase+chn+1] = val;
    }
}

/* xlLength - find the length of a list */
xlEXPORT xlFIXTYPE xlLength(xlValue list)
{
    xlFIXTYPE len;
    for (len = 0; xlConsP(list); list = xlCdr(list))
        ++len;
    return len;
}
