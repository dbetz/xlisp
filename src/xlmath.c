/* xlmath.c - xlisp built-in arithmetic functions */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"
#include <math.h>
#include <limits.h>

/* forward declarations */
static xlValue binary(int fcn);
static xlFIXTYPE remain(xlFIXTYPE num,xlFIXTYPE den);
static xlFIXTYPE modulo(xlFIXTYPE num,xlFIXTYPE den);
static xlValue unary(int fcn);
static xlValue predicate(int fcn);
static xlValue compare(int fcn);
static void checkizero(xlFIXTYPE iarg);
static void checkineg(xlFIXTYPE iarg);
static void checkfzero(xlFLOTYPE farg);
static void checkfneg(xlFLOTYPE farg);
static void badiop(void);
static void badfop(void);

/* xexactp - built-in function 'exact?' */
/**** THIS IS REALLY JUST A STUB FOR NOW ****/
xlValue xexactp(void)
{
    xlVal = xlGetArgNumber();
    xlLastArg();
    return xlFixnumP(xlVal) ? xlTrue : xlFalse;
}

/* xinexactp - built-in function 'inexact?' */
/**** THIS IS REALLY JUST A STUB FOR NOW ****/
xlValue xinexactp(void)
{
    xlVal = xlGetArgNumber();
    xlLastArg();
    return xlFixnumP(xlVal) ? xlFalse : xlTrue;
}

/* xatan - built-in function 'atan' */
xlValue xatan(void)
{
    xlValue arg,arg2;
    xlFLOTYPE val;
    
    /* get the first argument */
    arg = xlGetArgNumber();
    
    /* handle two argument (atan y x) */
    if (xlMoreArgsP()) {
        arg2 = xlGetArgNumber();
        xlLastArg();
        val = atan2(xlCnvToFlotype(arg),xlCnvToFlotype(arg2));
    }
    
    /* handle one argument (atan x) */
    else
        val = atan(xlCnvToFlotype(arg));

    /* return the resulting flonum */
    return xlMakeFlonum(val);
}

/* xfloor - built-in function 'floor' */
xlValue xfloor(void)
{
    xlValue arg;

    /* get the argument */
    arg = xlGetArg();
    xlLastArg();

    /* check its type */
    if (xlFixnumP(arg))
        return arg;
    else if (xlFlonumP(arg))
        return xlMakeFixnum((xlFIXTYPE)floor(xlGetFlonum(arg)));
    xlBadType(arg);
    return xlNil; /* never reached */
}

/* xceiling - built-in function 'ceiling' */
xlValue xceiling(void)
{
    xlValue arg;

    /* get the argument */
    arg = xlGetArg();
    xlLastArg();

    /* check its type */
    if (xlFixnumP(arg))
        return arg;
    else if (xlFlonumP(arg))
        return xlMakeFixnum((xlFIXTYPE)ceil(xlGetFlonum(arg)));
    xlBadType(arg);
    return xlNil; /* never reached */
}

/* xround - built-in function 'round' */
xlValue xround(void)
{
    xlFLOTYPE x,y,z;
    xlValue arg;

    /* get the argument */
    arg = xlGetArg();
    xlLastArg();

    /* check its type */
    if (xlFixnumP(arg))
        return arg;
    else if (xlFlonumP(arg)) {
        x = xlGetFlonum(arg);
        y = floor(x);
        z = x - y;
        if (z == 0.5) {
            if (((xlFIXTYPE)y & 1) == 1)
                y += 1.0;
            return xlMakeFixnum((xlFIXTYPE)y);
        }
        else if (z < 0.5)
            return xlMakeFixnum((xlFIXTYPE)y);
        else
            return xlMakeFixnum((xlFIXTYPE)(y + 1.0));
    }
    xlBadType(arg);
    return xlNil; /* never reached */
}

/* xtruncate - built-in function 'truncate' */
xlValue xtruncate(void)
{
    xlValue arg;

    /* get the argument */
    arg = xlGetArg();
    xlLastArg();

    /* check its type */
    if (xlFixnumP(arg))
        return arg;
    else if (xlFlonumP(arg))
        return xlMakeFixnum((xlFIXTYPE)(xlGetFlonum(arg)));
    xlBadType(arg);
    return xlNil; /* never reached */
}

#define FIXBITS ((int)(sizeof(xlFIXTYPE) * CHAR_BIT))

/* xash - arithmetic shift */
xlValue xash(void)
{
    xlFIXTYPE val,shift;
    xlVal = xlGetArgFixnum(); val = xlGetFixnum(xlVal);
    xlVal = xlGetArgFixnum(); shift = xlGetFixnum(xlVal); 
    xlLastArg();
    if (shift <= -FIXBITS)
        return xlMakeFixnum(val >> (FIXBITS - 1));
    else if (shift >= FIXBITS)
        return xlMakeFixnum(0);
    return xlMakeFixnum(shift > 0 ? val << shift : val >> -shift);
}

/* xlsh - logical shift */
xlValue xlsh(void)
{
    xlFIXTYPE val,shift;
    xlVal = xlGetArgFixnum(); val = xlGetFixnum(xlVal);
    xlVal = xlGetArgFixnum(); shift = xlGetFixnum(xlVal); 
    xlLastArg();
    if (shift <= -FIXBITS || shift >= FIXBITS)
        return xlMakeFixnum(0);
    return xlMakeFixnum(shift > 0 ? val << shift : (xlUFIXTYPE)val >> -shift);
}

/* binary functions */
xlValue xadd(void)                              /* + */
{
    if (!xlMoreArgsP())
        return xlMakeFixnum((xlFIXTYPE)0);
    return binary('+');
}
xlValue xmul(void)                              /* * */
{
    if (!xlMoreArgsP())
        return xlMakeFixnum((xlFIXTYPE)1);
    return binary('*');
}
xlValue xsub(void)    { return binary('-'); } /* - */
xlValue xdiv(void)    { return binary('/'); } /* / */
xlValue xquo(void)    { return binary('Q'); } /* quotient */
xlValue xrem(void)    { return binary('R'); } /* remainder */
xlValue xmod(void)    { return binary('r'); } /* modulo */
xlValue xmin(void)    { return binary('m'); } /* min */
xlValue xmax(void)    { return binary('M'); } /* max */
xlValue xexpt(void)   { return binary('E'); } /* expt */
xlValue xlogand(void) { return binary('&'); } /* logand */
xlValue xlogior(void) { return binary('|'); } /* logior */
xlValue xlogxor(void) { return binary('^'); } /* logxor */

/* binary - handle binary operations */
static xlValue binary(int fcn)
{
    xlFIXTYPE ival,iarg;
    xlFLOTYPE fval,farg;
    xlValue arg;
    int mode;

    /* get the first argument */
    arg = xlGetArg();

    /* set the type of the first argument */
    if (xlFixnumP(arg)) {
        ival = xlGetFixnum(arg);
        mode = 'I';
    }
    else if (xlFlonumP(arg)) {
        fval = xlGetFlonum(arg);
        mode = 'F';
    }
    else
        xlBadType(arg);

    /* treat a single argument as a special case */
    if (!xlMoreArgsP()) {
        switch (fcn) {
        case '-':
            switch (mode) {
            case 'I':
                ival = -ival;
                break;
            case 'F':
                fval = -fval;
                break;
            }
            break;
        case '/':
            switch (mode) {
            case 'I':
                checkizero(ival);
                if (ival != 1) {
                    fval = 1.0 / (xlFLOTYPE)ival;
                    mode = 'F';
                }
                break;
            case 'F':
                checkfzero(fval);
                fval = 1.0 / fval;
                break;
            }
        }
    }

    /* handle each remaining argument */
    while (xlMoreArgsP()) {

        /* get the next argument */
        arg = xlGetArg();

        /* check its type */
        if (xlFixnumP(arg)) {
            switch (mode) {
            case 'I':
                iarg = xlGetFixnum(arg);
                break;
            case 'F':
                farg = (xlFLOTYPE)xlGetFixnum(arg);
                break;
            }
        }
        else if (xlFlonumP(arg)) {
            switch (mode) {
            case 'I':
                fval = (xlFLOTYPE)ival;
                farg = xlGetFlonum(arg);
                mode = 'F';
                break;
            case 'F':
                farg = xlGetFlonum(arg);
                break;
            }
        }
        else
            xlBadType(arg);

        /* accumulate the result value */
        switch (mode) {
        case 'I':
            switch (fcn) {
            case '+':   ival += iarg; break;
            case '-':   ival -= iarg; break;
            case '*':   ival *= iarg; break;
            case '/':   checkizero(iarg);
                        if ((ival % iarg) == 0)     
                            ival /= iarg;
                        else {
                            fval = (xlFLOTYPE)ival;
                            farg = (xlFLOTYPE)iarg;
                            fval /= farg;
                            mode = 'F';
                        }
                        break;
            case 'Q':   checkizero(iarg); ival /= iarg; break;
            case 'R':   ival = remain(ival,iarg); break;
            case 'r':   ival = modulo(ival,iarg); break;
            case 'M':   if (iarg > ival) ival = iarg; break;
            case 'm':   if (iarg < ival) ival = iarg; break;
            case 'E':   return xlMakeFlonum((xlFLOTYPE)pow((xlFLOTYPE)ival,(xlFLOTYPE)iarg));
            case '&':   ival &= iarg; break;
            case '|':   ival |= iarg; break;
            case '^':   ival ^= iarg; break;
            default:    badiop();
            }
            break;
        case 'F':
            switch (fcn) {
            case '+':   fval += farg; break;
            case '-':   fval -= farg; break;
            case '*':   fval *= farg; break;
            case '/':   checkfzero(farg); fval /= farg; break;
            case 'M':   if (farg > fval) fval = farg; break;
            case 'm':   if (farg < fval) fval = farg; break;
            case 'E':   fval = pow(fval,farg); break;
            default:    badfop();
            }
            break;
        }
    }

    /* return the result */
    return mode == 'I' ? xlMakeFixnum(ival) : xlMakeFlonum(fval);
}

/* remain - divide two numbers returning the remainder */
static xlFIXTYPE remain(xlFIXTYPE num,xlFIXTYPE den)
{
    xlFIXTYPE result;
    checkizero(den);
    result = num % den;
    return result;
}

/* modulo - divide two numbers returning the modulo */
static xlFIXTYPE modulo(xlFIXTYPE num,xlFIXTYPE den)
{
    xlFIXTYPE result;
    checkizero(den);
    result = num % den;
    return result != 0  && (num < 0) != (den < 0) ? result + den : result;
}

/* unary functions */
xlValue xlognot(void)   { return unary('~'); } /* lognot */
xlValue xabs(void)      { return unary('A'); } /* abs */
xlValue xadd1(void)     { return unary('+'); } /* 1+ */
xlValue xsub1(void)     { return unary('-'); } /* -1+ */
xlValue xsin(void)      { return unary('S'); } /* sin */
xlValue xcos(void)      { return unary('C'); } /* cos */
xlValue xtan(void)      { return unary('T'); } /* tan */
xlValue xasin(void)     { return unary('s'); } /* asin */
xlValue xacos(void)     { return unary('c'); } /* acos */
xlValue xxexp(void)     { return unary('E'); } /* exp */
xlValue xsqrt(void)     { return unary('R'); } /* sqrt */
xlValue xxlog(void)     { return unary('L'); } /* log */
xlValue xrandom(void)   { return unary('?'); } /* random */

/* xsetrandomseed - set random number generator seed */
xlValue xsetrandomseed(void)
{
    xlVal = xlGetArgFixnum();
    xlLastArg();
    xlosSetRand(xlGetFixnum(xlVal));
    return xlVal;
}

/* unary - handle unary operations */
static xlValue unary(int fcn)
{
    xlFLOTYPE fval;
    xlFIXTYPE ival;
    xlValue arg;

    /* get the argument */
    arg = xlGetArg();
    xlLastArg();

    /* check its type */
    if (xlFixnumP(arg)) {
        ival = xlGetFixnum(arg);
        switch (fcn) {
        case '~':       ival = ~ival; break;
        case 'A':       ival = (ival < 0 ? -ival : ival); break;
        case '+':       ival++; break;
        case '-':       ival--; break;
        case 'S':       return xlMakeFlonum((xlFLOTYPE)sin((xlFLOTYPE)ival));
        case 'C':       return xlMakeFlonum((xlFLOTYPE)cos((xlFLOTYPE)ival));
        case 'T':       return xlMakeFlonum((xlFLOTYPE)tan((xlFLOTYPE)ival));
        case 's':       return xlMakeFlonum((xlFLOTYPE)asin((xlFLOTYPE)ival));
        case 'c':       return xlMakeFlonum((xlFLOTYPE)acos((xlFLOTYPE)ival));
        case 't':       return xlMakeFlonum((xlFLOTYPE)atan((xlFLOTYPE)ival));
        case 'E':       return xlMakeFlonum((xlFLOTYPE)exp((xlFLOTYPE)ival));
        case 'L':       return xlMakeFlonum((xlFLOTYPE)log((xlFLOTYPE)ival));
        case 'R':       checkineg(ival);
                        return xlMakeFlonum((xlFLOTYPE)sqrt((xlFLOTYPE)ival));
        case '?':       ival = (xlFIXTYPE)xlosRand((int)ival); break;
        default:        badiop();
        }
        return xlMakeFixnum(ival);
    }
    else if (xlFlonumP(arg)) {
        fval = xlGetFlonum(arg);
        switch (fcn) {
        case 'A':       fval = (fval < 0.0 ? -fval : fval); break;
        case '+':       fval += 1.0; break;
        case '-':       fval -= 1.0; break;
        case 'S':       fval = sin(fval); break;
        case 'C':       fval = cos(fval); break;
        case 'T':       fval = tan(fval); break;
        case 's':       fval = asin(fval); break;
        case 'c':       fval = acos(fval); break;
        case 't':       fval = atan(fval); break;
        case 'E':       fval = exp(fval); break;
        case 'L':       fval = log(fval); break;
        case 'R':       checkfneg(fval);
                        fval = sqrt(fval); break;
        default:        badfop();
        }
        return xlMakeFlonum(fval);
    }
    xlBadType(arg);
    return xlNil; /* never reached */
}

/* unary predicates */
xlValue xnegativep(void) { return predicate('-'); } /* negative? */
xlValue xzerop(void)     { return predicate('Z'); } /* zero? */
xlValue xpositivep(void) { return predicate('+'); } /* positive? */
xlValue xevenp(void)     { return predicate('E'); } /* even? */
xlValue xoddp(void)      { return predicate('O'); } /* odd? */

/* predicate - handle a predicate function */
static xlValue predicate(int fcn)
{
    xlFLOTYPE fval;
    xlFIXTYPE ival = 0;
    xlValue arg;

    /* get the argument */
    arg = xlGetArg();
    xlLastArg();

    /* check the argument type */
    if (xlFixnumP(arg)) {
        ival = xlGetFixnum(arg);
        switch (fcn) {
        case '-':       ival = (ival < 0); break;
        case 'Z':       ival = (ival == 0); break;
        case '+':       ival = (ival > 0); break;
        case 'E':       ival = ((ival & 1) == 0); break;
        case 'O':       ival = ((ival & 1) != 0); break;
        default:        badiop();
        }
    }
    else if (xlFlonumP(arg)) {
        fval = xlGetFlonum(arg);
        switch (fcn) {
        case '-':       ival = (fval < 0); break;
        case 'Z':       ival = (fval == 0); break;
        case '+':       ival = (fval > 0); break;
        default:        badfop();
        }
    }
    else
        xlBadType(arg);

    /* return the result value */
    return ival ? xlTrue : xlFalse;
}

/* comparison functions */
xlValue xlss(void) { return compare('<'); } /* < */
xlValue xleq(void) { return compare('L'); } /* <= */
xlValue xeql(void) { return compare('='); } /* = */
xlValue xneq(void) { return compare('#'); } /* = */
xlValue xgeq(void) { return compare('G'); } /* >= */
xlValue xgtr(void) { return compare('>'); } /* > */

/* compare - common compare function */
static xlValue compare(int fcn)
{
    xlFIXTYPE icmp,ival,iarg;
    xlFLOTYPE fcmp,fval,farg;
    xlValue arg;
    int mode;

    /* get the first argument */
    arg = xlGetArg();

    /* set the type of the first argument */
    if (xlFixnumP(arg)) {
        ival = xlGetFixnum(arg);
        mode = 'I';
    }
    else if (xlFlonumP(arg)) {
        fval = xlGetFlonum(arg);
        mode = 'F';
    }
    else
        xlBadType(arg);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && xlMoreArgsP(); ival = iarg, fval = farg) {

        /* get the next argument */
        arg = xlGetArg();

        /* check its type */
        if (xlFixnumP(arg)) {
            switch (mode) {
            case 'I':
                iarg = xlGetFixnum(arg);
                break;
            case 'F':
                farg = (xlFLOTYPE)xlGetFixnum(arg);
                break;
            }
        }
        else if (xlFlonumP(arg)) {
            switch (mode) {
            case 'I':
                fval = (xlFLOTYPE)ival;
                farg = xlGetFlonum(arg);
                mode = 'F';
                break;
            case 'F':
                farg = xlGetFlonum(arg);
                break;
            }
        }
        else
            xlBadType(arg);

        /* compute result of the compare */
        switch (mode) {
        case 'I':
            icmp = ival - iarg;
            switch (fcn) {
            case '<':   icmp = (icmp < 0); break;
            case 'L':   icmp = (icmp <= 0); break;
            case '=':   icmp = (icmp == 0); break;
            case '#':   icmp = (icmp != 0); break;
            case 'G':   icmp = (icmp >= 0); break;
            case '>':   icmp = (icmp > 0); break;
            }
            break;
        case 'F':
            fcmp = fval - farg;
            switch (fcn) {
            case '<':   icmp = (fcmp < 0.0); break;
            case 'L':   icmp = (fcmp <= 0.0); break;
            case '=':   icmp = (fcmp == 0.0); break;
            case '#':   icmp = (fcmp != 0.0); break;
            case 'G':   icmp = (fcmp >= 0.0); break;
            case '>':   icmp = (fcmp > 0.0); break;
            }
            break;
        }
    }
    xlPopArgs();

    /* return the result */
    return icmp ? xlTrue : xlFalse;
}

/* xlCnvToFlotype - convert a lisp value to a floating point number */
xlEXPORT xlFLOTYPE xlCnvToFlotype(xlValue val)
{
    /* must be a number for this to work */
    return xlNodeType(val) == xlFIXNUM ? (xlFLOTYPE)xlGetFixnum(val)
                                : xlGetFlonum(val);
}

/* checkizero - check for integer division by zero */
static void checkizero(xlFIXTYPE iarg)
{
    if (iarg == 0)
        xlFmtError("division by zero");
}

/* checkineg - check for square root of a negative number */
static void checkineg(xlFIXTYPE iarg)
{
    if (iarg < 0)
        xlFmtError("square root of a negative number");
}

/* checkfzero - check for floating point division by zero */
static void checkfzero(xlFLOTYPE farg)
{
    if (farg == 0.0)
        xlFmtError("division by zero");
}

/* checkfneg - check for square root of a negative number */
static void checkfneg(xlFLOTYPE farg)
{
    if (farg < 0.0)
        xlFmtError("square root of a negative number");
}

/* badiop - bad integer operation */
static void badiop(void)
{
    xlFmtError("bad integer operation");
}

/* badfop - bad floating point operation */
static void badfop(void)
{
    xlFmtError("bad floating point operation");
}
