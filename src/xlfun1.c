/* xlfun1.c - xlisp built-in functions - part 1 */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* gensym variables */
static char gsprefix[xlSTRMAX+1] = { 'G',0 };   /* gensym prefix string */
static xlFIXTYPE gsnumber = 1;                  /* gensym number */

/* external variables */
extern xlValue xlEnv,xlVal,xlDefaultObject;
extern xlValue xlUnboundObject,s_package,s_eql,k_uses,k_test,k_testnot,k_key;
extern xlValue xlPackages,xlLispPackage,xlLispPackage,xlLispPackage;

/* forward declarations */
static xlValue cxr(const char *adstr);
static xlValue member(int (*fcn)(xlValue,xlValue));
static xlValue assoc(int (*fcn)(xlValue,xlValue));
static xlValue copytree(xlValue tree);
static xlValue getpackagenamearg(void);
static xlValue nametostring(xlValue arg);
static xlValue getpackagearg(void);
static xlValue nametopackage(xlValue arg);
static xlValue vref(xlValue vector);
static xlValue vset(xlValue vector);
static xlValue makearray1(int argc,xlValue *argv);
static int stringequal(xlValue v1,xlValue v2);
static int vectorequal(xlValue v1,xlValue v2);

/* xcons - built-in function 'cons' */
xlValue xcons(void)
{
    xlValue carval,cdrval;
    carval = xlGetArg();
    cdrval = xlGetArg();
    xlLastArg();
    return xlCons(carval,cdrval);
}

/* xacons - built-in function 'acons' */
xlValue xacons(void)
{
    xlValue key,datum;
    key = xlGetArg();
    datum = xlGetArg();
    xlVal = xlGetArgList();
    xlLastArg();
    return xlCons(xlCons(key,datum),xlVal);
}

/* xcar - built-in function 'car' */
xlValue xcar(void)
{
    xlValue list;
    list = xlGetArgList();
    xlLastArg();
    return list ? xlCar(list) : xlNil;
}

/* xicar - built-in function '%car' */
xlValue xicar(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlCar(arg);
}

/* xcdr - built-in function 'cdr' */
xlValue xcdr(void)
{
    xlValue arg;
    arg = xlGetArgList();
    xlLastArg();
    return arg ? xlCdr(arg) : xlNil;
}

/* xicdr - built-in function '%cdr' */
xlValue xicdr(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlCdr(arg);
}

/* cxxr functions */
xlValue xcaar(void) { return cxr("aa"); }
xlValue xcadr(void) { return cxr("da"); }
xlValue xcdar(void) { return cxr("ad"); }
xlValue xcddr(void) { return cxr("dd"); }

/* cxxxr functions */
xlValue xcaaar(void) { return cxr("aaa"); }
xlValue xcaadr(void) { return cxr("daa"); }
xlValue xcadar(void) { return cxr("ada"); }
xlValue xcaddr(void) { return cxr("dda"); }
xlValue xcdaar(void) { return cxr("aad"); }
xlValue xcdadr(void) { return cxr("dad"); }
xlValue xcddar(void) { return cxr("add"); }
xlValue xcdddr(void) { return cxr("ddd"); }

/* cxxxxr functions */
xlValue xcaaaar(void) { return cxr("aaaa"); }
xlValue xcaaadr(void) { return cxr("daaa"); }
xlValue xcaadar(void) { return cxr("adaa"); }
xlValue xcaaddr(void) { return cxr("ddaa"); }
xlValue xcadaar(void) { return cxr("aada"); }
xlValue xcadadr(void) { return cxr("dada"); }
xlValue xcaddar(void) { return cxr("adda"); }
xlValue xcadddr(void) { return cxr("ddda"); }
xlValue xcdaaar(void) { return cxr("aaad"); }
xlValue xcdaadr(void) { return cxr("daad"); }
xlValue xcdadar(void) { return cxr("adad"); }
xlValue xcdaddr(void) { return cxr("ddad"); }
xlValue xcddaar(void) { return cxr("aadd"); }
xlValue xcddadr(void) { return cxr("dadd"); }
xlValue xcdddar(void) { return cxr("addd"); }
xlValue xcddddr(void) { return cxr("dddd"); }

/* cxr - common car/cdr routine */
static xlValue cxr(const char *adstr)
{
    xlValue list;

    /* get the list */
    list = xlGetArgList();
    xlLastArg();

    /* perform the car/cdr operations */
    while (*adstr && xlConsP(list))
        list = (*adstr++ == 'a' ? xlCar(list) : xlCdr(list));

    /* make sure the operation succeeded */
    if (*adstr && list)
        xlBadType(list);

    /* return the result */
    return list;
}

/* xsetcar - built-in function 'set-car!' */
xlValue xsetcar(void)
{
    xlValue arg,newcar;

    /* get the cons and the new car */
    arg = xlGetArgCons();
    newcar = xlGetArg();
    xlLastArg();

    /* replace the car */
    xlSetCar(arg,newcar);
    return arg;
}

/* xisetcar - built-in function '%set-car!' */
xlValue xisetcar(void)
{
    xlValue arg,newcar;

    /* get the cons and the new car */
    arg = xlGetArg();
    newcar = xlGetArg();
    xlLastArg();

    /* replace the car */
    xlSetCar(arg,newcar);
    return arg;
}

/* xsetcdr - built-in function 'set-cdr!' */
xlValue xsetcdr(void)
{
    xlValue arg,newcdr;

    /* get the cons and the new cdr */
    arg = xlGetArgCons();
    newcdr = xlGetArg();
    xlLastArg();

    /* replace the cdr */
    xlSetCdr(arg,newcdr);
    return arg;
}

/* xisetcdr - built-in function '%set-cdr!' */
xlValue xisetcdr(void)
{
    xlValue arg,newcdr;

    /* get the cons and the new cdr */
    arg = xlGetArg();
    newcdr = xlGetArg();
    xlLastArg();

    /* replace the cdr */
    xlSetCdr(arg,newcdr);
    return arg;
}

/* xnappend - destructively append lists */
xlValue xnappend(void)
{
    xlValue next,last;

    /* initialize */
    xlVal = xlNil;
    
    /* concatenate each argument */
    if (xlMoreArgsP()) {
        while (xlArgC > 1) {

            /* ignore everything except lists */
            if ((next = xlNextArg()) != xlNil && xlConsP(next)) {

                /* concatenate this list to the result list */
                if (xlVal) xlSetCdr(last,next);
                else xlVal = next;

                /* find the end of the list */
                while (xlConsP(xlCdr(next)))
                    next = xlCdr(next);
                last = next;
            }
        }

        /* handle the last argument */
        if (xlVal) xlSetCdr(last,xlNextArg());
        else xlVal = xlNextArg();
    }

    /* return the list */
    return xlVal;
}

/* xlist - built-in function 'list' */
xlValue xlist(void)
{
    xlValue last,next;

    /* initialize the list */
    xlVal = xlNil;

    /* add each argument to the list */
    if (xlMoreArgsP()) {
        xlVal = last = xlCons(xlNextArg(),xlNil);
        while (xlMoreArgsP()) {
            next = xlCons(xlNextArg(),xlNil);
            xlSetCdr(last,next);
            last = next;
        }
    }

    /* return the list */
    return xlVal;
}

/* xliststar - built-in function 'list*' */
xlValue xliststar(void)
{
    xlValue last,next;

    /* initialize the list */
    xlVal = xlNil;

    /* add each argument to the list */
    if (xlMoreArgsP()) {
        for (;;) {
            next = xlNextArg();
            if (xlMoreArgsP()) {
                next = xlCons(next,xlNil);
                if (xlVal) xlSetCdr(last,next);
                else xlVal = next;
                last = next;
            }
            else {
                if (xlVal) xlSetCdr(last,next);
                else xlVal = next;
                break;
            }
        }
    }

    /* return the list */
    return xlVal;
}

/* xpairlis - built-in function 'pairlis' */
xlValue xpairlis(void)
{
    xlValue keys,data;
    keys = xlGetArgList();
    data = xlGetArgList();
    xlVal = xlMoreArgsP() ? xlGetArgList() : xlNil;
    xlCheck(2);
    xlPush(keys);
    xlPush(data);
    while (xlConsP(keys) && xlConsP(data)) {
        xlVal = xlCons(xlCons(xlCar(keys),xlCar(data)),xlVal);
        keys = xlCdr(keys);
        data = xlCdr(data);
    }
    xlDrop(2);
    return xlVal;
}

/* xcopylist - built-in function 'copy-list' */
xlValue xcopylist(void)
{
    xlValue last;
    xlVal = xlGetArgList();
    xlLastArg();
    xlCPush(xlNil);
    if (xlVal) {
        last = xlCons(xlCar(xlVal),xlNil); xlSetTop(last);
        for (xlVal = xlCdr(xlVal); xlConsP(xlVal); xlVal = xlCdr(xlVal)) {
            xlSetCdr(last,xlCons(xlCar(xlVal),xlNil));
            last = xlCdr(last);
        }
    }
    return xlPop();
}

/* copytree - copytree helper function */
static xlValue copytree(xlValue tree)
{
    if (xlConsP(tree)) {
        xlCPush(copytree(xlCar(tree)));
        tree = copytree(xlCdr(tree));
        tree = xlCons(xlPop(),tree);
    }
    return tree;
}

/* xcopytree - built-in function 'copy-tree' */
xlValue xcopytree(void)
{
    xlVal = xlGetArgList();
    xlLastArg();
    return copytree(xlVal);
}

/* xcopyalist - built-in function 'copy-alist' */
xlValue xcopyalist(void)
{
    xlValue last,entry;
    xlVal = xlGetArgList();
    xlLastArg();
    xlCPush(xlNil);
    if (xlVal) {
        entry = xlCar(xlVal);
        if (xlConsP(entry)) entry = xlCons(xlCar(entry),xlCdr(entry));
        last = xlCons(entry,xlNil); xlSetTop(last);
        for (xlVal = xlCdr(xlVal); xlConsP(xlVal); xlVal = xlCdr(xlVal)) {
            entry = xlCar(xlVal);
            if (xlConsP(entry)) entry = xlCons(xlCar(entry),xlCdr(entry));
            xlSetCdr(last,xlCons(entry,xlNil));
            last = xlCdr(last);
        }
    }
    return xlPop();
}

/* xappend - built-in function 'append' */
xlValue xappend(void)
{
    xlValue next,this,last;

    /* append each argument */
    for (xlVal = last = xlNil; xlArgC > 1; )

        /* append each element of this list to the result list */
        for (next = xlGetArgList(); xlConsP(next); next = xlPop()) {
            xlCPush(xlCdr(next));
            this = xlCons(xlCar(next),xlNil);
            if (last) xlSetCdr(last,this);
            else xlVal = this;
            last = this;
        }

    /* tack on the last argument */
    if (xlMoreArgsP()) {
        if (last) xlSetCdr(last,xlGetArg());
        else xlVal = xlGetArg();
    }

    /* return the list */
    return xlVal;
}

/* xreverse - built-in function 'reverse' */
xlValue xreverse(void)
{
    xlValue val;
    
    /* get the list to reverse */
    xlVal = xlGetArgList();
    xlLastArg();

    /* append each element of this list to the result list */
    for (val = xlNil; xlConsP(xlVal); xlVal = xlCdr(xlVal))
        val = xlCons(xlCar(xlVal),val);

    /* return the list */
    return val;
}

/* xlength - built-in function 'length' */
xlValue xlength(void)
{
    xlFIXTYPE n;
    xlValue arg;

    /* get the argument */
    arg = xlGetArgList();
    xlLastArg();

    /* find the length */
    for (n = 0; xlConsP(arg); ++n)
        arg = xlCdr(arg);

    /* return the length */
    return xlMakeFixnum(n);
}

/* xxmember - built-in function 'member' */
xlValue xxmember(void)
{
    return member(xlEqual);
}

/* xxmemv - built-in function 'memv' */
xlValue xxmemv(void)
{
    return member(xlEqv);
}

/* xxmemq - built-in function 'memq' */
xlValue xxmemq(void)
{
    return member(xlEq);
}

/* member - common routine for member/memv/memq */
static xlValue member(int (*fcn)(xlValue,xlValue))
{
    xlValue x,list,val;

    /* get the expression to look for and the list */
    x = xlGetArg();
    list = xlGetArgList();
    xlLastArg();

    /* look for the expression */
    for (val = xlNil; xlConsP(list); list = xlCdr(list))
        if ((*fcn)(x,xlCar(list))) {
            val = list;
            break;
        }

    /* return the result */
    return val;
}

/* xxassoc - built-in function 'assoc' */
xlValue xxassoc(void)
{
    return assoc(xlEqual);
}

/* xxassv - built-in function 'assv' */
xlValue xxassv(void)
{
    return assoc(xlEqv);
}

/* xxassq - built-in function 'assq' */
xlValue xxassq(void)
{
    return assoc(xlEq);
}

/* assoc - common routine for assoc/assv/assq */
static xlValue assoc(int (*fcn)(xlValue,xlValue))
{
    xlValue x,alist,pair,val;

    /* get the expression to look for and the association list */
    x = xlGetArg();
    alist = xlGetArgList();
    xlLastArg();

    /* look for the expression */
    for (val = xlNil; xlConsP(alist); alist = xlCdr(alist))
        if ((pair = xlCar(alist)) != xlNil && xlConsP(pair))
            if ((*fcn)(x,xlCar(pair))) {
                val = pair;
                break;
            }

    /* return the result */
    return val;
}

/* xlast - return the last cons of a list */
xlValue xlast(void)
{
    xlValue list;

    /* get the list */
    list = xlGetArgList();
    xlLastArg();

    /* find the last cons */
    if (xlConsP(list))
        while (xlConsP(xlCdr(list)))
            list = xlCdr(list);

    /* return the last element */
    return list;
}

/* xlistref - built-in function 'list-ref' */
xlValue xlistref(void)
{
    xlValue list,num;
    xlFIXTYPE n;

    /* get the list and n */
    list = xlGetArgList();
    num = xlGetArgFixnum();
    xlLastArg();

    /* make sure the number isn't negative */
    if ((n = xlGetFixnum(num)) < 0)
        xlFmtError("bad argument");

    /* find the nth element */
    while (xlConsP(list) && --n >= 0)
        list = xlCdr(list);

    /* return the list beginning at the nth element */
    return xlConsP(list) ? xlCar(list) : xlNil;
}

/* xlisttail - return the nth cdr of a list */
xlValue xlisttail(void)
{
    xlValue list,num;
    xlFIXTYPE n;

    /* get n and the list */
    list = xlGetArgList();
    num = xlGetArgFixnum();
    xlLastArg();

    /* make sure the number isn't negative */
    if ((n = xlGetFixnum(num)) < 0)
        xlFmtError("bad argument");

    /* find the nth element */
    while (xlConsP(list) && --n >= 0)
        list = xlCdr(list);

    /* return the list beginning at the nth element */
    return list;
}

/* xmkpackage - make a package */
xlValue xmkpackage(void)
{
    xlValue uses;
    xlVal = getpackagenamearg();
    xlVal = xlNewPackage(xlGetString(xlVal));
    if (xlGetKeyArg(k_uses,xlNil,&uses)) {
        xlCPush(uses);
        for (; xlConsP(uses); uses = xlCdr(uses))
            xlUsePackage(nametopackage(xlCar(uses)),xlVal);
        xlDrop(1);
    }
    else {
        xlUsePackage(xlLispPackage,xlVal);
        xlUsePackage(xlLispPackage,xlVal);
        xlUsePackage(xlLispPackage,xlVal);
    }
    xlPopArgs();
    return xlVal;
}

/* xinpackage - switch to a package */
xlValue xinpackage(void)
{
    xlVal = getpackagearg();
    xlLastArg();
    xlSetValue(s_package,xlVal);
    return xlVal;
}

/* xfindpackage - find a package by name */
xlValue xfindpackage(void)
{
    xlVal = getpackagenamearg();
    xlLastArg();
    return xlFindPackage(xlGetString(xlVal));
}

/* xlistallpackages - return a list of all packages */
xlValue xlistallpackages(void)
{
    xlValue pack;
    xlLastArg();
    for (xlVal = xlNil, pack = xlPackages; xlPackageP(pack); pack = xlGetNextPackage(pack))
        xlVal = xlCons(pack,xlVal);
    return xlVal;
}

/* xpackagename - get the name of a package */
xlValue xpackagename(void)
{
    xlVal = getpackagearg();
    xlLastArg();
    return xlCar(xlGetNames(xlVal));
}

/* xpkgnicknames - get the nicknames of a package */
xlValue xpkgnicknames(void)
{
    xlVal = getpackagearg();
    xlLastArg();
    return xlCdr(xlGetNames(xlVal));
}

/* xusepackage - use a package */
xlValue xusepackage(void)
{
    xlValue dst;
    xlVal = xlGetArg();
    dst = (xlMoreArgsP() ? getpackagearg() : xlGetValue(s_package));
    xlLastArg();
    if (xlListP(xlVal))
        for (; xlConsP(xlVal); xlVal = xlCdr(xlVal))
            xlUsePackage(nametopackage(xlCar(xlVal)),dst);
    else
        xlUsePackage(nametopackage(xlVal),dst);
    return xlTrue;
}

/* xunusepackage - unuse a package */
xlValue xunusepackage(void)
{
    xlValue dst;
    xlVal = xlGetArg();
    dst = (xlMoreArgsP() ? getpackagearg() : xlGetValue(s_package));
    xlLastArg();
    if (xlListP(xlVal))
        for (; xlConsP(xlVal); xlVal = xlCdr(xlVal))
            xlUnusePackage(nametopackage(xlCar(xlVal)),dst);
    else
        xlUnusePackage(nametopackage(xlVal),dst);
    return xlTrue;
}

/* xpkguselist - get a package use list */
xlValue xpkguselist(void)
{
    xlVal = getpackagearg();
    xlLastArg();
    return xlGetUses(xlVal);
}

/* xpkgusedbylist - get a package used-by list */
xlValue xpkgusedbylist(void)
{
    xlVal = getpackagearg();
    xlLastArg();
    return xlGetUsedBy(xlVal);
}

/* xexport - export symbols from a package */
xlValue xexport(void)
{
    xlValue dst;
    xlVal = xlGetArg();
    dst = (xlMoreArgsP() ? getpackagearg() : xlGetValue(s_package));
    xlLastArg();
    if (xlListP(xlVal))
        for (; xlConsP(xlVal); xlVal = xlCdr(xlVal))
            if (xlSymbolP(xlCar(xlVal)))
                xlExport(xlCar(xlVal),dst);
            else
                xlError("expecting a symbol",xlCar(xlVal));
    else if (xlSymbolP(xlVal))
        xlExport(xlVal,dst);
    else
        xlError("expecting a symbol or list of symbols",xlVal);
    return xlTrue;
}

/* xunexport - unexport symbols from a package */
xlValue xunexport(void)
{
    xlValue dst;
    xlVal = xlGetArg();
    dst = (xlMoreArgsP() ? getpackagearg() : xlGetValue(s_package));
    xlLastArg();
    if (xlListP(xlVal))
        for (; xlConsP(xlVal); xlVal = xlCdr(xlVal))
            if (xlSymbolP(xlCar(xlVal)))
                xlUnexport(xlCar(xlVal),dst);
            else
                xlError("expecting a symbol",xlCar(xlVal));
    else if (xlSymbolP(xlVal))
        xlUnexport(xlVal,dst);
    else
        xlError("expecting a symbol or list of symbols",xlVal);
    return xlTrue;
}

/* ximport - import symbols into a package */
xlValue ximport(void)
{
    xlValue dst;
    xlVal = xlGetArg();
    dst = (xlMoreArgsP() ? getpackagearg() : xlGetValue(s_package));
    xlLastArg();
    if (xlListP(xlVal))
        for (; xlConsP(xlVal); xlVal = xlCdr(xlVal))
            if (xlSymbolP(xlCar(xlVal)))
                xlImport(xlCar(xlVal),dst);
            else
                xlError("expecting a symbol",xlCar(xlVal));
    else if (xlSymbolP(xlVal))
        xlImport(xlVal,dst);
    else
        xlError("expecting a symbol or list of symbols",xlVal);
    return xlTrue;
}

/* xmksymbol - make an uninterned symbol */
xlValue xmksymbol(void)
{
    xlVal = xlGetArgString();
    xlLastArg();
    return xlMakeSymbol(xlCopyString(xlVal));
}

/* xintern - intern a symbol in a package */
void xintern(void)
{
    xlValue dst,key;
    xlVal = xlGetArgString();
    dst = (xlMoreArgsP() ? getpackagearg() : xlGetValue(s_package));
    xlLastArg();
    xlVal = xlIntern(xlVal,dst,&key);
    xlArgC = 2;
    xlCPush(key);
    xlDrop(1);
    xlCDRestore();
}

/* xfindsymbol - find a symbol in a package */
void xfindsymbol(void)
{
    xlValue dst,key;
    xlVal = xlGetArgString();
    dst = (xlMoreArgsP() ? getpackagearg() : xlGetValue(s_package));
    xlLastArg();
    xlVal = xlFindSymbol(xlGetString(xlVal),dst,&key);
    xlArgC = 2;
    xlCPush(key);
    xlDrop(1);
    xlCDRestore();
}

/* xunintern - unintern a symbol from a package */
xlValue xunintern(void)
{
    xlValue dst;
    xlVal = xlGetArgSymbol();
    dst = (xlMoreArgsP() ? getpackagearg() : xlGetValue(s_package));
    xlLastArg();
    xlUnintern(xlVal,dst);
    return xlTrue;
}

/* getpackagenamearg - get a package name argument */
static xlValue getpackagenamearg(void)
{
    return nametostring(xlGetArg());
}

/* nametostring - convert package name to a string */
static xlValue nametostring(xlValue arg)
{
    if (xlSymbolP(arg))
        return xlGetPName(arg);
    else if (xlStringP(arg))
        return arg;
    xlError("expecting a package name",arg);
    return xlNil; /* never reached */
}

/* getpackagearg - get a package argument */
static xlValue getpackagearg(void)
{
    return nametopackage(xlGetArg());
}

/* nametopackage - convert a name to a package */
static xlValue nametopackage(xlValue arg)
{
    xlValue pack;
    if (xlPackageP(arg))
        return arg;
    else if (xlSymbolP(arg))
        arg = xlGetPName(arg);
    else if (!xlStringP(arg))
        xlError("expecting a package name",arg);
    if ((pack = xlFindPackage(xlGetString(arg))) == xlNil)
        xlError("no package",arg);
    return pack;
}

/* xboundp - is this a value bound to this symbol? */
xlValue xboundp(void)
{
    xlValue sym,env,tmp;
    int off;
    
    /* parse the arguments */
    sym = xlGetArgSymbol();
    env = xlMoreArgsP() ? xlGetEnv() : xlNil;
    xlLastArg();

    /* check the global environment */
    if (env == xlNil || (tmp = xlFindVar(env,sym,&off)) == xlNil)
        return xlBoundP(sym) ? xlTrue : xlFalse;

    /* bound as an instance variable or local variable */
    else
        return xlTrue;
}

/* xsymname - get the print name of a symbol */
xlValue xsymname(void)
{
    xlValue sym;
    sym = xlGetArgSymbol();
    xlLastArg();
    return xlGetPName(sym);
}

/* xsymvalue - get the value of a symbol */
xlValue xsymvalue(void)
{
    xlValue sym,env,tmp;
    int off;
    sym = xlGetArgSymbol();
    env = xlMoreArgsP() ? xlGetEnv() : xlNil;
    xlLastArg();

    /* return a global value */
    if (env == xlNil || (tmp = xlFindVar(env,sym,&off)) == xlNil)
        return xlGetValue(sym);

    /* return an instance variable */
    else if xlObjectP(tmp)
        return xlGetIVar(tmp,off);
    
    /* return a local variable */
    else
        return xlGetEnvElement(tmp,off);
}

/* xsetsymvalue - set the value of a symbol */
xlValue xsetsymvalue(void)
{
    xlValue sym,val,env,tmp;
    int off;

    /* get the symbol */
    sym = xlGetArgSymbol();
    val = xlGetArg();
    env = xlMoreArgsP() ? xlGetEnv() : xlNil;
    xlLastArg();

    /* set the global value */
    if (env == xlNil || (tmp = xlFindVar(env,sym,&off)) == xlNil)
        xlSetValue(sym,val);

    /* set an instance variable */
    else if (xlObjectP(tmp))
        xlSetIVar(tmp,off,val);

    /* set a local variable */
    else 
        xlSetEnvElement(tmp,off,val);

    /* return the value */
    return val;
}

/* xsympackage - get the home package of a symbol */
xlValue xsympackage(void)
{
    xlValue sym;
    sym = xlGetArgSymbol();
    xlLastArg();
    return xlGetPackage(sym);
}

/* xsymplist - get the property list of a symbol */
xlValue xsymplist(void)
{
    xlValue sym;

    /* get the symbol */
    sym = xlGetArgSymbol();
    xlLastArg();

    /* return the property list */
    return xlGetPList(sym);
}

/* xsetsymplist - set the property list of a symbol */
xlValue xsetsymplist(void)
{
    xlValue sym,val;

    /* get the symbol */
    sym = xlGetArgSymbol();
    val = xlGetArg();
    xlLastArg();

    /* set the property list */
    xlSetPList(sym,val);
    return val;
}

/* xget - get the value of a property */
xlValue xget(void)
{
    xlValue sym,prp;

    /* get the symbol and property */
    sym = xlGetArgSymbol();
    prp = xlGetArgSymbol();
    xlLastArg();

    /* retrieve the property value */
    return xlGetProp(sym,prp);
}

/* xput - set the value of a property */
xlValue xput(void)
{
    xlValue sym,val,prp;

    /* get the symbol and property */
    sym = xlGetArgSymbol();
    prp = xlGetArgSymbol();
    val = xlGetArg();
    xlLastArg();

    /* set the property value */
    xlPutProp(sym,val,prp);

    /* return the value */
    return val;
}

/* xremprop - remove a property value from a property list */
xlValue xremprop(void)
{
    xlValue sym,prp;

    /* get the symbol and property */
    sym = xlGetArgSymbol();
    prp = xlGetArgSymbol();
    xlLastArg();

    /* remove the property */
    xlRemProp(sym,prp);

    /* return nil */
    return xlNil;
}

/* xprocenvironment - built-in function 'procedure-environment' */
xlValue xprocenvironment(void)
{
    xlValue arg;
    arg = xlGetArgClosure();
    xlLastArg();
    return xlGetEnvironment(arg);
}

/* xenvp - built-in function 'environment?' */
xlValue xenvp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlEnvironmentP(arg) ? xlTrue : xlFalse;
}

/* xenvbindings - built-in function 'environment-bindings' */
xlValue xenvbindings(void)
{
    xlValue names,this,last;
    xlFIXTYPE len,i;

    /* get the environment */
    xlVal = xlGetEnv();
    xlLastArg();

    /* check for an empty environment */
    if (xlVal == xlNil)
        return xlNil;

    /* initialize */
    names = xlGetEnvNames(xlVal);
    len = xlGetEnvSize(xlVal);
    xlCPush(xlNil);

    /* build a list of dotted pairs */
    last = xlNil, i = xlFIRSTENV;

    /* the names can run out on a function call frame because of &optional
       and &key arguments */
    while (i < len && names != xlNil) {
        this = xlCons(xlCons(xlCar(names),xlGetEnvElement(xlVal,i)),xlNil);
        if (last == xlNil) xlSetTop(this);
        else xlSetCdr(last,this);
        last = this;
        names = xlCdr(names);
        ++i;
    }
    return xlPop();
}

/* xenvparent - built-in function 'environment-parent' */
xlValue xenvparent(void)
{
    xlValue env = xlGetEnv();
    xlLastArg();
    return env == xlNil ? xlNil : xlGetNextFrame(env);
}

/* xobjectp - built-in function 'object?' */
xlValue xobjectp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlObjectP(arg) ? xlTrue : xlFalse;
}

/* xvector - built-in function 'vector' */
xlValue xvector(void)
{
    xlValue vect,*p;
    vect = xlNewVector(xlArgC);
    for (p = xlGetVector(vect); xlMoreArgsP(); )
        *p++ = xlGetArg();
    return vect;
}

/* xmakevector - built-in function 'make-vector' */
xlValue xmakevector(void)
{
    xlValue arg,val,*p;
    xlFIXTYPE len;
    
    /* get the vector size */
    arg = xlGetArgFixnum();
    len = xlGetFixnum(arg);

    /* check for an initialization value */
    if (xlMoreArgsP()) {
        arg = xlGetArg();       /* get the initializer */
        xlLastArg();            /* make sure that's the last argument */
        xlCPush(arg);           /* save the initializer */
        val = xlNewVector(len); /* create the vector */
        p = xlGetVector(val);   /* initialize the vector */
        for (arg = xlPop(); --len >= 0; )
            *p++ = arg;
    }

    /* no initialization value */
    else
        val = xlNewVector(len); /* defaults to initializing to NIL */
    
    /* return the new vector */
    return val;
}

/* xvlength - built-in function 'vector-length' */
xlValue xvlength(void)
{
    xlValue arg;
    arg = xlGetArgVector();
    xlLastArg();
    return xlMakeFixnum((xlFIXTYPE)xlGetSize(arg));
}

/* xivlength - built-in function '%vector-length' */
xlValue xivlength(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlMakeFixnum((xlFIXTYPE)xlGetSize(arg));
}

/* xvref - built-in function 'vector-ref' */
xlValue xvref(void)
{
    return vref(xlGetArgVector());
}

/* xivref - built-in function '%vector-ref' */
xlValue xivref(void)
{
    return vref(xlGetArg());
}

/* vref - common code for xvref and xivref */
static xlValue vref(xlValue vector)
{
    xlValue index;
    xlFIXTYPE i;

    /* get the index */
    index = xlGetArgFixnum();
    xlLastArg();

    /* range check the index */
    if ((i = xlGetFixnum(index)) < 0 || i >= xlGetSize(vector))
        xlError("index out of range",index);

    /* return the vector element */
    return xlGetElement(vector,i);
}

/* xvset - built-in function 'vector-set!' */
xlValue xvset(void)
{
    return vset(xlGetArgVector());
}

/* xivset - built-in function '%vector-set!' */
xlValue xivset(void)
{
    return vset(xlGetArg());
}

/* vset - common code for xvset and xivset */
static xlValue vset(xlValue vector)
{
    xlValue index,val;
    xlFIXTYPE i;

    /* get the index and the new value */
    index = xlGetArgFixnum();
    val = xlGetArg();
    xlLastArg();

    /* range check the index */
    if ((i = xlGetFixnum(index)) < 0 || i >= xlGetSize(vector))
        xlError("index out of range",index);

    /* set the vector element and return the value */
    xlSetElement(vector,i,val);
    return val;
}

/* xibase - built-in function '%vector-base' */
xlValue xivbase(void)
{
    xlValue vector;
    vector = xlGetArg();
    xlLastArg();
    return xlMakeFixnum((xlFIXTYPE)xlGetVector(vector));
}

/* xvectlist - built-in function 'vector->list' */
xlValue xvectlist(void)
{
    xlFIXTYPE size;
    xlValue vect;

    /* get the vector */
    vect = xlGetArgVector();
    xlLastArg();
    
    /* make a list from the vector */
    xlCPush(vect);
    size = xlGetSize(vect);
    for (xlVal = xlNil; --size >= 0; )
        xlVal = xlCons(xlGetElement(vect,size),xlVal);
    xlDrop(1);
    return xlVal;
}

/* xlistvect - built-in function 'list->vector' */
xlValue xlistvect(void)
{
    xlValue vect,*p;
    xlFIXTYPE size;

    /* get the list */
    xlVal = xlGetArgList();
    xlLastArg();

    /* make a vector from the list */
    size = xlLength(xlVal);
    vect = xlNewVector(size);
    for (p = xlGetVector(vect); --size >= 0; xlVal = xlCdr(xlVal))
        *p++ = xlCar(xlVal);
    return vect;
}

/* xmakearray - built-in function 'make-array' */
xlValue xmakearray(void)
{
    xlValue val;
    val = makearray1(xlArgC,xlSP);
    xlPopArgs();
    return val;
}

/* makearray1 - helper function for xmakearray */
static xlValue makearray1(int argc,xlValue *argv)
{
    xlFIXTYPE size,i;
    xlValue arg;

    /* check for the end of the list of dimensions */
    if (--argc < 0)
        return xlNil;

    /* get this dimension */
    arg = *argv++;
    if (!xlFixnumP(arg))
        xlBadType(arg);
    size = xlGetFixnum(arg);

    /* make the new array */
    xlCPush(xlNewVector(size));

    /* fill the array and return it */
    for (i = 0; i < size; ++i)
        xlSetElement(xlTop(),i,makearray1(argc,argv));
    return xlPop();
}

/* xaref - built-in function 'array-ref' */
xlValue xaref(void)
{
    xlValue array,index;
    xlFIXTYPE i;

    /* get the array */
    array = xlGetArgVector();

    /* get each array index */
    while (xlArgC > 1) {
        index = xlGetArgFixnum(); i = xlGetFixnum(index);
        if (i < 0 || i > xlGetSize(array))
            xlError("index out of range",index);
        array = xlGetElement(array,i);
        if (!xlVectorP(array))
            xlBadType(array);
    }
    xlCPush(array); ++xlArgC;
    return xvref();
}

/* xaset - built-in function 'array-set!' */
xlValue xaset(void)
{
    xlValue array,index;
    xlFIXTYPE i;

    /* get the array */
    array = xlGetArgVector();

    /* get each array index */
    while (xlArgC > 2) {
        index = xlGetArgFixnum(); i = xlGetFixnum(index);
        if (i < 0 || i > xlGetSize(array))
            xlError("index out of range",index);
        array = xlGetElement(array,i);
        if (!xlVectorP(array))
            xlBadType(array);
    }
    xlCPush(array); ++xlArgC;
    return xvset();
}

/* xmaketable - built-in function 'make-table' */
xlValue xmaketable(void)
{
    xlFIXTYPE len = xlHSIZE;
    
    /* get the vector size */
    if (xlMoreArgsP()) {
        xlVal = xlGetArgFixnum();
        len = xlGetFixnum(xlVal);
    }
    xlLastArg();

    /* make the table */
    return xlNewTable(len);
}

/* xtablep - built-in function 'table?' */
xlValue xtablep(void)
{
    xlVal = xlGetArg();
    xlLastArg();
    return xlTableP(xlVal) ? xlTrue : xlFalse;
}

/* xtableref - built-in function 'table-ref' */
xlValue xtableref(void)
{
    xlValue key;

    /* parse the arguments */
    xlVal = xlGetArgTable();
    key = xlGetArg();
    xlLastArg();

    /* find the entry */
    xlVal = xlFindEntryInTable(xlVal,key);
    return xlVal == xlNil ? xlNil : xlCdr(xlVal);
}

/* xtableset - built-in function 'table-set!' */
xlValue xtableset(void)
{
    xlValue key,val;

    /* parse the arguments */
    xlVal = xlGetArgTable();
    key = xlGetArg();
    val = xlGetArg();
    xlLastArg();

    /* add the entry */
    xlAddEntryToTable(xlVal,key,val);
    return xlVal;
}

/* xtableremove - built-in function 'table-remove!' */
xlValue xtableremove(void)
{
    xlValue key;

    /* parse the arguments */
    xlVal = xlGetArgTable();
    key = xlGetArg();
    xlLastArg();

    /* remove the entry */
    xlVal = xlRemoveEntryFromTable(xlVal,key);
    return xlVal == xlNil ? xlNil : xlCdr(xlVal);
}

/* xemptytable - built-in function 'empty-table!' */
xlValue xemptytable(void)
{
    xlFIXTYPE size,i;

    /* parse the arguments */
    xlVal = xlGetArgTable();
    xlLastArg();

    /* empty the table */
    size = xlGetSize(xlVal);
    for (i = 0; i < size; ++i)
        xlSetElement(xlVal,i,xlNil);
    return xlVal;
}

/* xmapovertableentries - built-in function 'map-over-table-entries' */
xlValue xmapovertableentries(void)
{
    xlValue table,fun,list,last,val;
    xlFIXTYPE size,i;
    
    /* parse the arguments */
    table = xlGetArgTable();
    fun = xlGetArg();
    xlLastArg();

    /* save the table and function */
    xlCheck(3);
    xlPush(table);
    xlPush(fun);

    /* initialize */
    size = xlGetSize(table);
    xlVal = xlNil;
    last = xlNil;

    /* map over the table entries */
    for (i = 0; i < size; ++i) {
        for (list = xlGetElement(table,i); xlConsP(list); list = xlCdr(list)) {
            xlValue entry = xlCar(list);
            xlPush(list);
            xlInternalCall(&val,1,fun,2,xlCar(entry),xlCdr(entry));
            if (last == xlNil) {
                xlVal = xlCons(val,xlNil);
                last = xlVal;
            }
            else {
                xlSetCdr(last,xlCons(val,xlNil));
                last = xlCdr(last);
            }
            list = xlPop();
        }
    }
    xlDrop(2);

    /* return the list of values */
    return xlVal;
}

/* xiaddrof - built-in function '%address-of' */
xlValue xiaddrof(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlMakeFixnum((xlFIXTYPE)arg);
}

/* xifmtaddr - built-in function '%format-address' */
xlValue xifmtaddr(void)
{
    char buf[20];
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    sprintf(buf,xlAFMT,arg);
    return xlMakeCString(buf);
}

/* xnull - built-in function 'null?' */
xlValue xnull(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlNullP(arg) ? xlTrue : xlFalse;
}

/* xatom - built-in function 'atom?' */
xlValue xatom(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlAtomP(arg) ? xlTrue : xlFalse;
}

/* xlistp - built-in function 'list?' */
xlValue xlistp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlListP(arg) ? xlTrue : xlFalse;
}

/* xendp - built-in function 'endp' */
xlValue xendp(void)
{
    xlValue arg;
    arg = xlGetArgList();
    xlLastArg();
    return xlNullP(arg) ? xlTrue : xlFalse;
}

/* xnumberp - built-in function 'number?' */
xlValue xnumberp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlNumberP(arg) ? xlTrue : xlFalse;
}

/* xbooleanp - built-in function 'boolean?' */
xlValue xbooleanp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return arg == xlTrue || arg == xlFalse ? xlTrue : xlFalse;
}

/* xpairp - built-in function 'pair?' */
xlValue xpairp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlConsP(arg) ? xlTrue : xlFalse;
}

/* xsymbolp - built-in function 'symbol?' */
xlValue xsymbolp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlSymbolP(arg) ? xlTrue : xlFalse;
}

/* xintegerp - built-in function 'integer?' */
xlValue xintegerp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlFixnumP(arg) ? xlTrue : xlFalse;
}

/* xrealp - built-in function 'real?' */
xlValue xrealp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlFlonumP(arg) || xlFixnumP(arg) ? xlTrue : xlFalse;
}

/* xcharp - built-in function 'char?' */
xlValue xcharp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlCharacterP(arg) ? xlTrue : xlFalse;
}

/* xstringp - built-in function 'string?' */
xlValue xstringp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlStringP(arg) ? xlTrue : xlFalse;
}

/* xvectorp - built-in function 'vector?' */
xlValue xvectorp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlVectorP(arg) ? xlTrue : xlFalse;
}

#define isprocedure(x) \
(xlClosureP(x) || xlContinuationP(x) || xlSubrP(x) || xlXSubrP(x))

/* xprocedurep - built-in function 'procedure?' */
xlValue xprocedurep(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return isprocedure(arg) ? xlTrue : xlFalse;
}

/* xdefaultobjectp - built-in function 'default-object?' */
xlValue xdefaultobjectp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return arg == xlDefaultObject ? xlTrue : xlFalse;
}

/* xeq - built-in function 'eq?' */
xlValue xeq(void)
{
    xlValue arg1,arg2;
    arg1 = xlGetArg();
    arg2 = xlGetArg();
    xlLastArg();
    return xlEq(arg1,arg2) ? xlTrue : xlFalse;
}

/* eq - internal 'eq?' function */
int xlEq(xlValue arg1,xlValue arg2)
{
    return arg1 == arg2;
}

/* xeqv - built-in function 'eqv?' */
xlValue xeqv(void)
{
    xlValue arg1,arg2;
    arg1 = xlGetArg();
    arg2 = xlGetArg();
    xlLastArg();
    return xlEqv(arg1,arg2) ? xlTrue : xlFalse;
}

/* eqv - internal 'eqv?' function */
int xlEqv(xlValue arg1,xlValue arg2)
{
    /* try the eq test first */
    if (arg1 == arg2)
        return TRUE;

    /* compare fixnums, flonums and characters */
    if (!xlNullP(arg1) && !xlNullP(arg2)) {
        switch (xlNodeType(arg1)) {
        case xlFIXNUM:
            switch (xlNodeType(arg2)) {
            case xlFIXNUM:
                return xlGetFixnum(arg1) == xlGetFixnum(arg2);
            case xlFLONUM:
                return (double)xlGetFixnum(arg1) == xlGetFlonum(arg2);
            default:
                return FALSE;
            }
        case xlFLONUM:
            switch (xlNodeType(arg2)) {
            case xlFIXNUM:
                return xlGetFlonum(arg1) == (double)xlGetFixnum(arg2);
            case xlFLONUM:
                return xlGetFlonum(arg1) == xlGetFlonum(arg2);
            default:
                return FALSE;
            }
        case xlCHARACTER:
            return xlCharacterP(arg2) && xlGetChCode(arg1) == xlGetChCode(arg2);
        case xlSTRING:
            return xlStringP(arg2) && stringequal(arg1,arg2);
        }
    }
    return FALSE;
}

/* xequal - built-in function 'equal?' */
xlValue xequal(void)
{
    xlValue arg1,arg2;
    arg1 = xlGetArg();
    arg2 = xlGetArg();
    xlLastArg();
    return xlEqual(arg1,arg2) ? xlTrue : xlFalse;
}

/* equal - internal 'equal?' function */
int xlEqual(xlValue arg1,xlValue arg2)
{
    /* try the eq test first */
    if (arg1 == arg2)
        return TRUE;

    /* compare fixnums, flonums, characters, strings, vectors and conses */
    if (!xlNullP(arg1) && !xlNullP(arg2)) {
        switch (xlNodeType(arg1)) {
        case xlFIXNUM:
            switch (xlNodeType(arg2)) {
            case xlFIXNUM:
                return xlGetFixnum(arg1) == xlGetFixnum(arg2);
            case xlFLONUM:
                return (double)xlGetFixnum(arg1) == xlGetFlonum(arg2);
            default:
                return FALSE;
            }
        case xlFLONUM:
            switch (xlNodeType(arg2)) {
            case xlFIXNUM:
                return xlGetFlonum(arg1) == (double)xlGetFixnum(arg2);
            case xlFLONUM:
                return xlGetFlonum(arg1) == xlGetFlonum(arg2);
            default:
                return FALSE;
            }
        case xlCHARACTER:
            return xlCharacterP(arg2) && xlGetChCode(arg1) == xlGetChCode(arg2);
        case xlSTRING:
            return xlStringP(arg2) && stringequal(arg1,arg2);
        case xlVECTOR:
            return xlVectorP(arg2) && vectorequal(arg1,arg2);
        case xlCONS:
            return xlConsP(arg2) && xlEqual(xlCar(arg1),xlCar(arg2))
                               && xlEqual(xlCdr(arg1),xlCdr(arg2));
        }
    }
    return FALSE;
}

/* stringequal - compare two strings */
static int stringequal(xlValue s1,xlValue s2)
{
    const char *p1 = xlGetString(s1);
    const char *p2 = xlGetString(s2);
    xlFIXTYPE len;

    /* compare the vector lengths */
    if ((len = xlGetSLength(s1)) != xlGetSLength(s2))
        return FALSE;

    /* compare the vector elements */
    while (--len >= 0)
        if (*p1++ != *p2++)
            return FALSE;
    return TRUE;
}

/* vectorequal - compare two vectors */
static int vectorequal(xlValue v1,xlValue v2)
{
    xlFIXTYPE len,i;

    /* compare the vector lengths */
    if ((len = xlGetSize(v1)) != xlGetSize(v2))
        return FALSE;

    /* compare the vector elements */
    for (i = 0; i < len; ++i)
        if (!xlEqual(xlGetElement(v1,i),xlGetElement(v2,i)))
            return FALSE;
    return TRUE;
}

/* xidentity - built-in function 'identity' */
xlValue xidentity(void)
{
    xlVal = xlGetArg();
    xlLastArg();
    return xlVal;
}

/* xgensym - generate a symbol */
xlValue xgensym(void)
{
    char sym[xlSTRMAX+11]; /* enough space for prefix and number */
    xlValue x;

    /* get the prefix or number */
    if (xlMoreArgsP()) {
        if ((x = xlGetArg()) == xlNil)
            xlError("bad argument type",x);
        else
            switch (xlNodeType(x)) {
            case xlSYMBOL:
                x = xlGetPName(x);
            case xlSTRING:
                strncpy(gsprefix,xlGetString(x),xlSTRMAX);
                gsprefix[xlSTRMAX] = '\0';
                break;
            case xlFIXNUM:
                gsnumber = xlGetFixnum(x);
                break;
            default:
                xlError("bad argument type",x);
            }
    }
    xlLastArg();

    /* create the pname of the new symbol */
    sprintf(sym,"%s%ld",gsprefix,gsnumber++);

    /* make a symbol with this print name */
    return xlMakeSymbol(xlMakeCString(sym));
}

/* xlGetKeyArg - get a keyword argument */
xlEXPORT int xlGetKeyArg(xlValue key,xlValue def,xlValue *pval)
{
    xlValue *p;
    int n;
    for (n = xlArgC, p = xlSP; n >= 2; n -= 2, p += 2)
        if (*p == key) {
            *pval = p[1];
            return TRUE;
        }
    *pval = def;
    return FALSE;
}

/* xlGetKeyFixnum - get a fixnum keyword argument */
xlEXPORT int xlGetKeyFixnum(xlValue key,xlFIXTYPE def,xlFIXTYPE *pval)
{
    xlValue arg;
    if (xlGetKeyArg(key,xlNil,&arg)) {
        if (!xlFixnumP(arg))
            xlBadType(arg);
        *pval = xlGetFixnum(arg);
        return TRUE;
    }
    *pval = def;
    return FALSE;
}

/* xlGetKeyString - get a string keyword argument */
xlEXPORT int xlGetKeyString(xlValue key,xlValue def,xlValue *pval)
{
    xlValue arg;
    if (xlGetKeyArg(key,xlNil,&arg)) {
        if (!xlStringP(arg))
            xlBadType(arg);
        *pval = arg;
        return TRUE;
    }
    *pval = def;
    return FALSE;
}

/* xlGetTest - get the :test or :test-not keyword argument */
void xlGetTest(xlValue def,xlValue *pfcn,xlValue *ptresult)
{
    if (xlGetKeyArg(k_test,def,pfcn))           /* :test */
        *ptresult = xlTrue;
    else if (xlGetKeyArg(k_testnot,def,pfcn))   /* :test-not */
        *ptresult = xlFalse;
    else
        *ptresult = xlTrue;
}

/* xlGetPort - get a port */
xlEXPORT xlValue xlGetPort(void)
{
    xlValue arg;

    /* get a file, unnamed or object stream or nil */
    if ((arg = xlGetArg()) != xlNil) {
        if (xlFileStreamP(arg)) {
            if (xlGetFile(arg) == NULL && (xlGetPFlags(arg) & xlpfTERMINAL) == 0)
                xlError("port not open",arg);
        }
        else if (!xlUnnamedStreamP(arg) && !xlObjectStreamP(arg))
            xlBadType(arg);
    }
    return arg;
}

/* xlGetInputPort - get an input port */
xlEXPORT xlValue xlGetInputPort(void)
{
    xlValue arg = xlGetPort();
    if (xlPortP(arg) && (xlGetPFlags(arg) & xlpfINPUT) == 0)
        xlError("expecting input port",arg);
    return arg;
}

/* xlGetOutputPort - get an output port */
xlEXPORT xlValue xlGetOutputPort(void)
{
    xlValue arg = xlGetPort();
    if (xlPortP(arg) && (xlGetPFlags(arg) & xlpfOUTPUT) == 0)
        xlError("expecting output port",arg);
    return arg;
}

/* xlGetEnv - get an environment */
xlValue xlGetEnv(void)
{
    xlValue val = xlGetArg();
    if (xlClosureP(val))
        val = xlGetEnvironment(val);
    else if (!xlEnvironmentP(val))
        xlBadType(val);
    return val;
}
