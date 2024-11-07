/* xlsym.c - symbol handling routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* global variables */
xlValue xlPackages,xlLispPackage,xlXLispPackage,xlKeywordPackage;

/* external variables */
extern xlValue xlPackages,s_package,k_internal,k_external,k_inherited;

/* forward declarations */
static xlValue addtolist(xlValue list,xlValue val);
static xlValue removefromlist(xlValue list,xlValue val);
static xlValue findinlist(xlValue list,xlValue val);
static void addtotable(xlValue table,xlValue sym);
static void removefromtable(xlValue table,xlValue sym);
static xlValue findintable(xlValue table,xlValue sym);
static xlValue findnameintable(xlValue array,const char *name);
static void entersymbol(xlValue sym,xlValue table);
static xlValue findprop(xlValue sym,xlValue prp);
static int comparestr(const char *cstr,xlValue str);
static xlFIXTYPE hash(xlValue val,xlFIXTYPE size);
static xlUFIXTYPE hashstr(const char *str,xlFIXTYPE len);

/* xlInitPackages - initialize the packages */
void xlInitPackages(void)
{
    xlValue key;
    
    /* create the LISP package */
    xlLispPackage = xlNewPackage("LISP");
    s_package = xlInternCString("*PACKAGE*",xlLispPackage,&key);
    xlExport(s_package,xlLispPackage);
    xlSetValue(s_package,xlLispPackage);

    /* create the XLISP package */
    xlXLispPackage = xlNewPackage("XLISP");
    
    /* create keyword packages */
    xlKeywordPackage = xlNewPackage("KEYWORD");
}

/* xlSubr - define a built-in function */
xlEXPORT void xlSubr(const char *name,xlValue (*fcn)(void))
{
    xlValue sym,key,package;
    const char *p;
    if ((p = strchr(name,':')) == NULL)
        package = xlGetValue(s_package);
    else {
        char pname[xlSTRMAX + 1];
        int pnameLen = p - name;
        strncpy(pname,name,pnameLen);
        pname[pnameLen] = '\0';
        name = p + 1;
        if ((package = xlFindPackage(pname)) == xlNil)
            package = xlNewPackage(pname);
    }
    sym = xlInternCString(name,package,&key);
    xlSetValue(sym,xlMakeSubr(name,fcn));
    xlExport(sym,package);
}

/* xlXSubr - define a built-in function that returns multiple values */
xlEXPORT void xlXSubr(const char *name,void (*fcn)(void))
{
    xlValue sym,key,package;
    const char *p;
    if ((p = strchr(name,':')) == NULL)
        package = xlGetValue(s_package);
    else {
        char pname[xlSTRMAX + 1];
        int pnameLen = p - name;
        strncpy(pname,name,pnameLen);
        pname[pnameLen] = '\0';
        name = p + 1;
        if ((package = xlFindPackage(pname)) == xlNil)
            package = xlNewPackage(pname);
    }
    sym = xlInternCString(name,package,&key);
    xlSetValue(sym,xlMakeXSubr(name,fcn));
    xlExport(sym,package);
}

/* xlEnter - enter a symbol in the current package */
xlEXPORT xlValue xlEnter(const char *name)
{
    xlValue key;
    return xlInternCString(name,xlGetValue(s_package),&key);
}

/* xlEnterKeyword - enter a keyword */
xlEXPORT xlValue xlEnterKeyword(const char *name)
{
    return xlInternAndExport(name,xlKeywordPackage);
}

/* xlInternString - intern a symbol in a package */
xlEXPORT xlValue xlInternString(const char *name,xlFIXTYPE len,xlValue package,xlValue *pkey)
{
    xlValue sym;
    if ((sym = xlFindSymbol(name,package,pkey)) == xlNil) {
        xlCPush(xlMakeSymbol(xlMakeCString(name)));
        entersymbol(xlTop(),xlGetIntern(package));
        if (package == xlKeywordPackage)
            xlSetValue(xlTop(),xlTop());
        xSetPackage(xlTop(),package);
        sym = xlPop();
    }
    return sym;
}

/* xlInternCString - intern a symbol in a package */
xlEXPORT xlValue xlInternCString(const char *name,xlValue package,xlValue *pkey)
{
    xlValue sym;
    if ((sym = xlFindSymbol(name,package,pkey)) == xlNil) {
        xlCPush(xlMakeSymbol(xlMakeCString(name)));
        entersymbol(xlTop(),xlGetIntern(package));
        if (package == xlKeywordPackage)
            xlSetValue(xlTop(),xlTop());
        xSetPackage(xlTop(),package);
        sym = xlPop();
    }
    return sym;
}

/* xlFindPackage - find a package by name */
xlEXPORT xlValue xlFindPackage(const char *name)
{
    xlValue pack,p;
    for (pack = xlPackages; xlPackageP(pack); pack = xlGetNextPackage(pack))
        for (p = xlGetNames(pack); xlConsP(p); p = xlCdr(p))
            if (comparestr(name,xlCar(p)))
                return pack;
    return xlNil;
}

/* xlUsePackage - add a package to another package usedby list */
int xlUsePackage(xlValue src,xlValue dst)
{
    xlSetUses(dst,addtolist(xlGetUses(dst),src));
    xlSetUsedBy(src,addtolist(xlGetUsedBy(src),dst));
    return TRUE;
}

/* xlUnusePackage - remove a package from another package usedby list */
int xlUnusePackage(xlValue src,xlValue dst)
{
    xlSetUses(dst,removefromlist(xlGetUses(dst),src));
    xlSetUsedBy(src,removefromlist(xlGetUsedBy(src),dst));
    return TRUE;
}

/* import - import a symbol into a package */
int xlImport(xlValue sym,xlValue package)
{
    if (!xlPresentP(sym,package)) {
        addtotable(xlGetIntern(package),sym);
        if (xlGetPackage(sym) == xlNil)
            xSetPackage(sym,package);
    }
    return TRUE;
}

/* export - export a symbol from a package */
int xlExport(xlValue sym,xlValue package)
{
    if (xlPresentP(sym,package)) {
        removefromtable(xlGetIntern(package),sym);
        addtotable(xlGetExtern(package),sym);
    }
    else
        xlError("symbol is not present",sym);
    return TRUE;
}

/* unexport - unexport a symbol from a package */
int xlUnexport(xlValue sym,xlValue package)
{
    if (xlPresentP(sym,package)) {
        removefromtable(xlGetExtern(package),sym);
        addtotable(xlGetIntern(package),sym);
    }
    else
        xlError("symbol is not present",sym);
    return TRUE;
}

/* xlVisibleP - determine if a symbol is visible in a package */
int xlVisibleP(xlValue sym,xlValue package)
{
    xlValue list;
    if (xlPresentP(sym,package))
        return TRUE;
    for (list = xlGetUses(package); xlConsP(list); list = xlCdr(list))
        if (findintable(xlGetExtern(xlCar(list)),sym) != xlNil)
            return TRUE;
    return FALSE;
}

/* xlPresentP - determine if a symbol is present in a package */
int xlPresentP(xlValue sym,xlValue package)
{
    return findintable(xlGetExtern(package),sym) != xlNil
    ||     findintable(xlGetIntern(package),sym) != xlNil;
}

/* xlFindSymbol - find a symbol in a package */
xlValue xlFindSymbol(const char *name,xlValue package,xlValue *pkey)
{
    xlValue list,sym;
    if ((sym = findnameintable(xlGetExtern(package),name)) != xlNil) {
        *pkey = k_external;
        return sym;
    }
    else if ((sym = findnameintable(xlGetIntern(package),name)) != xlNil) {
        *pkey = k_internal;
        return sym;
    }
    for (list = xlGetUses(package); xlConsP(list); list = xlCdr(list))
        if ((sym = findnameintable(xlGetExtern(xlCar(list)),name)) != xlNil) {
            *pkey = k_inherited;
            return sym;
        }
    *pkey = xlNil;
    return xlNil;
}

/* intern - intern a symbol in a package */
xlValue xlIntern(xlValue name,xlValue package,xlValue *pkey)
{
    xlValue sym;
    if ((sym = xlFindSymbol(xlGetString(name),package,pkey)) == xlNil) {
        xlCPush(xlMakeSymbol(xlCopyString(name)));
        entersymbol(xlTop(),xlGetIntern(package));
        if (package == xlKeywordPackage)
            xlSetValue(xlTop(),xlTop());
        xSetPackage(xlTop(),package);
        sym = xlPop();
    }
    return sym;
}

/* xlInternAndExport - intern a symbol in a package and make it external */
xlValue xlInternAndExport(const char *name,xlValue package)
{
    xlValue sym,key;
    sym = xlInternCString(name,package,&key);
    xlExport(sym,package);
    return sym;
}

/* unintern - remove a symbol from a package */
void xlUnintern(xlValue sym,xlValue package)
{
    removefromtable(xlGetExtern(package),sym);
    removefromtable(xlGetIntern(package),sym);
    if (xlGetPackage(sym) == package)
        xSetPackage(sym,xlNil);
}

/* addtolist - add a value to a list if it isn't already there */
static xlValue addtolist(xlValue list,xlValue val)
{
    xlValue this;
    for (this = list; xlConsP(this); this = xlCdr(this))
        if (val == xlCar(this))
            return list;
    return xlCons(val,list);
}

/* removefromlist - remove an entry from a list */
static xlValue removefromlist(xlValue list,xlValue val)
{
    xlValue prev,this;
    for (prev = xlNil, this = list; xlConsP(this); prev = this, this = xlCdr(this))
        if (val == xlCar(this)) {
            if (prev != xlNil) xlSetCdr(prev,xlCdr(this));
            else list = xlCdr(this);
            break;
        }
    return list;
}

/* findinlist - find a value in a list */
static xlValue findinlist(xlValue list,xlValue val)
{
    xlValue this;
    for (this = list; xlConsP(this); this = xlCdr(this))
        if (val == xlCar(this))
            return val;
    return xlNil;
}

/* addtotable - add a symbol to a hash table */
static void addtotable(xlValue table,xlValue sym)
{
    xlValue pname = xlGetPName(sym);
    xlFIXTYPE i = hashstr(xlGetString(pname),xlGetSLength(pname)) % xlGetSize(table);
    xlSetElement(table,i,addtolist(xlGetElement(table,i),sym));
}

/* removefromtable - remove a symbol from a hash table */
static void removefromtable(xlValue table,xlValue sym)
{
    xlValue pname = xlGetPName(sym);
    xlFIXTYPE i = hashstr(xlGetString(pname),xlGetSLength(pname)) % xlGetSize(table);
    xlSetElement(table,i,removefromlist(xlGetElement(table,i),sym));
}

/* findintable - find a symbol in a hash table */
static xlValue findintable(xlValue table,xlValue sym)
{
    xlValue pname = xlGetPName(sym);
    xlFIXTYPE i = hashstr(xlGetString(pname),xlGetSLength(pname)) % xlGetSize(table);
    return findinlist(xlGetElement(table,i),sym);
}

/* findnameintable - find a symbol by name in a hash table */
static xlValue findnameintable(xlValue table,const char *name)
{
    xlFIXTYPE i = hashstr(name,strlen(name)) % xlGetSize(table);
    xlValue sym;
    for (sym = xlGetElement(table,i); sym != xlNil; sym = xlCdr(sym))
        if (comparestr(name,xlGetPName(xlCar(sym))))
            return xlCar(sym);
    return xlNil;
}

/* entersymbol - enter a symbol into a hash table */
static void entersymbol(xlValue sym,xlValue table)
{
    xlValue pname = xlGetPName(sym);
    xlFIXTYPE i = hashstr(xlGetString(pname),xlGetSLength(pname)) % xlGetSize(table);
    xlSetElement(table,i,xlCons(sym,xlGetElement(table,i)));
}

/* xlGetProp - get the value of a property */
xlEXPORT xlValue xlGetProp(xlValue sym,xlValue prp)
{
    xlValue p;
    return (p = findprop(sym,prp)) != xlNil ? xlCar(p) : xlNil;
}

/* xlPutProp - put a property value onto the property list */
xlEXPORT void xlPutProp(xlValue sym,xlValue val,xlValue prp)
{
    xlValue pair;
    if ((pair = findprop(sym,prp)) != xlNil)
        xlSetCar(pair,val);
    else
        xlSetPList(sym,xlCons(prp,xlCons(val,xlGetPList(sym))));
}

/* xlRemProp - remove a property from a property list */
xlEXPORT void xlRemProp(xlValue sym,xlValue prp)
{
    xlValue last,p;
    last = xlNil;
    for (p = xlGetPList(sym); xlConsP(p) && xlConsP(xlCdr(p)); p = xlCdr(last)) {
        if (xlCar(p) == prp) {
            if (last != xlNil)
                xlSetCdr(last,xlCdr(xlCdr(p)));
            else
                xlSetPList(sym,xlCdr(xlCdr(p)));
        }
        last = xlCdr(p);
    }
}

/* findprop - find a property pair */
static xlValue findprop(xlValue sym,xlValue prp)
{
    xlValue p;
    for (p = xlGetPList(sym); xlConsP(p) && xlConsP(xlCdr(p)); p = xlCdr(xlCdr(p)))
        if (xlCar(p) == prp)
            return xlCdr(p);
    return xlNil;
}

/* comparestr - compare a c string with a lisp string */
static int comparestr(const char *cstr,xlValue str)
{
    const char *lstr = xlGetString(str);
    xlFIXTYPE len = xlGetSLength(str);
    for (; --len >= 0 && *cstr != '\0'; ++cstr, ++lstr)
        if (*cstr != *lstr)
            return FALSE;
    return len == -1 && *cstr == '\0';
}

/* xlFindEntryInTable - find an entry in a hash table by key */
xlValue xlFindEntryInTable(xlValue table,xlValue key)
{
    xlFIXTYPE i = hash(key,xlGetSize(table));
    xlValue list = xlGetElement(table,i);
    for (; list != xlNil; list = xlCdr(list)) {
        xlValue entry = xlCar(list);
        if (xlEqv(key,xlCar(entry)))
            return entry;
    }
    return xlNil;
}

/* xlAddEntryToTable - add an entry to a hash table */
void xlAddEntryToTable(xlValue table,xlValue key,xlValue val)
{
    xlFIXTYPE i = hash(key,xlGetSize(table));
    xlValue list = xlGetElement(table,i);
    for (; list != xlNil; list = xlCdr(list)) {
        xlValue entry = xlCar(list);
        if (xlEqv(key,xlCar(entry))) {
            xlSetCdr(entry,val);
            return;
        }
    }
    xlSetElement(table,i,xlCons(xlCons(key,val),xlGetElement(table,i)));
}

/* xlRemoveEntryFromTable - remove an entry from a hash table by key */
xlValue xlRemoveEntryFromTable(xlValue table,xlValue key)
{
    xlFIXTYPE i = hash(key,xlGetSize(table));
    xlValue list = xlGetElement(table,i);
    xlValue prev = xlNil;
    for (; list != xlNil; prev = list, list = xlCdr(list)) {
        xlValue entry = xlCar(list);
        if (xlEqv(key,xlCar(entry))) {
            if (prev == xlNil)
                xlSetElement(table,i,xlCdr(list));
            else
                xlSetCdr(prev,xlCdr(list));
            return entry;
        }
    }
    return xlNil;
}

/* hash - hash a lisp value */
static xlFIXTYPE hash(xlValue val,xlFIXTYPE size)
{
    if (val == xlNil)
        return 0;
    else {
        switch (xlNodeType(val)) {
        case xlSTRING:
            return hashstr(xlGetString(val),xlGetSLength(val)) % size;
        case xlFIXNUM:
            return (xlUFIXTYPE)xlGetFixnum(val) % size;
        case xlFLONUM:
            return (xlUFIXTYPE)xlGetFlonum(val) % size;
        default:
            return (xlUFIXTYPE)val % size;
        }
    }
}

/* hashstr - hash a symbol name string */
static xlUFIXTYPE hashstr(const char *str,xlFIXTYPE len)
{
    xlUFIXTYPE i = 0;
    while (--len >= 0)
        i = (i << 2) ^ *str++;
    return i;
}
