/* xlprint.c - xlisp print routine */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* global variables */
int xlPRBreadth = -1;
int xlPRDepth = -1;

/* local variables */
static char buf[200];

/* external variables */
extern xlValue s_printcase,k_downcase;
extern xlValue s_fixfmt,s_flofmt,xlUnboundObject;

static void print(xlValue fptr,xlValue vptr,int escflag,int depth);
static void putatm(xlValue fptr,const char *tag,xlValue val);
static void putfstream(xlValue fptr,xlValue val);
static void putstring(xlValue fptr,xlValue str);
static void putqstring(xlValue fptr,xlValue str);
static void putsymbol(xlValue fptr,xlValue sym);
static void putsympname(xlValue fptr,xlValue pname);
static void putpackage(xlValue fptr,xlValue val);
static void putsubr(xlValue fptr,const char *tag,xlValue val);
static void putclosure(xlValue fptr,const char *tag,xlValue val);
static void putcode(xlValue fptr,const char *tag,xlValue val);
static void putnumber(xlValue fptr,xlFIXTYPE n);
static void putoct(xlValue fptr,int n);
static void putflonum(xlValue fptr,xlFLOTYPE n);
static void putcharacter(xlValue fptr,int ch);
static void putobject(xlValue fptr,xlValue val);
static void putforeignptr(xlValue fptr,xlValue val);
static void putfree(xlValue fptr,xlValue val);

/* xlDisplay - print an expression without quoting */
xlEXPORT void xlDisplay(xlValue expr,xlValue file)
{
    xlCheck(2);
    xlPush(file);
    xlPush(expr);
    print(file,expr,FALSE,0);
    xlDrop(2);
}

/* xlDisplaySize - determine the size of display output */
xlEXPORT xlFIXTYPE xlDisplaySize(xlValue val)
{
    extern xlFIXTYPE xlfsize;
    xlfsize = 0;
    xlDisplay(val,xlNil);
    return xlfsize;
}

/* xlWrite - print an expression with quoting */
xlEXPORT void xlWrite(xlValue expr,xlValue file)
{
    xlCheck(2);
    xlPush(file);
    xlPush(expr);
    print(file,expr,TRUE,0);
    xlDrop(2);
}

/* xlWriteSize - determine the size of write output */
xlEXPORT xlFIXTYPE xlWriteSize(xlValue val)
{
    extern xlFIXTYPE xlfsize;
    xlfsize = 0;
    xlWrite(val,xlNil);
    return xlfsize;
}

/* xlNewline - terminate the current print line */
xlEXPORT void xlNewline(xlValue fptr)
{
    xlPutC(fptr,'\n');
}

/* xlFreshLine - terminate the current print line if necessary */
xlEXPORT void xlFreshLine(xlValue fptr)
{
    if (!xlAtBOLP(fptr))
        xlNewline(fptr);
}

/* xlPutStr - output a string */
xlEXPORT void xlPutStr(xlValue fptr,const char *str)
{
    while (*str)
        xlPutC(fptr,*str++);
}

/* print - internal print routine */
static void print(xlValue fptr,xlValue vptr,int escflag,int depth)
{
    xlFIXTYPE size,i;
    xlValue nptr,next;
    int breadth;

    /* print nil */
    if (vptr == xlNil) {
        xlPutStr(fptr,"()");
        return;
    }

    /* check value type */
    switch (xlNodeType(vptr)) {
    case xlSUBR:
            putsubr(fptr,"Subr",vptr);
            break;
    case xlXSUBR:
            putsubr(fptr,"XSubr",vptr);
            break;
    case xlCONS:
            if (xlPRDepth >= 0 && depth >= xlPRDepth) {
                xlPutStr(fptr,"(...)");
                break;
            }
            xlPutC(fptr,'(');
            breadth = 0;
            for (nptr = vptr; nptr != xlNil; nptr = next) {
                if (xlPRBreadth >= 0 && breadth++ >= xlPRBreadth) {
                    xlPutStr(fptr,"...");
                    break;
                }
                print(fptr,xlCar(nptr),escflag,depth+1);
                if ((next = xlCdr(nptr)) != xlNil) {
                    if (xlConsP(next))
                        xlPutC(fptr,' ');
                    else {
                        xlPutStr(fptr," . ");
                        print(fptr,next,escflag,depth+1);
                        break;
                    }
                }
            }
            xlPutC(fptr,')');
            break;
    case xlVECTOR:
            xlPutStr(fptr,"#(");
            for (i = 0, size = xlGetSize(vptr); i < size; ++i) {
                if (i != 0) xlPutC(fptr,' ');
                print(fptr,xlGetElement(vptr,i),escflag,depth+1);
            }
            xlPutC(fptr,')');
            break;
    case xlSYMBOL:
            putsymbol(fptr,vptr);
            break;
    case xlPACKAGE:
            putpackage(fptr,vptr);
            break;
    case xlPROMISE:
            if (xlGetPProc(vptr) != xlNil)
                putatm(fptr,"Promise",vptr);
            else
                putatm(fptr,"Forced-promise",vptr);
            break;
    case xlCLOSURE:
            putclosure(fptr,"Procedure",vptr);
            break;
    case xlFIXNUM:
            putnumber(fptr,xlGetFixnum(vptr));
            break;
    case xlFLONUM:
            putflonum(fptr,xlGetFlonum(vptr));
            break;
    case xlCHARACTER:
            if (escflag)
                putcharacter(fptr,xlGetChCode(vptr));
            else
                xlPutC(fptr,xlGetChCode(vptr));
            break;
    case xlSTRING:
            if (escflag)
                putqstring(fptr,vptr);
            else
                putstring(fptr,vptr);
            break;
    case xlFSTREAM:
            putfstream(fptr,vptr);
            break;
    case xlUSTREAM:
            putatm(fptr,"Unnamed-stream",vptr);
            break;
    case xlOSTREAM:
            putatm(fptr,"Object-stream",vptr);
            break;
    case xlCODE:
            putcode(fptr,"Code",vptr);
            break;
    case xlCONTINUATION:
            putatm(fptr,"Escape-procedure",vptr);
            break;
    case xlENV:
            putatm(fptr,"Environment",vptr);
            break;
    case xlSENV:
            putatm(fptr,"Stack-environment",vptr);
            break;
    case xlMSENV:
            putatm(fptr,"Moved-stack-environment",vptr);
            break;
    case xlMENV:
            putatm(fptr,"Method-environment",vptr);
            break;
    case xlSMENV:
            putatm(fptr,"Stack-method-environment",vptr);
            break;
    case xlOBJECT:
            putobject(fptr,vptr);
            break;
    case xlFOREIGNPTR:
            putforeignptr(fptr,vptr);
            break;
    case xlTABLE:
            putatm(fptr,"Table",vptr);
            break;
    case xlFREE:
            putfree(fptr,vptr);
            break;
    default:
            putatm(fptr,"Foo",vptr);
            break;
    }
}

/* putatm - output an atom */
static void putatm(xlValue fptr,const char *tag,xlValue val)
{
    sprintf(buf,"#<%s #x",tag); xlPutStr(fptr,buf);
    sprintf(buf,xlAFMT,val); xlPutStr(fptr,buf);
    xlPutC(fptr,'>');
}

/* putfstream - output a file stream */
static void putfstream(xlValue fptr,xlValue val)
{
    xlPutStr(fptr,"#<File-stream #x");
    sprintf(buf,xlAFMT,val); xlPutStr(fptr,buf);
    xlPutC(fptr,':');
    sprintf(buf,xlAFMT,xlGetFile(val)); xlPutStr(fptr,buf);
    xlPutC(fptr,'>');
}

/* putstring - output a string */
static void putstring(xlValue fptr,xlValue str)
{
    xlFIXTYPE len,i;

    /* get the string length */
    len = xlGetSLength(str);

    /* output each character in the string */
    for (i = 0; i < len; ++i)
        xlPutC(fptr,xlGetString(str)[i]);
}

/* putqstring - output a quoted string */
static void putqstring(xlValue fptr,xlValue str)
{
    xlFIXTYPE len,i;
    int ch;

    /* get the string length */
    len = xlGetSLength(str);

    /* output the initial quote */
    xlPutC(fptr,'"');

    /* output each character in the string */
    for (i = 0; i < len; ++i)

        /* check for a control character */
        if ((ch = xlGetString(str)[i]) < 040 || ch == '\\' || ch == '"') {
            xlPutC(fptr,'\\');
            switch (ch) {
            case '\033':
                    xlPutC(fptr,'e');
                    break;
            case '\n':
                    xlPutC(fptr,'n');
                    break;
            case '\r':
                    xlPutC(fptr,'r');
                    break;
            case '\t':
                    xlPutC(fptr,'t');
                    break;
            case '\\':
            case '"':
                    xlPutC(fptr,ch);
                    break;
            default:
                    putoct(fptr,ch);
                    break;
            }
        }

        /* output a normal character */
        else
            xlPutC(fptr,ch);

    /* output the terminating quote */
    xlPutC(fptr,'"');
}

/* putsymbol - output a symbol */
static void putsymbol(xlValue fptr,xlValue sym)
{
    extern xlValue s_package,xlKeywordPackage,k_internal;
    xlValue package,key;
    if ((package = xlGetPackage(sym)) == xlNil)
        xlPutStr(fptr,"#:");
    else if (package == xlKeywordPackage)
        xlPutC(fptr,':');
    else if (!xlVisibleP(sym,xlGetValue(s_package))) {
        xlFindSymbol(xlGetString(xlGetPName(sym)),package,&key);
        putsympname(fptr,xlCar(xlGetNames(xlGetPackage(sym))));
        if (key == k_internal) xlPutC(fptr,':');
        xlPutC(fptr,':');
    }
    putsympname(fptr,xlGetPName(sym));
}

/* putsympname - output a symbol print name */
static void putsympname(xlValue fptr,xlValue pname)
{
    xlFIXTYPE len,i;
   
    /* get the string length */
    len = xlGetSLength(pname);

    /* print the symbol name */
    if (xlGetValue(s_printcase) == k_downcase)
        for (i = 0; i < len; ++i) {
            int ch = xlGetString(pname)[i];
            xlPutC(fptr,isupper(ch) ? tolower(ch) : ch);
        }
    else
        for (i = 0; i < len; ++i) {
            int ch = xlGetString(pname)[i];
            xlPutC(fptr,islower(ch) ? toupper(ch) : ch);
        }
}

/* putpackage - output a package */
static void putpackage(xlValue fptr,xlValue val)
{
    xlPutStr(fptr,"#<Package ");
    putstring(fptr,xlCar(xlGetNames(val)));
    xlPutStr(fptr,">");
}

/* putsubr - output a subr/fsubr */
static void putsubr(xlValue fptr,const char *tag,xlValue val)
{
    sprintf(buf,"#<%s %s>",tag,xlGetSubrName(val));
    xlPutStr(fptr,buf);
}

/* putclosure - output a closure */
static void putclosure(xlValue fptr,const char *tag,xlValue val)
{
    putcode(fptr,tag,xlGetCode(val));
}

/* putcode - output a code object */
static void putcode(xlValue fptr,const char *tag,xlValue val)
{
    xlValue name;
    if ((name = xlGetElement(val,1)) != xlNil) {
        xlPutStr(fptr,"#<");
        xlPutStr(fptr,tag);
        xlPutStr(fptr," ");
        putstring(fptr,xlGetPName(name));
        xlPutStr(fptr,">");
    }
    else
        putatm(fptr,tag,val);
}

/* putnumber - output a number */
static void putnumber(xlValue fptr,xlFIXTYPE n)
{
    xlValue fmt = xlGetValue(s_fixfmt);
    sprintf(buf,xlStringP(fmt) ? xlGetString(fmt) : xlIFMT,n);
    xlPutStr(fptr,buf);
}

/* putoct - output an octal byte value */
static void putoct(xlValue fptr,int n)
{
    sprintf(buf,"%03o",n);
    xlPutStr(fptr,buf);
}

/* putflonum - output a flonum */
static void putflonum(xlValue fptr,xlFLOTYPE n)
{
    xlValue fmt = xlGetValue(s_flofmt);
    sprintf(buf,xlStringP(fmt) ? xlGetString(fmt) : xlFFMT,n);
    xlPutStr(fptr,buf);
}

/* putcharacter - output a character value */
static void putcharacter(xlValue fptr,int ch)
{
    switch (ch) {
    case '\n':
        xlPutStr(fptr,"#\\Newline");
        break;
    case '\r':
        xlPutStr(fptr,"#\\Enter");
        break;
    case ' ':
        xlPutStr(fptr,"#\\Space");
        break;
    default:
        sprintf(buf,"#\\%c",ch);
        xlPutStr(fptr,buf);
        break;
    }
}

/* putobject - output an object value */
static void putobject(xlValue fptr,xlValue obj)
{
    extern xlValue s_print;
    xlInternalCall(&obj,1,obj,2,s_print,fptr);
}

/* putforeignptr - output a foreign pointer value */
static void putforeignptr(xlValue fptr,xlValue val)
{
    char buf[100];
    xlPutStr(fptr,"#<FP:");
    xlPutStr(fptr,xlGetFPType(val)->def->name);
    xlPutStr(fptr," #");
    sprintf(buf,xlAFMT,val);
    strcat(buf,":");
    sprintf(&buf[strlen(buf)],xlAFMT,xlGetFPtr(val));
    strcat(buf,">");
    xlPutStr(fptr,buf);
}

/* must be in type id order */
static const char *typenames[] = {
"FREE",
"CONS",
"SYMBOL",
"FIXNUM",
"FLONUM",
"STRING",
"FSTREAM",
"USTREAM",
"OSTREAM",
"VECTOR",
"CLOSURE",
"CODE",
"SUBR",
"XSUBR",
"CONTINUATION",
"CHARACTER",
"PROMISE",
"ENV",
"SENV",
"MSENV",
"MENV",
"SMENV",
"OBJECT",
"PACKAGE",
"FOREIGNPTR"
};

/* putfree - output a free value */
static void putfree(xlValue fptr,xlValue val)
{
    char buf[100];
    int typeid;
    xlPutStr(fptr,"#<Free #");
    sprintf(buf,xlAFMT,val);
    strcat(buf," was ");
    typeid = (int)(long)xlCar(val);
    if (typeid >= 0 && typeid <= xlMAXTYPEID)
        sprintf(&buf[strlen(buf)],"%s",typenames[typeid]);
    else
        sprintf(&buf[strlen(buf)],"unknown type (%d)",typeid);
    strcat(buf,">");
    xlPutStr(fptr,buf);
}


