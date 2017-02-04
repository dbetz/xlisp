/* xlRead.c - xlisp input routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"
#include <math.h>

/* symbol name constituents */
#define WSPACE "\t \f\r\n"
#define CONST1 "!$%&*+-./0123456789:<=>?@[]^_{}~"
#define CONST2 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

/* return values for readone() */
#define RO_EXPR         1
#define RO_COMMENT      2
#define RO_EOF          3

/* external variables */
extern xlValue s_package,s_quote,s_function,s_quasiquote,s_unquote,s_unquotesplicing,s_dot;
extern xlValue xlEofObject;

/* forward declarations */
static int readone(xlValue fptr,xlValue *pval);
static xlValue read_list(xlValue fptr);
static void read_cdr(xlValue fptr,xlValue last);
static void read_comment(xlValue fptr);
static xlValue read_vector(xlValue fptr);
static xlValue read_comma(xlValue fptr);
static xlValue read_quote(xlValue fptr,xlValue sym);
static xlValue read_symbol(xlValue fptr);
static xlValue read_string(xlValue fptr);
static int read_special(xlValue fptr,int ch,xlValue *pval);
static xlValue read_radix(xlValue fptr,int radix);
static int isradixdigit(int ch,int radix);
static int getdigit(int ch);
static int getsymbol(xlValue fptr,char *buf);
static int scan(xlValue fptr);
static int checkeof(xlValue fptr);
static int isconstituent(int ch);
static xlValue tentry(int ch);

/* xlRead - read an expression */
int xlRead(xlValue fptr,xlValue *pval)
{
    int sts;

    /* read an expression skipping any leading comments */
    xlCPush(fptr);
    while ((sts = readone(fptr,pval)) == RO_COMMENT)
        ;
        
    /* skip over any trailing spaces up to a newline */
    if (sts == RO_EXPR) {
        while (xlInputReadyP(fptr)) {
            int ch = xlPeek(fptr);
            if (isspace(ch)) {
                xlGetC(fptr); /* skip over the character */
                if (ch == '\n')
                    break;
            }
            else
                break;
        }
    }
    xlDrop(1);

    /* return with status */
    return sts == RO_EXPR;
}

/* readone - read a single expression (maybe) */
static int readone(xlValue fptr,xlValue *pval)
{
    int argc,ch;
    xlValue entry;

    /* get the next character */
    if ((ch = scan(fptr)) == EOF)
        return RO_EOF;
    
    /* check for a constituent */
    else if ((entry = tentry(ch)) == xlSymConst) {
        xlUngetC(fptr,ch);
        *pval = read_symbol(fptr);
        return RO_EXPR;
    }
    
    /* check for a read macro for this character (type . [function | vector]) */
    else if (xlConsP(entry)) {
        entry = xlCdr(entry);
        
        /* check for a dispatch macro */
        if (xlVectorP(entry)) {
            ch = xlGetC(fptr);
            if (ch == EOF)
                xlFmtError("unexpected end of file");
            else if (ch >= xlGetSize(entry))
                xlFmtError("character out of bounds ~S",xlMakeChar(ch));
            argc = xlInternalCall(pval,1,xlGetElement(entry,ch),2,fptr,xlMakeChar(ch));
        }
        
        /* handle a normal read macro */
        else
            argc = xlInternalCall(pval,1,entry,2,fptr,xlMakeChar(ch));

        /* treat as white space if macro returned no values */
        return argc == 0 ? RO_COMMENT : RO_EXPR;
    }
    
    /* handle illegal characters */
    else {
        xlFmtError("unknown character ~S",xlMakeChar(ch));
        return RO_COMMENT; /* never reached */
    }
}

/* xread - built-in function 'read' */
xlValue xread(void)
{
    xlValue val;

    /* get file pointer and eof value */
    xlVal = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();

    /* read an expression */
    if (!xlRead(xlVal,&val))
        val = xlEofObject;

    /* return the expression */
    return val;
}

/* xreaddelimitedlist - read a delimited list */
xlValue xreaddelimitedlist(void)
{
    xlValue fptr,last,val;
    int tch,ch;

    /* parse the argument list */
    tch = xlGetChCode(xlGetArgChar());
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    
    /* protect the input stream */
    xlCPush(fptr);
    
    /* build the list */
    xlVal = last = xlNil;
    while ((ch = scan(fptr)) != tch) {
        if (ch == EOF)
            xlFmtError("unexpected EOF");
        xlUngetC(fptr,ch);
        switch (readone(fptr,&val)) {
        case RO_EOF:
            xlFmtError("unexpected EOF");
        case RO_EXPR:
            if (val == s_dot)
                xlFmtError("misplaced dot");
            else {
                val = xlCons(val,xlNil);
                if (last) xlSetCdr(last,val);
                else xlVal = val;
                last = val;
            }
            break;
        }
    }
    xlDrop(1);
    return xlVal;
}

/* xrmhash - read macro %RM-HASH */
void xrmhash(void)
{
    xlValue mch,val;
    
    /* parse the argument list */
    xlVal = xlGetInputPort();
    mch = xlGetArgChar();
    xlLastArg();
    
    /* return zero arguments for comments */
    if (read_special(xlVal,xlGetChCode(mch),&val) == RO_COMMENT)
        xlMVReturn(0);
    xlVal = val;
    xlSVReturn();
}

/* xrmquote - read macro %RM-QUOTE */
void xrmquote(void)
{
    xlValue mch;
    
    /* parse the argument list */
    xlVal = xlGetInputPort();
    mch = xlGetArgChar();
    xlLastArg();
    
    /* return the result */
    xlVal = read_quote(xlVal,s_quote);
    xlSVReturn();
}

/* xrmdquote - read macro %RM-DOUBLE-QUOTE */
void xrmdquote(void)
{
    xlValue mch;
    
    /* parse the argument list */
    xlVal = xlGetInputPort();
    mch = xlGetArgChar();
    xlLastArg();
    
    /* return the result */
    xlVal = read_string(xlVal);
    xlSVReturn();
}

/* xrmbquote - read macro %RM-BACKQUOTE */
void xrmbquote(void)
{
    xlValue mch;
    
    /* parse the argument list */
    xlVal = xlGetInputPort();
    mch = xlGetArgChar();
    xlLastArg();
    
    /* return the result */
    xlVal = read_quote(xlVal,s_quasiquote);
    xlSVReturn();
}

/* xrmcomma - read macro %RM-COMMA */
void xrmcomma(void)
{
    xlValue mch;
    
    /* parse the argument list */
    xlVal = xlGetInputPort();
    mch = xlGetArgChar();
    xlLastArg();
    
    /* return the result */
    xlVal = read_comma(xlVal);
    xlSVReturn();
}

/* xrmlparen - read macro %RM-LEFT-PAREN */
void xrmlparen(void)
{
    xlValue mch;
    
    /* parse the argument list */
    xlVal = xlGetInputPort();
    mch = xlGetArgChar();
    xlLastArg();
    
    /* return the result */
    xlVal = read_list(xlVal);
    xlSVReturn();
}

/* xrmrparen - read macro %RM-RIGHT-PAREN */
void xrmrparen(void)
{
    xlValue mch;
    
    /* parse the argument list */
    xlVal = xlGetInputPort();
    mch = xlGetArgChar();
    xlLastArg();
    
    /* illegal in this context */
    xlFmtError("misplaced right paren");
}

/* xrmsemi - read macro %RM-SEMICOLON */
void xrmsemi(void)
{
    xlValue mch;
    
    /* parse the argument list */
    xlVal = xlGetInputPort();
    mch = xlGetArgChar();
    xlLastArg();
    
    /* skip over the comment */
    read_comment(xlVal);
    xlMVReturn(0);
}

/* read_list - read a list */
static xlValue read_list(xlValue fptr)
{
    xlValue last,val;
    int ch;
    xlCPush(xlNil); last = xlNil;
    while ((ch = scan(fptr)) != ')') {
        if (ch == EOF)
            xlFmtError("unexpected EOF");
        xlUngetC(fptr,ch);
        switch (readone(fptr,&val)) {
        case RO_EOF:
            xlFmtError("unexpected EOF");
        case RO_EXPR:
            if (val == s_dot) {
                if (last == xlNil)
                    xlFmtError("misplaced dot");
                read_cdr(fptr,last);
                return xlPop();
            }
            else {
                val = xlCons(val,xlNil);
                if (last) xlSetCdr(last,val);
                else xlSetTop(val);
                last = val;
            }
            break;
        }
    }
    return xlPop();
}

/* read_cdr - read the cdr of a dotted pair */
static void read_cdr(xlValue fptr,xlValue last)
{
    xlValue val;
    int ch;
    
    /* read the cdr expression */
    if (!xlRead(fptr,&val))
        xlFmtError("unexpected EOF");
    xlSetCdr(last,val);
    
    /* check for the close paren */
    while ((ch = scan(fptr)) == ';')
        read_comment(fptr);
    if (ch != ')')
        xlFmtError("missing right paren");
}

/* read_comment - read a comment (to end of line) */
static void read_comment(xlValue fptr)
{
    int ch;
    while ((ch = xlGetC(fptr)) != EOF && ch != '\n')
        ;
    if (ch != EOF) xlUngetC(fptr,ch);
}

/* read_vector - read a vector */
static xlValue read_vector(xlValue fptr)
{
    int len=0,ch,i;
    xlValue last,val;
    
    xlCPush(xlNil); last = xlNil;
    while ((ch = scan(fptr)) != ')') {
        if (ch == EOF)
            xlFmtError("unexpected EOF");
        xlUngetC(fptr,ch);
        switch (readone(fptr,&val)) {
        case RO_EOF:
            xlFmtError("unexpected EOF");
        case RO_EXPR:
            val = xlCons(val,xlNil);
            if (last) xlSetCdr(last,val);
            else xlSetTop(val);
            last = val;
            ++len;
            break;
        }
    }
    val = xlNewVector(len);
    for (last = xlPop(), i = 0; i < len; ++i, last = xlCdr(last))
        xlSetElement(val,i,xlCar(last));
    return val;
}

/* read_comma - read a unquote or unquote-splicing expression */
static xlValue read_comma(xlValue fptr)
{
    int ch;
    if ((ch = xlGetC(fptr)) == '@')
        return read_quote(fptr,s_unquotesplicing);
    else {
        xlUngetC(fptr,ch);
        return read_quote(fptr,s_unquote);
    }
}

/* read_quote - parse the tail of a quoted expression */
static xlValue read_quote(xlValue fptr,xlValue sym)
{
    xlValue val;
    if (!xlRead(fptr,&val))
        xlFmtError("unexpected EOF");
    xlCPush(xlCons(val,xlNil));
    xlSetTop(xlCons(sym,xlTop()));
    return xlPop();
}

/* read_symbol - parse a symbol (or a number) */
static xlValue read_symbol(xlValue fptr)
{
    extern xlValue xlKeywordPackage,k_external;
    char buf[xlSTRMAX+1],*sname;
    xlValue package,val,key;
    
    /* get the symbol name */
    if (!getsymbol(fptr,buf))
        xlFmtError("expecting a symbol or number");
    
    /* check to see if it's a number */
    if (xlNumberStringP(buf,&val))
        return val;
    
    /* handle an implicit package reference */
    if ((sname = strchr(buf,':')) == '\0')
        return xlInternCString(buf,xlGetValue(s_package),&key);
        
    /* handle an explicit package reference */
    else {
        
        /* handle keywords */
        if (sname == buf) {
            if (strchr(++sname,':'))
                xlFmtError("invalid symbol ~A",xlMakeCString(sname));
            return xlInternCString(sname,xlKeywordPackage,&key);
        }
        
        /* terminate the package name */
        *sname++ = '\0';
        
        /* find the package */
        if ((package = xlFindPackage(buf)) == xlNil)
            xlFmtError("no package ~A",xlMakeCString(buf));
            
        /* handle an internal symbol reference */
        if (*sname == ':') {
            if (strchr(++sname,':'))
                xlFmtError("invalid symbol ~A",xlMakeCString(sname));
            return xlFindSymbol(sname,package,&key);
        }
        
        /* handle an external symbol reference */
        else {
            if (strchr(sname,':'))
                xlFmtError("invalid symbol ~A",xlMakeCString(sname));
            if ((val = xlFindSymbol(sname,package,&key)) == xlNil || key != k_external)
                xlFmtError("no external symbol ~A in ~S",xlMakeCString(sname),package);
            return val;
        }
    }
}

/* read_string - parse a string */
static xlValue read_string(xlValue fptr)
{
    char buf[xlSTRMAX],*p;
    int ch,d2,d3;
    xlFIXTYPE len;

    /* collect in the buffer to start */
    xlCPush(xlNil);
    p = buf;
    len = 0;
    
    /* loop looking for a closing quote */
    while ((ch = checkeof(fptr)) != '"') {

        /* handle escaped characters */
        switch (ch) {
        case '\\':
                switch (ch = checkeof(fptr)) {
                case 't':
                        ch = '\011';
                        break;
                case 'n':
                        ch = '\012';
                        break;
                case 'f':
                        ch = '\014';
                        break;
                case 'r':
                        ch = '\015';
                        break;
                default:
                        if (ch >= '0' && ch <= '7') {
                            d2 = checkeof(fptr);
                            d3 = checkeof(fptr);
                            if (d2 < '0' || d2 > '7'
                             || d3 < '0' || d3 > '7')
                                xlFmtError("invalid octal digit");
                            ch -= '0'; d2 -= '0'; d3 -= '0';
                            ch = (ch << 6) | (d2 << 3) | d3;
                        }
                        break;
                }
        }

        /* store the character */
        if (++len > xlSTRMAX) {
            if (xlTop() == xlNil) {
                xlFIXTYPE cnt = xlSTRMAX;
                xlSetTop(xlNewUStream());
                for (p = buf; --cnt >= 0; )
                    xlPutC(xlTop(),*p++);    
            }
            xlPutC(xlTop(),ch);
        }
        else
            *p++ = ch;
    }

    /* return the new string */
    return xlTop() == xlNil ? xlPop(), xlMakeString(buf,len) : xlGetStrOutput(xlPop());
}

/* read_special - parse an atom starting with '#' */
static int read_special(xlValue fptr,int ch,xlValue *pval)
{
    char buf[xlSTRMAX+1],buf2[xlSTRMAX+3];
    int lastch;
    xlValue key;
    switch (ch) {
    case '!':
        if (getsymbol(fptr,buf)) {
            if (strcmp(buf,"TRUE") == 0) {
                *pval = xlTrue;
                return RO_EXPR;
            }
            else if (strcmp(buf,"FALSE") == 0) {
                *pval = xlFalse;
                return RO_EXPR;
            }
            else if (strcmp(buf,"NULL") == 0) {
                *pval = xlNil;
                return RO_EXPR;
            }
            else {
                sprintf(buf2,"#!%s",buf);
                *pval = xlInternCString(buf2,xlGetValue(s_package),&key);
                return RO_EXPR;
            }
        }
        else
            xlFmtError("expecting symbol after '#!'");
        break;
    case '\\':
        ch = checkeof(fptr);    /* get the next character */
        xlUngetC(fptr,ch);      /* but allow getsymbol to get it also */
        if (getsymbol(fptr,buf)) {
            if (strcmp(buf,"NEWLINE") == 0)
                ch = '\n';
            else if (strcmp(buf,"ENTER") == 0)
                ch = '\r';
            else if (strcmp(buf,"SPACE") == 0)
                ch = ' ';
            else if (strlen(buf) > 1)
                xlError("unexpected symbol after '#\\'",xlMakeCString(buf));
        }
        else                    /* wasn't a symbol, get the character */
            ch = checkeof(fptr);
        *pval = xlMakeChar(ch);
        return RO_EXPR;
    case '(':
        *pval = read_vector(fptr);
        return RO_EXPR;
    case ':':
        if (!getsymbol(fptr,buf))
            xlFmtError("expecting a symbol after #:");
        *pval = xlMakeSymbol(xlMakeCString(buf));
        return RO_EXPR;
    case '\'':
        *pval = read_quote(fptr,s_function);
        return RO_EXPR;
    case 'b':
    case 'B':
        *pval = read_radix(fptr,2);
        return RO_EXPR;
    case 'o':
    case 'O':
        *pval = read_radix(fptr,8);
        return RO_EXPR;
    case 'd':
    case 'D':
        *pval = read_radix(fptr,10);
        return RO_EXPR;
    case 'x':
    case 'X':
        *pval = read_radix(fptr,16);
        return RO_EXPR;
    case '|':
        for (lastch = '\0'; (ch = xlGetC(fptr)) != EOF; lastch = ch)
            if (lastch == '|' && ch == '#')
                break;
        if (ch == EOF)
            xlFmtError("End of file within comment");
        return RO_COMMENT;
    default:
        xlUngetC(fptr,ch);
        if (getsymbol(fptr,buf)) {
            if (strcmp(buf,"T") == 0) {
                *pval = xlTrue;
                return RO_EXPR;
            }
            else if (strcmp(buf,"F") == 0) {
                *pval = xlFalse;
                return RO_EXPR;
            }
            else
                xlError("unexpected symbol after '#'",xlMakeCString(buf));
        }
        else
            xlError("unexpected character after '#'",xlMakeChar(xlGetC(fptr)));
        break;
    }
    return RO_COMMENT; /* never reached */
}

/* read_radix - read a number in a specified radix */
static xlValue read_radix(xlValue fptr,int radix)
{
    char buf[xlSTRMAX+1];
    xlValue val;
    int ch,i;

    /* get number */
    for (i = 0; (ch = xlGetC(fptr)) != EOF && isconstituent(ch); ) {
        if (islower(ch)) ch = toupper(ch);
        if (!isradixdigit(ch,radix))
            xlError("invalid digit",xlMakeChar(ch));
        if (i < xlSTRMAX)
            buf[i++] = ch;
    }
    buf[i] = '\0';

    /* save the break character */
    xlUngetC(fptr,ch);

    /* convert the string to a number */
    xlRadixNumberStringP(buf,radix,&val);
    return val;
}

/* xlRadixNumberStringP - convert a string to a number in the specified radix */
int xlRadixNumberStringP(char *str,int radix,xlValue *pval)
{
    xlFIXTYPE val = 0;
    int ch;
    
    /* get number */
    while ((ch = *str++) != '\0' && isconstituent(ch)) {
        if (islower(ch)) ch = toupper(ch);
        if (!isradixdigit(ch,radix))
            return FALSE;
        val = val * radix + getdigit(ch);
    }

    /* return the number */
    *pval = xlMakeFixnum(val);
    return TRUE;
}

/* isradixdigit - check to see if a character is a digit in a radix */
static int isradixdigit(int ch,int radix)
{
    switch (radix) {
    case 2:     return ch >= '0' && ch <= '1';
    case 8:     return ch >= '0' && ch <= '7';
    case 10:    return ch >= '0' && ch <= '9';
    case 16:    return (ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'F');
    }
    return FALSE; /* never reached */
}

/* getdigit - convert an ascii code to a digit */
static int getdigit(int ch)
{
    return ch <= '9' ? ch - '0' : ch - 'A' + 10;
}

/* getsymbol - get a symbol name */
static int getsymbol(xlValue fptr,char *buf)
{
    xlValue type;
    int ch,i;

    /* get symbol name */
    for (i = 0; (ch = xlGetC(fptr)) != EOF && ((type = xlCharType(ch)) == xlSymConst || type == xlSymNMacro); )
        if (i < xlSTRMAX)
            buf[i++] = (islower(ch) ? toupper(ch) : ch);
    buf[i] = '\0';

    /* save the break character */
    xlUngetC(fptr,ch);
    return buf[0] != '\0';
}

/* xlNumberStringP - check if this string is a number */
int xlNumberStringP(char *str,xlValue *pval)
{
    int dl,dot,dr;
    char *p;

    /* initialize */
    p = str; dl = dot = dr = 0;

    /* check for a sign */
    if (*p == '+' || *p == '-')
        p++;

    /* check for a string of digits */
    while (isdigit(*p))
        p++, dl++;

    /* check for a decimal point */
    if (*p == '.') {
        p++; dot = 1;
        while (isdigit(*p))
            p++, dr++;
    }

    /* check for an exponent */
    if ((dl || dr) && *p == 'E') {
        p++; dot = 1;

        /* check for a sign */
        if (*p == '+' || *p == '-')
            p++;

        /* check for a string of digits */
        while (isdigit(*p))
            p++, dr++;
    }

    /* make sure there was at least one digit and this is the end */
    if ((dl == 0 && dr == 0) || *p)
        return FALSE;

    /* convert the string to an integer and return successfully */
    if (pval) {
        if (*str == '+') ++str;
        if (str[strlen(str)-1] == '.') str[strlen(str)-1] = 0;
        *pval = (dot ? xlMakeFlonum(atof(str)) : xlMakeFixnum(xlICNV(str)));
    }
    return TRUE;
}

/* scan - scan for the first non-blank character */
static int scan(xlValue fptr)
{
    int ch;

    /* look for a non-blank character */
    while ((ch = xlGetC(fptr)) != EOF && tentry(ch) == xlSymWSpace)
        ;

    /* return the character */
    return ch;
}

/* checkeof - get a character and check for end of file */
static int checkeof(xlValue fptr)
{
    int ch;
    if ((ch = xlGetC(fptr)) == EOF)
        xlFmtError("unexpected EOF");
    return ch;
}

/* isconstituent - is this a symbol constituent? */
static int isconstituent(int ch)
{
    return xlCharType(ch) == xlSymConst;
}

/* xlCharType - get readtable character type for a character */
xlEXPORT xlValue xlCharType(int ch)
{
    xlValue entry = tentry(ch);
    return xlConsP(entry) ? xlCar(entry) : entry;
}

/* tentry - get readtable entry for a character */
static xlValue tentry(int ch)
{
    extern xlValue xlSymReadTable;
    xlValue rtable = xlGetValue(xlSymReadTable);
    if (xlVectorP(rtable) && ch >= 0 && ch < xlGetSize(rtable))
        return xlGetElement(rtable,ch);
    return xlNil;
}

/* defmacro - define a read macro */
static void defmacro(int ch,xlValue type,char *name)
{
    xlSetElement(xlGetValue(xlSymReadTable),ch,xlCons(type,xlGetValue(xlEnter(name))));
}

/* defdmacro - define a dispatching read macro */
static void defdmacro(xlValue dtable,int ch,char *name)
{
    xlSetElement(dtable,ch,xlGetValue(xlEnter(name)));
}

/* xlInitReader - initialize the reader */
void xlInitReader(void)
{
    xlValue rtable,dtable;
    char *p;
    int ch;
    
    /* create the read table */
    rtable = xlNewVector(256);
    xlSetValue(xlSymReadTable,rtable);
    
    /* initialize the readtable */
    for (p = WSPACE; (ch = *p++) != '\0'; )
        xlSetElement(rtable,ch,xlSymWSpace);
    for (p = CONST1; (ch = *p++) != '\0'; )
        xlSetElement(rtable,ch,xlSymConst);
    for (p = CONST2; (ch = *p++) != '\0'; )
        xlSetElement(rtable,ch,xlSymConst);
        
    /* install the built-in read macros */
    defmacro('\'',xlSymTMacro,"%RM-QUOTE");
    defmacro('"', xlSymTMacro,"%RM-DOUBLE-QUOTE");
    defmacro('`' ,xlSymTMacro,"%RM-BACKQUOTE");
    defmacro(',', xlSymTMacro,"%RM-COMMA");
    defmacro('(', xlSymTMacro,"%RM-LEFT-PAREN");
    defmacro(')', xlSymTMacro,"%RM-RIGHT-PAREN");
    defmacro(';', xlSymTMacro,"%RM-SEMICOLON");
    
    /* setup the # dispatch table */
    dtable = xlNewVector(256);
    xlSetElement(rtable,'#',xlCons(xlSymNMacro,dtable));
    
    /* install the dispatch macros */
    defdmacro(dtable,'!', "%RM-HASH");
    defdmacro(dtable,'\\',"%RM-HASH");
    defdmacro(dtable,'(', "%RM-HASH");
    defdmacro(dtable,':', "%RM-HASH");
    defdmacro(dtable,'\'',"%RM-HASH");
    defdmacro(dtable,'t', "%RM-HASH");
    defdmacro(dtable,'T', "%RM-HASH");
    defdmacro(dtable,'f', "%RM-HASH");
    defdmacro(dtable,'F', "%RM-HASH");
    defdmacro(dtable,'b', "%RM-HASH");
    defdmacro(dtable,'B', "%RM-HASH");
    defdmacro(dtable,'o', "%RM-HASH");
    defdmacro(dtable,'O', "%RM-HASH");
    defdmacro(dtable,'d', "%RM-HASH");
    defdmacro(dtable,'D', "%RM-HASH");
    defdmacro(dtable,'x', "%RM-HASH");
    defdmacro(dtable,'X', "%RM-HASH");
    defdmacro(dtable,'|', "%RM-HASH");
}
