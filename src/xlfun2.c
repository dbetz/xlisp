/* xlfun2.c - xlisp built-in functions - part 2 */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* local definitions */
#define fix(n)  xlMakeFixnum((xlFIXTYPE)(n))
#define TLEFT   1
#define TRIGHT  2

/* external variables */
extern xlValue xlEofObject,s_hexfmt;
extern xlValue s_stdin,s_stdout,s_stderr,s_error;
extern xlValue k_start,k_end,k_1start,k_1end,k_2start,k_2end,k_fromend;
extern int xlPRBreadth,xlPRDepth;

/* forward declarations */
static xlValue setit(int *pvar);
static xlValue openfile(short flags,char *mode);
static void format(xlValue stream);
static void formatA(xlValue arg,xlValue stream);
static void formatX(xlValue arg,xlValue stream);
static void getbounds(xlValue str,xlValue skey,xlValue ekey,xlFIXTYPE *pstart,xlFIXTYPE *pend);
static char *radixnumtostr(xlFIXTYPE n,int radix,char *buf,int len);
static int inbag(int ch,xlValue bag);
static xlValue strcompare(int fcn,int icase);
static xlValue strsearch(int icase);
static xlValue chrcompare(int fcn,int icase);
static xlValue changecase(int fcn,int destructive);
static xlValue trim(int fcn);
static xlValue room(void);

/* xsymstr - built-in function 'symbol->string' */
xlValue xsymstr(void)
{
    xlVal = xlGetArgSymbol();
    xlLastArg();
    return xlGetPName(xlVal);
}

/* xstrsym - built-in function 'string->symbol' */
xlValue xstrsym(void)
{
    extern xlValue s_package;
    xlValue key;
    xlVal = xlGetArgString();
    xlLastArg();
    return xlIntern(xlVal,xlGetValue(s_package),&key);
}

/* xreadline - read a line from a file */
xlValue xreadline(void)
{
    xlValue fptr;
    int ch;

    /* get file pointer */
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();

    /* get character and check for eof */
    if ((ch = xlGetC(fptr)) == EOF)
        xlVal = xlNil;
    else {
        char buf[xlSTRMAX],*p = buf;
        xlFIXTYPE len = 0;
        xlVal = xlNil;
        xlCPush(fptr);
        while (ch != EOF && ch != '\n') {
            if (++len > xlSTRMAX) {
                if (xlVal == xlNil) {
                    xlFIXTYPE cnt = xlSTRMAX;
                    xlVal = xlNewUStream();
                    for (p = buf; --cnt >= 0; )
                        xlPutC(xlVal,*p++);    
                }
                xlPutC(xlVal,ch);
            }
            else
                *p++ = ch;
            ch = xlGetC(fptr);
        }
        xlVal = xlVal == xlNil ? xlMakeString(buf,len) : xlGetStrOutput(xlVal);
        xlDrop(1);
    }

    /* return the string */
    return xlVal;
}

/* xrdchar - built-in function 'read-char' */
xlValue xrdchar(void)
{
    xlValue fptr;
    int ch;
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    return (ch = xlGetC(fptr)) == EOF ? xlEofObject : xlMakeChar(ch);
}

/* xunreadchar - built-in function 'unread-char' */
xlValue xunreadchar(void)
{
    xlValue fptr,ch;
    ch = xlGetArgChar();
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();
    xlUngetC(fptr,(int)xlGetChCode(ch));
    return xlTrue;
}

/* xpkchar - peek at a character from a file */
xlValue xpkchar(void)
{
    xlValue fptr;
    int ch;

    /* get file pointer */
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();

    /* return the character */
    return (ch = xlPeek(fptr)) == EOF ? xlEofObject : xlMakeChar(ch);
}

/* xcharready - built-in function 'char-read?' */
xlValue xcharready(void)
{
    xlValue fptr;
    
    /* parse the argument list */
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    
    /* return true if there is a character to read */
    return xlInputReadyP(fptr) ? xlTrue : xlFalse;
}

/* xclearinput - built-in function 'clear-input' */
xlValue xclearinput(void)
{
    xlValue fptr;
    
    /* parse the argument list */
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    
    /* flush buffered input */
    xlFlushInput(fptr);
    return xlNil;
}

/* xrdbyte - built-in function 'read-byte' */
xlValue xrdbyte(void)
{
    xlValue fptr;
    int ch;
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    return (ch = xlGetC(fptr)) == EOF ? xlEofObject : xlMakeFixnum((xlFIXTYPE)ch);
}

/* xrdshort - built-in function 'read-short' */
xlValue xrdshort(void)
{
    unsigned char *p;
    short int val=0;
    xlValue fptr;
    int ch,n;
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    for (n = sizeof(short int), p = (unsigned char *)&val; --n >= 0; ) {
        if ((ch = xlGetC(fptr)) == EOF)
            return xlEofObject;
        *p++ = ch;
    }
    return xlMakeFixnum((xlFIXTYPE)val);
}

/* xrdshorthf - built-in function 'read-short-high-first' */
xlValue xrdshorthf(void)
{
    short int val;
    xlValue fptr;
    int ch;
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val = (ch & 0xff) << 8;
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val |= ch & 0xff;
    return xlMakeFixnum((xlFIXTYPE)val);
}

/* xrdshortlf - built-in function 'read-short-low-first' */
xlValue xrdshortlf(void)
{
    short int val;
    xlValue fptr;
    int ch;
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val = ch & 0xff;
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val |= (ch & 0xff) << 8;
    return xlMakeFixnum((xlFIXTYPE)val);
}

/* xrdlong - built-in function 'read-long' */
xlValue xrdlong(void)
{
    unsigned char *p;
    long int val=0;
    xlValue fptr;
    int ch,n;
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    for (n = sizeof(long int), p = (unsigned char *)&val; --n >= 0; ) {
        if ((ch = xlGetC(fptr)) == EOF)
            return xlEofObject;
        *p++ = ch;
    }
    return xlMakeFixnum((xlFIXTYPE)val);
}

/* xrdlonghf - built-in function 'read-long-high-first' */
xlValue xrdlonghf(void)
{
    long int val;
    xlValue fptr;
    int ch;
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val = (ch & 0xff) << 24;
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val |= (ch & 0xff) << 16;
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val |= (ch & 0xff) << 8;
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val |= ch & 0xff;
    return xlMakeFixnum((xlFIXTYPE)val);
}

/* xrdlonglf - built-in function 'read-long-low-first' */
xlValue xrdlonglf(void)
{
    long int val;
    xlValue fptr;
    int ch;
    fptr = xlMoreArgsP() ? xlGetInputPort() : xlCurInput();
    xlLastArg();
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val = ch & 0xff;
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val |= (ch & 0xff) << 8;
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val |= (ch & 0xff) << 16;
    if ((ch = xlGetC(fptr)) == EOF)
        return xlEofObject;
    val |= (ch & 0xff) << 24;
    return xlMakeFixnum((xlFIXTYPE)val);
}

/* xeofobjectp - built-in function 'eof-object?' */
xlValue xeofobjectp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return arg == xlEofObject ? xlTrue : xlFalse;
}

/* xwrchar - built-in function 'write-char' */
xlValue xwrchar(void)
{
    xlValue fptr,ch;
    ch = xlGetArgChar();
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();
    xlPutC(fptr,(int)xlGetChCode(ch));
    return xlTrue;
}

/* xwrbyte - built-in function 'write-byte' */
xlValue xwrbyte(void)
{
    xlValue fptr,ch;
    ch = xlGetArgFixnum();
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();
    xlPutC(fptr,(int)xlGetFixnum(ch));
    return xlTrue;
}

/* xwrshort - built-in function 'write-short' */
xlValue xwrshort(void)
{
    unsigned char *p;
    short int val;
    xlValue fptr,v;
    int n;
    v = xlGetArgFixnum(); val = (short int)xlGetFixnum(v);
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();
    for (n = sizeof(short int), p = (unsigned char *)&val; --n >= 0; )
        xlPutC(fptr,*p++);
    return xlTrue;
}

/* xwrshorthf - built-in function 'write-short-high-first' */
xlValue xwrshorthf(void)
{
    short int val;
    xlValue fptr,v;
    v = xlGetArgFixnum(); val = (short int)xlGetFixnum(v);
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();
    xlPutC(fptr,(val >> 8) & 0xff);
    xlPutC(fptr,val & 0xff);
    return xlTrue;
}

/* xwrshortlf - built-in function 'write-short-low-first' */
xlValue xwrshortlf(void)
{
    short int val;
    xlValue fptr,v;
    v = xlGetArgFixnum(); val = (short int)xlGetFixnum(v);
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();
    xlPutC(fptr,val & 0xff);
    xlPutC(fptr,(val >> 8) & 0xff);
    return xlTrue;
}

/* xwrlong - built-in function 'write-long' */
xlValue xwrlong(void)
{
    unsigned char *p;
    long int val;
    xlValue fptr,v;
    int n;
    v = xlGetArgFixnum(); val = (long int)xlGetFixnum(v);
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();
    for (n = sizeof(long int), p = (unsigned char *)&val; --n >= 0; )
        xlPutC(fptr,*p++);
    return xlTrue;
}

/* xwrlonghf - built-in function 'write-long-high-first' */
xlValue xwrlonghf(void)
{
    short int val;
    xlValue fptr,v;
    v = xlGetArgFixnum(); val = (short int)xlGetFixnum(v);
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();
    xlPutC(fptr,(val >> 24) & 0xff);
    xlPutC(fptr,(val >> 16) & 0xff);
    xlPutC(fptr,(val >> 8) & 0xff);
    xlPutC(fptr,val & 0xff);
    return xlTrue;
}

/* xwrlonglf - built-in function 'write-long-low-first' */
xlValue xwrlonglf(void)
{
    short int val;
    xlValue fptr,v;
    v = xlGetArgFixnum(); val = (short int)xlGetFixnum(v);
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();
    xlPutC(fptr,val & 0xff);
    xlPutC(fptr,(val >> 8) & 0xff);
    xlPutC(fptr,(val >> 16) & 0xff);
    xlPutC(fptr,(val >> 24) & 0xff);
    return xlTrue;
}

/* xwrite - built-in function 'write' */
xlValue xwrite(void)
{
    xlValue fptr,val;

    /* get expression to print and file pointer */
    val = xlGetArg();
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();

    /* print the value */
    xlWrite(val,fptr);
    return xlTrue;
}

/* xdisplay - built-in function 'display' */
xlValue xdisplay(void)
{
    xlValue fptr,val;

    /* get expression to print and file pointer */
    val = xlGetArg();
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();

    /* print the value */
    xlDisplay(val,fptr);
    return xlTrue;
}

/* xprint - built-in function 'print' */
xlValue xprint(void)
{
    xlValue fptr,val;

    /* get expression to print and file pointer */
    val = xlGetArg();
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();

    /* print the value one a new line */
    xlNewline(fptr);
    xlWrite(val,fptr);
    xlPutC(fptr,' ');
    return xlTrue;
}

/* xnewline - terminate the current print line */
xlValue xnewline(void)
{
    xlValue fptr;

    /* get file pointer */
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();

    /* terminate the print line and return nil */
    xlNewline(fptr);
    return xlTrue;
}

/* xfreshline - start a fresh print line */
xlValue xfreshline(void)
{
    xlValue fptr;

    /* get file pointer */
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();

    /* terminate the print line and return nil */
    xlFreshLine(fptr);
    return xlTrue;
}

/* xflushoutput - flush the output buffer */
xlValue xflushoutput(void)
{
    xlLastArg();
    xlosFlushOutput();
    return xlTrue;
}

/* xwritesize - built-in function 'write-size' */
xlValue xwritesize(void)
{
    xlValue val;

    /* get expression to compute the size of */
    val = xlGetArg();
    xlLastArg();

    /* compute the size */
    return xlMakeFixnum(xlWriteSize(val));
}

/* xdisplaysize - built-in function 'display-size' */
xlValue xdisplaysize(void)
{
    xlValue val;

    /* get expression to compute the size of */
    val = xlGetArg();
    xlLastArg();

    /* compute the size */
    return xlMakeFixnum(xlDisplaySize(val));
}

/* xprbreadth - set the maximum number of elements to be printed */
xlValue xprbreadth(void)
{
    return setit(&xlPRBreadth);
}

/* xprdepth - set the maximum depth of nested lists to be printed */
xlValue xprdepth(void)
{
    return setit(&xlPRDepth);
}

/* setit - common routine for prbreadth/prdepth */
static xlValue setit(int *pvar)
{
    xlValue arg;

    /* get the optional argument */
    if (xlMoreArgsP()) {
        arg = xlGetArg();
        xlLastArg();
        *pvar = (xlFixnumP(arg) ? (int)xlGetFixnum(arg) : -1);
    }

    /* return the value of the variable */
    return *pvar >= 0 ? xlMakeFixnum((xlFIXTYPE)*pvar) : xlNil;
}

/* xparsepathstring - built-in function 'parse-path-string' */
xlValue xparsepathstring(void)
{
    xlValue this,last;
    char *p,*entry;

    /* get the load path */
    xlVal = xlGetArgString();
    xlLastArg();
    
    /* initialize */
    p = xlGetString(xlVal);
    xlCPush(xlNil);

    /* append each directory to the path */
    while ((entry = xlosParsePath(&p)) != NULL) {
        this = xlCons(xlMakeCString(entry),xlNil);
        if (xlTop() == xlNil)
            xlSetTop(this);
        else
            xlSetCdr(last,this);
        last = this;
    }

    /* return the path list */
    return xlPop();
}

/* xsplitpathfromfilename - built-in function 'split-path-from-filename' */
void xsplitpathfromfilename(void)
{
    int sch = xlosDirectorySeparator();
    char *pathstr,*p;
    xlValue file;

    /* parse the argument list */
    xlVal = xlGetArgString();
    xlLastArg();

    /* initialize */
    pathstr = xlGetString(xlVal);
    xlCheck(2);
    
    /* get the path string */
    if ((p = strrchr(pathstr,sch)) != NULL) {
        int len = p - pathstr;
        xlPush(xlVal);
        xlVal = xlNewString(len);
        memcpy(xlGetString(xlVal),pathstr,len);
        file = xlMakeCString(++p);
        xlDrop(1);
    }

    /* no path component */
    else {
        file = xlVal;
        xlVal = xlFalse;
    }

    /* return the path and filename */
    xlPush(file);
    xlPush(xlVal);
    xlMVReturn(2);
}

/* xcombinepathwithfilename - built-in function 'combine-path-with-filename' */
xlValue xcombinepathwithfilename(void)
{
    int sch = xlosDirectorySeparator();
    char *pathstr,*filestr,*p;
    xlValue path,file;
    xlFIXTYPE len;
    int addSepP;

    /* parse the argument list */
    path = xlGetArgString();
    file = xlGetArgString();
    xlLastArg();

    /* protect the strings */
    xlCheck(2);
    xlPush(path);
    xlPush(file);

    /* get the path and filename strings */
    pathstr = xlGetString(path);
    filestr = xlGetString(file);

    /* compute the length of the new string */
    len = strlen(pathstr) + strlen(filestr);

    /* make sure the path ends with a directory separator */
    if (pathstr[strlen(pathstr) - 1] == sch)
        addSepP = FALSE;
    else {
        addSepP = TRUE;
        ++len;
    }

    /* make the output string */
    xlVal = xlNewString(len);
    p = xlGetString(xlVal);

    /* combine the path and filename strings */
    strcpy(p,pathstr); p += strlen(pathstr);
    if (addSepP)
        *p++ = sch;
    strcpy(p,filestr);

    /* restore the stack and return the new string */
    xlDrop(2);
    return xlVal;
}

/* xfilemodtime - built-in function 'file-modification-time' */
xlValue xfilemodtime(void)
{
    xlFIXTYPE mtime;

    /* parse the arguments */
    xlVal = xlGetArgString();
    xlLastArg();

    /* get the file modification time */
    return xlosFileModTime(xlGetString(xlVal),&mtime) ? xlMakeFixnum(mtime) : xlNil;
}

/* xopeni - built-in function 'open-input-file' */
xlValue xopeni(void)
{
    return openfile(xlpfINPUT,"r");
}

/* xopeno - built-in function 'open-output-file' */
xlValue xopeno(void)
{
    return openfile(xlpfOUTPUT | xlpfBOL,"w");
}

/* xopena - built-in function 'open-append-file' */
xlValue xopena(void)
{
    return openfile(xlpfOUTPUT | xlpfBOL,"a");
}

/* xopenu - built-in function 'open-update-file' */
xlValue xopenu(void)
{
    return openfile(xlpfINPUT | xlpfOUTPUT | xlpfBOL,"r+");
}

/* openfile - open an ascii or binary file */
static xlValue openfile(short flags,char *mode)
{
    xlValue file,modekey;
    char *name;
    FILE *fp;

    /* get the file name and direction */
    name = xlGetString(xlGetArgString());
    modekey = xlMoreArgsP() ? xlGetArgSymbol() : xlNil;
    xlLastArg();

    /* check for binary mode */
    if (modekey != xlNil) {
        if (modekey == xlEnter("BINARY"))
            flags |= xlpfBINARY;
        else if (modekey != xlEnter("TEXT"))
            xlError("unrecognized open mode",modekey);
    }

    /* try to open the file */
    file = xlMakeFileStream(NULL,flags);
    fp = ((flags & xlpfBINARY) == 0 ? xlosOpenText(name,mode) : xlosOpenBinary(name,mode));
    if (fp == NULL)
        return xlNil;
    xlSetSData(file,fp);
    return file;
}

/* xclose - built-in function 'close-port' */
xlValue xclose(void)
{
    xlValue fptr;
    fptr = xlGetArgPort();
    xlLastArg();
    if (xlFileStreamP(fptr) && xlGetFile(fptr) != NULL) {
        xlosClose(xlGetFile(fptr));
        xlSetSData(fptr,NULL);
    }
    return xlNil;
}

/* xclosei - built-in function 'close-input-port' */
xlValue xclosei(void)
{
    xlValue fptr;
    fptr = xlGetArgInputPort();
    xlLastArg();
    if (xlFileStreamP(fptr) && xlGetFile(fptr) != NULL) {
        xlosClose(xlGetFile(fptr));
        xlSetSData(fptr,NULL);
    }
    return xlNil;
}

/* xcloseo - built-in function 'close-output-port' */
xlValue xcloseo(void)
{
    xlValue fptr;
    fptr = xlGetArgOutputPort();
    xlLastArg();
    if (xlFileStreamP(fptr) && xlGetFile(fptr) != NULL) {
        xlosClose(xlGetFile(fptr));
        xlSetSData(fptr,NULL);
    }
    return xlNil;
}

/* xgetfposition - built-in function 'get-file-position' */
xlValue xgetfposition(void)
{
    xlValue fptr;
    fptr = xlGetArgFStream();
    xlLastArg();
    return xlMakeFixnum(xlosTell(xlGetFile(fptr)));
}

/* xsetfposition - built-in function 'set-file-position!' */
xlValue xsetfposition(void)
{
    xlValue fptr,val;
    long position;
    int whence;
    fptr = xlGetArgFStream();
    val = xlGetArgFixnum(); position = xlGetFixnum(val);
    val = xlGetArgFixnum(); whence = (int)xlGetFixnum(val);
    xlLastArg();
    return xlosSeek(xlGetFile(fptr),position,whence) == 0 ? xlTrue : xlFalse;
}

/* xcurinput - built-in function 'current-input-port' */
xlValue xcurinput(void)
{
    xlLastArg();
    return xlCurInput();
}

/* xlCurInput - get the current input port */
xlEXPORT xlValue xlCurInput(void)
{
    return xlGetValue(s_stdin);
}

/* xcuroutput - built-in function 'current-output-port' */
xlValue xcuroutput(void)
{
    xlLastArg();
    return xlCurOutput();
}

/* xlCurOutput - get the current output port */
xlEXPORT xlValue xlCurOutput(void)
{
    return xlGetValue(s_stdout);
}

/* xcurerror - built-in function 'current-error-port' */
xlValue xcurerror(void)
{
    xlLastArg();
    return xlCurError();
}

/* xlCurError - get the current error port */
xlEXPORT xlValue xlCurError(void)
{
    return xlGetValue(s_stderr);
}

/* xportp - built-in function 'port?' */
xlValue xportp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlPortP(arg) ? xlTrue : xlFalse;
}

/* xinputportp - built-in function 'input-port?' */
xlValue xinputportp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlInputPortP(arg) ? xlTrue : xlFalse;
}

/* xoutputportp - built-in function 'output-port?' */
xlValue xoutputportp(void)
{
    xlValue arg;
    arg = xlGetArg();
    xlLastArg();
    return xlOutputPortP(arg) ? xlTrue : xlFalse;
}

/* xmkstrinput - make a string input stream */
xlValue xmkstrinput(void)
{
    xlFIXTYPE start,end,len;
    xlValue val;
    
    /* get the string and length */
    xlVal = xlGetArgString();
    len = xlGetSLength(xlVal);

    /* get the starting offset */
    if (xlMoreArgsP()) {
        val = xlGetArgFixnum();
        start = xlGetFixnum(val);
    }
    else start = 0;

    /* get the ending offset */
    if (xlMoreArgsP()) {
        val = xlGetArgFixnum();
        end = xlGetFixnum(val);
    }
    else end = len;
    xlLastArg();

    /* check the bounds */
    if (start < 0 || start > len)
        xlError("string index out of bounds",xlMakeFixnum((xlFIXTYPE)start));
    if (end < 0 || end > len)
        xlError("string index out of bounds",xlMakeFixnum((xlFIXTYPE)end));

    /* create the stream */
    xlCPush(xlNewUStream());
    
    /* copy the characters into the stream */
    for (; start < end; ++start) {
        char *p = xlGetString(xlVal) + start;
        xlPutC(xlTop(),*p);
    }

    /* return the new stream */
    return xlPop();
}

/* xmkstroutput - make a string output stream */
xlValue xmkstroutput(void)
{
    xlLastArg();
    return xlNewUStream();
}

/* xgetstroutput - get output stream string */
xlValue xgetstroutput(void)
{
    xlValue stream;
    stream = xlGetArgUnnamedStream();
    xlLastArg();
    return xlGetStrOutput(stream);
}

/* xmkobjstream - built-in function 'make-object-stream' */
xlValue xmkobjstream(void)
{
    /* parse the argument list */
    xlVal = xlGetArgObject();
    xlLastArg();

    /* create the object stream */
    return xlMakeObjectStream(xlVal,xlpfINPUT | xlpfOUTPUT | xlpfBOL);
}

/* xformat - formatted output function */
xlValue xformat(void)
{
    xlValue stream,val;
    
    /* get the stream */
    stream = xlGetArg();
    if (stream == xlNil)
        val = stream = xlNewUStream();
    else {
        if (stream == xlTrue)
            stream = xlCurOutput();
        else if (!xlOutputPortP(stream))
            xlBadType(stream);
        val = xlNil;
    }
    
    /* do the formatted output */
    format(stream);

    /* return the value */
    return val == xlNil ? xlNil : xlGetStrOutput(val);
}

/* format - finish 'format' and 'error' handling */
static void format(xlValue stream)
{
    int atseen,argseen,arg,saveArgC,argc,ch,i;
    xlValue fmtstring,*argp,val;
    xlFIXTYPE len;

    /* get the format string */
    fmtstring = xlGetArgString();
    len = xlGetSLength(fmtstring);
    i = 0;

    /* setup the argument pointer and count */
    argp = xlSP;
    saveArgC = argc = xlArgC;
    
    /* protect the stream and format string */
    xlCheck(2);
    xlPush(stream);
    xlPush(fmtstring);

    /* process the format string */
    for (;;) {
        char *fmt = xlGetString(fmtstring);
        if (i >= len)
            break;
        else if ((ch = fmt[i++]) == '~' && i < len) {
            
            /* check for the '@' modifier */
            atseen = fmt[i] == '@';
            if (atseen)
                ++i;
            
            /* check for a numeric argument */
            argseen = isdigit(fmt[i]);
            if (argseen) {
                for (arg = 0; isdigit(fmt[i]); ++i)
                    arg = arg * 10 + fmt[i] - '0';
            }
                
            /* dispatch on the formatting directive */
            switch (ch = fmt[i++]) {
            case 'a': case 'A':
                if (--argc < 0)
                    xlTooFew();
                formatA(*argp++,stream);
                break;
            case 's': case 'S':
                if (--argc < 0)
                    xlTooFew();
                xlWrite(*argp++,stream);
                break;
            case 'x': case 'X':
                if (--argc < 0)
                    xlTooFew();
                val = *argp++;
                if (xlFixnumP(val))
                    formatX(val,stream);
                else
                    formatA(val,stream);
                break;
            case '%':
                xlNewline(stream);
                break;
            case '&':
                xlFreshLine(stream);
                break;
            case '~':
                xlPutC(stream,'~');
                break;
            case '\n':
                while ((ch = fmt[i]) != '\0' && isspace(ch) && ch != '\n')
                    ++i;
                break;
            default:
                xlError("unknown format directive",xlMakeChar(ch));
            }
        }
        else
            xlPutC(stream,ch);
    }
    
    /* get rid of the format arguments */
    xlDrop(saveArgC + 2);
}

/* formatA - print an argument with ~A format */
static void formatA(xlValue arg,xlValue stream)
{
    xlDisplay(arg,stream);
}

/* formatX - print an argument with ~X format */
static void formatX(xlValue arg,xlValue stream)
{
    xlValue fmt = xlGetValue(s_hexfmt);
    xlFIXTYPE n = xlGetFixnum(arg);
    char buf[100],*p;
    sprintf(buf,xlStringP(fmt) ? xlGetString(fmt) : xlXFMT,n);
    for (p = buf; *p != 0; )
        xlPutC(stream,*p++);
}

/* xtranson - built-in function 'transcript-on' */
xlValue xtranson(void)
{
    char *name;

    /* get the file name and direction */
    name = xlGetString(xlGetArgString());
    xlLastArg();

    /* close any currently open transcript file */
    if (xlTranscriptFP) { xlosClose(xlTranscriptFP); xlTranscriptFP = NULL; }

    /* try to open the file */
    return (xlTranscriptFP = xlosOpenText(name,"w")) == NULL ? xlFalse : xlTrue;
}

/* xtransoff - built-in function 'transcript-off' */
xlValue xtransoff(void)
{
    /* make sure there aren't any arguments */
    xlLastArg();

    /* make sure the transcript is open */
    if (xlTranscriptFP == NULL)
        return xlFalse;

    /* close the transcript and return successfully */
    xlosClose(xlTranscriptFP); xlTranscriptFP = NULL;
    return xlTrue;
}

/* xmakestring - built-in function 'make-string' */
xlValue xmakestring(void)
{
    xlFIXTYPE size;
    int fill;
    char *p;
    
    /* parse the argument list */
    xlVal = xlGetArgFixnum(); size = xlGetFixnum(xlVal);
    fill = xlMoreArgsP() ? xlGetChCode(xlGetArgChar()) : -1;
    xlLastArg();
    
    /* make the string */
    xlVal = xlNewString(size);
    
    /* fill it if necessary */
    if (fill != -1)
        for (p = xlGetString(xlVal); --size >= 0; )
            *p++ = fill;
    
    /* return the new string */
    return xlVal;
}

/* xstrlen - built-in function 'string-length' */
xlValue xstrlen(void)
{
    xlValue str;
    str = xlGetArgString();
    xlLastArg();
    return xlMakeFixnum((xlFIXTYPE)(xlGetSLength(str)));
}

/* xstrnullp - built-in function 'string-null?' */
xlValue xstrnullp(void)
{
    xlValue str;
    str = xlGetArgString();
    xlLastArg();
    return xlGetSLength(str) == 0 ? xlTrue : xlFalse;
}

/* xstrappend - built-in function 'string-append' */
xlValue xstrappend(void)
{
    xlValue *savesp,arg;
    int saveargc;
    xlFIXTYPE len;
    char *str;

    /* save the argument list */
    saveargc = xlArgC;
    savesp = xlSP;

    /* find the length of the new string */
    for (len = 0; xlMoreArgsP(); ) {
        arg = xlGetArgString();
        len += xlGetSLength(arg);
    }

    /* restore the argument list */
    xlArgC = saveargc;
    xlSP = savesp;
    
    /* create the result string */
    xlVal = xlNewString(len);
    str = xlGetString(xlVal);

    /* combine the strings */
    while (xlMoreArgsP()) {
        arg = xlNextArg();
        len = xlGetSLength(arg);
        memcpy(str,xlGetString(arg),(size_t)len);
        str += len;
    }

    /* return the new string */
    return xlVal;
}

/* xstrref - built-in function 'string-ref' */
xlValue xstrref(void)
{
    xlValue str,num;
    xlFIXTYPE n;

    /* get the string and the index */
    str = xlGetArgString();
    num = xlGetArgFixnum();
    xlLastArg();

    /* range check the index */
    if ((n = xlGetFixnum(num)) < 0 || n >= xlGetSLength(str))
        xlError("index out of range",num);

    /* return the character */
    return xlMakeChar(((unsigned char *)xlGetString(str))[n]);
}

/* xstrset - built-in function 'string-set!' */
xlValue xstrset(void)
{
    xlValue str,num,ch;
    xlFIXTYPE n;

    /* get the string and the index */
    str = xlGetArgString();
    num = xlGetArgFixnum();
    ch = xlGetArgChar();
    xlLastArg();

    /* range check the index */
    if ((n = xlGetFixnum(num)) < 0 || n >= xlGetSLength(str))
        xlError("index out of range",num);

    /* return the character */
    xlGetString(str)[n] = xlGetChCode(ch);
    return ch;
}

/* xsubstring - built-in function 'substring' */
xlValue xsubstring(void)
{
    xlFIXTYPE start,end,len;
    xlValue src,dst;

    /* get string and starting and ending positions */
    src = xlGetArgString();

    /* get the starting position */
    dst = xlGetArgFixnum(); start = xlGetFixnum(dst);
    if (start < 0 || start > xlGetSLength(src))
        xlError("index out of range",dst);

    /* get the ending position */
    if (xlMoreArgsP()) {
        dst = xlGetArgFixnum(); end = xlGetFixnum(dst);
        if (end < 0 || end > xlGetSLength(src))
            xlError("index out of range",dst);
    }
    else
        end = xlGetSLength(src);
    xlLastArg();

    /* compute the length of the substring */
    len = end - start;

    /* make a destination string and setup the pointer */
    dst = xlNewString(len);

    /* copy the source to the destination */
    memcpy(xlGetString(dst),xlGetString(src) + start,(size_t)len);

    /* return the substring */
    return dst;
}

/* xstrlist - built-in function 'string->list' */
xlValue xstrlist(void)
{
    xlFIXTYPE size;
    xlValue str;

    /* get the vector */
    str = xlGetArgString();
    xlLastArg();
    
    /* make a list from the vector */
    xlCPush(str);
    size = xlGetSLength(str);
    for (xlVal = xlNil; --size >= 0; )
        xlVal = xlCons(xlMakeChar(((unsigned char *)xlGetString(str))[size]),xlVal);
    xlDrop(1);
    return xlVal;
}

/* xliststring - built-in function 'list->string' */
xlValue xliststring(void)
{
    xlFIXTYPE size;
    xlValue str;
    char *p;

    /* get the list */
    xlVal = xlGetArgList();
    xlLastArg();

    /* make a vector from the list */
    size = xlLength(xlVal);
    str = xlNewString(size);
    for (p = xlGetString(str); --size >= 0; xlVal = xlCdr(xlVal))
        if (xlCharacterP(xlCar(xlVal)))
            *p++ = xlGetChCode(xlCar(xlVal));
        else
            xlBadType(xlCar(xlVal));
    return str;
}

/* case conversion functions */
xlValue xupcase(void)   { return changecase('U',FALSE); }
xlValue xdowncase(void) { return changecase('D',FALSE); }

/* destructive case conversion functions */
xlValue xnupcase(void)   { return changecase('U',TRUE); }
xlValue xndowncase(void) { return changecase('D',TRUE); }

/* changecase - change case */
static xlValue changecase(int fcn,int destructive)
{
    xlFIXTYPE start,end,len,i;
    char *srcp,*dstp;
    xlValue src,dst;
    int ch;

    /* get the arguments */
    src = xlGetArgString();
    getbounds(src,k_start,k_end,&start,&end);
    xlPopArgs();

    /* make a destination string */
    len = xlGetSLength(src);
    dst = (destructive ? src : xlNewString(len));

    /* setup the string pointers */
    srcp = xlGetString(src);
    dstp = xlGetString(dst);

    /* copy the source to the destination */
    for (i = 0; i < len; ++i) {
        ch = *srcp++;
        if (i >= start && i < end)
            switch (fcn) {
            case 'U':   if (islower(ch)) ch = toupper(ch); break;
            case 'D':   if (isupper(ch)) ch = tolower(ch); break;
            }
        *dstp++ = ch;
    }

    /* return the new string */
    return dst;
}

/* trim functions */
xlValue xtrim(void)      { return trim(TLEFT|TRIGHT); }
xlValue xlefttrim(void)  { return trim(TLEFT); }
xlValue xrighttrim(void) { return trim(TRIGHT); }

/* trim - trim character from a string */
static xlValue trim(int fcn)
{
    char *leftp,*rightp,*dstp;
    xlValue bag,src,dst;

    /* get the bag and the string */
    bag = xlGetArgString();
    src = xlGetArgString();
    xlLastArg();

    /* setup the string pointers */
    leftp = xlGetString(src);
    rightp = leftp + xlGetSLength(src) - 1;

    /* trim leading characters */
    if (fcn & TLEFT)
        while (leftp <= rightp && inbag(*leftp,bag))
            ++leftp;

    /* trim character from the right */
    if (fcn & TRIGHT)
        while (rightp >= leftp && inbag(*rightp,bag))
            --rightp;

    /* make a destination string and setup the pointer */
    dst = xlNewString(rightp - leftp + 1);
    dstp = xlGetString(dst);

    /* copy the source to the destination */
    while (leftp <= rightp)
        *dstp++ = *leftp++;

    /* return the new string */
    return dst;
}

/* getbounds - get the start and end bounds of a string */
static void getbounds(xlValue str,xlValue skey,xlValue ekey,xlFIXTYPE *pstart,xlFIXTYPE *pend)
{
    xlFIXTYPE len;

    /* get the length of the string */
    len = xlGetSLength(str);

    /* get the starting and ending indicies */
    xlGetKeyFixnum(skey,0,pstart);
    xlGetKeyFixnum(ekey,len,pend);

    /* check the starting and ending indicies */
    if (*pstart < 0 || *pstart > len)
        xlError("string index out of bounds",xlMakeFixnum((xlFIXTYPE)*pstart));
    if (*pend < 0 || *pend > len)
        xlError("string index out of bounds",xlMakeFixnum((xlFIXTYPE)*pend));

    /* make sure the start is less than or equal to the end */
    if (*pstart > *pend)
        xlError("starting index error",xlMakeFixnum((xlFIXTYPE)*pstart));
}

/* inbag - test if a character is in a bag */
static int inbag(int ch,xlValue bag)
{
    char *p;
    for (p = xlGetString(bag); *p != '\0'; ++p)
        if (*p == ch)
            return TRUE;
    return FALSE;
}

/* string comparision functions */
xlValue xstrlss(void) { return strcompare('<',FALSE); } /* string< */
xlValue xstrleq(void) { return strcompare('L',FALSE); } /* string<= */
xlValue xstreql(void) { return strcompare('=',FALSE); } /* string= */
xlValue xstrneq(void) { return strcompare('#',FALSE); } /* string/= */
xlValue xstrgeq(void) { return strcompare('G',FALSE); } /* string>= */
xlValue xstrgtr(void) { return strcompare('>',FALSE); } /* string> */

/* string comparison functions (not case sensitive) */
xlValue xstrilss(void) { return strcompare('<',TRUE); } /* string-lessp */
xlValue xstrileq(void) { return strcompare('L',TRUE); } /* string-not-greaterp */
xlValue xstrieql(void) { return strcompare('=',TRUE); } /* string-equal */
xlValue xstrineq(void) { return strcompare('#',TRUE); } /* string-not-equal */
xlValue xstrigeq(void) { return strcompare('G',TRUE); } /* string-not-lessp */
xlValue xstrigtr(void) { return strcompare('>',TRUE); } /* string-greaterp */

/* strcompare - compare strings */
static xlValue strcompare(int fcn,int icase)
{
    xlFIXTYPE start1,end1,start2,end2;
    xlValue str1,str2;
    char *p1,*p2;
    int ch1,ch2;

    /* get the strings */
    str1 = xlGetArgString();
    str2 = xlGetArgString();

    /* get the substring specifiers */
    getbounds(str1,k_1start,k_1end,&start1,&end1);
    getbounds(str2,k_2start,k_2end,&start2,&end2);
    xlPopArgs();

    /* setup the string pointers */
    p1 = &xlGetString(str1)[start1];
    p2 = &xlGetString(str2)[start2];

    /* compare the strings */
    for (; start1 < end1 && start2 < end2; ++start1,++start2) {
        ch1 = *p1++;
        ch2 = *p2++;
        if (icase) {
            if (isupper(ch1)) ch1 = tolower(ch1);
            if (isupper(ch2)) ch2 = tolower(ch2);
        }
        if (ch1 != ch2)
            switch (fcn) {
            case '<':   return ch1 < ch2 ? xlMakeFixnum(start1) : xlFalse;
            case 'L':   return ch1 <= ch2 ? xlMakeFixnum(start1) : xlFalse;
            case '=':   return xlFalse;
            case '#':   return xlMakeFixnum(start1);
            case 'G':   return ch1 >= ch2 ? xlMakeFixnum(start1) : xlFalse;
            case '>':   return ch1 > ch2 ? xlMakeFixnum(start1) : xlFalse;
            }
    }

    /* check the termination condition */
    switch (fcn) {
    case '<':   return start1 >= end1 && start2 < end2 ? xlMakeFixnum(start1) : xlFalse;
    case 'L':   return start1 >= end1 ? xlMakeFixnum(start1) : xlFalse;
    case '=':   return start1 >= end1 && start2 >= end2 ? xlTrue : xlFalse;
    case '#':   return start1 >= end1 && start2 >= end2 ? xlFalse : xlMakeFixnum(start1);
    case 'G':   return start2 >= end2 ? xlMakeFixnum(start1) : xlFalse;
    case '>':   return start2 >= end2 && start1 < end1 ? xlMakeFixnum(start1) : xlFalse;
    }
    return xlNil; /* never reached */
}

/* xstrsearch - built-in function 'string-search' */
xlValue xstrsearch(void)
{
    return strsearch(FALSE);
}

/* xstrisearch - built-in function 'string-search-ci' */
xlValue xstrisearch(void)
{
    return strsearch(TRUE);
}

/* strsearch - string search */
static xlValue strsearch(int icase)
{
    xlFIXTYPE start1,end1,start2,end2,last2,i;
    xlValue str1,str2,fromendp;
    int ch1,ch2;
    char *p1,*p2;

    /* get the strings */
    str1 = xlGetArgString();
    str2 = xlGetArgString();

    /* get the substring specifiers */
    getbounds(str1,k_1start,k_1end,&start1,&end1);
    getbounds(str2,k_2start,k_2end,&start2,&end2);

    /* get the reverse indicator */
    xlGetKeyArg(k_fromend,xlFalse,&fromendp);
    xlPopArgs();

    /* search from start */
    if (fromendp == xlFalse) {
        for (last2 = end2 - end1 + start1; start2 <= last2; ++start2) {
            p1 = &xlGetString(str1)[start1];
            p2 = &xlGetString(str2)[start2];
            for (i = start1; i < end1; ++i) {
                ch1 = *p1++;
                ch2 = *p2++;
                if (icase) {
                    if (isupper(ch1)) ch1 = tolower(ch1);
                    if (isupper(ch2)) ch2 = tolower(ch2);
                }
                if (ch1 != ch2)
                    break;
            }
            if (i >= end1)
                return xlMakeFixnum(start2);
        }
    }

    /* search from end */
    else {
        for (last2 = end2 - end1 + start1; start2 <= last2; --last2) {
            p1 = &xlGetString(str1)[start1];
            p2 = &xlGetString(str2)[last2];
            for (i = start1; i < end1; ++i) {
                ch1 = *p1++;
                ch2 = *p2++;
                if (icase) {
                    if (isupper(ch1)) ch1 = tolower(ch1);
                    if (isupper(ch2)) ch2 = tolower(ch2);
                }
                if (ch1 != ch2)
                    break;
            }
            if (i >= end1)
                return xlMakeFixnum(last2);
        }
    }

    /* check the termination condition */
    return xlFalse;
}

/* xchupcase - built-in function 'char-upcase' */
xlValue xchupcase(void)
{
    xlValue arg;
    int ch;
    arg = xlGetArgChar(); ch = xlGetChCode(arg);
    xlLastArg();
    return islower(ch) ? xlMakeChar(toupper(ch)) : arg;
}

/* xchdowncase - built-in function 'char-downcase' */
xlValue xchdowncase(void)
{
    xlValue arg;
    int ch;
    arg = xlGetArgChar(); ch = xlGetChCode(arg);
    xlLastArg();
    return isupper(ch) ? xlMakeChar(tolower(ch)) : arg;
}

/* xdigitchar - built-in function 'digit->char' */
xlValue xdigitchar(void)
{
    xlValue arg;
    int n;
    arg = xlGetArgFixnum(); n = (int)xlGetFixnum(arg);
    xlLastArg();
    if (n >= 0 && n <= 9)
        return xlMakeChar(n + '0');
    else if (n >= 10 && n <= 15)
        return xlMakeChar(n + 'a' - 10);
    return xlNil;
}

/* xstring - return a string consisting of a single character */
xlValue xstring(void)
{
    char *buf;
    xlValue arg;

    /* return a zero length string if called with no arguments (for Scheme compatibility) */
    if (!xlMoreArgsP())
        return xlNewString(0);
    
    /* get the first argument */
    arg = xlGetArg();

    /* make sure its not NIL */
    if (xlNullP(arg))
        xlBadType(arg);

    /* check the argument type */
    switch (xlNodeType(arg)) {
    case xlSTRING:
        xlLastArg();
        return arg;
    case xlSYMBOL:
        xlLastArg();
        return xlGetPName(arg);
    case xlCHARACTER:
        xlVal = xlNewString(xlArgC + 1);
        buf = xlGetString(xlVal);
        *buf++ = xlGetChCode(arg);
        while (xlMoreArgsP())
            *buf++ = xlGetChCode(xlGetArgChar());
        return xlVal;
    default:
        xlBadType(arg);
    }
    return xlNil; /* never reached */
}

/* xnumstr - built-in function 'number->string' */
xlValue xnumstr(void)
{
    char buf[256], *str = NULL;
    int radix = 10;
    xlValue n;
    
    /* parse argument list */
    n = xlGetArg();
    if (xlMoreArgsP()) {
        xlVal = xlGetArgFixnum();
        radix = (int)xlGetFixnum(xlVal);
    }
    xlLastArg();
    
    /* convert to a string */
    if (xlFixnumP(n))
        str = radixnumtostr(xlGetFixnum(n),radix,buf,sizeof(buf));
    else if (xlFlonumP(n))
        { sprintf(buf,xlFFMT,xlGetFlonum(n)); str = buf; }
    else
        xlBadType(n);
    return xlMakeCString(str);
}

/* radixnumtostr - convert a number to a string in a given radix */
static char *radixnumtostr(xlFIXTYPE n,int radix,char *buf,int len)
{
    char *p = &buf[len];
    unsigned long un;
    int sign;
    
    /* determine the sign */
    if (n < 0) {
        un = (unsigned long)-n;
        sign = -1;
    }
    else {
        un = (unsigned long)n;
        sign = 1;
    }
    
    /* insert the terminating nul */
    *--p = '\0';
    
    /* convert the number */
    do {
        *--p = "0123456789abcdefghijklmnopqrstuvwxyz"[un % radix];
        un /= radix;
    } while (un > 0);
    
    /* insert the sign */
    if (sign < 0)
        *--p = '-';
        
    /* return the string */
    return p;
}

/* xstrnum - built-in function 'string->number' */
xlValue xstrnum(void)
{
    int radix = 10;
    xlValue str;
    
    /* parse argument list */
    str = xlGetArgString();
    if (xlMoreArgsP()) {
        xlVal = xlGetArgFixnum();
        radix = (int)xlGetFixnum(xlVal);
    }
    xlLastArg();
    
    /* convert to a string */
    if (radix == 10)
        return xlNumberStringP(xlGetString(str),&str) ? str : xlNil;
    else
        return xlRadixNumberStringP(xlGetString(str),radix,&str) ? str : xlNil;
}

/* xchar - extract a character from a string */
xlValue xchar(void)
{
    xlValue str,num;
    xlFIXTYPE n;

    /* get the string and the index */
    str = xlGetArgString();
    num = xlGetArgFixnum();
    xlLastArg();

    /* range check the index */
    if ((n = xlGetFixnum(num)) < 0 || n >= xlGetSLength(str))
        xlError("index out of range",num);

    /* return the character */
    return xlMakeChar(xlGetString(str)[n]);
}

/* xcharint - built-in function 'char->integer' */
xlValue xcharint(void)
{
    xlValue arg;
    arg = xlGetArgChar();
    xlLastArg();
    return xlMakeFixnum((xlFIXTYPE)xlGetChCode(arg));
}

/* xintchar - built-in function 'integer->char' */
xlValue xintchar(void)
{
    xlValue arg;
    arg = xlGetArgFixnum();
    xlLastArg();
    return xlMakeChar((int)xlGetFixnum(arg));
}

/* character comparision functions */
xlValue xchrlss(void) { return chrcompare('<',FALSE); } /* char< */
xlValue xchrleq(void) { return chrcompare('L',FALSE); } /* char<= */
xlValue xchreql(void) { return chrcompare('=',FALSE); } /* char= */
xlValue xchrneq(void) { return chrcompare('#',FALSE); } /* char/= */
xlValue xchrgeq(void) { return chrcompare('G',FALSE); } /* char>= */
xlValue xchrgtr(void) { return chrcompare('>',FALSE); } /* char> */

/* character comparision functions (case insensitive) */
xlValue xchrilss(void) { return chrcompare('<',TRUE); } /* char-lessp */
xlValue xchrileq(void) { return chrcompare('L',TRUE); } /* char-not-greaterp */
xlValue xchrieql(void) { return chrcompare('=',TRUE); } /* char-equalp */
xlValue xchrineq(void) { return chrcompare('#',TRUE); } /* char-not-equalp */
xlValue xchrigeq(void) { return chrcompare('G',TRUE); } /* char-not-lessp */
xlValue xchrigtr(void) { return chrcompare('>',TRUE); } /* char-greaterp */

/* chrcompare - compare characters */
static xlValue chrcompare(int fcn,int icase)
{
    int ch1,ch2,icmp;
    xlValue arg;
    
    /* get the characters */
    arg = xlGetArgChar(); ch1 = xlGetChCode(arg);

    /* convert to lowercase if case insensitive */
    if (icase && isupper(ch1))
        ch1 = tolower(ch1);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && xlMoreArgsP(); ch1 = ch2) {

        /* get the next argument */
        arg = xlGetArgChar(); ch2 = xlGetChCode(arg);

        /* convert to lowercase if case insensitive */
        if (icase && isupper(ch2))
            ch2 = tolower(ch2);

        /* compare the characters */
        switch (fcn) {
        case '<':       icmp = (ch1 < ch2); break;
        case 'L':       icmp = (ch1 <= ch2); break;
        case '=':       icmp = (ch1 == ch2); break;
        case '#':       icmp = (ch1 != ch2); break;
        case 'G':       icmp = (ch1 >= ch2); break;
        case '>':       icmp = (ch1 > ch2); break;
        }
    }

    /* return the result */
    return icmp ? xlTrue : xlFalse;
}

/* xuppercasep - built-in function 'upper-case-p' */
xlValue xuppercasep(void)
{
    int ch;
    ch = xlGetChCode(xlGetArgChar());
    xlLastArg();
    return isupper(ch) ? xlTrue : xlFalse;
}

/* xlowercasep - built-in function 'lower-case-p' */
xlValue xlowercasep(void)
{
    int ch;
    ch = xlGetChCode(xlGetArgChar());
    xlLastArg();
    return islower(ch) ? xlTrue : xlFalse;
}

/* xbothcasep - built-in function 'both-case-p' */
xlValue xbothcasep(void)
{
    int ch;
    ch = xlGetChCode(xlGetArgChar());
    xlLastArg();
    return isupper(ch) || islower(ch) ? xlTrue : xlFalse;
}

/* xdigitp - built-in function 'digit-char-p' */
xlValue xdigitp(void)
{
    int ch;
    ch = xlGetChCode(xlGetArgChar());
    xlLastArg();
    return isdigit(ch) ? xlMakeFixnum((xlFIXTYPE)(ch - '0')) : xlFalse;
}

/* xalphanumericp - built-in function 'alphanumericp' */
xlValue xalphanumericp(void)
{
    int ch;
    ch = xlGetChCode(xlGetArgChar());
    xlLastArg();
    return isupper(ch) || islower(ch) || isdigit(ch) ? xlTrue : xlFalse;
}

/* xwhitespacep - built-in function 'char-whitespace?' */
xlValue xwhitespacep(void)
{
    int ch;
    ch = xlGetChCode(xlGetArgChar());
    xlLastArg();
    return isspace(ch) ? xlTrue : xlFalse;
}

/* xcompile - built-in function 'compile' */
xlValue xcompile(void)
{
    xlValue env;

    /* get the expression to compile and the environment */
    xlVal = xlGetArg();
    env = xlMoreArgsP() ? xlGetArgEnv() : xlNil;
    xlLastArg();
    
    /* build the closure */
    xlCPush(env);
    xlVal = xlCompile(xlVal,xlTop());
    xlVal = xlMakeClosure(xlVal,xlTop());
    xlDrop(1);
    return xlVal;
}

/* xdecompile - built-in function 'decompile' */
xlValue xdecompile(void)
{
    xlValue code,env=NULL,fptr;

    /* get the closure (or code and env) and file pointer */
    code = xlGetArg();
    if (xlClosureP(code)) {
        env = xlGetEnvironment(code);
        code = xlGetCode(code);
    }
    else if (xlCodeP(code))
        env = xlGetArgEnv();
    else
        xlError("expecting a closure or code and environment",code);
    fptr = xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput();
    xlLastArg();

    /* decompile (disassemble) the procedure */
    xlDecodeProcedure(fptr,code,env);
    return xlNil;
}

/* xsetdebugmode - built-in function 'set-debug-mode!' */
xlValue xsetdebugmode(void)
{
    int oldMode = xlDebugModeP;
    xlVal = xlGetArg();
    xlLastArg();
    xlDebugModeP = xlVal != xlFalse;
    return oldMode ? xlTrue : xlFalse;
}

/* xsave - save the memory image */
xlValue xsave(void)
{
    char *name;

    /* get the file name, verbose flag and print flag */
    name = xlGetString(xlGetArgString());
    xlLastArg();

    /* save the memory image */
    return xlSaveImage(name) ? xlTrue : xlFalse;
}

/* xrestore - restore a saved memory image */
xlValue xrestore(void)
{
    char *name;

    /* get the file name, verbose flag and print flag */
    name = xlGetString(xlGetArgString());
    xlLastArg();

    /* restore the saved memory image */
    if (!xlRestoreImage(name))
        return xlFalse;

    /* return directly to the top level */
    xlStdPutStr("[ returning to the top level ]\n");
    xlTopLevel();
    return xlNil; /* never reached */
}

/* xgc - function to force garbage collection */
xlValue xgc(void)
{
    xlFIXTYPE arg1,arg2;
    xlValue arg;
    
    /* check the argument list and call the garbage collector */
    if (xlMoreArgsP()) {
        arg = xlGetArgFixnum(); arg1 = xlGetFixnum(arg);
        arg = xlGetArgFixnum(); arg2 = xlGetFixnum(arg);
        xlLastArg();
        while (--arg1 >= 0) xlNExpand(xlNSSize);
        while (--arg2 >= 0) xlVExpand(xlVSSize);
    }
    else
        xlGC();
    return room();
}

/* xroom - return the amount of memory currently available */
xlValue xroom(void)
{
    xlLastArg();
    return room();
}

/* room - create a list containing memory allocation statistics */
static xlValue room(void)
{
    xlVal = xlCons(xlMakeFixnum(xlTotal),xlNil);
    xlVal = xlCons(xlMakeFixnum((xlFIXTYPE)xlVSCount),xlVal);
    xlVal = xlCons(xlMakeFixnum((xlFIXTYPE)xlNSCount),xlVal);
    xlVal = xlCons(xlMakeFixnum(xlNFree),xlVal);
    xlVal = xlCons(xlMakeFixnum(xlNNodes),xlVal);
    xlVal = xlCons(xlMakeFixnum(xlGCCalls),xlVal);
    return xlVal;
}

/* xerror - built-in function 'error' */
xlValue xerror(void)
{
    /* display the error message */
    xlErrPutStr("\nerror: ");
    format(xlCurError());

    /* show the error context */
    xlShowErr(xlFindTopProcedure());

    /* call the handler */
    xlCallErrorHandler();
    return xlNil; /* never reached */
}

/* xgetarg - return a command line argument */
xlValue xgetarg(void)
{
    xlValue arg;
    int n;
    arg = xlGetArgFixnum(); n = (int)xlGetFixnum(arg);
    xlLastArg();
    return n >= 0 && n < xlCmdLineArgC ? xlMakeCString(xlCmdLineArgV[n]) : xlNil;
}

/* xshowstack - built-in function 'show-stack' */
void xshowstack(void)
{
    int levels = 20;
    if (xlMoreArgsP()) {
        xlVal = xlGetArgFixnum();
        levels = (int)xlGetFixnum(xlVal);
    }
    xlLastArg();
    xlShowCallStack(levels);
    xlMVReturn(0);
}

/* xshowcontrolstack - built-in function 'show-control-stack' */
void xshowcontrolstack(void)
{
    int levels = 20;
    if (xlMoreArgsP()) {
        xlVal = xlGetArgFixnum();
        levels = (int)xlGetFixnum(xlVal);
    }
    xlLastArg();
    xlShowControlStack(levels);
    xlMVReturn(0);
}

/* xshowvaluestack - built-in function 'show-value-stack' */
void xshowvaluestack(void)
{
    int levels = 20;
    if (xlMoreArgsP()) {
        xlVal = xlGetArgFixnum();
        levels = (int)xlGetFixnum(xlVal);
    }
    xlLastArg();
    xlShowValueStack(levels);
    xlMVReturn(0);
}

/* xgettime - get the current time */
xlValue xgettime(void)
{
    xlLastArg();
    return (xlMakeFixnum((xlFIXTYPE)xlosTime()));
}

/* xgetenv - get the value of an environment variable */
xlValue xgetenv(void)
{
    char *val;
    xlVal = xlGetArgString();
    xlLastArg();
    if ((val = xlosGetEnv(xlGetString(xlVal))) == NULL)
        return xlNil;
    return xlMakeCString(val);
}

/* xidle - call the idle handler */
xlValue xidle(void)
{
    xlLastArg();
    xlIdle();
    return xlNil;
}

/* xexit - exit to the operating system */
xlValue xexit(void)
{
    xlLastArg();
    xlWrapUp();
    return xlNil; /* never reached */
}

#if 0
/* crecord field types */
#define CRTYPE_CHAR     1
#define CRTYPE_UCHAR    2
#define CRTYPE_SHORT    3
#define CRTYPE_USHORT   4
#define CRTYPE_INT      5
#define CRTYPE_UINT     6
#define CRTYPE_LONG     7
#define CRTYPE_ULONG    8
#define CRTYPE_PTR      9

/* xallocatecmemory - built-in function 'allocate-cmemory' */
xlValue xalloccmemory(void)
{
    xlValue type;
    xlFIXTYPE size;
    char *ptr;
    
    /* parse the argument list */
    type = xlGetArgSymbol();
    xlVal = xlGetArgFixnum(); size = xlGetFixnum(xlVal);
    xlLastArg();
    
    /* allocate the memory and create the foreign pointer */
    ptr = xlosAlloc(size);
    return ptr ? xlMakeForeignPtr(type,ptr) : xlNil;
}

/* xfreecmemory - built-in function 'free-cmemory' */
xlValue xfreecmemory(void)
{
    void *ptr;
    
    /* parse the argument list */
    xlVal = xlGetArgForeignPtr(); ptr = xlGetFPtr(xlVal);
    xlLastArg();
    
    /* free the pointer */
    if (ptr) xlosFree(ptr);
    xlSetFPtr(xlVal,0);
    return xlNil;
}

/* xforeignptrp - built-in function 'foreign-pointer?' */
xlValue xforeignptrp(void)
{
    xlValue ptr,type;
    
    /* get the pointer */
    ptr = xlGetArg();
    type = xlMoreArgsP() ? xlGetArg() : xlNil;
    xlLastArg();
    
    /* return its type */
    if (!xlForeignPtrP(ptr))
        return xlFalse;
    else if (type == xlNil)
        return xlTrue;
    else
        return xlGetFPType(ptr) == type ? xlTrue : xlFalse;
}

/* xforeignptrtype - built-in function 'foreign-pointer-type' */
xlValue xforeignptrtype(void)
{
    xlValue ptr;
    
    /* get the pointer */
    ptr = xlGetArgForeignPtr();
    xlLastArg();
    
    /* return its type */
    return xlGetFPType(ptr);
}

/* xforeignptreqp - built-in function 'foreign-pointer-eq?' */
xlValue xforeignptreqp(void)
{
    xlValue ptr1,ptr2;
    
    /* get the pointer */
    ptr1 = xlGetArgForeignPtr();
    ptr2 = xlGetArgForeignPtr();
    xlLastArg();
    
    /* check the pointers */
    return xlGetFPtr(ptr1) == xlGetFPtr(ptr2) ? xlTrue : xlFalse;
}

/* xsetforeignptrtype - built-in function 'set-foreign-pointer-type!' */
xlValue xsetforeignptrtype(void)
{
    xlValue ptr,type;
    
    /* get the pointer and new type */
    ptr = xlGetArgForeignPtr();
    type = xlGetArg();
    xlLastArg();
    
    /* set the type and return the pointer */
    xlSetFPType(ptr,type);
    return ptr;
}

/* xforeignptrtypep - built-in function 'foreign-pointer-type?' */
xlValue xforeignptrtypep(void)
{
    xlValue ptr,type;
    
    /* get the pointer and type */
    ptr = xlGetArgForeignPtr();
    type = xlGetArg();
    xlLastArg();
    
    /* check the type */
    return xlGetFPType(ptr) == type ? xlTrue : xlFalse;
}

/* xgetcrecfield - built-in function 'get-crecord-field' */
xlValue xgetcrecfield(void)
{
    xlValue record;
    long offset;
    void *ptr;
    int type;
    
    /* parse argument list */
    record = xlGetArgForeignPtr();
    xlVal = xlGetArgFixnum(); offset = xlGetFixnum(xlVal);
    xlVal = xlGetArgFixnum(); type = (int)xlGetFixnum(xlVal);
    xlLastArg();
    
    /* get the field pointer */
    if ((ptr = xlGetFPtr(record)) == 0)
        xlError("pointer is null",record);
    ptr = (void *)((char *)ptr + offset);
    
    /* dispatch on field type */
    switch (type) {
    case CRTYPE_CHAR:
        {   char ival = *(char *)ptr;
            xlVal = xlMakeFixnum((xlFIXTYPE)ival);
            break;
        }
    case CRTYPE_UCHAR:
        {   unsigned char ival = *(unsigned char *)ptr;
            xlVal = xlMakeFixnum((xlFIXTYPE)ival);
            break;
        }
    case CRTYPE_SHORT:
        {   short ival = *(short *)ptr;
            xlVal = xlMakeFixnum((xlFIXTYPE)ival);
            break;
        }
    case CRTYPE_USHORT:
        {   unsigned short ival = *(unsigned short *)ptr;
            xlVal = xlMakeFixnum((xlFIXTYPE)ival);
            break;
        }
    case CRTYPE_INT:
        {   int ival = *(int *)ptr;
            xlVal = xlMakeFixnum((xlFIXTYPE)ival);
            break;
        }
    case CRTYPE_UINT:
        {   unsigned int ival = *(unsigned int *)ptr;
            xlVal = xlMakeFixnum((xlFIXTYPE)ival);
            break;
        }
    case CRTYPE_LONG:
        {   long ival = *(long *)ptr;
            xlVal = xlMakeFixnum((xlFIXTYPE)ival);
            break;
        }
    case CRTYPE_ULONG:
        {   unsigned long ival = *(unsigned long *)ptr;
            xlVal = xlMakeFixnum((xlFIXTYPE)ival);
            break;
        }
    case CRTYPE_PTR:
        {   void *ival = *(void **)ptr;
            xlVal = xlMakeForeignPtr(xlNil,ival);
            break;
        }
    default:
        xlError("bad type code",xlVal);
        break;
    }
    return xlVal;
}

/* xgetcrecfieldaddr - built-in function 'get-crecord-field-address' */
xlValue xgetcrecfieldaddr(void)
{
    xlValue record,type;
    long offset;
    void *ptr;
    
    /* parse argument list */
    record = xlGetArgForeignPtr();
    xlVal = xlGetArgFixnum(); offset = xlGetFixnum(xlVal);
    type = xlGetArgSymbol();
    xlLastArg();
    
    /* get the field pointer */
    if ((ptr = xlGetFPtr(record)) == 0)
        xlError("pointer is null",record);
    ptr = (void *)((char *)ptr + offset);
    
    /* make a pointer to the field */
    return xlMakeForeignPtr(type,ptr);
}

/* xsetcrecfield - built-in function 'set-crecord-field!' */
xlValue xsetcrecfield(void)
{
    xlValue record,value;
    long offset;
    void *ptr;
    int type;
    
    /* parse argument list */
    record = xlGetArgForeignPtr();
    xlVal = xlGetArgFixnum(); offset = xlGetFixnum(xlVal);
    xlVal = xlGetArgFixnum(); type = (int)xlGetFixnum(xlVal);
    value = xlGetArg();
    xlLastArg();
    
    /* get the field pointer */
    if ((ptr = xlGetFPtr(record)) == 0)
        xlError("pointer is null",record);
    ptr = (void *)((char *)ptr + offset);
    
    /* dispatch on field type */
    switch (type) {
    case CRTYPE_CHAR:
    case CRTYPE_UCHAR:
        {   if (!xlFixnumP(value))
                xlError("expecting a fixnum",value);
            *(char *)ptr = (char)xlGetFixnum(value);
            break;
        }
    case CRTYPE_SHORT:
    case CRTYPE_USHORT:
        {   if (!xlFixnumP(value))
                xlError("expecting a fixnum",value);
            *(short *)ptr = (short)xlGetFixnum(value);
            break;
        }
    case CRTYPE_INT:
    case CRTYPE_UINT:
        {   if (!xlFixnumP(value))
                xlError("expecting a fixnum",value);
            *(int *)ptr = (int)xlGetFixnum(value);
            break;
        }
    case CRTYPE_LONG:
    case CRTYPE_ULONG:
        {   if (!xlFixnumP(value))
                xlError("expecting a fixnum",value);
            *(long *)ptr = (long)xlGetFixnum(value);
            break;
        }
    case CRTYPE_PTR:
        {   if (!xlForeignPtrP(value))
                xlError("expecting a foreign pointer",value);
            *(void **)ptr = xlGetFPtr(value);
            break;
        }
    default:
        xlError("bad type code",xlVal);
        break;
    }
    return value;
} 

/* xgetcrecstring - built-in function 'get-crecord-string' */
xlValue xgetcrecstring(void)
{
    xlFIXTYPE offset,length;
    char *src,*dst;
    xlValue val;
    
    /* parse argument list */
    xlVal = xlGetArgForeignPtr();
    val = xlGetArgFixnum(); offset = xlGetFixnum(val);
    val = xlGetArgFixnum(); length = xlGetFixnum(val);
    xlLastArg();
    
    /* get the field pointer */
    if ((src = (char *)xlGetFPtr(xlVal)) == 0)
        xlError("pointer is null",xlVal);
    src += offset;
    
    /* make a string */
    val = xlNewString(length);
    
    /* copy the value */
    for (dst = xlGetString(val); --length >= 0; )
        *dst++ = *src++;
    
    /* return the string */
    return val;
}
    
/* xsetcrecstring - built-in function 'set-crecord-string!' */
xlValue xsetcrecstring(void)
{
    xlFIXTYPE offset,length,cnt;
    xlValue record,str;
    char *src,*dst;
    
    /* parse argument list */
    record = xlGetArgForeignPtr();
    xlVal = xlGetArgFixnum(); offset = xlGetFixnum(xlVal);
    xlVal = xlGetArgFixnum(); length = xlGetFixnum(xlVal);
    str = xlGetArgString();
    xlLastArg();
    
    /* get the field pointer */
    if ((dst = (char *)xlGetFPtr(record)) == 0)
        xlError("pointer is null",record);
    dst += offset;
    
    /* compute the number of bytes to copy */
    if ((cnt = xlGetSLength(str)) > length)
        cnt = length;
    
    /* copy the value */
    for (src = xlGetString(str); --cnt >= 0; --length)
        *dst++ = *src++;

    /* fill the rest of the destination with nulls */
    while (--length >= 0)
        *dst++ = '\0';
        
    /* return the string */
    return str;
}
    
/* xgetcrectypesize - built-in function 'get-crecord-type-size' */
xlValue xgetcrectypesize(void)
{
    size_t size;
    int type;
    
    /* parse argument list */
    xlVal = xlGetArgFixnum(); type = (int)xlGetFixnum(xlVal);
    xlLastArg();
    
    /* dispatch on field type */
    switch (type) {
    case CRTYPE_CHAR:
    case CRTYPE_UCHAR:
        size = sizeof(char);
        break;
    case CRTYPE_SHORT:
    case CRTYPE_USHORT:
        size = sizeof(short);
        break;
    case CRTYPE_INT:
    case CRTYPE_UINT:
        size = sizeof(int);
        break;
    case CRTYPE_LONG:
    case CRTYPE_ULONG:
        size = sizeof(long);
        break;
    case CRTYPE_PTR:
        size = sizeof(void *);
        break;
    default:
        xlError("bad type code",xlVal);
        break;
    }
    return xlMakeFixnum(size);
}

/* xnullpointerp - built-in function 'null-pointer?' */
xlValue xnullpointerp(void)
{
    xlValue record = xlGetArgForeignPtr();
    xlLastArg();
    return xlGetFPtr(record) == 0 ? xlTrue : xlFalse;
}
#endif
