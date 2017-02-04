/* msstuff.c - default o/s callbacks for ms-dos */
/*	Copyright (c) 1984-2002, by David Michael Betz
	All Rights Reserved
	See the included file 'license.txt' for the full license.
*/

#include <stdlib.h>
#include <ctype.h>
#include <windows.h>
#include <conio.h>
#include <time.h>
#include <signal.h>
#include <sys/stat.h>
#include "xlisp.h"

#define LBSIZE 255

/* local functions */
static int xgetc(void);
static void xputc(int ch);

/* local variables */
static xlValue s_usestdio = NULL;
static char progpath[LBSIZE+1] = "";
static char lbuf[LBSIZE+1];
static int lpos[LBSIZE];
static int lindex;
static int lcount = 0;
static int lposition;

/* built-in functions */
static xlValue xsystem(void);
static xlValue xloadlibrary(void);

xlSubrDef xlosSubrTab[] = {
{   "SYSTEM",           xsystem         },
{   "LOAD-LIBRARY",     xloadlibrary    },
{0,0}};

xlXSubrDef xlosXSubrTab[] = {
{0,0}};

/* osloadpath - return the load path */
static char *osloadpath(void)
{
    char *path;
    if ((path = getenv("XLPATH")) == NULL)
        path = progpath;
    return path;
}

/* osparsepath - parse a path string */
static char *osparsepath(char **pp)
{
    static char buf[256];
    char *src,*dst;
    
    /* find the next directory in the path */
    for (src = *pp, dst = buf; *src != '\0'; *dst++ = *src++)
	if (*src == ';') {
	    ++src;
	    break;
	}
    *dst = '\0';

    /* make sure the directory ends with a backslash */
    if (dst > buf && dst[-1] != '\\')
	strcat(dst,"\\");
    
    /* return this directory and position in the path */
    *pp = src;
    return dst == buf ? NULL : buf;
}

/* osdirectoryseparator - return the directory separator character */
static int osdirectoryseparator(void)
{
    return '\\';
}

/* osexit - exit from XLISP */
static void osexit(int sts)
{
    exit(sts);
}

/* osfindsubr - find a built-in function */
static xlValue (*osfindsubr(char *name))(void)
{
    return NULL;
}

/* oserror - print an error message */
static void oserror(char *msg)
{
    xlInfo("error: %s\n",msg);
}

/* osfmodtime - return the modification time of a file */
static int osfmodtime(char *fname,xlFIXTYPE *pModTime)
{                        
    struct stat info;
    int sts = stat(fname,&info);
    *pModTime = info.st_mtime;
    return sts == 0;
}

/* ostgetc - get a character from the terminal */
static int ostgetc(void)
{
    int ch;

    /* check for stdio input */
    if (s_usestdio && xlGetValue(s_usestdio) != xlFalse)
        return getchar();
    
    /* check for a buffered character */
    if (lcount--)
	return (lbuf[lindex++]);

    /* get an input line */
    for (lcount = 0; ; )
	switch (ch = xgetc()) {
	case '\r':
		if (lcount >= LBSIZE)
		    xlFmtError("line too long");
		lbuf[lcount++] = '\n';
		xputc('\r'); xputc('\n'); lposition = 0;
		lindex = 0; lcount--;
		return (lbuf[lindex++]);
	case '\010':
	case '\177':
		if (lcount) {
		    lcount--;
		    while (lposition > lpos[lcount]) {
			xputc('\010'); xputc(' '); xputc('\010');
			lposition--;
		    }
		}
		break;
	case '\032':
		xlosConsoleFlush();
		return (EOF);
	default:
		if (lcount >= LBSIZE)
		    xlFmtError("line too long");
		if (ch == '\t' || (ch >= 0x20 && ch < 0x7F)) {
		    lbuf[lcount] = ch;
		    lpos[lcount] = lposition;
		    if (ch == '\t')
			do {
			    xputc(' ');
			} while (++lposition & 7);
		    else {
			xputc(ch); lposition++;
		    }
		    lcount++;
		}
		else {
		    xlosConsoleFlush();
		    switch (ch) {
		    case '\003':	xlTopLevel();	/* control-c */
		    case '\007':	xlCleanup();	/* control-g */
		    case '\020':	xlContinue();	/* control-p */
		    case '\032':	return (EOF);	/* control-z */
		    case '\034':	xlWrapUp();	/* control-\ */
		    default:		return (ch);
		    }
		}
	}
}

/* ostputc - put a character to the terminal */
static void ostputc(int ch)
{
    /* check for control characters */
    xlosCheck();

    /* check for stdio output */
    if (s_usestdio && xlGetValue(s_usestdio) != xlFalse) {
       if (ch == '\n')
            lposition = 0;
       else
            ++lposition;
       putchar(ch);
    }
    
    /* console output */
    else {
        /* output the character */
        if (ch == '\n') {
	    xputc('\r'); xputc('\n');
	    lposition = 0;
        }
        else {
	    xputc(ch);
	    lposition++;
       }
    }
}

/* ostatbol - are we at the beginning of a line? */
static int ostatbol(void)
{
    return lposition == 0;
}

/* ostflush - flush the terminal input buffer */
static void ostflush(void)
{
    lindex = lcount = lposition = 0;
}

/* ostcheck - check for control characters during execution */
static int ostcheck(void)
{
    if (s_usestdio && xlGetValue(s_usestdio) == xlFalse)
	return kbhit() ? getch() : 0;
    else
        return 0;
}

/* osflushoutput - flush the output buffer */
static void osflushoutput(void)
{
}

/* xgetc - get a character from the terminal without echo */
static int xgetc(void)
{
    while (!kbhit() && xlIdle())
        ;
    return getch();
}

/* xputc - put a character to the terminal */
static void xputc(int ch)
{
    putch(ch);
}

/* xsystem - execute a system command */
static xlValue xsystem(void)
{
    char *cmd = "COMMAND";
    if (xlMoreArgsP())
	cmd = xlGetString(xlGetArgString());
    xlLastArg();
    return xlMakeFixnum((xlFIXTYPE)system(cmd));
}

/* xloadlibrary - import functions from a dll */
static xlValue xloadlibrary(void)
{
    HINSTANCE lib;
    int (*init)(void);
    
    /* parse the argument list */
    xlVal = xlGetArgString();
    xlLastArg();

    /* load the library */
    if ((lib = LoadLibrary(xlGetString(xlVal))) == NULL)
        return xlFalse;

    /* get the initialization routine */
    if ((init = (int (*)(void))GetProcAddress(lib,"initialize")) == NULL)
        return xlFalse;

    /* call the initialization routine */
    return (*init)() ? xlTrue : xlFalse;
}

/* xlDefaultCallbacks - setup the default o/s interface callbacks */
xlEXPORT xlCallbacks *xlDefaultCallbacks(char *programPath)
{
    static xlCallbacks callbacks;

    /* get the program path */
    if (programPath) {
	char *lastDelimiter,*p;
        int len,ch;

        /* copy the full path to the program */
	strcpy(progpath,programPath);

        /* find the last delimiter and convert uppercase letters */
        for (p = progpath, lastDelimiter = NULL; (ch = *p) != '\0'; ++p) {
            if (ch == ':')
                lastDelimiter = p;
            else if (ch == '\\')
                lastDelimiter = p;
            else if (isupper(ch))
                *p = tolower(ch);
        }

        /* remove the program name */
        if (lastDelimiter)
            *++lastDelimiter = '\0';
        else
            progpath[0] = '\0';

        /* check for a trailing \bin\ */
        if ((len = strlen(progpath)) >= 5
        &&  strcmp(&progpath[len - 5],"\\bin\\") == 0)
            progpath[len - 4] = '\0';
    }

    /* setup the callbacks */
    callbacks.loadPath = osloadpath;
    callbacks.parsePath = osparsepath;
    callbacks.directorySeparator = osdirectoryseparator;
    callbacks.exit = osexit;
    callbacks.findSubr = osfindsubr;
    callbacks.error = oserror;
    callbacks.fileModTime = osfmodtime;
    callbacks.consoleGetC = ostgetc;
    callbacks.consolePutC = ostputc;
    callbacks.consoleAtBOLP = ostatbol;
    callbacks.consoleFlushInput = ostflush;
    callbacks.consoleFlushOutput = osflushoutput;
    callbacks.consoleCheck = ostcheck;

    /* return the callback structure */
    return &callbacks;
}
