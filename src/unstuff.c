/* unstuff.c - unix specific routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use
        Contact me at dbetz@xlisper.mv.com for commercial permission    */

#include <time.h>
#include <sys/stat.h>
#ifdef LOADLIB
#include <dlfcn.h>
#endif
#include "xlisp.h"

/* external variables */
extern xlValue s_unbound;

/* local variables */
#define LBSIZE 100
static char progpath[LBSIZE+1] = "./";
static int lposition;

static char *osloadpath(void);
static char *osparsepath(char **pp);
static void osexit(int sts);
static void oserror(char *msg);
static int osfmodtime(char *fname,xlFIXTYPE *pModTime);
static int ostgetc(void);
static void ostputc(int ch);
static int ostatbol(void);
static void ostflush(void);
static int ostcheck(void);
static void osflushoutput(void);

static xlValue xsystem(void);
#ifdef LOADLIB
static xlValue xloadlibrary(void);
#endif

xlSubrDef xlosSubrTab[] = {
{       "SYSTEM",           xsystem             },
#ifdef LOADLIB
{       "LOAD-LIBRARY",     xloadlibrary    },
#endif
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
        if (*src == ':') {
            ++src;
            break;
        }
    *dst = '\0';

    /* make sure the directory ends with a slash */
    if (dst > buf && dst[-1] != '/')
        strcat(dst,"/");
    
    /* return this directory and position in the path */
    *pp = src;
    return dst == buf ? NULL : buf;
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
    return getchar();
}

/* ostputc - put a character to the terminal */
static void ostputc(int ch)
{
   if (ch == '\n')
        lposition = 0;
   else
        ++lposition;
   putchar(ch);
}

/* ostatbol - are we at the beginning of a line? */
static int ostatbol(void)
{
    return lposition == 0;
}

/* ostflush - flush the terminal input buffer */
static void ostflush(void)
{
}

/* ostcheck - check for control characters during execution */
static int ostcheck(void)
{
    return 0;
}

/* osflushoutput - flush the output buffer */
static void osflushoutput(void)
{
}

/* xsystem - execute a system command */
static xlValue xsystem(void)
{
    char *cmdLine = xlGetString(xlGetArgString());
    int sts;
    xlLastArg();
    sts = system(cmdLine);
    return sts >= 127 ? xlNil : xlMakeFixnum((xlFIXTYPE)sts);
}

#ifdef LOADLIB
/* xloadlibrary - import functions from a dll */
static xlValue xloadlibrary(void)
{
    void *lib;
    int (*init)(void);
    
    /* parse the argument list */
    xlVal = xlGetArgString();
    xlLastArg();

    /* load the library */
    if ((lib = dlopen(xlGetString(xlVal),RTLD_NOW)) == NULL)
        return xlFalse;

    /* get the initialization routine */
    if ((init = (int (*)(void))dlsym(lib,"initialize")) == NULL)
        return xlFalse;

    /* call the initialization routine */
    return (*init)() ? xlTrue : xlFalse;
}
#endif

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

        /* find the last delimiter */
        for (p = progpath, lastDelimiter = NULL; (ch = *p) != '\0'; ++p)
            if (ch == '/')
                lastDelimiter = p;

        /* remove the program name */
        if (lastDelimiter)
            *++lastDelimiter = '\0';
        else
            progpath[0] = '\0';

        /* check for a trailing /bin/ */
        if ((len = strlen(progpath)) >= 5
        &&  strcmp(&progpath[len - 5],"/bin/") == 0)
            progpath[len - 4] = '\0';
    }

    /* setup the callbacks */
    callbacks.loadPath = osloadpath;
    callbacks.parsePath = osparsepath;
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
