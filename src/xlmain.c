/* xlmain.c - xlisp main routine */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include <stdarg.h>
#include "xlisp.h"

/* the program banner */
#define BANNER  "\
XLISP 3.3, September 6, 2002 Copyright (c) 1984-2002, by David Betz"

/* global variables */
int xlCmdLineArgC = 0;                  /* command line argument count */
char **xlCmdLineArgV = NULL;            /* array of command line arguments */
xlEXPORT int xlInitializedP = FALSE;    /* true if initialization is done */
xlEXPORT FILE *xlTranscriptFP = NULL;   /* trace file pointer */

/* external variables */
extern xlValue s_package,xlUnboundObject,s_stderr,s_error,s_backtrace;
extern xlFIXTYPE xlNSSize,xlVSSize;
extern int xlTraceBytecodes;

/* local prototypes */
static void fmterror(char *tag,char *fmt,va_list ap);

/* xlInit - the initialization routine */
xlEXPORT int xlInit(xlCallbacks *callbacks,int argc,char *argv[],char *workspace)
{
    xlErrorTarget target;
    int src,dst;
    
    /* store the callback structure pointer */
    xlSetCallbacks(callbacks);

    /* process the arguments */
    for (src = dst = 1; src < argc; ++src) {
        int usedP = FALSE;

        /* handle options */
        if (argv[src][0] == '-' && argv[src][1] == '%') {
            switch (argv[src][2]) {
            case 't':           /* trace byte codes */
                if (!argv[src][3]) {
                    xlTraceBytecodes = TRUE;
                    usedP = TRUE;
                }
                break;
            case 'n':           /* node segment size */
                if (argv[src][3]) {
                    if (isdigit(argv[src][3])) {
                        xlNSSize = atol(&argv[src][3]);
                        usedP = TRUE;
                    }
                }
                else if (++src < argc) {
                    xlNSSize = atol(argv[src]);
                    usedP = TRUE;
                }
                break;
            case 'v':           /* vector segment size */
                if (argv[src][3]) {
                    if (isdigit(argv[src][3])) {
                        xlVSSize = atol(&argv[src][3]);
                        usedP = TRUE;
                    }
                }
                else if (++src < argc) {
                    xlVSSize = atol(argv[src]);
                    usedP = TRUE;
                }
                break;
            }
        }

        /* pass on any arguments that were not used */
        if (!usedP)
            argv[dst++] = argv[src];
    }

    /* setup the global argument variables */
    xlCmdLineArgV = argv;
    xlCmdLineArgC = dst;

    /* setup an initialization error handler */
    xlPushTarget(&target);
    if (setjmp(target.target)) {
        xlPopTarget();
        return FALSE;
    }

    /* restore the default workspace, otherwise create a new one */
    if (!workspace || !xlRestoreImage(workspace))
        xlInitWorkspace(xlSTACKSIZE);
        
    /* done with initialization */
    xlPopTarget();

    /* return successfully */
    xlInitializedP = TRUE;
    return TRUE;
}

/* xlBanner - display the banner */
xlEXPORT char *xlBanner(void)
{
    return BANNER;
}

/* xlCleanup - cleanup after an error (leave the debugger) */
xlEXPORT void xlCleanup(void)
{
    xlThrowError(xlEnter("CLEANUP"));
}

/* xlTopLevel - return to the top level */
xlEXPORT void xlTopLevel(void)
{
    xlThrowError(xlEnter("RESET"));
}

/* xlContinue - continue from the debugger */
xlEXPORT void xlContinue(void)
{
    /* unimplemented */
}

/* xlBreak - enter the debugger */
xlEXPORT void xlBreak(void)
{
    /* unimplemented */
}

/* xlWrapUp - clean up and exit to the operating system */
xlEXPORT void xlWrapUp(void)
{
    if (xlTranscriptFP != NULL)
        xlosClose(xlTranscriptFP);
    xlosExit(0);
}

/* xlError - print an error message */
xlEXPORT void xlError(char *msg,xlValue arg)
{
    char fmt[256];
    sprintf(fmt,"%s - ~S",msg);
    xlFmtError(fmt,arg);
}

/* xlFmtError - report an error */
xlEXPORT void xlFmtError(char *fmt,...)
{
    va_list ap;
    va_start(ap,fmt);
    fmterror("\nError: ",fmt,ap);
    va_end(ap);
    xlCallErrorHandler();
}

/* xlAbort - print an error message and abort */
xlEXPORT void xlAbort(char *msg,xlValue arg)
{
    char fmt[256];
    sprintf(fmt,"%s - ~S",msg);
    xlFmtAbort(fmt,arg);
}

/* xlFmtAbort - report an error */
xlEXPORT void xlFmtAbort(char *fmt,...)
{
    va_list ap;
    va_start(ap,fmt);
    fmterror("\nAbort: ",fmt,ap);
    va_end(ap);
    xlJumpToTarget(-1);
}

/* fmterror - report an error */
static void fmterror(char *tag,char *fmt,va_list ap)
{
    xlValue stream = xlGetValue(s_stderr);
    int ch;
    
    /* an opportunity to break out */
    xlosCheck();
    
    /* flush the input buffer */
    xlFlush();
    
    /* display the error message */
    xlErrPutStr(tag);
    
    /* process the format string */
    for (;;) {
        if ((ch = *fmt++) == '\0')
            break;
        else if (ch == '~' && *fmt != '\0') {
            switch (*fmt++) {
            case 'a': case 'A':
                xlDisplay(va_arg(ap,xlValue),stream);
                break;
            case 's': case 'S':
                xlWrite(va_arg(ap,xlValue),stream);
                break;
            case '%':
                xlNewline(stream);
                break;
            case '~':
                xlPutC(stream,'~');
                break;
            case '\n':
                while ((ch = *fmt) != '\0' && isspace(ch) && ch != '\n')
                    ++fmt;
                break;
            default:
                xlError("unknown format directive",xlMakeChar(ch));
            }
        }
        else
            xlPutC(stream,ch);
    }
    
    /* print the function where the error occurred */
    xlShowErr(xlFun);
}

/* xlShowErr - show where the error happened */
void xlShowErr(xlValue fun)
{
    xlValue levels = xlGetValue(s_backtrace);

    /* print the function where the error occurred */
    if (fun != xlNil) {
        xlErrPutStr("\nhappened in: ");
        xlErrPrint(fun);
    }

    /* show the call stack */
    if (xlFixnumP(levels)) {
        xlErrPutStr("\ncall stack:");
        xlShowCallStack((int)xlGetFixnum(levels));
    }
}

/* xlFatal - print a fatal error message and exit */
xlEXPORT void xlFatal(char *fmt,...)
{
    char buf[1024];
    va_list ap;
    va_start(ap,fmt);
    vsprintf(buf,fmt,ap);
    va_end(ap);
    xlosError(buf);
    xlosExit(1);
}

/* xlInfo - display debugging information */
xlEXPORT void xlInfo(char *fmt,...)
{
    char buf[1024],*p=buf;
    va_list ap;
    va_start(ap,fmt);
    vsprintf(buf,fmt,ap);
    va_end(ap);
    while (*p != '\0')
        xlosConsolePutC(*p++);
}
