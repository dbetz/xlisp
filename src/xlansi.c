/* xlansi.c - ansi i/o functions */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* local variables */
static long rseed = 1L;

/* xlosSetRand - set random number seed */
xlEXPORT void xlosSetRand(long seed)
{
    rseed = seed;
}

/* xlosRand - return a random number between 0 and n-1 */
xlEXPORT long xlosRand(long n)
{
    long k1;

    /* make sure we don't get stuck at zero */
    if (rseed == 0L) rseed = 1L;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
        rseed += 2147483647L;

    /* return a random number between 0 and n-1 */
    return rseed % n;
}

/* xlosOpenText - open an ascii file */
xlEXPORT FILE *xlosOpenText(char *name,char *mode)
{
    return fopen(name,mode);
}

/* xlosOpenBinary - open a binary file */
xlEXPORT FILE *xlosOpenBinary(char *name,char *mode)
{
    char bmode[10];
    strcpy(bmode,mode); strcat(bmode,"b");
    return fopen(name,bmode); 
}

/* xlosClose - close a file */
xlEXPORT int xlosClose(FILE *fp)
{
    return fclose(fp);
}

/* xlosTell - get the current file position */
xlEXPORT long xlosTell(FILE *fp)
{
    return ftell(fp);
}

/* xlosSeek - set the current file position */
xlEXPORT int xlosSeek(FILE *fp,long offset,int whence)
{
    return fseek(fp,offset,whence);
}

/* xlosReadFile - read from a file */
xlEXPORT size_t xlosReadFile(void *buf,size_t size,size_t count,FILE *fp)
{
    return fread(buf,size,count,fp);
}

/* xlosWriteFile - write to a file */
xlEXPORT size_t xlosWriteFile(void *buf,size_t size,size_t count,FILE *fp)
{
    return fwrite(buf,size,count,fp);
}

/* xlosCheck - check for control characters during execution */
xlEXPORT void xlosCheck(void)
{
    switch (xlosConsoleCheck()) {
    case '\002':        /* control-b */
        xlosConsoleFlush();
        xlBreak();
        break;
    case '\003':        /* control-c */
        xlosConsoleFlush();
        xlTopLevel();
        break;
    case '\024':        /* control-t */
        xlosInfo();
        break;
    case '\023':        /* control-s */
        while (xlosConsoleCheck() != '\021')
            ;
        break;
    case '\034':        /* control-\ */
        xlWrapUp();
        break;
    }
}

/* xlosInfo - show information on control-t */
xlEXPORT void xlosInfo(void)
{
    extern xlFIXTYPE xlNFree,xlGCCalls,xlTotal;
    char buf[80];
    sprintf(buf,"\n[ Free: %ld, GC calls: %ld, Total: %ld ]",xlNFree,xlGCCalls,xlTotal);
    xlErrPutStr(buf);
}

/* xlosTime - return the time */
xlEXPORT time_t xlosTime(void)
{
    return time(0);
}

/* xlosAlloc - allocate memory */
xlEXPORT void *xlosAlloc(xlFIXTYPE size)
{
    return calloc(1,(size_t)size);
}

/* xlosFree - free memory */
xlEXPORT void xlosFree(void *ptr)
{
    free(ptr);
}

/* xlosGetEnv - get the value of an environment variable */
xlEXPORT char *xlosGetEnv(char *name)
{
    return getenv(name);
}

