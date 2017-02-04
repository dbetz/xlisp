/* xlterm.c - terminal emulator routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include <stdio.h>
#include <ctype.h>
#include "xlisp.h"
#include "xledit.h"

/* program limits */
#define LINEMAX         256     /* maximum line length */

/* globals */
xlEXPORT xlEditWindow *xlListener = NULL;

/* local variables */
static char linebuf[LINEMAX+1];
static char *lineptr = linebuf;
static int linelen = 0;
static int atbol = TRUE;

/* prototypes */
static void ScrollOffTop(xlEditBuffer *buf,long count);

/* xlMakeListenerWindow - make a (the) listener window */
xlEXPORT xlEditWindow *xlMakeListenerWindow(void *window,long size)
{
    xlEditBuffer *buf = xlMakeBuffer(size);
    if (buf) {
        xlEditWindow *win = xlMakeEditWindow(window,buf);
        if (win) {
            win->window = window;
            if (!xlListener)
                xlListener = win;
            return win;
        }
        xlFreeBuffer(buf);
    }
    return NULL;
}

/* xlTermPutC - output a character to a terminal window */
xlEXPORT void xlTermPutC(int ch,xlEditWindow *win)
{
    if (xlEditWindowPutC(ch,win) == EOF) {
        ScrollOffTop(win->buffer,1);
        xlEditWindowPutC(ch,win);
    }
}

/* xlListenerGetC - get a character from the listener window */
int xlListenerGetC(void)
{
    int ch,i;

    if (linelen--) return *lineptr++;
    lineptr = linebuf;
    linelen = 0;
    while ((ch = xlEditWindowGetC(xlListener)) != '\r')
        switch (ch) {
        case EOF:
            return linelen > 0 ? xlListenerGetC() : EOF;
        case '\010':
            if (lineptr > linebuf) {
                char *src = lineptr;
                char *dst = --lineptr;
                char *top = linebuf + linelen;
                xlEditWindowPrevC(xlListener);
                xlEditWindowDeleteC(xlListener);
                while (src < top)
                    *dst++ = *src++;
                --linelen;
            }
            break;
        case xlEBKC_LEFT_ARROW:
            if (lineptr > linebuf) {
                xlEditWindowPrevC(xlListener);
                --lineptr;
            }
            break;
        case xlEBKC_RIGHT_ARROW:
            if (lineptr < linebuf + linelen) {
                xlEditWindowNextC(xlListener);
                ++lineptr;
            }
            break;
        case xlEBKC_UP_ARROW:
        case xlEBKC_DOWN_ARROW:
            break;
        default:
            if (linelen < LINEMAX) {
                char *src = linebuf + linelen;
                char *dst = src + 1;
                xlTermPutC(ch,xlListener);
                while (src > lineptr)
                    *--dst = *--src;
                *lineptr++ = ch;
                ++linelen;
            }
            break;
        }
    while (++lineptr <= linebuf + linelen)
        xlEditWindowNextC(xlListener);
    linebuf[linelen++] = '\n';
    xlTermPutC('\n',xlListener);
    atbol = TRUE;
    if (xlTranscriptFP)
        for (i = 0; i <= linelen; ++i)
            putc(linebuf[i],xlTranscriptFP);
    lineptr = linebuf; linelen--;
    return *lineptr++;
}

/* xlListenerPutC - put a character to the listener window */
void xlListenerPutC(int ch)
{
    xlTermPutC(ch,xlListener);
    atbol = ch == '\n';
    if (xlTranscriptFP)
        putc(ch,xlTranscriptFP);
}

/* xlListenerAtBOPP - determine if the cursor is at the beginning of a line */
int xlListenerAtBOLP(void)
{
    return atbol;
}

/* xlListenerFlushInput - flush the input buffer */
void xlListenerFlushInput(void)
{
    lineptr = linebuf;
    linelen = 0;
}

/* xlListenerFlushOutput - flush pending output */
void xlListenerFlushOutput(void)
{
}

/* xlListenerCheck - check for an available input character */
int xlListenerCheck(void)
{
    return EOF;
}

/* ScrollOffTop - scroll lines off the top of a buffer */
static void ScrollOffTop(xlEditBuffer *buf,long lines)
{
    xlBufferPointer *bp = xlMakeBufferPointer(buf);
    if (bp) {
        while (--lines >= 0)
            xlBufferNextL(bp,0);
        xlDeleteRange(buf,0,bp->offset);
        xlFreeBufferPointer(bp);
    }
}
