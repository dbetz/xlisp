/* xledit.c - a simple editor for XLISP */
/*      Copyright (c) 1997, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "xledit.h"

/* local variables */
static xlEditCallbacks *callbacks;

/* local functions */
static void xlEditWindowRedrawCurrentLine(xlEditWindow *win,long x);
static void xlEditWindowClearInputBuffer(xlEditWindow *win);
static void xlEditWindowScroll(xlEditWindow *win);
static void xlEditWindowScrollToCursor(xlEditWindow *win);
static void xlEditWindowCheckForDelimiter(xlEditWindow *win);
static void xlEditWindowFindMatchingDelimiter(xlEditWindow *win,int lch,int rch);
static void osWaitForInput(xlEditWindow *win);
static void osScrollWindow(xlEditWindow *win,long y,long amount);
static void osAdjustScrollBar(xlEditWindow *win);
static void osStartOutput(xlEditWindow *win,int update);
static void osFinishOutput(xlEditWindow *win,int update);
static void osOutputLine(xlEditWindow *win,long x,long y,char *buffer,long length);
static void osSetCursorPosition(xlEditWindow *win);

/* xlSetEditCallbacks - set the edit callbacks */
xlEXPORT void xlSetEditCallbacks(xlEditCallbacks *cb)
{
    callbacks = cb;
}

/* xlDefaultEditCallbacks - setup the default edit callbacks */
xlEXPORT xlEditCallbacks *xlDefaultEditCallbacks(xlCallbacks *cb)
{
    static xlEditCallbacks callbacks;

    /* setup to call the listener routines */
    cb->consoleGetC = xlListenerGetC;
    cb->consolePutC = xlListenerPutC;
    cb->consoleAtBOLP = xlListenerAtBOLP;
    cb->consoleFlushInput = xlListenerFlushInput;
    cb->consoleFlushOutput = xlListenerFlushOutput;
    cb->consoleCheck = xlListenerCheck;
    
    /* setup the default callbacks */
    callbacks.waitForInput = osWaitForInput;
    callbacks.scrollWindow = osScrollWindow;
    callbacks.adjustScrollBar = osAdjustScrollBar;
    callbacks.startOutput = osStartOutput;
    callbacks.finishOutput = osFinishOutput;
    callbacks.outputLine = osOutputLine;
    callbacks.setCursorPosition = osSetCursorPosition;
    return &callbacks;
}

/* xlMakeEditWindow - make an edit window */
xlEXPORT xlEditWindow *xlMakeEditWindow(void *window,xlEditBuffer *buf)
{
    xlEditWindow *win = (xlEditWindow *)malloc(sizeof(xlEditWindow));
    if (win) {
        
        /* make a buffer pointer to the top line */
        win->topline = xlMakeBufferPointer(buf);
        win->ntopline = 0;
        
        /* make a buffer pointer for the insertion point and other position */
        win->dot = xlMakeBufferPointer(buf);
        win->other = xlMakeBufferPointer(buf);
        
        /* make sure the allocation requests succeeded */
        if (win->topline == NULL || win->dot == NULL || win->other == NULL) {
            if (win->topline) xlFreeBufferPointer(win->topline);
            if (win->dot) xlFreeBufferPointer(win->dot);
            if (win->other) xlFreeBufferPointer(win->other);
            free(win);
            return NULL;
        }

        /* initialize the new edit window */
        win->window = window;
        win->buffer = buf;
        win->desiredx = -1;
        win->otherSet = FALSE;
    
        /* setup the input ring buffer */
        win->inptr = win->outptr = win->charbuf;
        win->charcnt = 0;
    }
    return win;
}

/* xlEditWindowSetup - setup an edit window */
xlEXPORT void xlEditWindowSetup(xlEditWindow *win)
{
    xlBufferPointer *bp = xlMakeBufferPointer(win->buffer);
    xlBufferPointer *topline = win->topline;
    long nLines = 0;
    int ch;
    
    /* count the number of lines in the buffer */
    if (bp) {
        while ((ch = xlBufferGetC(bp)) != EOF)
            if (ch == '\n')
                ++nLines;
        xlFreeBufferPointer(bp);
    }
    ++nLines;
    
    /* compute the number of lines to skip */
    xlBufferSetOffset(topline,0);
    xlBufferSetLineNumber(topline,0);
    if ((nLines -= win->height) > 0) {
        win->ntopline = nLines;
        while ((ch = xlBufferGetC(topline)) != EOF)
            if (ch == '\n' && --nLines <= 0)
                break;
    }
    else
        win->ntopline = 0;
    (*callbacks->setCursorPosition)(win);
}

/* xlFreeEditWindow - free an edit window */
xlEXPORT void xlFreeEditWindow(xlEditWindow *win)
{
    xlFreeBufferPointer(win->topline);
    xlFreeBufferPointer(win->dot);
    free(win);
}

/* xlEditWindowBufferC - put a character into an edit window input buffer */
xlEXPORT void xlEditWindowBufferC(int ch,xlEditWindow *win)
{
    if (win->charcnt < xlEditMax) {
        *win->inptr++ = ch; win->charcnt++;
        if (win->inptr >= &win->charbuf[xlEditMax])
            win->inptr = win->charbuf;
    }
}

/* xlEditWindowStuffBuffer - stuff an edit window input buffer */
xlEXPORT void xlEditWindowStuffBuffer(xlEditWindow *win,char *buf)
{
    int length = strlen(buf);
    short *p = win->charbuf;
    if (length > xlEditMax) length = xlEditMax;
    win->charcnt = length;
    win->inptr = win->charbuf + length;
    win->outptr = win->charbuf;
    while (--length >= 0)
        *p++ = *buf++;
}

/* xlEditWindowGetC - get a character from an edit window */
xlEXPORT int xlEditWindowGetC(xlEditWindow *win)
{
    int ch;
    if (win->charcnt == 0) {
        xlEditWindowRedrawCurrentLine(win,0);
        (*callbacks->waitForInput)(win);
    }
    ch = *win->outptr++; win->charcnt--;
    if (win->outptr >= &win->charbuf[xlEditMax])
        win->outptr = win->charbuf;
    return ch;
}

/* xlEditWindowPutC - put a character into an edit window */
xlEXPORT int xlEditWindowPutC(int ch,xlEditWindow *win)
{
    xlEditWindowScrollToCursor(win);
    if (xlBufferPutC(ch,win->dot) != EOF) {
        if (ch == '\n') {
            xlBufferPrevC(win->dot);
            xlEditWindowRedrawCurrentLine(win,0);
            xlBufferNextC(win->dot);
            xlEditWindowScroll(win);
            (*callbacks->adjustScrollBar)(win);
        }
        xlEditWindowCheckForDelimiter(win);
        (*callbacks->setCursorPosition)(win);
    }
    win->desiredx = -1;
    return ch;
}

/* xlEditWindowScroll - scroll an edit window */
static void xlEditWindowScroll(xlEditWindow *win)
{
    long cursory = xlBufferGetLineNumber(win->dot);
    
    /* scroll the whole window down */
    if (cursory < win->ntopline) {
        if (xlBufferPrevL(win->topline,0) != EOF) {
            (*callbacks->scrollWindow)(win,0,1);
            --win->ntopline;
        }
    }
    
    /* scroll the whole window up */
    else if (cursory >= win->ntopline + win->height) {
        if (xlBufferNextL(win->topline,0) != EOF) {
            (*callbacks->scrollWindow)(win,win->height,-1);
            ++win->ntopline;
        }
    }
    
    /* scroll part of the window down */
    else
        (*callbacks->scrollWindow)(win,cursory - win->ntopline,1);

    /* set the cursor position */
    (*callbacks->setCursorPosition)(win);
}

/* xlEditWindowNextC - forward a character */
xlEXPORT int xlEditWindowNextC(xlEditWindow *win)
{
    long cursory = xlBufferGetLineNumber(win->dot);
    int ch = xlBufferNextC(win->dot);
    xlEditWindowScrollToCursor(win);
    if (ch != EOF) {
        if (ch == '\n' && cursory >= win->ntopline + win->height - 1)
            xlEditWindowScroll(win);
        xlEditWindowCheckForDelimiter(win);
        (*callbacks->setCursorPosition)(win);
        win->desiredx = -1;
        return ch;
    }
    return EOF;
}

/* xlEditWindowPrevC - backward a character */
xlEXPORT int xlEditWindowPrevC(xlEditWindow *win)
{
    long cursory = xlBufferGetLineNumber(win->dot);
    int ch = xlBufferPrevC(win->dot);
    xlEditWindowScrollToCursor(win);
    if (ch != EOF) {
        if (ch == '\n' && cursory <= win->ntopline)
            xlEditWindowScroll(win);
        xlEditWindowCheckForDelimiter(win);
        (*callbacks->setCursorPosition)(win);
        win->desiredx = -1;
        return ch;
    }
    return EOF;
}

/* xlEditWindowNextL - forward a line */
xlEXPORT int xlEditWindowNextL(xlEditWindow *win)
{
    long cursorx,cursory;
    char *line;
    xlBufferGetPosition(win->dot,&line,&cursorx,&cursory);
    xlEditWindowScrollToCursor(win);
    if (win->desiredx == -1)
        win->desiredx = cursorx;
    if (xlBufferNextL(win->dot,win->desiredx) != EOF) {
        if (cursory >= win->ntopline + win->height - 1)
            xlEditWindowScroll(win);
        xlEditWindowCheckForDelimiter(win);
        (*callbacks->setCursorPosition)(win);
        return 0;
    }
    return EOF;
}

/* xlEditWindowPrevL - backward a line */
xlEXPORT int xlEditWindowPrevL(xlEditWindow *win)
{
    long cursorx,cursory;
    char *line;
    xlBufferGetPosition(win->dot,&line,&cursorx,&cursory);
    xlEditWindowScrollToCursor(win);
    if (win->desiredx == -1)
        win->desiredx = cursorx;
    if (xlBufferPrevL(win->dot,win->desiredx) != EOF) {
        if (cursory <= win->ntopline)
            xlEditWindowScroll(win);
        xlEditWindowCheckForDelimiter(win);
        (*callbacks->setCursorPosition)(win);
        return 0;
    }
    return EOF;
}

/* xlEditWindowDeleteC - delete the character to the right of the cursor */
xlEXPORT int xlEditWindowDeleteC(xlEditWindow *win)
{
    long nlines = win->buffer->nlines;
    int ch = xlBufferDeleteC(win->dot);
    xlEditWindowScrollToCursor(win);
    if (ch != EOF) {
        if (nlines != win->buffer->nlines)
            (*callbacks->adjustScrollBar)(win);
        xlEditWindowRedrawCurrentLine(win,xlBufferGetLineOffset(win->dot));
        win->desiredx = -1;
        return ch;
    }
    return EOF;
}

/* xlEditWindowClearInputBuffer - clear the input buffer */
static void xlEditWindowClearInputBuffer(xlEditWindow *win)
{
    win->inptr = win->outptr = win->charbuf;
    win->charcnt = -1;
}

/* xlEditWindowScrollToCursor - scroll cursor into the window if it isn't already */
static void xlEditWindowScrollToCursor(xlEditWindow *win)
{
    long cursory = xlBufferGetLineNumber(win->dot);
    if (cursory < win->ntopline
    ||  cursory >= win->ntopline + win->height)
        xlEditWindowScrollToLine(win,cursory);
}

/* xlEditWindowScrollToLine - scroll to the specified line */
xlEXPORT void xlEditWindowScrollToLine(xlEditWindow *win,long line)
{
    xlBufferPointer *topline = win->topline;
    long count = line;
    int ch;
    
    /* find the specified line */
    topline->offset = 0;
    if (count > 0)
        while ((ch = xlBufferGetC(topline)) != EOF)
            if (ch == '\n' && --count <= 0)
                break;
    
    /* move the top line */
    win->ntopline = line - count;
    
    /* adjust the scroll bar */
    (*callbacks->adjustScrollBar)(win);

    /* redraw the window */
    xlEditWindowRedraw(win,FALSE);
    (*callbacks->setCursorPosition)(win);
}

/* xlEditWindowRedrawCurrentLine - redraw the current line starting at the cursor */
static void xlEditWindowRedrawCurrentLine(xlEditWindow *win,long x)
{
    long winy = xlBufferGetLineNumber(win->dot) - win->ntopline;
    char *buffer;
    long length;
    if (winy >= 0 && winy < win->height) {
        xlBufferGetLine(win->dot,&buffer,&length);
        (*callbacks->startOutput)(win,FALSE);
        (*callbacks->outputLine)(win,x,winy,buffer,length - x);
        (*callbacks->finishOutput)(win,FALSE);
    }
}

/* xlEditWindowRedraw - redraw an edit window */
xlEXPORT void xlEditWindowRedraw(xlEditWindow *win,int update)
{
    xlBufferPointer *topline = win->topline;
    long saveoffset = topline->offset;
    int count = win->height;
    long length,y;
    char *buffer;
    (*callbacks->startOutput)(win,update);
    for (y = 0; --count >= 0; ++y) {
        if (xlBufferAtEOF(topline))
            (*callbacks->outputLine)(win,0,y,"",0);
        else {
            xlBufferGetLine(topline,&buffer,&length);
            (*callbacks->outputLine)(win,0,y,buffer,length);
            while (--length >= 0)
                xlBufferNextC(topline);
            xlBufferNextC(topline);
        }
    }
    topline->offset = saveoffset;
    (*callbacks->finishOutput)(win,update);
}

/* xlEditWindowFindLine - find a line in an edit buffer */
xlEXPORT int xlEditWindowFindLine(xlEditWindow *win,int *py,char **pline,long *plength)
{
    long y = win->ntopline + *py;
    int found = xlBufferFindLine(win->buffer,&y,pline,plength);
    *py = y - win->ntopline;
    return found;
}

/* xlEditWindowSetCursorPosition - set the position of the cursor */
xlEXPORT void xlEditWindowSetCursorPosition(xlEditWindow *win,int x,int y)
{
    long cursorx = x;
    long cursory = win->ntopline + y;
    
    /* move the cursor */
    xlBufferSetPosition(win->dot,&cursorx,&cursory);
    
    /* update the cursor position */
    (*callbacks->setCursorPosition)(win);
    
    /* check for delimiters */
    xlEditWindowCheckForDelimiter(win);
}

/* xlEditWindowGetCursorPosition - get the position of the cursor */
xlEXPORT int xlEditWindowGetCursorPosition(xlEditWindow *win,char **pline,int *px,int *py)
{
    char *line;
    long x,y;
    xlBufferGetPosition(win->dot,&line,&x,&y);
    y -= win->ntopline;
    if (y >= 0 && y < win->height) {
        *pline = line;
        *px = (int)x;
        *py = (int)y;
        return TRUE;
    }
    return FALSE;
}
   
/* xlEditWindowGetOtherPosition - get the position of the matching delimiter */
xlEXPORT int xlEditWindowGetOtherPosition(xlEditWindow *win,char **pline,int *px,int *py)
{
    char *line;
    long x,y;
    if (win->otherSet) {
        xlBufferGetPosition(win->other,&line,&x,&y);
        y -= win->ntopline;
        if (y >= 0 && y < win->height) {
            *pline = line;
            *px = (int)x;
            *py = (int)y;
            return TRUE;
        }
    }
    return FALSE;
}

/* xlEditWindowCheckForDelimiter - check for a delimiter to the left of the cursor */
static void xlEditWindowCheckForDelimiter(xlEditWindow *win)
{
    int ch = xlBufferPrevC(win->dot);
    switch (ch) {
    case EOF:
        break;
    case ')':
        xlEditWindowFindMatchingDelimiter(win,'(',')');
        xlBufferNextC(win->dot);
        break;
    case '"':
        xlEditWindowFindMatchingDelimiter(win,'"',EOF);
        xlBufferNextC(win->dot);
        break;
    default:
        win->otherSet = FALSE;
        xlBufferNextC(win->dot);
        break;
    }
}

/* xlEditWindowFindMatchingDelimiter - find a matching delimiter */
static void xlEditWindowFindMatchingDelimiter(xlEditWindow *win,int lch,int rch)
{
    long level = 1;
    int ch;
    
    /* start at the current position */
    xlCopyBufferPointer(win->other,win->dot);
    
    /* find the matching left paren */
    while ((ch = xlBufferPrevC(win->other)) != EOF)
        if (ch == lch && --level <= 0)
            break;
        else if (ch == rch)
            ++level;
        
    /* remember the position */
    win->otherSet = ch == lch && level <= 0;
}

/* default handlers */
static void osWaitForInput(xlEditWindow *win) {}
static void osScrollWindow(xlEditWindow *win,long y,long amount) {}
static void osAdjustScrollBar(xlEditWindow *win) {}
static void osStartOutput(xlEditWindow *win,int update) {}
static void osFinishOutput(xlEditWindow *win,int update) {}
static void osOutputLine(xlEditWindow *win,long x,long y,char *buffer,long length) {}
static void osSetCursorPosition(xlEditWindow *win) {}
