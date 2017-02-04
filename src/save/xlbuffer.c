/* xlbuffer.c - a simple editor for XLISP */
/*	Copyright (c) 1997, by David Michael Betz
	All Rights Reserved
	Permission is granted for unrestricted non-commercial use	*/

#include <stdio.h>
#include <stdlib.h>
#include "xledit.h"

/* prototypes */
static void UpdateBufferPointers(xlEditBuffer *buf,long offset,long delta,long deltaLines);

/* xlMakeBuffer - make an edit buffer */
xlEXPORT xlEditBuffer *xlMakeBuffer(long size)
{
    xlEditBuffer *buf = (xlEditBuffer *)malloc(sizeof(xlEditBuffer));
    if (buf) {
        buf->base = malloc(size);
        if (buf->base == NULL) {
            free(buf);
            return NULL;
        }
        buf->file = NULL;
        buf->changed = FALSE;
        buf->nlines = 1;
        buf->next = buf->base;
        buf->top = buf->base + size;
        buf->pointers = NULL;
    }
    return buf;
}

/* xlFreeBuffer - free an edit buffer */
xlEXPORT void xlFreeBuffer(xlEditBuffer *buf)
{
    xlBufferPointer *bp,*nextp;
    for (bp = buf->pointers; bp != NULL; bp = nextp) {
        nextp = bp->next;
        free(bp);
    }
    free(buf->base);
    free(buf);
}

/* xlMakeBufferPointer - make a buffer pointer */
xlEXPORT xlBufferPointer *xlMakeBufferPointer(xlEditBuffer *buf)
{
    xlBufferPointer *bp = (xlBufferPointer *)malloc(sizeof(xlBufferPointer));
    if (bp) {
        bp->buffer = buf;
        bp->offset = 0;
        bp->lineNumber = 0;
        bp->next = buf->pointers;
        buf->pointers = bp;
    }
    return bp;
}

/* xlCopyBufferPointer - copy a buffer pointer */
xlEXPORT void xlCopyBufferPointer(xlBufferPointer *dst,xlBufferPointer *src)
{
    dst->offset = src->offset;
    dst->lineNumber = src->lineNumber;
}

/* xlFreeBufferPointer - free a buffer pointer */
xlEXPORT void xlFreeBufferPointer(xlBufferPointer *bp)
{
    xlBufferPointer *p,**pprev;
    for (pprev = &bp->buffer->pointers; (p = *pprev) != NULL; pprev = &p->next)
        if (p == bp) {
            *pprev = p->next;
            break;
        }
    free(bp);
}

/* xlDeleteRange - delete a range of characters in a buffer */
xlEXPORT void xlDeleteRange(xlEditBuffer *buf,long offset,long length)
{
    char *dst = buf->base + offset;
    char *src = dst + length;
    long count = length;
    long lines = 0;
    while (--count >= 0)
	if (*src++ == '\n')
	    ++lines;
    buf->nlines -= lines;
    for (count = length, src = dst + length; --count >= 0; )
        *dst++ = *src++;
    UpdateBufferPointers(buf,offset,-length,-lines);
    buf->changed = TRUE;
}

/* UpdateBufferPointers - update buffer pointers after an edit */
static void UpdateBufferPointers(xlEditBuffer *buf,long offset,long delta,long deltaLines)
{
    xlBufferPointer *bp;
    for (bp = buf->pointers; bp != NULL; bp = bp->next)
        if (bp->offset > offset) {
            if ((bp->offset += delta) < offset)
                bp->offset = offset;
            bp->lineNumber += deltaLines;
        }
}

/* xlBufferGetC - get a character from a buffer at a pointer */
xlEXPORT int xlBufferGetC(xlBufferPointer *bp)
{
    char *p = bp->buffer->base + bp->offset;
    if (p < bp->buffer->next) {
	int ch = *p;
	if (ch == '\n')
	    ++bp->lineNumber;
	++bp->offset;
	return ch;
    }
    return EOF;
}

/* xlBufferPutC - put a character into a buffer at a pointer */
xlEXPORT int xlBufferPutC(int ch,xlBufferPointer *bp)
{
    xlEditBuffer *buf = bp->buffer;
    if (buf->next < buf->top) {
        char *p = buf->base + bp->offset;
        char *src = buf->next;
        char *dst = ++buf->next;
        while (src > p)
            *--dst = *--src;
        UpdateBufferPointers(buf,bp->offset,1,ch == '\n' ? 1 : 0);
        if (ch == '\n') {
            ++bp->lineNumber;
            ++buf->nlines;
        }
        ++bp->offset;
        buf->changed = TRUE;
        return *p = ch;    
    }
    return EOF;
}

/* xlBufferNextC - move forward one character */
xlEXPORT int xlBufferNextC(xlBufferPointer *bp)
{
    xlEditBuffer *buf = bp->buffer;
    char *p = buf->base + bp->offset;
    if (p < buf->next) {
        int ch = *p;
        if (ch == '\n')
            ++bp->lineNumber;
        ++bp->offset;
        return ch;
    }
    return EOF;    
}

/* xlBufferPrevC - move backward one character */
xlEXPORT int xlBufferPrevC(xlBufferPointer *bp)
{
    xlEditBuffer *buf = bp->buffer;
    char *p = buf->base + bp->offset;
    if (p > buf->base) {
        int ch = p[-1];
        if (ch == '\n')
            --bp->lineNumber;
        --bp->offset;
        return ch;
    }
    return EOF;    
}

/* xlBufferNextL - move forward one line */
xlEXPORT int xlBufferNextL(xlBufferPointer *bp,long targetx)
{
    xlEditBuffer *buf = bp->buffer;
    char *p = buf->base + bp->offset;
    while (p < buf->next)
        if (*p++ == '\n') {
            while (p < buf->next && *p != '\n' && --targetx >= 0)
                ++p;
            bp->offset = p - bp->buffer->base;
            ++bp->lineNumber;
            return 0;
        }
    return EOF;
}

/* xlBufferPrevL - move backward one line */
xlEXPORT int xlBufferPrevL(xlBufferPointer *bp,long targetx)
{
    xlEditBuffer *buf = bp->buffer;
    char *p = buf->base + bp->offset;
    while (p > buf->base)
        if (*--p == '\n') {
            char *last = p;
            while (p > buf->base && p[-1] != '\n')
                --p;
            if (last - p > targetx)
                bp->offset = p - bp->buffer->base + targetx;
            else
                bp->offset = last - bp->buffer->base;
            --bp->lineNumber;
            return 0;
        }
    return EOF;
}

/* xlBufferDeleteC - delete the character to the right of the pointer */
xlEXPORT int xlBufferDeleteC(xlBufferPointer *bp)
{
    xlEditBuffer *buf = bp->buffer;
    char *dst = buf->base + bp->offset;
    if (dst < buf->next) {
        char *src = dst + 1;
        int ch = *dst;
        while (src < buf->next)
            *dst++ = *src++;
        UpdateBufferPointers(buf,bp->offset,-1,ch == '\n' ? -1 : 0);
        if (ch == '\n') --buf->nlines;
        --buf->next;
        buf->changed = TRUE;
        return ch;
    }
    return EOF;
}

/* xlBufferSetPosition - set the position of a buffer pointer */
xlEXPORT void xlBufferSetPosition(xlBufferPointer *bp,long *px,long *py)
{
    xlEditBuffer *buf = bp->buffer;
    char *p = buf->base;
    char *start;
    long x,y;
        
    /* find the line */
    for (y = 0, start = p; y < *py && p < buf->next; )
        if (*p++ == '\n') {
            start = p;
            ++y;
        }
    
    /* find the line offset */
    for (x = 0, p = start; x < *px && p < buf->next && *p != '\n'; ++p, ++x)
        ;
        
    /* set the new offset */
    bp->offset = p - buf->base;
    
    /* return the actual position */
    bp->lineNumber = y;
    *px = x; *py = y;
}

/* xlBufferGetPosition - get the position of a buffer pointer */
xlEXPORT void xlBufferGetPosition(xlBufferPointer *bp,char **pline,long *px,long *py)
{
    char *p = bp->buffer->base + bp->offset;
    long x = 0;
     
    /* find the start of the line */
    while (p > bp->buffer->base) {
        if (*--p == '\n') {
            ++p;
            break;
        }
        ++x;
    }
    *pline = p;
    *px = x;
    *py = bp->lineNumber;
}

/* xlBufferFindLine - find a line in a buffer */
xlEXPORT int xlBufferFindLine(xlEditBuffer *buf,long *py,char **pline,long *plength)
{
    char *p = buf->base;
    char *lastLine = p;
    long y = 0;
    int found;
     
    /* find the start of the line */
    while (y < *py && p < buf->next)
        if (*p++ == '\n') {
            lastLine = p;
            ++y;
        }
    found = y == *py;
    
    /* compute the length of the line */
    *py = y;
    *pline = p = lastLine;
    *plength = 0;
    while (p < buf->next) {
        if (*p++ == '\n')
            break;
        ++*plength;
    }
    return found;
}

/* xlBufferGetLine - get a line from a buffer pointer */
xlEXPORT void xlBufferGetLine(xlBufferPointer *bp,char **pline,long *plength)
{
    char *p = bp->buffer->base + bp->offset;
    
    /* find the start of the line */
    while (p > bp->buffer->base)
        if (*--p == '\n') {
            ++p;
            break;
        }
    
    /* compute the length of the line */
    *pline = p;
    *plength = 0;
    while (p < bp->buffer->next) {
        if (*p++ == '\n')
            break;
        ++*plength;
    }
}

/* xlBufferGetLineOffset - get the offset within line of a buffer pointer */
xlEXPORT long xlBufferGetLineOffset(xlBufferPointer *bp)
{
    char *p = bp->buffer->base + bp->offset;
    long offset = 0;
     
    /* find the start of the line */
    while (p > bp->buffer->base) {
        if (*--p == '\n')
            break;
        ++offset;
    }
    return offset;
}
    
/* xlBufferAtEOF - is a buffer pointer at the end of the buffer? */
xlEXPORT int xlBufferAtEOF(xlBufferPointer *bp)
{
    xlEditBuffer *buf = bp->buffer;
    return buf->base + bp->offset == buf->next;
}