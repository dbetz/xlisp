/* xledit.h - definitions for xledit.c */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#ifndef __XLEDIT__
#define __XLEDIT__

#include "xlisp.h"

/* program limits */
#define xlEditMax       100     /* maximum number of buffered input characters */

/* special key codes */
#define xlEBKC_LEFT_ARROW       0x100
#define xlEBKC_RIGHT_ARROW      0x101
#define xlEBKC_UP_ARROW         0x102
#define xlEBKC_DOWN_ARROW       0x103

/* types */
typedef struct xlEditWindow xlEditWindow;
typedef struct xlEditBuffer xlEditBuffer;
typedef struct xlBufferPointer xlBufferPointer;

/* window structure */
struct xlEditWindow {
  
  /* miscellaneous fields */
  void *window;             /* the window */
  void *data;               /* a place for client data */

  /* edit buffer fields */
  xlEditBuffer *buffer;     /* the buffer */
  xlBufferPointer *topline; /* top line displayed in window */
  xlBufferPointer *dot;     /* cursor pointer */
  long ntopline;            /* number of the top line */
  long desiredx;            /* desired column for vertical cursor motions */
  xlBufferPointer *other;   /* other pointer (matching delimiter) */
  int otherSet;             /* boolean indicating if other pointer is set */
  
  /* window dimension fields */
  int height;               /* window height in lines */
  
  /* type ahead buffer fields */
  short charbuf[xlEditMax]; /* type ahead buffer */
  short *inptr,*outptr;     /* type ahead buffer pointers */
  int charcnt;              /* type ahead character count */
};

/* buffer structure */
struct xlEditBuffer {
  char *file;
  int changed;
  long nlines;
  char *base;
  char *next;
  char *top;
  xlBufferPointer *pointers;
};

/* buffer pointer structure */
struct xlBufferPointer {
  xlEditBuffer *buffer;
  long offset;
  long lineNumber;
  xlBufferPointer *next;
};

/* macros */
#define xlBufferGetOffset(p)        ((p)->offset)
#define xlBufferSetOffset(p,o)      ((p)->offset = (o))
#define xlBufferGetLineNumber(p)    ((p)->lineNumber)
#define xlBufferSetLineNumber(p,n)  ((p)->lineNumber = (n))

/* callback structure */
typedef struct {
  void (*waitForInput)(xlEditWindow *win);
  void (*scrollWindow)(xlEditWindow *win,long y,long amount);
  void (*adjustScrollBar)(xlEditWindow *win);
  void (*startOutput)(xlEditWindow *win,int update);
  void (*finishOutput)(xlEditWindow *win,int update);
  void (*outputLine)(xlEditWindow *win,long x,long y,char *buffer,long length);
  void (*setCursorPosition)(xlEditWindow *win);
} xlEditCallbacks;

/* globals */
xlEXPORT extern xlEditWindow *xlListener;

/* xlterm.c functions */
xlEXPORT void xlSetEditCallbacks(xlEditCallbacks *cb);
xlEXPORT xlEditCallbacks *xlDefaultEditCallbacks(xlCallbacks *cb);
xlEXPORT xlEditWindow *xlMakeListenerWindow(void *window,long size);
xlEXPORT void xlTermPutC(int ch,xlEditWindow *win);
int xlListenerGetC(void);
void xlListenerPutC(int ch);
int xlListenerAtBOLP(void);
void xlListenerFlushInput(void);
void xlListenerFlushOutput(void);
int xlListenerCheck(void);

/* xledit.c functions */
xlEXPORT xlEditWindow *xlMakeEditWindow(void *window,xlEditBuffer *buf);
xlEXPORT void xlEditWindowSetup(xlEditWindow *win);
xlEXPORT void xlFreeEditWindow(xlEditWindow *win);
xlEXPORT void xlEditWindowBufferC(int ch,xlEditWindow *win);
xlEXPORT void xlEditWindowStuffBuffer(xlEditWindow *win,char *buf);
xlEXPORT int xlEditWindowGetC(xlEditWindow *win);
xlEXPORT int xlEditWindowPutC(int ch,xlEditWindow *win);
xlEXPORT int xlEditWindowNextC(xlEditWindow *win);
xlEXPORT int xlEditWindowPrevC(xlEditWindow *win);
xlEXPORT int xlEditWindowNextL(xlEditWindow *win);
xlEXPORT int xlEditWindowPrevL(xlEditWindow *win);
xlEXPORT int xlEditWindowDeleteC(xlEditWindow *win);
xlEXPORT void xlEditWindowRedraw(xlEditWindow *win,int update);
xlEXPORT void xlEditWindowScrollToLine(xlEditWindow *win,long line);
xlEXPORT int xlEditWindowFindLine(xlEditWindow *win,int *py,char **pline,long *plength);
xlEXPORT void xlEditWindowSetCursorPosition(xlEditWindow *win,int x,int y);
xlEXPORT int xlEditWindowGetCursorPosition(xlEditWindow *win,char **pline,int *px,int *py);
xlEXPORT int xlEditWindowGetOtherPosition(xlEditWindow *win,char **pline,int *px,int *py);

/* xlbuffer.c functions */
xlEXPORT xlEditBuffer *xlMakeBuffer(long size);
xlEXPORT void xlFreeBuffer(xlEditBuffer *buf);
xlEXPORT xlBufferPointer *xlMakeBufferPointer(xlEditBuffer *buf);
xlEXPORT void xlCopyBufferPointer(xlBufferPointer *dst,xlBufferPointer *src);
xlEXPORT void xlFreeBufferPointer(xlBufferPointer *bp);
xlEXPORT void xlDeleteRange(xlEditBuffer *buf,long offset,long length);
xlEXPORT int xlBufferGetC(xlBufferPointer *bp);
xlEXPORT int xlBufferPutC(int ch,xlBufferPointer *bp);
xlEXPORT int xlBufferNextC(xlBufferPointer *bp);
xlEXPORT int xlBufferPrevC(xlBufferPointer *bp);
xlEXPORT int xlBufferNextL(xlBufferPointer *bp,long targetx);
xlEXPORT int xlBufferPrevL(xlBufferPointer *bp,long targetx);
xlEXPORT int xlBufferDeleteC(xlBufferPointer *bp);
xlEXPORT void xlBufferGetPosition(xlBufferPointer *bp,char **pline,long *px,long *py);
xlEXPORT void xlBufferSetPosition(xlBufferPointer *bp,long *px,long *py);
xlEXPORT int xlBufferFindLine(xlEditBuffer *buf,long *py,char **pline,long *plength);
xlEXPORT void xlBufferGetLine(xlBufferPointer *bp,char **pline,long *plength);
xlEXPORT long xlBufferGetLineOffset(xlBufferPointer *bp);
xlEXPORT int xlBufferAtEOF(xlBufferPointer *bp);

#endif
