/* xlbcode.h - xlisp compiler byte code definitions */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#ifndef __XLBCODE_H__
#define __XLBCODE_H__

#define xlopNOP         0x00    /* nop */
#define xlopBRT         0x01    /* branch on true */
#define xlopBRF         0x02    /* branch on false */
#define xlopBR          0x03    /* branch unconditionally */
#define xlopLIT         0x04    /* load literal */
#define xlopGREF        0x05    /* global symbol value */
#define xlopGSET        0x06    /* set global symbol value */
#define xlopEREF        0x07    /* environment variable value */
#define xlopIREF        0x08    /* instance variable value (must be xlopEREF + 1) */
#define xlopESET        0x09    /* set environment variable value */
#define xlopISET        0x0A    /* set instance variable value (must be xlopESET + 1) */
#define xlopCALL        0x0B    /* call a function */
#define xlopTCALL       0x0C    /* tail recursive call */
#define xlopRETURN      0x0D    /* return from a function */
#define xlopT           0x0E    /* load 'val' with t */
#define xlopNIL         0x0F    /* load 'val' with nil */
#define xlopPUSH        0x10    /* push the 'val' register */
#define xlopCLOSE       0x11    /* create a closure */

#define xlopARGSEQ      0x12    /* argument count == m, n extra slots */
#define xlopARGSGE      0x13    /* argument count >= min, m extra slots */
#define xlopARGSBT      0x14    /* argument count >= min, <= max, n extra slots */
#define xlopOPTARG      0x15    /* check for an &optional argument */
#define xlopREST        0x16    /* build a &rest list with arguments n... */
#define xlopKEYARG      0x17    /* check for a &key argument */

#define xlopATOM        0x18    /* atom predicate */
#define xlopEQ          0x19    /* eq? predicate */
#define xlopNULL        0x1A    /* null? (or not) predicate */
#define xlopCONS        0x1B    /* cons */
#define xlopCAR         0x1C    /* car */
#define xlopCDR         0x1D    /* cdr */
#define xlopSETCAR      0x1E    /* set-car! */
#define xlopSETCDR      0x1F    /* set-cdr! */
#define xlopDELAY       0x20    /* create a promise */

#define xlopADD         0x21    /* add two numeric expressions */
#define xlopSUB         0x22    /* subtract two numeric expressions */
#define xlopMUL         0x23    /* multiply two numeric expressions */
#define xlopQUO         0x24    /* divide two integer expressions */
#define xlopLSS         0x25    /* less than */
#define xlopEQL         0x26    /* equal to */
#define xlopGTR         0x27    /* greater than */

#define xlopFRAME       0x28    /* create a new environment frame */
#define xlopUNFRAME     0x29    /* remove an environment frame */
#define xlopMETHOD      0x2A    /* mark the current frame as a method frame */

#define xlopMVFRAME     0x2B    /* create a multiple value environment frame */
#define xlopMVPUSH      0x2C    /* push multiple values */
#define xlopMVCALL      0x2D    /* call a function with multiple values */
#define xlopMVTCALL     0x2E    /* tail recursive MVCALL */
#define xlopMVPOP       0x2F    /* pop multiple values */

#define xlopCATCH       0x30    /* push a throw target frame */
#define xlopUNCATCH     0x31    /* pop a throw target frame */
#define xlopPROTECT     0x32    /* push a protect frame */
#define xlopUNPROTECT   0x33    /* pop a protect frame */

#define xlopENV         0x34    /* the current environment */

#endif
