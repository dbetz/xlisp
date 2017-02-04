/* xlisp.h - xlisp definitions */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#ifndef __XLISP_H__
#define __XLISP_H__

#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

/* xlAFMT       printf format for addresses ("%x") */
/* xlOFFTYPE    number the size of an address (int) */
/* xlFIXTYPE    data type for fixed point numbers (long) */
/* xlUFIXTYPE   data type for fixed point numbers (unsigned long) */
/* xlICNV       fixed point input conversion routine (atol) */
/* xlIFMT       printf format for fixed point numbers ("%ld") */
/* xlXFMT       printf format for hexadecimal fixed point numbers (%lx) */
/* xlFLOTYPE    data type for floating point numbers (float) */
/* xlFFMT       printf format for floating point numbers (%.15g) */
/* xlCHRTYPE    data type for characters */
/* xlSTACKSIZE  size of control/data stack */
/* xlEXPORT     storage class for exported functions */

/* use zero for nil for now */
#define xlNil           (xlValue)0

/* size of each type of memory segment */
#ifndef xlNSSIZE        /* number of nodes per node segment */
#define xlNSSIZE        20000
#endif
#ifndef xlVSSIZE        /* number of xlValue's per vector segment */
#define xlVSSIZE        200000
#endif

/* default important definitions */
#ifndef xlAFMT
#define xlAFMT          "%p"
#endif
#ifndef xlOFFTYPE
#define xlOFFTYPE       long
#endif
#ifndef xlFIXTYPE
#define xlFIXTYPE       long
typedef unsigned long   xlUFIXTYPE;
#endif
#ifndef xlICNV
#define xlICNV(n)       atol(n)
#endif
#ifndef xlIFMT
#define xlIFMT          "%ld"
#endif
#ifndef xlXFMT
#define xlXFMT          "%lx"
#endif
#ifndef xlFLOTYPE
#define xlFLOTYPE       double
#endif
#ifndef xlFFMT
#define xlFFMT          "%.15g"
#endif
#ifndef xlSFIXMIN
#define xlSFIXMIN       -1073741824L
#define xlSFIXMAX       1073741823L
#endif
#ifndef xlCHRTYPE
#define xlCHRTYPE       int
#endif
#ifndef xlINSEGMENT
#define xlINSEGMENT(n,s)    ((n) >= &(s)->ns_data[0] \
                          && (n) <  &(s)->ns_data[0] + (s)->ns_size)
#endif
#ifndef xlVCOMPARE
#define xlVCOMPARE(f,s,t)   ((s) <= (xlFIXTYPE)((t) - (f)))
#endif
#ifndef xlVSFREE
#define xlVSFREE(f,t)       ((xlFIXTYPE)((t) - (f)))
#endif
#ifndef xlSTACKSIZE
#define xlSTACKSIZE     65536
#endif
#ifndef xlEXPORT
#if     !defined(XLISPDLL)
#define xlEXPORT
#else
#if !defined(MINGW)
#if defined(WIN32) || defined(__WIN32__)
#if defined(WINDLL) || defined(__DLL__)
#define xlEXPORT    __declspec(dllexport)
#else
#define xlEXPORT    __declspec(dllimport)
#endif
#else
#define xlEXPORT
#endif
#else
#define xlEXPORT
#endif
#endif
#endif

/* useful definitions */
#ifndef TRUE
#define TRUE    1
#define FALSE   0
#endif

/* program limits */
#define xlSTRMAX        256             /* maximum length of string constant */
#define xlHSIZE         199             /* symbol hash table size */
#define xlSRATE         1000            /* control character sample rate */

/* value stack manipulation macros */
#define xlCheck(n)      do { \
                            if (xlSP - (n) <= xlCSP) \
                                xlStkOver(); \
                        } while (0)
#define xlCPush(v)      do { \
                            xlCheck(1); \
                            xlPush(v); \
                        } while (0) 
#define xlPush(v)       (*--xlSP = (v))
#define xlPop()         (*xlSP++)
#define xlTop()         (*xlSP)
#define xlSetTop(v)     (*xlSP = (v))
#define xlDrop(n)       (xlSP += (n))

/* control stack manipulation macros */
#define xlCtlCheck(n)   do { \
                            if (xlCSP + (n) >= xlSP) \
                                xlCStkOver(); \
                        } while (0)
#define xlCtlPush(v)    (*xlCSP++ = (v))
#define xlCtlPop()      (*--xlCSP)
#define xlCtlDrop(n)    (xlCSP -= (n))

/* argument list parsing macros */
#define xlGetArg()      xlTestArg(xlNextArg())
#define xlLastArg()     do { \
                            if (xlMoreArgsP()) \
                                xlTooMany(); \
                        } while (0)
#define xlPopArgs()     xlDrop(xlArgC)
#define xlTestArg(e)    (xlMoreArgsP() ? (e) : xlTooFew())
#define xlTypeArg(tp)   (tp(*xlSP) ? xlNextArg() : xlBadType(*xlSP))
#define xlNextArg()     (--xlArgC, *xlSP++)
#define xlMoreArgsP()   (xlArgC > 0)

/* macros to get arguments of a particular type */
#define xlGetArgCons()          xlTestArg(xlTypeArg(xlConsP))
#define xlGetArgList()          xlTestArg(xlTypeArg(xlListP))
#define xlGetArgSymbol()        xlTestArg(xlTypeArg(xlSymbolP))
#define xlGetArgString()        xlTestArg(xlTypeArg(xlStringP))
#define xlGetArgFixnum()        xlTestArg(xlTypeArg(xlFixnumP))
#define xlGetArgFlonum()        xlTestArg(xlTypeArg(xlFlonumP))
#define xlGetArgNumber()        xlTestArg(xlTypeArg(xlNumberP))
#define xlGetArgChar()          xlTestArg(xlTypeArg(xlCharacterP))
#define xlGetArgVector()        xlTestArg(xlTypeArg(xlVectorP))
#define xlGetArgPort()          xlTestArg(xlTypeArg(xlPortP))
#define xlGetArgInputPort()     xlTestArg(xlTypeArg(xlInputPortP))
#define xlGetArgOutputPort()    xlTestArg(xlTypeArg(xlOutputPortP))
#define xlGetArgFStream()       xlTestArg(xlTypeArg(xlFileStreamP))
#define xlGetArgUnnamedStream() xlTestArg(xlTypeArg(xlUnnamedStreamP))
#define xlGetArgClosure()       xlTestArg(xlTypeArg(xlClosureP))
#define xlGetArgEnv()           xlTestArg(xlTypeArg(xlEnvironmentP))
#define xlGetArgObject()        xlTestArg(xlTypeArg(xlObjectP))
#define xlGetArgForeignPtr()    xlTestArg(xlTypeArg(xlForeignPtrP))
#define xlGetArgTable()         xlTestArg(xlTypeArg(xlTableP))

/* node types */
#define xlFREE          0
#define xlCONS          1
#define xlSYMBOL        2
#define xlFIXNUM        3
#define xlFLONUM        4
#define xlSTRING        5
#define xlFSTREAM       6       /* file stream */
#define xlUSTREAM       7       /* unnamed stream */
#define xlOSTREAM       8       /* object stream */
#define xlVECTOR        9
#define xlCLOSURE       10
#define xlCODE          11
#define xlSUBR          12
#define xlXSUBR         13
#define xlCONTINUATION  14
#define xlCHARACTER     15
#define xlPROMISE       16
#define xlENV           17      /* heap environment */
#define xlSENV          18      /* stack environment (must be xlSENV + 1) */
#define xlMSENV         19      /* moved stack environment (or method environment) */
#define xlMENV          20      /* method environment */
#define xlSMENV         21      /* stack method environment (must be xlMENV + 1) */
#define xlOBJECT        22
#define xlPACKAGE       23
#define xlFOREIGNPTR    24      /* foreign pointer */
#define xlTABLE         25
#define xlMAXTYPEID     25

/* update the typename table in xlprin.c if you add more type ids to the above list */

/* node flags */
#define xlMARK          1
#define xlLEFT          2

/* port flags */
#define xlpfINPUT       0x0001
#define xlpfOUTPUT      0x0002
#define xlpfBINARY      0x0004
#define xlpfTERMINAL    0x0008
#define xlpfBOL         0x0010

/* new node access macros */
#define xlNodeType(x)           ((xlOFFTYPE)(x) & 1 ? xlFIXNUM : (x)->type)
#define xlTypeIs(x,t)           ((x) != xlNil && xlNodeType(x) == (t))
#define xlPointerTypeIs(x,t)    ((x)->type == (t))

/* macro to determine if a non-nil value is a pointer */
#define xlPointerP(x)           ((x) != xlNil && ((xlOFFTYPE)(x) & 1) == 0)

/* type predicates */                          
#define xlAtomP(x)          ((x) == xlNil || xlNodeType(x) != xlCONS)
#define xlNullP(x)          ((x) == xlNil)
#define xlListP(x)          ((x) == xlNil || xlNodeType(x) == xlCONS)
#define xlNumberP(x)        ((x) != xlNil && (xlNodeType(x) == xlFIXNUM || xlNodeType(x) == xlFLONUM))
#define xlBoundP(x)         (xlGetValue(x) != xlUnboundObject)
#define xlPortP(x)          (xlPointerP(x) \
                          && (xlPointerTypeIs(x,xlFSTREAM) \
                           || xlPointerTypeIs(x,xlUSTREAM) \
                           || xlPointerTypeIs(x,xlOSTREAM)))
#define xlInputPortP(x)     (xlPortP(x) && (xlGetPFlags(x) & xlpfINPUT) != 0)
#define xlOutputPortP(x)    (xlPortP(x) && (xlGetPFlags(x) & xlpfOUTPUT) != 0)

/* basic type predicates */                            
#define xlConsP(x)                  xlTypeIs(x,xlCONS)
#define xlStringP(x)                xlTypeIs(x,xlSTRING)
#define xlSymbolP(x)                xlTypeIs(x,xlSYMBOL)
#define xlFileStreamP(x)            xlTypeIs(x,xlFSTREAM)
#define xlUnnamedStreamP(x)         xlTypeIs(x,xlUSTREAM)
#define xlObjectStreamP(x)          xlTypeIs(x,xlOSTREAM)
#define xlFixnumP(x)                xlTypeIs(x,xlFIXNUM)
#define xlFlonumP(x)                xlTypeIs(x,xlFLONUM)
#define xlVectorP(x)                xlTypeIs(x,xlVECTOR)
#define xlClosureP(x)               xlTypeIs(x,xlCLOSURE)
#define xlContinuationP(x)          xlTypeIs(x,xlCONTINUATION)
#define xlCodeP(x)                  xlTypeIs(x,xlCODE)
#define xlSubrP(x)                  xlTypeIs(x,xlSUBR)
#define xlXSubrP(x)                 xlTypeIs(x,xlXSUBR)
#define xlCharacterP(x)             xlTypeIs(x,xlCHARACTER)
#define xlPromiseP(x)               xlTypeIs(x,xlPROMISE)
#define xlEnvironmentP(x)           ((x) == xlNil \
                                  || xlNodeType(x) == xlENV \
                                  || xlNodeType(x) == xlSENV \
                                  || xlNodeType(x) == xlMENV \
                                  || xlNodeType(x) == xlSMENV)
#define xlStackEnvironmentP(x)      ((x) != xlNil \
                                  && (xlNodeType(x) == xlSENV \
                                   || xlNodeType(x) == xlSMENV))
#define xlMovedStackEnvironmentP(x) xlTypeIs(x,xlMSENV)
#define xlMethodEnvironmentP(x)     ((x) != xlNil \
                                  && (xlNodeType(x) == xlMENV \
                                   || xlNodeType(x) == xlSMENV))
#define xlObjectP(x)                xlTypeIs(x,xlOBJECT)
#define xlBooleanP(x)               ((x) == xlNil || xlNodeType(x) == BOOLEAN)
#define xlPackageP(x)               xlTypeIs(x,xlPACKAGE)
#define xlForeignPtrP(x)            xlTypeIs(x,xlFOREIGNPTR)
#define xlTableP(x)                 xlTypeIs(x,xlTABLE)

/* cons access macros */
#define xlCar(x)                ((x)->value.cons.car)
#define xlCdr(x)                ((x)->value.cons.cdr)
#define xlSetCar(x,y)           ((x)->value.cons.car = (y))
#define xlSetCdr(x,y)           ((x)->value.cons.cdr = (y))

/* package access macros */
#define xlGetNames(x)           xlGetElement(x,0)
#define xlSetNames(x,v)         xlSetElement(x,0,v)
#define xlGetExtern(x)          xlGetElement(x,1)
#define xlSetExtern(x,v)        xlSetElement(x,1,v)
#define xlGetIntern(x)          xlGetElement(x,2)
#define xlSetIntern(x,v)        xlSetElement(x,2,v)
#define xlGetUses(x)            xlGetElement(x,3)
#define xlSetUses(x,v)          xlSetElement(x,3,v)
#define xlGetUsedBy(x)          xlGetElement(x,4)
#define xlSetUsedBy(x,v)        xlSetElement(x,4,v)
#define xlGetNextPackage(x)     xlGetElement(x,5)
#define xlSetNextPackage(x,v)   xlSetElement(x,5,v)
#define xlPACKAGESIZE           6

/* symbol access macros */
#define xlGetValue(x)           xlGetElement(x,0)
#define xlSetValue(x,v)         xlSetElement(x,0,v)
#define xlGetPName(x)           xlGetElement(x,1)
#define xlSetPName(x,v)         xlSetElement(x,1,v)
#define xlGetPList(x)           xlGetElement(x,2)
#define xlSetPList(x,v)         xlSetElement(x,2,v)
#define xlGetPackage(x)         xlGetElement(x,3)
#define xSetPackage(x,v)        xlSetElement(x,3,v)
#define xlSYMBOLSIZE            4

/* vector access macros */
#define xlGetSize(x)            ((x)->value.vector.size)
#define xlSetSize(x,v)          ((x)->value.vector.size = (v))
#define xlGetElement(x,i)       ((x)->value.vector.data[i])
#define xlSetElement(x,i,v)     do { \
                                    xlValue vutmp = (v); \
                                    (x)->value.vector.data[i] = vutmp; \
                                } while (0)
#define xlGetVector(x)          ((x)->value.vector.data)
#define xlSetVector(x,v)        ((x)->value.vector.data = (v))

/* environment access macros */
#define xlGetFrameType(x)       ((x)->type)
#define xlSetFrameType(x,v)     ((x)->type = (v))
#define xlGetEnvSize(x)         xlGetSize(x)
#define xlSetEnvSize(x,v)       ((x)->value.vector.size = (v))
#define xlGetNextFrame(x)       xlGetElement(x,0)
#define xlSetNextFrame(x,v)     xlSetElement(x,0,v)
#define xlGetEnvNames(x)        xlGetElement(x,1)
#define xlSetEnvNames(x,v)      xlSetElement(x,1,v)
#define xlGetEnvElement(x,i)    xlGetElement(x,i)
#define xlSetEnvElement(x,i,v)  xlSetElement(x,i,v)
#define xlFIRSTENV              2

/* moved stack environment access macros */
#define xlGetForwardingAddr(x)  xlGetElement(x,0)
#define xlSetForwardingAddr(x,v) xlSetElement(x,0,v)

/* object access macros */
#define xlGetClass(x)           xlGetElement(x,0)
#define xlSetClass(x,v)         xlSetElement(x,0,v)
#define xlGetIVar(x,i)          xlGetElement(x,i)
#define xlSetIVar(x,i,v)        xlSetElement(x,i,v)
#define xlFIRSTIVAR             1

/* instance variable numbers for the class 'Class' */
#define xlivNAME                (xlFIRSTIVAR+0) /* class name */
#define xlivMESSAGES            (xlFIRSTIVAR+1) /* list of messages */
#define xlivIVARS               (xlFIRSTIVAR+2) /* list of instance variable names */
#define xlivCVARS               (xlFIRSTIVAR+3) /* env containing class variables */
#define xlivSUPERCLASS          (xlFIRSTIVAR+4) /* pointer to the superclass */
#define xlivIVARCNT             (xlFIRSTIVAR+5) /* number of class instance variables */
#define xlivIVARTOTAL           (xlFIRSTIVAR+6) /* total number of instance variables */

/* number of instance variables for the class 'Class' */
#define xlCLASSSIZE     7

/* promise access macros */
#define xlGetPProc(x)           xlCar(x)
#define xlSetPProc(x,v)         xlSetCar(x,v)
#define xlGetPValue(x)          xlCdr(x)
#define xlSetPValue(x,v)        xlSetCdr(x,v)

/* closure access macros */
#define xlGetCode(x)            xlCar(x)
#define xlGetEnvironment(x)     xlCdr(x)

/* code access macros */
#define xlGetBCode(x)           xlGetElement(x,0)
#define xlSetBCode(x,v)         xlSetElement(x,0,v)
#define xlGetCName(x)           xlGetElement(x,1)
#define xlSetCName(x,v)         xlSetElement(x,1,v)
#define xlGetVNames(x)          xlGetElement(x,2)
#define xlSetVNames(x,v)        xlSetElement(x,2,v)
#define xlGetCodeStr(x)         ((unsigned char *)xlGetString(xlGetBCode(x)))
#define xlFIRSTLIT              3

/* fixnum/flonum/character access macros */
#define xlGetFixnum(x)          ((xlOFFTYPE)(x) & 1 ? xlGetSmallFixnum(x) \
                                                    : (x)->value.fixnum)
#define xlGetFlonum(x)          ((x)->value.flonum)
#define xlGetChCode(x)          ((x)->value.chcode)

/* small fixnum access macros */
#define xlFastMakeFixnum(x)     (xlSmallFixnumP(x) ? xlMakeSmallFixnum(x) \
                                                   : xlMakeFixnum(x))   
#define xlMakeSmallFixnum(x)    ((xlValue)(((xlOFFTYPE)(x) << 1) | 1))
#define xlGetSmallFixnum(x)     ((xlFIXTYPE)((xlOFFTYPE)(x) >> 1))
#define xlSmallFixnumP(x)       ((x) >= xlSFIXMIN && (x) <= xlSFIXMAX)
        
/* string access macros */
#define xlGetString(x)          ((char *)(x)->value.vector.data)
#define xlGetSLength(x)         xlGetSize(x)
#define xlSetSLength(x,v)       xlSetSize(x,v)

/* fstream/ustream/ostream access macros */
#define xlGetFile(x)            ((FILE *)xlGetSData(x))
#define xlGetUStream(x)         ((xlValue)xlGetSData(x))
#define xlGetSObject(x)         ((xlValue)xlGetSData(x))
#define xlGetSData(x)           ((x)->value.stream.data)
#define xlSetSData(x,v)         ((x)->value.stream.data = (void *)(v))
#define xlGetSaveCh(x)          ((x)->value.stream.savech)
#define xlSetSaveCh(x,v)        ((x)->value.stream.savech = (v))
#define xlGetPFlags(x)          ((x)->value.stream.pflags)
#define xlSetPFlags(x,v)        ((x)->value.stream.pflags = (v))

/* unnamed stream access macros */
#define xlGetStrHead(x)         xlGetElement(xlGetUStream(x),0)
#define xlSetStrHead(x,v)       xlSetElement(xlGetUStream(x),0,v)
#define xlGetStrTail(x)         xlGetElement(xlGetUStream(x),1)
#define xlSetStrTail(x,v)       xlSetElement(xlGetUStream(x),1,v)
#define xlGetStrIPtr(x)         xlGetElement(xlGetUStream(x),2)
#define xlSetStrIPtr(x,v)       xlSetElement(xlGetUStream(x),2,v)
#define xlGetStrOPtr(x)         xlGetElement(xlGetUStream(x),3)
#define xlSetStrOPtr(x,v)       xlSetElement(xlGetUStream(x),3,v)
#define xlUSTRSIZE              4
#define xlUSTRBUFSIZE           4096

/* subr access macros */
#define xlGetSubr(x)            ((x)->value.subr.subr)
#define xlGetSubrName(x)        ((x)->value.subr.name)

/* foreign pointer access macros */
#define xlGetFPType(x)          ((x)->value.foreignptr.type)
#define xlSetFPType(x,v)        ((x)->value.foreignptr.type = (v))
#define xlGetFPtr(x)            ((x)->value.foreignptr.ptr)
#define xlSetFPtr(x,v)          ((x)->value.foreignptr.ptr = (v))

/* forward type declarations */
typedef struct xlCClass xlCClass;

/* node structure */
typedef struct xlNode xlNode,*xlValue;
struct xlNode {
    char type;              /* type of node */
    char flags;             /* flag bits */
    union xlNodeValue {     /* value */
        xlFIXTYPE fixnum;       /* fixnum node */
        xlFLOTYPE flonum;       /* flonum node */
        xlCHRTYPE chcode;       /* character node */
        struct {                /* cons node */
            xlValue car;            /* the car pointer */
            xlValue cdr;            /* the cdr pointer */
        } cons;
        struct {                /* vector node */
            xlFIXTYPE size;         /* vector size */
            xlValue *data;          /* vector data */
        } vector;
        struct {                /* stream node */
            void *data;             /* the file pointer/stream/object */
            short savech;           /* lookahead character */
            short pflags;           /* port flags */
        } stream;
        struct {                /* subr/fsubr node */
            xlValue (*subr)(void);  /* function pointer */
            char *name;             /* function name */
        } subr;
        struct {                /* foreign pointer node */
            xlCClass *type;           /* type symbol */
            void *ptr;              /* the pointer */
        } foreignptr;
    } value;
};

/* memory allocator definitions */

/* number of pointers in a protected pointer block */
#define xlPPBSIZE       100

typedef struct xlProtectedPtrBlk {
    struct xlProtectedPtrBlk *next;
    xlValue *pointers[xlPPBSIZE];
    xlValue **free;
    int count;
} xlProtectedPtrBlk;

/* macros to compute the size of a segment */
#define xlNSegSize(n) (sizeof(xlNodeSegment) + ((n) - 1) * sizeof(xlNode))
#define xlVSegSize(n) (sizeof(xlVectorSegment) + ((n) - 1) * sizeof(xlValue))

/* macro to convert a byte size to a word size */
#define xlByteToWordSize(n)     (((n) + sizeof(xlValue) - 1) / sizeof(xlValue))

/* number of LVALs in >= the size of a xlNode */
#define xlNODEWORDS     xlByteToWordSize(sizeof(xlNode))

/* node segment structure */
typedef struct xlNodeSegment xlNodeSegment;
struct xlNodeSegment {
    xlNodeSegment *ns_next;     /* next node segment */
    xlFIXTYPE ns_size;          /* number of nodes in this segment */
    xlNode ns_data[1];          /* segment data */
};

/* vector segment structure */
typedef struct xlVectorSegment xlVectorSegment;
struct xlVectorSegment {
    xlVectorSegment *vs_next;   /* next vector segment */
    xlValue *vs_free;           /* next free location in this segment */
    xlValue *vs_top;            /* top of segment (plus one) */
    xlValue vs_data[1];         /* segment data */
};

/* node space */
extern xlFIXTYPE xlNSSize;              /* node segment size */
extern xlNodeSegment *xlNSegments;      /* list of node segments */
extern int xlNSCount;                   /* number of node segments */
extern xlFIXTYPE xlNNodes;              /* total number of nodes */
extern xlFIXTYPE xlNFree;               /* number of nodes in free list */

/* vector (and string) space */
extern xlFIXTYPE xlVSSize;              /* vector segment size */
extern xlVectorSegment *xlVSegments;    /* list of vector segments */
extern int xlVSCount;                   /* number of vector segments */
extern xlValue *xlVFree;                /* next free location in vector space */
extern xlValue *xlVTop;                 /* top of vector space */

/* memory manager variables */
extern xlFIXTYPE xlTotal;               /* total number of bytes of memory in use */
extern xlFIXTYPE xlGCCalls;             /* number of calls to the garbage collector */

/* argument list */
extern char **xlCmdLineArgV;
extern int xlCmdLineArgC;

/* subr definition structure */
typedef struct {
    char *name;
    xlValue (*subr)(void);
} xlSubrDef;

/* xsubr definition structure */
typedef struct {
    char *name;
    void (*subr)(void);
} xlXSubrDef;

/* instance variable holding the foreign pointer for a C class */
#define xlHANDLEIVAR    xlFIRSTIVAR

/* free handler type */
typedef void xlCClassFreeHandler(void *ptr);

/* macro to set the foreign pointer of a C class */
#define xlSetCObjectPtr(o,p) xlSetFPtr(xlGetIVar((o),xlHANDLEIVAR),(p))

/* C class definition structure */
typedef struct xlCClassDef xlCClassDef;
struct xlCClassDef {
    char *name;
    xlSubrDef *methods;
    xlXSubrDef *xmethods;
    xlCClassFreeHandler *free;
};

/* C class structure */
struct xlCClass {
    xlCClass *next;
    xlCClassDef *def;
    xlValue cls;
};

/* continuation dispatch structure */
typedef struct {
    void (*cd_restore)(void);
    xlValue *(*cd_mark)(xlValue *);
    xlValue *(*cd_unmark)(xlValue *);
    void (*cd_unwind)(void);
    xlValue *(*cd_unstack)(xlValue *);
    xlValue *(*cd_print)(xlValue *);
} xlCDispatch;

/* macros to call the continuation dispatch functions */
#define xlCDRestore()   ((*((xlCDispatch *)xlCtlPop())->cd_restore)())
#define xlCDMark(p)     (--(p), (*((xlCDispatch *)*(p))->cd_mark)(p))
#define xlCDUnmark(p)   (--(p), (*((xlCDispatch *)*(p))->cd_unmark)(p))
#define xlCDUnwind()    ((*((xlCDispatch *)xlCtlPop())->cd_unwind)())
#define xlCDUnstack(p)  (--(p), (*((xlCDispatch *)*(p))->cd_unstack)(p))
#define xlCDPrint(p)    (--(p), (*((xlCDispatch *)*(p))->cd_print)(p))

/* return a single value */
#define xlSVReturn()    do { \
                            xlArgC = 1; \
                            xlCDRestore(); \
                            return; \
                        } while (0)
#define xlMVReturn(n)   do { \
                            int argcxxx = (n); \
                            xlArgC = argcxxx; \
                            xlVal = argcxxx > 0 ? *xlSP : xlNil; \
                            xlDrop(xlArgC); \
                            xlCDRestore(); \
                            return; \
                        } while (0)

/* C continuation structure */
typedef struct {
    void (*cc_cont)(void);      /* continuation function */
    void (*cc_unwind)(void);    /* unwind function (or NULL) */
    int cc_cnt;                 /* number of values on control stack */
    char *cc_names;             /* names of the values on the stack (comma separated) */
} xlCContinuation;

/* error target structure */
typedef struct xlErrorTarget {
    jmp_buf target;
    struct xlErrorTarget *next;
} xlErrorTarget;

/* callback structure */
typedef struct {
    char *(*loadPath)(void);
    char *(*parsePath)(char **pp);
    int (*directorySeparator)(void);
    xlValue (*(*findSubr)(char *name))(void);
    void (*error)(char *msg);
    int (*fileModTime)(char *fname,xlFIXTYPE *pModTime);
    int (*consoleGetC)(void);
    void (*consolePutC)(int ch);
    int (*consoleAtBOLP)(void);
    void (*consoleFlushInput)(void);
    void (*consoleFlushOutput)(void);
    int (*consoleCheck)(void);
    void (*exit)(int sts);
} xlCallbacks;

/* external variables */
xlEXPORT extern int xlInitializedP;     /* true if initialization is done */
xlEXPORT extern FILE *xlTranscriptFP;   /* transcript file pointer */
xlEXPORT extern xlValue *xlStkBase;     /* base of value stack */
xlEXPORT extern xlValue *xlStkTop;      /* top of value stack */
xlEXPORT extern xlValue *xlSP;          /* value stack pointer */
xlEXPORT extern xlValue *xlCSP;         /* control stack pointer */
xlEXPORT extern int xlArgC;             /* number of arguments remaining */
xlEXPORT extern void (*xlNext)(void);   /* pointer to the next function to call (xlApply) */
xlEXPORT extern int xlDebugModeP;       /* true to turn off tail recursion */

/* virtual machine registers */
xlEXPORT extern xlValue xlVal;
xlEXPORT extern xlValue xlFun;
xlEXPORT extern xlValue xlEnv;

/* important values */
xlEXPORT extern xlValue xlTrue;
xlEXPORT extern xlValue xlFalse;

/* important symbols */
xlEXPORT extern xlValue xlSymReadTable;
xlEXPORT extern xlValue xlSymNMacro;
xlEXPORT extern xlValue xlSymTMacro;
xlEXPORT extern xlValue xlSymWSpace;
xlEXPORT extern xlValue xlSymConst;
xlEXPORT extern xlValue xlSymSEscape;
xlEXPORT extern xlValue xlSymMEscape;

/* API status codes */
#define xlsSuccess      0
#define xlsEndOfFile    -1
#define xlsError        -2

/* xlmain.c */
xlEXPORT int xlInit(xlCallbacks *callbacks,int argc,char *argv[],char *workspace);
xlEXPORT char *xlBanner(void);
xlEXPORT void xlContinue(void);
xlEXPORT void xlBreak(void);
xlEXPORT void xlCleanup(void);
xlEXPORT void xlTopLevel(void);
xlEXPORT void xlWrapUp(void);
xlEXPORT void xlError(char *msg,xlValue arg);
xlEXPORT void xlFmtError(char *fmt,...);
xlEXPORT void xlAbort(char *msg,xlValue arg);
xlEXPORT void xlFmtAbort(char *fmt,...);
xlEXPORT void xlFatal(char *fmt,...);
xlEXPORT void xlInfo(char *fmt,...);
void xlShowErr(xlValue fun);

/* xlapi.c */
xlEXPORT void xlSetSoftwareType(char *type);
xlEXPORT int xlCallFunction(xlValue *values,int vmax,xlValue fun,int argc,...);
xlEXPORT int xlCallFunctionByName(xlValue *values,int vmax,char *fname,int argc,...);
xlEXPORT int xlSendMessage(xlValue *values,int vmax,xlValue obj,xlValue selector,int argc,...);
xlEXPORT int xlSendMessageByName(xlValue *values,int vmax,xlValue obj,char *selector,int argc,...);
xlEXPORT int xlEvaluateCString(xlValue *values,int vmax,char *str);
xlEXPORT int xlEvaluateString(xlValue *values,int vmax,char *str,xlFIXTYPE len);
xlEXPORT int xlEvaluate(xlValue *values,int vmax,xlValue expr);
xlEXPORT int xlLoadFile(char *fname);
xlEXPORT FILE *xlLoadOpen(char *name,char *mode,char *pathsym,char *rpath);
xlEXPORT int xlReadFromCString(char *str,xlValue *pval);
xlEXPORT int xlReadFromString(char *str,xlFIXTYPE len,xlValue *pval);
xlEXPORT char *xlWriteToString(xlValue expr,char *buf,xlFIXTYPE len);
xlEXPORT char *xlDisplayToString(xlValue expr,char *buf,xlFIXTYPE len);
xlEXPORT void xlFreeString(char *str);
xlEXPORT xlValue xlGetArgInstance(xlValue cls);
xlEXPORT void xlSetIdleHandler(void (*handler)(void *data),void *data);
xlEXPORT int xlIdle(void);

/* xlcom.c */
xlValue xlCompile(xlValue expr,xlValue ctenv);
xlValue xlCompileMethod(xlValue fun,xlValue fargs,xlValue body,xlValue ctenv);
xlEXPORT xlFIXTYPE xlLength(xlValue list);

/* xldbg.c */
void xlDecodeProcedure(xlValue fptr,xlValue code,xlValue env);
int xlDecodeInstruction(xlValue fptr,xlValue code,xlFIXTYPE lc,xlValue env);

/* xldmem.c */
xlEXPORT xlValue xlCons(xlValue x,xlValue y);
xlEXPORT xlValue xlNewFrame(int type,xlValue parent,xlFIXTYPE size);
xlEXPORT xlValue xlMakeString(char *str,xlFIXTYPE len);
xlEXPORT xlValue xlMakeCString(char *str);
xlEXPORT xlValue xlCopyString(xlValue str);
xlEXPORT xlValue xlMakeFileStream(FILE *fp,short flags);
xlEXPORT xlValue xlMakeUnnamedStream(char *buf,xlFIXTYPE len);
xlEXPORT xlValue xlMakeObjectStream(xlValue obj,short flags);
xlEXPORT xlValue xlMakeSymbol(xlValue pname);
xlEXPORT xlValue xlMakeFixnum(xlFIXTYPE n);
xlEXPORT xlValue xlMakeFlonum(xlFLOTYPE n);
xlEXPORT xlValue xlMakeChar(int ch);
xlEXPORT xlValue xlMakeClosure(xlValue code,xlValue env);
xlEXPORT xlValue xlMakePromise(xlValue code,xlValue env);
xlEXPORT xlValue xlMakeSubr(char *name,xlValue (*fcn)(void));
xlEXPORT xlValue xlMakeXSubr(char *name,void (*fcn)(void));
xlEXPORT xlValue xlNewPackage(char *name);
xlEXPORT xlValue xlNewVector(xlFIXTYPE size);
xlEXPORT xlValue xlNewTable(xlFIXTYPE size);
xlEXPORT xlValue xlNewString(xlFIXTYPE size);
xlEXPORT xlValue xlNewCode(xlFIXTYPE nlits);
xlEXPORT xlValue xlNewStream(int type,short flags);
xlEXPORT xlValue xlNewUStream(void);
xlEXPORT xlValue xlNewContinuation(xlFIXTYPE size);
xlEXPORT xlValue xlMakeForeignPtr(xlCClass *type,void *p);
int xlCheckVMemory(xlFIXTYPE size);
int xlMakeVMemory(xlFIXTYPE size);
int xlNExpand(xlFIXTYPE size);
int xlVExpand(xlFIXTYPE size);
xlNodeSegment *xlNewNSegment(xlFIXTYPE n);
xlVectorSegment *xlNewVSegment(xlFIXTYPE n);
xlEXPORT void xlGC(void);
void xlMark(xlValue);
void xlCheckVectorSpace(void);
xlEXPORT int xlProtect(xlValue *p);
xlEXPORT int xlUnprotect(xlValue *p);
char *xlCopyCString(char *str);
void xlInitMemory(xlFIXTYPE ssize);
void xlResetStack(void);

/* xlfasl.c */
xlValue xloadfaslfile(void);
xlValue xfaslwriteprocedure(void);
xlValue xfaslreadprocedure(void);

/* xlftab.c */
void xlInitFunctions(void);
xlValue (*xlFindSubr(char *name))(void);

/* xlfun1.c */
xlValue xcons(void);
xlValue xacons(void);
xlValue xcar(void);
xlValue xicar(void);
xlValue xcdr(void);
xlValue xicdr(void);
xlValue xcaar(void);
xlValue xcadr(void);
xlValue xcdar(void);
xlValue xcddr(void);
xlValue xcaaar(void);
xlValue xcaadr(void);
xlValue xcadar(void);
xlValue xcaddr(void);
xlValue xcdaar(void);
xlValue xcdadr(void);
xlValue xcddar(void);
xlValue xcdddr(void);
xlValue xcaaaar(void);
xlValue xcaaadr(void);
xlValue xcaadar(void);
xlValue xcaaddr(void);
xlValue xcadaar(void);
xlValue xcadadr(void);
xlValue xcaddar(void);
xlValue xcadddr(void);
xlValue xcdaaar(void);
xlValue xcdaadr(void);
xlValue xcdadar(void);
xlValue xcdaddr(void);
xlValue xcddaar(void);
xlValue xcddadr(void);
xlValue xcdddar(void);
xlValue xcddddr(void);
xlValue xsetcar(void);
xlValue xisetcar(void);
xlValue xsetcdr(void);
xlValue xisetcdr(void);
xlValue xnappend(void);
xlValue xlist(void);
xlValue xliststar(void);
xlValue xpairlis(void);
xlValue xcopylist(void);
xlValue xcopytree(void);
xlValue xcopyalist(void);
xlValue xappend(void);
xlValue xreverse(void);
xlValue xlast(void);
xlValue xlength(void);
xlValue xxmember(void);
xlValue xxmemv(void);
xlValue xxmemq(void);
xlValue xxassoc(void);
xlValue xxassv(void);
xlValue xxassq(void);
xlValue xlistref(void);
xlValue xlisttail(void);
xlValue xmkpackage(void);
xlValue xfindpackage(void);
xlValue xlistallpackages(void);
xlValue xpackagename(void);
xlValue xpkgnicknames(void);
xlValue xinpackage(void);
xlValue xusepackage(void);
xlValue xpkguselist(void);
xlValue xunusepackage(void);
xlValue xpkgusedbylist(void);
xlValue xexport(void);
xlValue xunexport(void);
xlValue ximport(void);
void xintern(void);
void xfindsymbol(void);
xlValue xunintern(void);
xlValue xmksymbol(void);
xlValue xboundp(void);
xlValue xsymname(void);
xlValue xsymvalue(void);
xlValue xsetsymvalue(void);
xlValue xsympackage(void);
xlValue xsymplist(void);
xlValue xsetsymplist(void);
xlValue xget(void);
xlValue xput(void);
xlValue xremprop(void);
xlValue xprocenvironment(void);
xlValue xenvp(void);
xlValue xenvbindings(void);
xlValue xenvparent(void);
xlValue xobjectp(void);
xlValue xvector(void);
xlValue xmakevector(void);
xlValue xvlength(void);
xlValue xivlength(void);
xlValue xvref(void);
xlValue xivref(void);
xlValue xvset(void);
xlValue xivset(void);
xlValue xivbase(void);
xlValue xvectlist(void);
xlValue xlistvect(void);
xlValue xmakearray(void);
xlValue xaref(void);
xlValue xaset(void);
xlValue xmaketable(void);
xlValue xtablep(void);
xlValue xtableref(void);
xlValue xtableset(void);
xlValue xtableremove(void);
xlValue xemptytable(void);
xlValue xmapovertableentries(void);
xlValue xiaddrof(void);
xlValue xifmtaddr(void);
xlValue xnull(void);
xlValue xatom(void);
xlValue xlistp(void);
xlValue xendp(void);
xlValue xnumberp(void);
xlValue xbooleanp(void);
xlValue xpairp(void);
xlValue xsymbolp(void);
xlValue xintegerp(void);
xlValue xrealp(void);
xlValue xcharp(void);
xlValue xstringp(void);
xlValue xvectorp(void);
xlValue xprocedurep(void);
xlValue xdefaultobjectp(void);
xlValue xeq(void);
int xlEq(xlValue arg1,xlValue arg2);
xlValue xeqv(void);
int xlEqv(xlValue arg1,xlValue arg2);
xlValue xequal(void);
int xlEqual(xlValue arg1,xlValue arg2);
xlValue xidentity(void);
xlValue xgensym(void);
xlEXPORT int xlGetKeyArg(xlValue key,xlValue def,xlValue *pval);
xlEXPORT int xlGetKeyFixnum(xlValue key,xlFIXTYPE def,xlFIXTYPE *pval);
xlEXPORT int xlGetKeyString(xlValue key,xlValue def,xlValue *pval);
void xlGetTest(xlValue def,xlValue *pfcn,xlValue *ptresult);
xlEXPORT xlValue xlGetPort(void);
xlEXPORT xlValue xlGetInputPort(void);
xlEXPORT xlValue xlGetOutputPort(void);
xlValue xlGetEnv(void);

/* xlfun2.c */
xlValue xsymstr(void);
xlValue xstrsym(void);
xlValue xreadline(void);
xlValue xrdchar(void);
xlValue xunreadchar(void);
xlValue xpkchar(void);
xlValue xcharready(void);
xlValue xclearinput(void);
xlValue xrdbyte(void);
xlValue xrdshort(void);
xlValue xrdshorthf(void);
xlValue xrdshortlf(void);
xlValue xrdlong(void);
xlValue xrdlonghf(void);
xlValue xrdlonglf(void);
xlValue xeofobjectp(void);
xlValue xwrite(void);
xlValue xprint(void);
xlValue xwrchar(void);
xlValue xwrbyte(void);
xlValue xwrshort(void);
xlValue xwrshorthf(void);
xlValue xwrshortlf(void);
xlValue xwrlong(void);
xlValue xwrlonghf(void);
xlValue xwrlonglf(void);
xlValue xdisplay(void);
xlValue xnewline(void);
xlValue xfreshline(void);
xlValue xflushoutput(void);
xlValue xwritesize(void);
xlValue xdisplaysize(void);
xlValue xprbreadth(void);
xlValue xprdepth(void);
xlValue xparsepathstring(void);
void xsplitpathfromfilename(void);
xlValue xcombinepathwithfilename(void);
xlValue xfilemodtime(void);
xlValue xopeni(void);
xlValue xopeno(void);
xlValue xopena(void);
xlValue xopenu(void);
xlValue xclose(void);
xlValue xclosei(void);
xlValue xcloseo(void);
xlValue xgetfposition(void);
xlValue xsetfposition(void);
xlValue xcurinput(void);
xlEXPORT xlValue xlCurInput(void);
xlValue xcuroutput(void);
xlEXPORT xlValue xlCurOutput(void);
xlValue xcurerror(void);
xlEXPORT xlValue xlCurError(void);
xlValue xportp(void);
xlValue xinputportp(void);
xlValue xoutputportp(void);
xlValue xmkstrinput(void);
xlValue xmkstroutput(void);
xlValue xgetstroutput(void);
xlValue xmkobjstream(void);
xlValue xformat(void);
xlValue xtranson(void);
xlValue xtransoff(void);
xlValue xmakestring(void);
xlValue xstrlen(void);
xlValue xstrnullp(void);
xlValue xstrappend(void);
xlValue xstrref(void);
xlValue xstrset(void);
xlValue xsubstring(void);
xlValue xstrlist(void);
xlValue xliststring(void);
xlValue xstrlss(void);
xlValue xstrleq(void);
xlValue xstreql(void);
xlValue xstrneq(void);
xlValue xstrgeq(void);
xlValue xstrgtr(void);
xlValue xstrilss(void);
xlValue xstrileq(void);
xlValue xstrieql(void);
xlValue xstrineq(void);
xlValue xstrigeq(void);
xlValue xstrigtr(void);
xlValue xstrsearch(void);
xlValue xstrisearch(void);
xlValue xnumstr(void);
xlValue xstrnum(void);
xlValue xcharint(void);
xlValue xintchar(void);
xlValue xchrlss(void);
xlValue xchrleq(void);
xlValue xchreql(void);
xlValue xchrneq(void);
xlValue xchrgeq(void);
xlValue xchrgtr(void);
xlValue xchrilss(void);
xlValue xchrileq(void);
xlValue xchrieql(void);
xlValue xchrineq(void);
xlValue xchrigeq(void);
xlValue xchrigtr(void);
xlValue xupcase(void);
xlValue xdowncase(void);
xlValue xnupcase(void);
xlValue xndowncase(void);
xlValue xtrim(void);
xlValue xlefttrim(void);
xlValue xrighttrim(void);
xlValue xchupcase(void);
xlValue xchdowncase(void);
xlValue xdigitchar(void);
xlValue xstring(void);
xlValue xchar(void);
xlValue xuppercasep(void);
xlValue xlowercasep(void);
xlValue xbothcasep(void);
xlValue xdigitp(void);
xlValue xalphanumericp(void);
xlValue xwhitespacep(void);
xlValue xcompile(void);
xlValue xdecompile(void);
xlValue xsetdebugmode(void);
xlValue xsave(void);
xlValue xrestore(void);
xlValue xgc(void);
xlValue xroom(void);
xlValue xerror(void);
xlValue xgetarg(void);
void xshowstack(void);
void xshowcontrolstack(void);
void xshowvaluestack(void);
xlValue xgettime(void);
xlValue xgetenv(void);
xlValue xidle(void);
xlValue xexit(void);
xlValue xalloccmemory(void);
xlValue xfreecmemory(void);
xlValue xforeignptrp(void);
xlValue xforeignptrtype(void);
xlValue xsetforeignptrtype(void);
xlValue xforeignptrtypep(void);
xlValue xforeignptreqp(void);
xlValue xgetcrecfield(void);
xlValue xgetcrecfieldaddr(void);
xlValue xsetcrecfield(void);
xlValue xgetcrecstring(void);
xlValue xsetcrecstring(void);
xlValue xgetcrectypesize(void);
xlValue xnullpointerp(void);

/* xlfun3.c */
void xapply(void);
void xvalues(void);
void xvalueslist(void);
void xcallcc(void);
void xcallwi(void);
void xcallwo(void);
void xload(void);
void xloadnoisily(void);
void xforce(void);

/* xlimage.c */
int xlSaveImage(char *fname);
int xlRestoreImage(char *fname);

/* xlinit.c */
void xlInitWorkspace(int ssize);
void xlEnterSymbols(void);

/* xlint.c */
void xlInitInterpreter(void);
xlValue xtraceon(void);
xlValue xtraceoff(void);
void xthrow(void);
void xthrowerror(void);
xlValue xGetStackPointer(void);
void xGetStackFrame(void);
int xlInternalCall(xlValue *values,int vmax,xlValue fun,int argc,...);
int xlInvokeInterpreter(xlValue *values,int maxv,xlValue fun,xlValue sel,int argc,va_list ap);
void xlJumpToTarget(int sts);
void xlPushTarget(xlErrorTarget *target);
void xlPopTarget(void);
xlValue xlFindVar(xlValue env,xlValue var,int *poff);
void xlApply(void);
xlValue xlMakeContinuation(void);
xlValue xlUnstackEnvironment(xlValue);
void xlCallErrorHandler(void);
void xlThrowError(xlValue type);
xlValue xlFindTopProcedure(void);
void xlGCProtect(void (*protected_fcn)(void));
void xlShowCallStack(int cmax);
void xlShowControlStack(int cmax);
void xlShowValueStack(int vmax);
xlEXPORT void xlStkOver(void);
xlEXPORT void xlCStkOver(void);
xlEXPORT xlValue xlTooFew(void);
xlEXPORT void xlTooMany(void);
xlEXPORT xlValue xlBadType(xlValue val);

/* xlio.c */
xlEXPORT int xlGetC(xlValue fptr);
xlEXPORT void xlUngetC(xlValue fptr,int ch);
xlEXPORT int xlPeek(xlValue fptr);
xlEXPORT int xlInputReadyP(xlValue fptr);
xlEXPORT void xlFlushInput(xlValue fptr);
xlEXPORT void xlPutC(xlValue fptr,int ch);
xlEXPORT int xlAtBOLP(xlValue fptr);
xlEXPORT void xlFlush(void);
xlEXPORT xlFIXTYPE xlGetStrLength(xlValue stream);
xlEXPORT xlValue xlGetStrOutput(xlValue stream);
xlEXPORT void xlStdPrint(xlValue expr);
xlEXPORT void xlStdPutStr(char *str);
xlEXPORT void xlErrPrint(xlValue expr);
xlEXPORT void xlErrPutStr(char *str);

/* xliterseq.c */
void xfind(void);
void xfindif(void);
void xfindifnot(void);
void xmember(void);
void xmemberif(void);
void xmemberifnot(void);
void xassoc(void);
void xassocif(void);
void xassocifnot(void);
void xrassoc(void);
void xrassocif(void);
void xrassocifnot(void);
void xremove(void);
void xremoveif(void);
void xremoveifnot(void);
void xdelete(void);
void xdeleteif(void);
void xdeleteifnot(void);
void xcount(void);
void xcountif(void);
void xcountifnot(void);
void xposition(void);
void xpositionif(void);
void xpositionifnot(void);
void xmapcar(void);
void xmapc(void);
void xmapcan(void);
void xmaplist(void);
void xmapl(void);
void xmapcon(void);
void xsome(void);
void xevery(void);
void xnotany(void);
void xnotevery(void);

/* xlmath.c */
xlValue xexactp(void);
xlValue xinexactp(void);
xlValue xatan(void);
xlValue xfloor(void);
xlValue xceiling(void);
xlValue xround(void);
xlValue xtruncate(void);
xlValue xash(void);
xlValue xlsh(void);
xlValue xadd(void);
xlValue xmul(void);
xlValue xsub(void);
xlValue xdiv(void);
xlValue xquo(void);
xlValue xrem(void);
xlValue xmod(void);
xlValue xmin(void);
xlValue xmax(void);
xlValue xexpt(void);
xlValue xlogand(void);
xlValue xlogior(void);
xlValue xlogxor(void);
xlValue xlognot(void);
xlValue xabs(void);
xlValue xadd1(void);
xlValue xsub1(void);
xlValue xsin(void);
xlValue xcos(void);
xlValue xtan(void);
xlValue xasin(void);
xlValue xacos(void);
xlValue xxexp(void);
xlValue xsqrt(void);
xlValue xxlog(void);
xlValue xsetrandomseed(void);
xlValue xrandom(void);
xlValue xnegativep(void);
xlValue xzerop(void);
xlValue xpositivep(void);
xlValue xevenp(void);
xlValue xoddp(void);
xlValue xlss(void);
xlValue xleq(void);
xlValue xeql(void);
xlValue xneq(void);
xlValue xgeq(void);
xlValue xgtr(void);
xlEXPORT xlFLOTYPE xlCnvToFlotype(xlValue val);

/* xlobj.c */
xlEXPORT xlValue xlClass(char *name,xlValue super,char *vars);
xlEXPORT void xlMethod(xlValue cls,char *selector,xlValue (*handler)(void));
xlEXPORT void xlXMethod(xlValue cls,char *selector,void (*handler)(void));
xlEXPORT int xlInstanceP(xlValue cls,xlValue obj);
xlEXPORT xlValue xlNewInstance(xlValue cls);
int xlFindIVarOffset(xlValue cls,xlValue sym,int *pOffset);
void xlSend(xlValue obj,xlValue sym);
void xsendsuper(void);
void xlObSymbols(void);
void xlInitObjects(void);

/* xlcobj.c */
xlEXPORT xlCClass *xlMakeCClass(xlCClassDef *def,xlValue super);
xlEXPORT void *xlGetArgUninitializedCInstance(xlCClass *ccls);
xlEXPORT void *xlGetArgCInstance(xlCClass *ccls);
xlEXPORT void *xlMakeUninitializedCInstance(xlCClass *ccls);

/* xlprint.c */
xlEXPORT void xlDisplay(xlValue expr,xlValue file);
xlEXPORT xlFIXTYPE xlDisplaySize(xlValue val);
xlEXPORT void xlWrite(xlValue expr,xlValue file);
xlEXPORT xlFIXTYPE xlWriteSize(xlValue val);
xlEXPORT void xlNewline(xlValue fptr);
xlEXPORT void xlFreshLine(xlValue fptr);
xlEXPORT void xlPutStr(xlValue fptr,char *str);

/* xlread.c */
void xlInitReader(void);
xlEXPORT int xlRead(xlValue fptr,xlValue  *pval);
xlValue xread(void);
xlValue xreaddelimitedlist(void);
void xrmhash(void);
void xrmquote(void);
void xrmdquote(void);
void xrmbquote(void);
void xrmcomma(void);
void xrmlparen(void);
void xrmrparen(void);
void xrmsemi(void);
xlEXPORT xlValue xlCharType(int ch);
int xlNumberStringP(char *str,xlValue *pval);
int xlRadixNumberStringP(char *str,int radix,xlValue *pval);

/* xlsym.c */
void xlInitPackages(void);
xlEXPORT xlValue xlFindPackage(char *name);
int xlUsePackage(xlValue dst,xlValue src);
int xlUnusePackage(xlValue dst,xlValue src);
xlValue xlIntern(xlValue name,xlValue package,xlValue *pkey);
xlValue xlInternAndExport(char *name,xlValue package);
void xlUnintern(xlValue sym,xlValue package);
int xlVisibleP(xlValue sym,xlValue package);
int xlPresentP(xlValue sym,xlValue package);
xlValue xlFindSymbol(char *name,xlValue package,xlValue *pkey);
int xlImport(xlValue sym,xlValue package);
int xlImportAndExport(xlValue srcpack,char *name,xlValue dstpack);
int xlExport(xlValue sym,xlValue package);
int xlUnexport(xlValue sym,xlValue package);
xlEXPORT void xlSubr(char *name,xlValue (*fcn)(void));
xlEXPORT void xlXSubr(char *name,void (*fcn)(void));
xlEXPORT xlValue xlEnter(char *name);
xlEXPORT xlValue xlEnterKeyword(char *name);
xlEXPORT xlValue xlInternString(char *name,xlFIXTYPE len,xlValue package,xlValue *pkey);
xlEXPORT xlValue xlInternCString(char *name,xlValue package,xlValue *pkey);
xlEXPORT xlValue xlGetProp(xlValue sym,xlValue prp);
xlEXPORT void xlPutProp(xlValue sym,xlValue val,xlValue prp);
xlEXPORT void xlRemProp(xlValue sym,xlValue prp);
xlValue xlFindEntryInTable(xlValue table,xlValue key);
void xlAddEntryToTable(xlValue table,xlValue key,xlValue val);
xlValue xlRemoveEntryFromTable(xlValue table,xlValue key);

/* xlansi.c */
xlEXPORT void xlosSetRand(long seed);
xlEXPORT long xlosRand(long n);
xlEXPORT FILE *xlosOpenText(char *name,char *mode);
xlEXPORT FILE *xlosOpenBinary(char *name,char *mode);
xlEXPORT int xlosClose(FILE *fp);
xlEXPORT long xlosTell(FILE *fp);
xlEXPORT int xlosSeek(FILE *fp,long offset,int whence);
xlEXPORT size_t xlosReadFile(void *buf,size_t size,size_t count,FILE *fp);
xlEXPORT size_t xlosWriteFile(void *buf,size_t size,size_t count,FILE *fp);
xlEXPORT void xlosCheck(void);
xlEXPORT void xlosInfo(void);
xlEXPORT time_t xlosTime(void);
xlEXPORT void *xlosAlloc(xlFIXTYPE size);
xlEXPORT void xlosFree(void *ptr);
xlEXPORT char *xlosGetEnv(char *name);

/* xlosint.c */
xlEXPORT char *xlosLoadPath(void);
xlEXPORT char *xlosParsePath(char **pp);
xlEXPORT int xlosDirectorySeparator(void);
xlEXPORT void xlosExit(int sts);
xlEXPORT xlValue (*xlosFindSubr(char *name))(void);
xlEXPORT void xlosError(char *msg);
xlEXPORT int xlosFileModTime(char *fname,xlFIXTYPE *pModTime);
xlEXPORT int xlosConsoleGetC(void);
xlEXPORT void xlosConsolePutC(int ch);
xlEXPORT void xlosConsolePutS(char *str);
xlEXPORT int xlosConsoleAtBOLP(void);
xlEXPORT void xlosConsoleFlush(void);
xlEXPORT int xlosConsoleCheck(void);
xlEXPORT void xlosFlushOutput(void);

/* ??stuff.c */
xlEXPORT void xlSetCallbacks(xlCallbacks *cb);
void xlosEnter(void);
extern xlSubrDef xlosSubrTab[];
extern xlXSubrDef xlosXSubrTab[];

/* setup default callbacks */
xlEXPORT xlCallbacks *xlDefaultCallbacks(char *programPath);

#endif

