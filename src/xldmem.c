/* xldmem.c - xlisp dynamic memory management routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

#undef DEBUG_GC

/* virtual machine registers */
xlEXPORT xlValue xlFun;         /* current function */
xlEXPORT xlValue xlEnv;         /* current environment */
xlEXPORT xlValue xlVal;         /* value of most recent instruction */
xlEXPORT xlValue *xlSP;         /* value stack pointer */
xlEXPORT xlValue *xlCSP;        /* control stack pointer */

/* important values */
xlEXPORT xlValue xlTrue;
xlEXPORT xlValue xlFalse;
#ifndef xlNil
xlEXPORT xlValue xlNil;
#endif

/* stack limits */
xlEXPORT xlValue *xlStkBase;    /* base of the stack space - 1 */
xlEXPORT xlValue *xlStkTop;     /* top of the stack + 1 */

/* variables shared with xlimage.c */
xlFIXTYPE xlTotal;              /* total number of bytes of memory in use */
xlFIXTYPE xlGCCalls;            /* number of calls to the garbage collector */

/* node space */
xlProtectedPtrBlk *xlPPointers; /* protected pointers */
xlFIXTYPE xlNSSize = xlNSSIZE;  /* number of nodes per segment */
xlNodeSegment *xlNSegments;     /* list of node segments */
xlNodeSegment *xlNSLast;        /* last node segment */
int xlNSCount;                  /* number of node segments */
xlFIXTYPE xlNNodes;             /* total number of nodes */
xlFIXTYPE xlNFree;              /* number of nodes in free list */
xlValue xlFNodes;               /* list of free nodes */

/* vector (and string) space */
xlFIXTYPE xlVSSize = xlVSSIZE;  /* number of LVALS per vector segment */
xlVectorSegment *xlVSegments;   /* list of vector segments */
xlVectorSegment *xlVSCurrent;   /* current vector segment */
int xlVSCount;                  /* number of vector segments */
xlValue *xlVFree;               /* next free location in vector space */
xlValue *xlVTop;                /* top of vector space */

/* external variables */
extern xlValue xlPackages;      /* list of packages */
extern xlValue xlUnboundObject; /* unbound indicator */
extern xlValue xlDefaultObject; /* default object */
extern xlValue xlEofObject;     /* eof object */

/* forward declarations */
static xlValue allocnode(int);
static void findmemory(void);
static xlValue allocvector(int,xlFIXTYPE);
static int findvmemory(xlFIXTYPE);
static void markvector(xlValue);
static void markcontinuation(xlValue);
static void compact(void);
static void compact_vector(struct xlVectorSegment *);
static void sweep(void);
static void sweep_segment(struct xlNodeSegment *);
static void freeforeignptr(xlValue fptr);

/* xlCons - construct a new cons node */
xlEXPORT xlValue xlCons(xlValue x,xlValue y)
{
    xlValue nnode;

    /* get a free node */
    if ((nnode = xlFNodes) == xlNil) {
        xlCheck(2);
        xlPush(x);
        xlPush(y);
        findmemory();
        if ((nnode = xlFNodes) == xlNil)
            xlFmtAbort("insufficient node space");
        xlDrop(2);
    }

    /* unlink the node from the free list */
    xlFNodes = xlCdr(nnode);
    --xlNFree;

    /* initialize the new node */
    nnode->type = xlCONS;
    xlSetCar(nnode,x);
    xlSetCdr(nnode,y);

    /* return the new node */
    return nnode;
}

/* xlNewFrame - create a new environment frame */
xlEXPORT xlValue xlNewFrame(int type,xlValue parent,xlFIXTYPE size)
{
    xlValue env;
    env = xlNewVector(size);
    xlSetNextFrame(env,parent);
    env->type = type;
    return env;
}

/* xlMakeString - convert a string to a string node */
xlEXPORT xlValue xlMakeString(const char *str,xlFIXTYPE len)
{
    xlValue val = xlNewString(len);
    memcpy(xlGetString(val),str,(size_t)len);
    return val;
}

/* xlMakeCString - convert a c string to a string node */
xlEXPORT xlValue xlMakeCString(const char *str)
{
    size_t len = strlen(str);
    xlValue val = xlNewString((xlFIXTYPE)len);
    memcpy(xlGetString(val),str,len);
    return val;
}

/* xlCopyString - copy a string */
xlEXPORT xlValue xlCopyString(xlValue str)
{
    return xlMakeString(xlGetString(str),xlGetSLength(str));
}

/* xlMakeFileStream - convert a file pointer to a fstream */
xlEXPORT xlValue xlMakeFileStream(FILE *fp,short flags)
{
    xlValue val = xlNewStream(xlFSTREAM,flags);
    xlSetSData(val,fp);
    return val;
}

/* xlMakeUnnamedStream - convert a character array to a ustream */
xlEXPORT xlValue xlMakeUnnamedStream(const char *buf,xlFIXTYPE len)
{
    /* create an unnamed stream */
    xlCPush(xlNewUStream());
    
    /* copy the characters into the stream */
    while (--len >= 0)
        xlPutC(xlTop(),*buf++);

    /* return the new stream */
    return xlPop();
}

/* xlMakeObjectStream - convert an object to an ostream */
xlEXPORT xlValue xlMakeObjectStream(xlValue obj,short flags)
{
    xlValue val = xlNewStream(xlOSTREAM,flags);
    xlSetSData(val,obj);
    return val;
}

/* xlMakeSymbol - convert a string to a symbol */
xlEXPORT xlValue xlMakeSymbol(xlValue pname)
{
    xlValue val;
    xlCPush(pname);
    val = allocvector(xlSYMBOL,xlSYMBOLSIZE);
    xlSetValue(val,xlUnboundObject);
    xlSetPName(val,xlPop());
    xlSetPList(val,xlNil);
    return val;
}

/* xlMakeFixnum - convert an integer to a fixnum node */
xlEXPORT xlValue xlMakeFixnum(xlFIXTYPE n)
{
    xlValue val;
    if (xlSmallFixnumP(n))
        return xlMakeSmallFixnum(n);
    val = allocnode(xlFIXNUM);
    val->value.fixnum = n;
    return val;
}

/* xlMakeFlonum - convert a floating point number to a flonum node */
xlEXPORT xlValue xlMakeFlonum(xlFLOTYPE n)
{
    xlValue val;
    val = allocnode(xlFLONUM);
    val->value.flonum = n;
    return val;
}

/* xlMakeChar - convert an integer to a character node */
xlEXPORT xlValue xlMakeChar(int ch)
{
    xlValue val;
    val = allocnode(xlCHARACTER);
    val->value.chcode = ch;
    return val;
}

/* xlMakeClosure - convert code and an environment to a closure */
xlEXPORT xlValue xlMakeClosure(xlValue code,xlValue env)
{
    xlValue val;
    val = xlCons(code,env);
    val->type = xlCLOSURE;
    return val;
}

/* xlMakePromise - convert a procedure to a promise */
xlEXPORT xlValue xlMakePromise(xlValue code,xlValue env)
{
    xlValue val;
    val = xlCons(xlMakeClosure(code,env),xlNil);
    val->type = xlPROMISE;
    return val;
}

/* xlMakeSubr - convert a function to a subr */
xlEXPORT xlValue xlMakeSubr(const char *name,xlValue (*subr)(void))
{
    xlValue val;
    val = allocnode(xlSUBR);
    val->value.subr.subr = subr;
    val->value.subr.name = xlCopyCString(name);
    return val;
}

/* xlMakeXSubr - convert a function to an xsubr */
xlEXPORT xlValue xlMakeXSubr(const char *name,void (*subr)(void))
{
    xlValue val;
    val = allocnode(xlXSUBR);
    val->value.subr.subr = (xlValue (*)(void))subr;
    val->value.subr.name = xlCopyCString(name);
    return val;
}

/* xlNewVector - allocate and initialize a new vector */
xlEXPORT xlValue xlNewVector(xlFIXTYPE size)
{
    if (size < 0) xlError("vector length negative",xlMakeFixnum(size));
    return allocvector(xlVECTOR,size);
}

/* xlNewTable - allocate and initialize a new table */
xlEXPORT xlValue xlNewTable(xlFIXTYPE size)
{
    if (size < 0) xlError("table length negative",xlMakeFixnum(size));
    return allocvector(xlTABLE,size);
}

/* xlNewString - allocate and initialize a new string */
xlEXPORT xlValue xlNewString(xlFIXTYPE size)
{
    xlValue val;
    if (size < 0) xlError("string length negative",xlMakeFixnum(size));
    val = allocvector(xlSTRING,xlByteToWordSize(size + 1));
    xlGetString(val)[size] = '\0'; /* in case we need to use it as a c string */
    val->value.vector.size = size;
    return val;
}

/* xlNewPackage - create a new package */
xlEXPORT xlValue xlNewPackage(const char *name)
{
    xlValue pack;
    if (xlFindPackage(name) != xlNil)
        xlError("duplicate package name",xlMakeCString(name));
    pack = allocvector(xlPACKAGE,xlPACKAGESIZE);
    xlCPush(pack);
    xlSetNames(pack,xlCons(xlMakeCString(name),xlNil));
    xlSetExtern(pack,xlNewVector(xlHSIZE));
    xlSetIntern(pack,xlNewVector(xlHSIZE));
    xlSetUses(pack,xlNil);
    xlSetUsedBy(pack,xlNil);
    xlSetNextPackage(pack,xlPackages);
    xlPackages = pack;
    return xlPop();
}

/* xlNewCode - create a new code object */
xlEXPORT xlValue xlNewCode(xlFIXTYPE nlits)
{
    return allocvector(xlCODE,nlits);
}

/* xlNewContinuation - create a new continuation object */
xlEXPORT xlValue xlNewContinuation(xlFIXTYPE size)
{
    return allocvector(xlCONTINUATION,size);
}

/* xlNewStream - allocate and initialize a new stream */
xlEXPORT xlValue xlNewStream(int type,short flags)
{
    xlValue val = allocnode(type);
    xlSetPFlags(val,flags);
    xlSetSaveCh(val,'\0');
    return val;
}

/* xlNewUStream - create a new unnamed stream */
xlEXPORT xlValue xlNewUStream(void)
{
    xlValue val;
    xlCPush(allocvector(xlVECTOR,xlUSTRSIZE));
    val = xlNewStream(xlUSTREAM,xlpfINPUT | xlpfOUTPUT | xlpfBOL);
    xlSetSData(val,xlPop());
    xlSetStrHead(val,xlNil);
    xlSetStrTail(val,xlNil);
    xlSetStrIPtr(val,xlMakeSmallFixnum(0));
    xlSetStrOPtr(val,xlMakeSmallFixnum(0));
    return val;
}

/* xlMakeForeignPtr - convert a c pointer to a foreign pointer */
xlEXPORT xlValue xlMakeForeignPtr(xlCClass *type,void *p)
{
    xlValue val;
    val = allocnode(xlFOREIGNPTR);
    xlSetFPType(val,type);
    xlSetFPtr(val,p);
    return val;
}

/* allocnode - allocate a new node */
static xlValue allocnode(int type)
{
    xlValue nnode;

    /* get a free node */
    if ((nnode = xlFNodes) == xlNil) {
        findmemory();
        if ((nnode = xlFNodes) == xlNil)
            xlFmtAbort("insufficient node space");
    }

    /* unlink the node from the free list */
    xlFNodes = xlCdr(nnode);
    --xlNFree;

    /* initialize the new node */
    nnode->type = type;
    xlSetCdr(nnode,xlNil);

    /* return the new node */
    return nnode;
}

/* findmemory - garbage collect, then add more node space if necessary */
static void findmemory(void)
{
    /* first try garbage collecting */
    xlGC();

    /* expand memory only if less than one segment is free */
    if (xlNFree < xlNSSize)
        xlNExpand(xlNSSize);
    
    /* expand vector space if less than one segment is free */
    if (xlVSFREE(xlVFree,xlVTop) < xlVSSize / 2)
        xlVExpand(xlVSSize);
}

/* xlNExpand - expand node space */
int xlNExpand(xlFIXTYPE size)
{
    xlNodeSegment *newseg;
    xlValue p;

    /* allocate the new segment */
    if ((newseg = xlNewNSegment(size)) != NULL) {

        /* add each new node to the free list */
        for (p = &newseg->ns_data[0]; size > 0; ++p, --size) {
            p->type = xlFREE;
            p->flags = 0;
            xlSetCdr(p,xlFNodes);
            xlFNodes = p;
        }
    }
    return newseg != NULL;
}

/* allocvector - allocate and initialize a new vector node */
static xlValue allocvector(int type,xlFIXTYPE size)
{
    register xlValue val,*p;

    /* get a free node */
    if ((val = xlFNodes) == xlNil) {
        findmemory();
        if ((val = xlFNodes) == xlNil)
            xlFmtAbort("insufficient node space");
    }

    /* unlink the node from the free list */
    xlFNodes = xlCdr(xlFNodes);
    --xlNFree;

    /* initialize the vector node */
    val->type = type;
    val->value.vector.size = size;
    val->value.vector.data = NULL;
    xlCPush(val);

    /* add space for the backpointer and length */
    size += 2;
    
    /* make sure there's enough space */
    if (!xlVCOMPARE(xlVFree,size,xlVTop)
    &&  !xlCheckVMemory(size)
    &&  !findvmemory(size))
        xlFmtAbort("insufficient vector space");

    /* allocate the next available block */
    p = xlVFree;
    xlVFree += size;
    
    /* store the backpointer and length */
    *p++ = xlTop();
    *p++ = (xlValue)size;
    val->value.vector.data = p;

    /* set all the elements to NIL */
    for (size -= 2; --size >= 0; )
        *p++ = xlNil;

    /* return the new vector */
    return xlPop();
}

/* xlCheckVMemory - check for vector memory (used by 'xlimage.c') */
int xlCheckVMemory(xlFIXTYPE size)
{
    xlVectorSegment *vseg;
    for (vseg = xlVSegments; vseg != NULL; vseg = vseg->vs_next)
        if (vseg != xlVSCurrent && xlVCOMPARE(vseg->vs_free,size,vseg->vs_top)) {
            if (xlVSCurrent != NULL)
                xlVSCurrent->vs_free = xlVFree;
            xlVFree = vseg->vs_free;
            xlVTop = vseg->vs_top;
            xlVSCurrent = vseg;
            return TRUE;
        }       
    return FALSE;
}
    
/* findvmemory - find vector memory */
static int findvmemory(xlFIXTYPE size)
{
    /* try garbage collecting */
    xlGC();

    /* check to see if we found enough memory */
    if (xlVCOMPARE(xlVFree,size,xlVTop) || xlCheckVMemory(size))
        return TRUE;

    /* expand vector space */
    return xlMakeVMemory(size);
}

/* xlMakeVMemory - make vector memory (used by 'xlimage.c') */
int xlMakeVMemory(xlFIXTYPE size)
{
    return xlVExpand(size < xlVSSize ? xlVSSize : size);
}

/* xlVExpand - expand vector space */
int xlVExpand(xlFIXTYPE size)
{
    xlVectorSegment *vseg;

    /* allocate the new segment */
    if ((vseg = xlNewVSegment(size)) != NULL) {
        if (xlVSCurrent != NULL)
            xlVSCurrent->vs_free = xlVFree;
        xlVFree = vseg->vs_free;
        xlVTop = vseg->vs_top;
        xlVSCurrent = vseg;
    }
    return vseg != NULL;
}

/* xlNewNSegment - create a new node segment */
xlNodeSegment *xlNewNSegment(xlFIXTYPE n)
{
    xlNodeSegment *newseg;

    /* allocate the new segment */
    if ((newseg = (xlNodeSegment *)xlosAlloc(xlNSegSize(n))) == NULL)
        return NULL;

    /* initialize the new segment */
    newseg->ns_size = n;
    newseg->ns_next = NULL;
    if (xlNSegments == NULL)
        xlNSegments = newseg;
    else
        xlNSLast->ns_next = newseg;
    xlNSLast = newseg;

    /* update the statistics */
    xlTotal += xlNSegSize(n);
    xlNNodes += n;
    xlNFree += n;
    ++xlNSCount;

    /* return the new segment */
    return newseg;
}
 
/* xlNewVSegment - create a new vector segment */
xlVectorSegment *xlNewVSegment(xlFIXTYPE n)
{
    xlVectorSegment *newseg;

    /* allocate the new segment */
    if ((newseg = (xlVectorSegment *)xlosAlloc(xlVSegSize(n))) == NULL)
        return NULL;

    /* initialize the new segment */
    newseg->vs_free = newseg->vs_data;
    newseg->vs_top = newseg->vs_free + n;
    newseg->vs_next = xlVSegments;
    xlVSegments = newseg;

    /* update the statistics */
    xlTotal += xlVSegSize(n);
    ++xlVSCount;

    /* return the new segment */
    return newseg;
}
 
/* xlGC - garbage collect */
xlEXPORT void xlGC(void)
{
    register xlValue *p;
    xlProtectedPtrBlk *ppb;
    
#ifdef DEBUG_GC
/*if ((gccalls % 10) == 0) */
{ char buf[20];
  sprintf(buf,"\n[GC %ld",gccalls);
  xlStdPutStr(buf);
}
#endif

    /* reset the mark flags on the control stack */
    for (p = xlCSP; p > xlStkBase; )
        p = xlCDUnmark(p);
 
    /* mark the current package and environment */
    xlMark(xlPackages);
    xlMark(xlFun);
    xlMark(xlEnv);
    xlMark(xlVal);
    xlMark(xlDefaultObject);
    xlMark(xlEofObject);
    xlMark(xlTrue);
    xlMark(xlFalse);

    /* mark the value stack */
    for (p = xlSP; p < xlStkTop; )
        xlMark(*p++);

    /* mark the control stack */
    for (p = xlCSP; p > xlStkBase; )
        p = xlCDMark(p);

    /* mark protected pointers */
    for (ppb = xlPPointers; ppb != NULL; ppb = ppb->next) {
        xlValue **pp = ppb->pointers;
        int count = ppb->count;
        for (; --count >= 0; ++pp)
            if (!((xlOFFTYPE)*pp & 1))
                xlMark(**pp);
    }
    
    /* compact vector space */
    xlGCProtect(compact);

    /* sweep memory collecting all unmarked nodes */
    sweep();

#ifdef DEBUG_GC
/*if ((gccalls % 10) == 0)*/
  xlStdPutStr(" - done]");
#endif

    /* count the xlGC call */
    ++xlGCCalls;
}

/* mark - mark all accessible nodes */
void xlMark(xlValue ptr)
{
    register xlValue this,prev,tmp;

    /* check for a non-pointer */
    if (!xlPointerP(ptr))
        return;

    /* initialize */
    prev = xlNil;
    this = ptr;

    /* mark this node */
    for (;;) {

        /* descend as far as we can */
        while (!(this->flags & xlMARK))

            /* mark this node and trace its children */
            switch (this->type) {
            case xlCONS:                /* mark cons-like nodes */
            case xlCLOSURE:
            case xlPROMISE:
                this->flags |= xlMARK;
                tmp = xlCar(this);
                if (xlPointerP(tmp)) {
                    this->flags |= xlLEFT;
                    xlSetCar(this,prev);
                    prev = this;
                    this = tmp;
                }
                else {
                    tmp = xlCdr(this);
                    if (xlPointerP(tmp)) {
                        xlSetCdr(this,prev);
                        prev = this;
                        this = tmp;
                    }
                }
                break;
            case xlSYMBOL:      /* mark vector-like nodes */
            case xlVECTOR:
            case xlCODE:
            case xlPACKAGE:
            case xlENV:
            case xlSENV:
            case xlMSENV:
            case xlMENV:
            case xlSMENV:
            case xlOBJECT:
            case xlTABLE:
                this->flags |= xlMARK;
                markvector(this);
                break;
            case xlUSTREAM:
            case xlOSTREAM:
                this->flags |= xlMARK;
                xlMark((xlValue)xlGetSData(this));
                break;
            case xlCONTINUATION:
                this->flags |= xlMARK;
                markcontinuation(this);
                break;
            case xlFIXNUM:      /* mark objects that don't contain pointers */
            case xlFLONUM:
            case xlSTRING:
            case xlFSTREAM:
            case xlSUBR:
            case xlXSUBR:
            case xlCHARACTER:
            case xlFOREIGNPTR:
                this->flags |= xlMARK;
                break;
            default:            /* bad object type */
                xlFatal("%lx: bad object type %d",this,this->type);
                break;
            }

        /* backup to a point where we can continue descending */
        for (;;)

            /* make sure there is a previous node */
            if (prev != xlNil) {
                if (prev->flags & xlLEFT) {     /* came from left side */
                    prev->flags &= ~xlLEFT;
                    tmp = xlCar(prev);
                    xlSetCar(prev,this);
                    this = xlCdr(prev);
                    if (xlPointerP(this)) {
                        xlSetCdr(prev,tmp);                     
                        break;
                    }
                }
                else {                          /* came from right side */
                    tmp = xlCdr(prev);
                    xlSetCdr(prev,this);
                }
                this = prev;                    /* step back up the branch */
                prev = tmp;
            }

            /* no previous node, must be done */
            else
                return;
    }
}

/* markvector - mark a vector-like node */
static void markvector(xlValue vect)
{
    register xlFIXTYPE n;
    register xlValue *p;
    if ((p = vect->value.vector.data) != NULL)
        for (n = xlGetSize(vect); --n >= 0; )
            xlMark(*p++);
}

/* markcontinuation - mark a continuation node */
static void markcontinuation(xlValue cont)
{
    register xlValue *p,*sp;
    xlFIXTYPE vsize,csize;

    /* make sure there is data */
    if (cont->value.vector.data != NULL) {

        /* get the stack sizes */
        vsize = xlGetFixnum(xlGetElement(cont,0));
        csize = xlGetSize(cont) - vsize - 1;

        /* mark the value stack */
        for (sp = &cont->value.vector.data[1]; --vsize >= 0; )
            xlMark(*sp++);
            
        /* mark the control stack */
        for (p = &sp[csize]; p > sp; )
            p = xlCDMark(p);
    }
}

/* compact - compact vector space */
static void compact(void)
{
    xlVectorSegment *vseg;

    /* store the current segment information */
    if (xlVSCurrent != NULL)
        xlVSCurrent->vs_free = xlVFree;

    /* compact each vector segment */
    for (vseg = xlVSegments; vseg != NULL; vseg = vseg->vs_next)
        compact_vector(vseg);

    /* make the first vector segment current */
    if ((xlVSCurrent = xlVSegments) != NULL) {
        xlVFree = xlVSCurrent->vs_free;
        xlVTop = xlVSCurrent->vs_top;
    }
}

/* compact_vector - compact a vector segment */
static void compact_vector(xlVectorSegment *vseg)
{
    register xlValue *vdata,*vnext,*vfree,vector;
    register xlFIXTYPE vsize;

    vdata = vnext = vseg->vs_data;
    vfree = vseg->vs_free;
    while (vdata < vfree) {
        vector = *vdata;
        vsize = (xlFIXTYPE)vdata[1];
        if (vector->flags & xlMARK) {
            if (vdata == vnext) {
                vdata += vsize;
                vnext += vsize;
            }
            else {
                vector->value.vector.data = vnext + 2;
                while (--vsize >= 0)
                    *vnext++ = *vdata++;
            }
        }
        else
            vdata += vsize;
    }
    vseg->vs_free = vnext;
}

/* sweep - sweep all unmarked nodes and add them to the free list */
static void sweep(void)
{
    xlNodeSegment *nseg;

    /* empty the free list */
    xlFNodes = xlNil;
    xlNFree = 0;

    /* sweep each node segment */
    for (nseg = xlNSegments; nseg != NULL; nseg = nseg->ns_next)
        sweep_segment(nseg);
}

/* sweep_segment - sweep a node segment */
static void sweep_segment(xlNodeSegment *nseg)
{
    register xlFIXTYPE n;
    register xlValue p;

    /* add all unmarked nodes */
    for (p = &nseg->ns_data[0], n = nseg->ns_size; --n >= 0L; ++p)
        if (!(p->flags & xlMARK)) {
            switch (p->type) {
            case xlSUBR:
            case xlXSUBR:
                if (xlGetSubrName(p))
                    free(xlGetSubrName(p));
                break;
            case xlFSTREAM:
                if (xlGetFile(p))
                    xlosClose(xlGetFile(p));
                break;
            case xlFOREIGNPTR:
                freeforeignptr(p);
                break;
            }
            xlSetCar(p,(xlValue)(xlOFFTYPE)p->type); /* for debugging */
            p->type = xlFREE;
            xlSetCdr(p,xlFNodes);
            xlFNodes = p;
            ++xlNFree;
        }
        else
            p->flags &= ~xlMARK;
}

/* freeforeignptr - free a foreign pointer */
static void freeforeignptr(xlValue fptr)
{
    void *ptr = xlGetFPtr(fptr);
    if (ptr != NULL) {
        xlCClass *type = xlGetFPType(fptr);
        if (type->def->free)
            (*type->def->free)(ptr);
    }
}

/* xlProtect - protect a pointer */
xlEXPORT int xlProtect(xlValue *p)
{
    xlProtectedPtrBlk *ppb;
    xlValue **pp;

    /* initialize the pointer */
    *p = xlNil;

    /* look for a block with a free pointer */
    for (ppb = xlPPointers; ppb != NULL; ppb = ppb->next) {

        /* check to see if there is room at the end of the block */
        if (ppb->count < xlPPBSIZE) {
            ppb->pointers[ppb->count++] = p;
            return TRUE;
        }

        /* check to see if a pointer has been freed */
        else if ((pp = ppb->free) != NULL) {
            ppb->free = (xlValue **)((xlOFFTYPE)*pp & ~1);
            *pp = p;
            return TRUE;
        }
    }

    /* allocate a new pointer block */
    if ((ppb = (xlProtectedPtrBlk *)xlosAlloc(sizeof(xlProtectedPtrBlk))) == NULL)
        return FALSE;
    ppb->next = xlPPointers;
    ppb->free = NULL;
    ppb->count = 1;
    ppb->pointers[0] = p;
    xlPPointers = ppb;
    return TRUE;
}

/* xlUnprotect - unprotect a pointer */
xlEXPORT int xlUnprotect(xlValue *p)
{
    xlProtectedPtrBlk *ppb;
    xlValue **pp;
    int n;

    /* look for a block with a free pointer */
    for (ppb = xlPPointers; ppb != NULL; ppb = ppb->next)
        for (n = ppb->count, pp = ppb->pointers; --n >= 0; ++pp)
            if (p == *pp) {
                *pp = (xlValue *)((xlFIXTYPE)ppb->free | 1);
                ppb->free = pp;
                return TRUE;
            }

    /* pointer not found */
    return FALSE;
}

/* xlCopyCString - copy a string */
char *xlCopyCString(const char *str)
{
    char *copy = xlosAlloc(strlen(str) + 1);
    if (copy) strcpy(copy,str);
    return copy;
}

/* xlInitMemory - initialize the dynamic memory module */
void xlInitMemory(xlFIXTYPE ssize)
{
    xlFIXTYPE n;

    /* initialize some important variables */
    xlPPointers = NULL;
#ifndef xlNil
    xlNil = (xlValue)0;
#endif
    xlTrue = xlFalse = xlNil;

    /* initialize structures that are marked by the collector */
    xlPackages = xlUnboundObject = xlDefaultObject = xlEofObject = xlNil;
    xlFun = xlEnv = xlVal = xlNil;

    /* initialize our internal variables */
    xlGCCalls = 0;
    xlTotal = 0L;

    /* initialize node space */
    xlNSegments = xlNSLast = NULL;
    xlNSCount = 0;
    xlNNodes = xlNFree = 0;
    xlFNodes = xlNil;

    /* initialize vector space */
    xlVSegments = xlVSCurrent = NULL;
    xlVSCount = 0;
    xlVFree = xlVTop = NULL;
    
    /* allocate the value and control stack space */
    n = ssize * sizeof(xlValue);
    if ((xlStkBase = (xlValue *)xlosAlloc(n)) == NULL)
        xlFatal("insufficient memory");
    xlTotal += n;

    /* initialize the stacks */
    xlStkTop = xlStkBase + ssize;
    xlResetStack();
}

/* xlResetStack - reset the stacks */
void xlResetStack(void)
{
    xlSP = xlStkTop;
    xlCSP = xlStkBase;
}

