/* xlimage.c - xlisp memory image save/restore functions */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* global variables */
extern xlValue xlLispPackage,xlLispPackage,xlKeywordPackage,xlLispPackage;
extern xlValue xlPackages,xlEofObject,xlDefaultObject;

/* local variables */
static xlOFFTYPE off,foff;
static FILE *fp;

/* local prototypes */
static xlValue requirepackage(const char *name);
static void freeimage(void);
static void setoffset(void);
static void writenode(xlValue node);
static void writeptr(xlOFFTYPE off);
static void readnode(int type,xlValue node);
static xlOFFTYPE readptr(void);
static xlValue cviptr(xlOFFTYPE o);
static xlOFFTYPE cvoptr(xlValue p);
static xlValue *getvspace(xlValue node,xlFIXTYPE size);

/* xlSaveImage - save the memory image */
int xlSaveImage(const char *fname)
{
    xlNodeSegment *nseg;
    xlFIXTYPE size,n;
    xlValue p,*vp;
    const char *cp;

    /* open the output file */
    if ((fp = xlosOpenBinary(fname,"w")) == NULL)
        return FALSE;

    /* first call the garbage collector to clean up memory */
    xlGC();

    /* write out the stack size */
    writeptr((xlOFFTYPE)(xlStkTop-xlStkBase-1));

    /* write out the package list and various constants */
    writeptr(cvoptr(xlPackages));
    writeptr(cvoptr(xlEofObject));
    writeptr(cvoptr(xlDefaultObject));

    /* setup the initial file offsets */
    off = foff = (xlOFFTYPE)2;

    /* write out all nodes that are still in use */
    for (nseg = xlNSegments; nseg != NULL; nseg = nseg->ns_next) {
        p = &nseg->ns_data[0];
        n = nseg->ns_size;
        for (; --n >= 0; ++p, off += sizeof(xlNode))
            switch (xlNodeType(p)) {
            case xlFREE:
                break;
            case xlCONS:
            case xlCLOSURE:
            case xlPROMISE:
                setoffset();
                putc(p->type,fp);
                writeptr(cvoptr(xlCar(p)));
                writeptr(cvoptr(xlCdr(p)));
                foff += sizeof(xlNode);
                break;
            case xlSYMBOL:
            case xlVECTOR:
            case xlCODE:
            case xlCONTINUATION:
            case xlPACKAGE:
            case xlENV:
            case xlMENV:
            case xlOBJECT:
            case xlTABLE:
                setoffset();
                putc(p->type,fp);
                size = xlGetSize(p);
                writeptr((xlOFFTYPE)size);
                for (vp = xlGetVector(p); --size >= 0; )
                    writeptr(cvoptr(*vp++));
                foff += sizeof(xlNode);
                break;
            case xlSTRING:
                setoffset();
                putc(p->type,fp);
                size = xlGetSLength(p);
                writeptr((xlOFFTYPE)size);
                for (cp = xlGetString(p); --size >= 0; )
                    putc(*cp++,fp);
                foff += sizeof(xlNode);
                break;
            case xlSUBR:
            case xlXSUBR:
                setoffset();
                putc(p->type,fp);
                if ((cp = xlGetSubrName(p)) != NULL)
                    while (*cp != '\0')
                        putc(*cp++,fp);
                putc('\0',fp);
                foff += sizeof(xlNode);
                break;
            case xlFOREIGNPTR:
                setoffset();
                putc(p->type,fp);
                if ((cp = xlGetFPType(p)->def->name) != NULL)
                    while (*cp != '\0')
                        putc(*cp++,fp);
                putc('\0',fp);
                foff += sizeof(xlNode);
                break;
            case xlUSTREAM:
            case xlOSTREAM:
                setoffset();
                putc(p->type,fp);
                writeptr((xlOFFTYPE)xlGetPFlags(p));
                writeptr((xlOFFTYPE)xlGetSaveCh(p));
                writeptr(cvoptr((xlValue)xlGetSData(p)));
                break;
            default:
                setoffset();
                writenode(p);
                foff += sizeof(xlNode);
                break;
            }
    }

    /* write the terminator */
    putc(xlFREE,fp);
    writeptr((xlOFFTYPE)0);

    /* close the output file */
    xlosClose(fp);

    /* return successfully */
    return TRUE;
}

/* xlRestoreImage - restore a saved memory image */
int xlRestoreImage(const char *fname)
{
    xlFIXTYPE ssize,size;
    xlValue p,*vp;
    char *cp;
    int type;

    /* open the file */
    if ((fp = xlosOpenBinary(fname,"r")) == NULL)
        return FALSE;

    /* free the old memory image */
    freeimage();

    /* read the stack size */
    ssize = readptr();

    /* allocate memory for the workspace */
    xlInitMemory(ssize);

    /* read the package list and various constants */
    xlPackages = cviptr(readptr());
    xlEofObject = cviptr(readptr());
    xlDefaultObject = cviptr(readptr());
    
    /* read each node */
    for (off = (xlOFFTYPE)2; (type = getc(fp)) >= 0; )
        switch (type) {
        case xlFREE:
            if ((off = readptr()) == (xlOFFTYPE)0)
                goto done;
            break;
        case xlCONS:
        case xlCLOSURE:
        case xlPROMISE:
            p = cviptr(off);
            p->type = type;
            xlSetCar(p,cviptr(readptr()));
            xlSetCdr(p,cviptr(readptr()));
            off += sizeof(xlNode);
            break;
        case xlSYMBOL:
        case xlVECTOR:
        case xlCODE:
        case xlCONTINUATION:
        case xlPACKAGE:
        case xlENV:
        case xlMENV:
        case xlOBJECT:
        case xlTABLE:
            p = cviptr(off);
            p->type = type;
            p->value.vector.size = size = readptr();
            p->value.vector.data = getvspace(p,size);
            for (vp = p->value.vector.data; --size >= 0; )
                *vp++ = cviptr(readptr());
            off += sizeof(xlNode);
            break;
        case xlSTRING:
            p = cviptr(off);
            p->type = type;
            p->value.vector.size = size = readptr();
            p->value.vector.data = getvspace(p,xlByteToWordSize(size));
            for (cp = xlGetString(p); --size >= 0; )
                *cp++ = getc(fp);
            off += sizeof(xlNode);
            break;
        case xlFSTREAM:
            p = cviptr(off);
            readnode(type,p);
            xlSetSData(p,NULL);
            off += sizeof(xlNode);
            break;
        case xlSUBR:
            {   char name[100];
                xlValue (*subr)(void);
                p = cviptr(off);
                p->type = type;
                for (cp = name; (*cp++ = getc(fp)) != '\0'; )
                    ;
                if ((subr = xlosFindSubr(name)) == NULL
                &&  (subr = xlFindSubr(name)) == NULL)
                    xlFatal("no definition for subr: %s",name);
                p->type = xlSUBR;
                p->value.subr.subr = subr;
                p->value.subr.name = *name ? xlCopyCString(name) : NULL;
                off += sizeof(xlNode);
            }
            break;
        case xlXSUBR:
            {   char name[100];
                xlValue (*subr)(void);
                p = cviptr(off);
                p->type = type;
                for (cp = name; (*cp++ = getc(fp)) != '\0'; )
                    ;
                if ((subr = xlosFindSubr(name)) == NULL
                &&  (subr = xlFindSubr(name)) == NULL)
                    xlFatal("no definition for xsubr: %s",name);
                p->type = xlXSUBR;
                p->value.subr.subr = subr;
                p->value.subr.name = *name ? xlCopyCString(name) : NULL;
                off += sizeof(xlNode);
            }
            break;
        case xlFOREIGNPTR:
            {   char name[100];
                p = cviptr(off);
                p->type = type;
                for (cp = name; (*cp++ = getc(fp)) != '\0'; )
                    ;
                /* need to find C class */
                xlSetFPtr(p,NULL);
                off += sizeof(xlNode);
            }
            break;
        case xlUSTREAM:
        case xlOSTREAM:
            p = cviptr(off);
            p->type = type;
            xlSetPFlags(p,(short)readptr());
            xlSetSaveCh(p,(short)readptr());
            xlSetSData(p,cviptr(readptr()));
            break;
        default:
            readnode(type,cviptr(off));
            off += sizeof(xlNode);
            break;
        }
done:

    /* close the input file */
    xlosClose(fp);

    /* collect to initialize the free space */
    xlGC();

    /* lookup the packages */
    xlLispPackage   = requirepackage("XLISP");
    xlLispPackage  = requirepackage("SCHEME");
    xlKeywordPackage = requirepackage("KEYWORD");
    xlLispPackage  = requirepackage("SYSTEM");

    /* lookup all of the symbols the interpreter uses */
    xlEnterSymbols();

    /* return successfully */
    return TRUE;
}

/* requirepackage - require that a package exist */
static xlValue requirepackage(const char *name)
{
    xlValue pack = xlFindPackage(name);
    if (pack == xlNil)
        xlFatal("missing package: %s",name);
    return pack;
}

/* freeimage - free the current memory image */
static void freeimage(void)
{
    xlNodeSegment *nextnseg;
    xlVectorSegment *nextvseg;
    xlFIXTYPE n;
    FILE *fp;
    xlValue p;

    /* close all open ports and free each node segment */
    for (; xlNSegments != NULL; xlNSegments = nextnseg) {
        nextnseg = xlNSegments->ns_next;
        p = &xlNSegments->ns_data[0];
        n = xlNSegments->ns_size;
        for (; --n >= 0; ++p)
            switch (xlNodeType(p)) {
            case xlFSTREAM:
                if ((fp = xlGetFile(p)) != NULL
                &&  (xlGetPFlags(p) & xlpfTERMINAL) == 0)
                    xlosClose(fp);
                break;
            case xlFOREIGNPTR:
                break;
            }
        xlosFree(xlNSegments);
    }

    /* free each vector segment */
    for (; xlVSegments != NULL; xlVSegments = nextvseg) {
        nextvseg = xlVSegments->vs_next;
        xlosFree(xlVSegments);
    }
    
    /* free the stack */
    if (xlStkBase)
        xlosFree(xlStkBase);
}

/* setoffset - output a positioning command if nodes have been skipped */
static void setoffset(void)
{
    if (off != foff) {
        putc(xlFREE,fp);
        writeptr(off);
        foff = off;
    }
}

/* writenode - write a node to a file */
static void writenode(xlValue node)
{
    const char *p = (const char *)&node->value;
    int n = sizeof(union xlNodeValue);
    putc(node->type,fp);
    while (--n >= 0)
        putc(*p++,fp);
}

/* writeptr - write a pointer to a file */
static void writeptr(xlOFFTYPE off)
{
    const char *p = (const char *)&off;
    int n = sizeof(xlOFFTYPE);
    while (--n >= 0)
        putc(*p++,fp);
}

/* readnode - read a node */
static void readnode(int type,xlValue node)
{
    char *p = (char *)&node->value;
    int n = sizeof(union xlNodeValue);
    node->type = type;
    while (--n >= 0)
        *p++ = getc(fp);
}

/* readptr - read a pointer */
static xlOFFTYPE readptr(void)
{
    xlOFFTYPE off;
    char *p = (char *)&off;
    int n = sizeof(xlOFFTYPE);
    while (--n >= 0)
        *p++ = getc(fp);
    return off;
}

/* cviptr - convert a pointer on input */
static xlValue cviptr(xlOFFTYPE o)
{
    xlOFFTYPE off = (xlOFFTYPE)2;
    xlOFFTYPE nextoff;
    xlNodeSegment *nseg;

    /* check for nil and small fixnums */
    if (o == (xlOFFTYPE)0 || (o & 1) == 1)
        return (xlValue)o;

    /* compute a pointer for this offset */
    for (nseg = xlNSegments; nseg != NULL; nseg = nseg->ns_next) {
        nextoff = off + (xlOFFTYPE)(nseg->ns_size * sizeof(xlNode));
        if (o >= off && o < nextoff)
            return (xlValue)((xlOFFTYPE)&nseg->ns_data[0] + o - off);
        off = nextoff;
    }

    /* create new segments if necessary */
    for (;;) {

        /* create the next segment */
        if ((nseg = xlNewNSegment(xlNSSize)) == NULL)
            xlFatal("insufficient memory - segment");

        /* check to see if the offset is in this segment */
        nextoff = off + (xlOFFTYPE)(nseg->ns_size * sizeof(xlNode));
        if (o >= off && o < nextoff)
            return (xlValue)((xlOFFTYPE)&nseg->ns_data[0] + o - off);
        off = nextoff;
    }
}

/* cvoptr - convert a pointer on output */
static xlOFFTYPE cvoptr(xlValue p)
{
    xlOFFTYPE off = (xlOFFTYPE)2;
    xlNodeSegment *nseg;

    /* check for nil and small fixnums */
    if (!xlPointerP(p))
        return (xlOFFTYPE)p;

    /* compute an offset for this pointer */
    for (nseg = xlNSegments; nseg != NULL; nseg = nseg->ns_next) {
        if (xlINSEGMENT(p,nseg))
            return off + ((xlOFFTYPE)p - (xlOFFTYPE)&nseg->ns_data[0]);
        off += (xlOFFTYPE)(nseg->ns_size * sizeof(xlNode));
    }

    /* pointer not within any segment */
    xlError("bad pointer found during image save",p);
    return (xlOFFTYPE)0; /* never reached */
}

/* getvspace - allocate vector space */
static xlValue *getvspace(xlValue node,xlFIXTYPE size)
{
    xlValue *p;
    ++size; /* space for the back pointer */
    if (!xlVCOMPARE(xlVFree,size,xlVTop)
    &&  !xlCheckVMemory(size)
    &&  !xlMakeVMemory(size))
        xlFatal("insufficient vector space");
    p = xlVFree;
    xlVFree += size;
    *p++ = node;
    return p;
}

