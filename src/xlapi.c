/* xlapi.c - xlisp api routines */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* local variables */
static void (*idleHandler)(void *data) = NULL;
static void *idleData;

/* external variables */
extern xlValue *xlcatch,s_eval,s_load,xlUnboundObject,xlEofObject;

/* prototypes */
static const char *PrintToString(xlValue expr,char *buf,xlFIXTYPE len,int escFlag);

/* xlSetSoftwareType - set the *SOFTWARE-TYPE* variable */
xlEXPORT void xlSetSoftwareType(const char *type)
{
    xlSetValue(xlEnter("*SOFTWARE-TYPE*"),xlEnter(type));
}

/* xlCallFunction - call a function */
xlEXPORT int xlCallFunction(xlValue *values,int vmax,xlValue fun,int argc,...)
{
    xlErrorTarget target;
    xlValue *save_catch;
    va_list ap;
    int valc;
    
    /* initialize the catch list */
    save_catch = xlcatch;
    xlcatch = NULL;
    
    /* setup the error handler */
    xlPushTarget(&target);
    if (setjmp(target.target) != 0) {
        xlcatch = save_catch;
        xlPopTarget();
        return xlsError;
    }

    /* execute the function call */
    va_start(ap,argc);
    valc = xlInvokeInterpreter(values,vmax,fun,xlNil,argc,ap);
    va_end(ap);
    
    /* only needed error handler for dealing with entering selector */
    xlPopTarget();
    
    /* restore the old catch list */
    xlcatch = save_catch;
    
    /* return the value count or error code */
    return valc >= 0 ? valc : xlsError;
}

/* xlCallFunctionByName - call a function by name */
xlEXPORT int xlCallFunctionByName(xlValue *values,int vmax,const char *fname,int argc,...)
{
    xlValue *save_catch,sym,fun;
    xlErrorTarget target;
    va_list ap;
    int valc;
    
    /* initialize the catch list */
    save_catch = xlcatch;
    xlcatch = NULL;
    
    /* setup the error handler */
    xlPushTarget(&target);
    if (setjmp(target.target) != 0) {
        xlcatch = save_catch;
        xlPopTarget();
        return xlsError;
    }

    /* get the function */
    sym = xlEnter(fname);
    if ((fun = xlGetValue(sym)) == xlUnboundObject)
        xlError("unbound symbol",sym);

    /* only needed error handler for dealing with unbound symbols */
    xlPopTarget();
    
    /* execute the function call */
    va_start(ap,argc);
    valc = xlInvokeInterpreter(values,vmax,fun,xlNil,argc,ap);
    va_end(ap);
    
    /* restore the old catch list */
    xlcatch = save_catch;
    
    /* return the value count or error code */
    return valc >= 0 ? valc : xlsError;
}

/* xlSendMessage - send a message to an object */
xlEXPORT int xlSendMessage(xlValue *values,int vmax,xlValue obj,xlValue selector,int argc,...)
{
    xlErrorTarget target;
    xlValue *save_catch;
    va_list ap;
    int valc;
    
    /* initialize the catch list */
    save_catch = xlcatch;
    xlcatch = NULL;

    /* setup the error handler */
    xlPushTarget(&target);
    if (setjmp(target.target) != 0) {
        xlcatch = save_catch;
        xlPopTarget();
        return xlsError;
    }

    /* count the selector as an argument */
    ++argc;
    
    /* execute the function call */
    va_start(ap,argc);
    valc = xlInvokeInterpreter(values,vmax,obj,selector,-argc,ap);
    va_end(ap);
    
    /* only needed error handler for dealing with entering selector */
    xlPopTarget();
    
    /* restore the old catch list */
    xlcatch = save_catch;
    
    /* return the value count or error code */
    return valc >= 0 ? valc : xlsError;
}

/* xlSendMessageByName - send a message to an object */
xlEXPORT int xlSendMessageByName(xlValue *values,int vmax,xlValue obj,const char *sname,int argc,...)
{
    xlValue *save_catch,selector;
    xlErrorTarget target;
    va_list ap;
    int valc;
    
     /* initialize the catch list */
    save_catch = xlcatch;
    xlcatch = NULL;
    
    /* setup the error handler */
    xlPushTarget(&target);
    if (setjmp(target.target) != 0) {
        xlcatch = save_catch;
        xlPopTarget();
        return xlsError;
    }

    /* get the selector symbol (and count it as an argument) */
    selector = xlEnter(sname);
    ++argc;

    /* only needed error handler for dealing with entering selector */
    xlPopTarget();
    
    /* execute the function call */
    va_start(ap,argc);
    valc = xlInvokeInterpreter(values,vmax,obj,selector,-argc,ap);
    va_end(ap);
    
    /* restore the old catch list */
    xlcatch = save_catch;
    
    /* return the value count or error code */
    return valc >= 0 ? valc : xlsError;
}

/* xlEvaluateCString - evaluate an expression from a c string */
xlEXPORT int xlEvaluateCString(xlValue *values,int vmax,const char *str)
{
    return xlEvaluateString(values,vmax,str,(xlFIXTYPE)strlen(str));
}

/* xlEvaluateString - evaluate an expression from a string */
xlEXPORT int xlEvaluateString(xlValue *values,int vmax,const char *str,xlFIXTYPE len)
{
    xlValue val;
    int sts;
    if ((sts = xlReadFromString(str,len,&val)) != xlsSuccess)
        return sts;
    return xlEvaluate(values,vmax,val);    
}

/* xlReadFromCString - read an expression from a c string */
xlEXPORT int xlReadFromCString(const char *str,xlValue *pval)
{
    return xlReadFromString(str,(xlFIXTYPE)strlen(str),pval);
}

/* xlReadFromString - read an expression from a string */
xlEXPORT int xlReadFromString(const char *str,xlFIXTYPE len,xlValue *pval)
{
    xlErrorTarget target;
    int sts;

    /* trap errors */
    xlPushTarget(&target);
    if (setjmp(target.target)) {
        xlPopTarget();
        return xlsError;
    }
 
    /* create the string stream */
    xlCPush(xlMakeUnnamedStream(str,len));
    
    /* read from the stream */
    if (!(sts = xlRead(xlPop(),pval)))
        *pval = xlEofObject;
    xlPopTarget();
    
    /* return status */
    return sts ? xlsSuccess : xlsEndOfFile;
}

/* xlLoadFile - load an XLISP source file */
xlEXPORT int xlLoadFile(const char *fname)
{
    xlErrorTarget target;
    xlValue val;
    int valc;

    /* trap errors */
    xlPushTarget(&target);
    if (setjmp(target.target)) {
        xlPopTarget();
        return xlsError;
    }
 
    /* convert the filename */
    xlCPush(xlMakeCString(fname));
    xlPopTarget();
    
    /* call the LOAD function */
    valc = xlCallFunction(&val,1,xlGetValue(s_load),1,xlPop());

    /* return status */
    return valc >= 0 && val == xlTrue ? xlsSuccess : xlsError;
}

/* xlLoadOpen - open a file for loading */
xlEXPORT FILE *xlLoadOpen(const char *name,const char *mode,const char *pathsym,char *rpath)
{
    xlValue dir,fullpath,path;
    xlFIXTYPE dirlen,namelen;
    char *pathstr;
    FILE *fp;

    /* first try just opening the file with no additional path information */
    if ((fp = xlosOpenText(name,mode)) != NULL) {
        if (rpath)
            strcpy(rpath,name);
        return fp;
    }

    /* initialize */
    path = xlGetValue(xlEnter(pathsym));
    namelen = strlen(name);
    xlCheck(1);

    /* try each directory in the load path */
    for (; xlConsP(path); path = xlCdr(path)) {

        /* try the next directory */
        dir = xlCar(path);
        if (xlStringP(dir)) {

            /* build a full path to the file */
            xlPush(path);
            dirlen = xlGetSLength(dir);
            fullpath = xlNewString(dirlen + namelen);
            pathstr = xlGetString(fullpath);
            memcpy(pathstr,xlGetString(dir),(size_t)dirlen);
            memcpy(pathstr + dirlen,name,(size_t)namelen);
            xlDrop(1);
            
            /* attempt to open the file */
            if ((fp = xlosOpenText(xlGetString(fullpath),mode)) != NULL) {
                if (rpath)
                    strcpy(rpath,xlGetString(fullpath));
                return fp;
            }
        }
    }
    return NULL;
}

/* xlEvaluate - evaluate an expression */
xlEXPORT int xlEvaluate(xlValue *values,int vmax,xlValue expr)
{
    int valc = xlCallFunction(values,vmax,xlGetValue(s_eval),1,expr);
    return valc >= 0 ? valc : xlsError;
}

/* xlWriteToString - print an expression to a string with quoting */
xlEXPORT const char *xlWriteToString(xlValue expr,char *buf,xlFIXTYPE len)
{
    return PrintToString(expr,buf,len,TRUE);
}

/* xlDisplayToString - print an expression to a string */
xlEXPORT const char *xlDisplayToString(xlValue expr,char *buf,xlFIXTYPE len)
{
    return PrintToString(expr,buf,len,FALSE);
}

/* PrintToString - print an expression to a string */
static const char *PrintToString(xlValue expr,char *buf,xlFIXTYPE len,int escFlag)
{
    xlErrorTarget target;
    xlValue stream;
    char *p;
    int ch;

    /* trap errors */
    xlPushTarget(&target);
    if (setjmp(target.target)) {
        xlPopTarget();
        return NULL;
    }

    /* create a stream */
    xlCPush(expr);
    stream = xlNewUStream();
    
    /* print the expression to the stream */
    if (escFlag)
        xlWrite(expr,stream);
    else
        xlDisplay(expr,stream);
    xlDrop(1);

    /* done trapping errors */
    xlPopTarget();

    /* allocate the string if necessary */
    if (buf == NULL) {
        len = xlGetStrLength(stream) + 1;
        if ((buf = malloc(len)) == NULL)
            return NULL;
    }
    p = buf;
    
    /* copy the stream characters to the buffer */
    while ((ch = xlGetC(stream)) != EOF) {
        if (--len < 1)
            return NULL;
        *p++ = ch;
    }
    *p = '\0';
    
    /* return successfully */
    return buf;
}

/* xlFreeString - free a string allocated by xlWriteToString or xlDisplayToString */
xlEXPORT void xlFreeString(char *str)
{
    free(str);
}

/* xlGetArgInstance - get an instance of a class */
xlEXPORT xlValue xlGetArgInstance(xlValue cls)
{
    xlValue obj = xlGetArgObject();
    if (!xlInstanceP(cls,obj))
        xlFmtError("expecting an instance of ~S - ~S",cls,obj);
    return obj;
}

/* xlSetIdleHandler - set the idle handler */
xlEXPORT void xlSetIdleHandler(void (*handler)(void *data),void *data)
{
    idleHandler = handler;
    idleData = data;
}

/* xlIdle - call idle handler */
xlEXPORT int xlIdle(void)
{
    if (idleHandler) {
        (*idleHandler)(idleData);
        xlosFlushOutput();
        return TRUE;
    }
    return FALSE;
}
