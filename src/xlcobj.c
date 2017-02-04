#include "xlisp.h"

/* xlMakeCClass - make a new C class object */
xlEXPORT xlCClass *xlMakeCClass(xlCClassDef *def,xlValue super)
{
    xlCClass *ccls;
    
    /* create the class with a single instance variable for the handle */
    xlPush(xlClass(def->name,super,"HANDLE"));
    
    /* define all of the methods */
    if (def->methods) {
        xlSubrDef *m;
        for (m = def->methods; m->name != NULL; ++m)
            xlMethod(xlTop(),m->name,m->subr);
    }
        
    /* define all of the methods that return multiple values */
    if (def->xmethods) {
        xlXSubrDef *xm;
        for (xm = def->xmethods; xm->name != NULL; ++xm)
            xlXMethod(xlTop(),xm->name,xm->subr);
    }
    
    /* allocate and initialize the C class structure */
    if (!(ccls = (xlCClass *)malloc(sizeof(xlCClass))))
        return NULL;
    ccls->def = def;
    ccls->cls = xlPop();
    
    /* return the C class */
    return ccls;
}

/* xlMakeUninitializedCInstance - make an uninitialized instance of a C class */
xlEXPORT void *xlMakeUninitializedCInstance(xlCClass *ccls)
{
    xlPush(xlNewInstance(ccls->cls));
    xlSetIVar(xlTop(),xlHANDLEIVAR,xlMakeForeignPtr(ccls,NULL));
    return xlPop();
}

/* xlGetArgUninitializedCInstance - get an uninitialized instance of a C class */
xlEXPORT void *xlGetArgUninitializedCInstance(xlCClass *ccls)
{
    xlValue obj;
    
    /* get an object argument */
    obj = xlGetArgObject();
    
    /* make sure it is an instance of the specified class */
    if (!xlInstanceP(ccls->cls,obj))
        xlFmtError("expecting an instance of ~S - ~S",ccls->cls,obj);

    /* make an uninitalized foreign pointer */
    xlSetIVar(obj,xlHANDLEIVAR,xlMakeForeignPtr(ccls,NULL));
    
    /* return the partially initialized object */
    return obj;
}

/* xlGetArgCInstance - get an instance of a C class */
xlEXPORT void *xlGetArgCInstance(xlCClass *ccls)
{
    xlValue obj,handle;
    void *ptr;
    
    /* get an object argument */
    obj = xlGetArgObject();
    
    /* make sure it is an instance of the specified class */
    if (!xlInstanceP(ccls->cls,obj))
        xlFmtError("expecting an instance of ~S - ~S",ccls->cls,obj);

    /* get the handle associated with the C object */
    handle = xlGetIVar(obj,xlHANDLEIVAR);
    
    /* make sure it is a foreign pointer of the correct type */
    if (!xlForeignPtrP(handle) || xlGetFPType(handle) != ccls)
        xlError("bad handle in C object",handle);
        
    /* make sure that the foreign pointer has been initialized */
    if ((ptr = xlGetFPtr(handle)) == NULL)
        xlError("uninitalized handle in C object",handle);
        
    /* return the foreign pointer */
    return ptr;
}
