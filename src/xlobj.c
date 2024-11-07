/* xlobj.c - xlisp object-oriented programming support */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* external variables */
extern xlValue s_stdout;

/* local variables */
static xlValue k_initialize;
static xlValue c_class,c_object;

/* local prototypes */
static int FindIVarOffset(xlValue cls,xlValue sym,int *pOffset);
static xlValue newobject(xlValue cls,int size);
static void clnew(void);
static xlValue clmakeinstance(void);
static xlValue clinitialize(void);
static xlValue clanswer(void);
static xlValue clshow(void);
static xlValue obinitialize(void);
static xlValue obclass(void);
static xlValue obprint(void);
static xlValue obshow(void);
static xlValue obgetvariable(void);
static xlValue obsetvariable(void);
static xlValue obinstancebindings(void);
static int instancebindings(xlValue cls,xlValue obj,int offset,xlValue *pLast);
static void showobject(xlValue obj,xlValue fptr);
static int ShowIVars(xlValue fptr,xlValue obj,xlValue cls,int offset);
static void parse_cvars(xlValue frame,xlValue defs);
static xlValue entermsg(xlValue cls,xlValue msg);
static int getivcnt(xlValue cls,int ivar);
static xlValue copylist(xlValue list);

/* xlClass - make a new class object */
xlEXPORT xlValue xlClass(const char *name,xlValue super,const char *vars)
{
    xlValue class,new,last = xlNil;
    xlFIXTYPE superCnt,iCnt = 0;
    char pname[xlSTRMAX + 1],*p;
    int ch;

    /* set the superclass to 'object' if none was specified */
    if (!super)
        super = c_object;

    /* protect the superclass and start the variable list */
    xlCheck(3);
    xlPush(super);
    xlPush(xlNil);

    /* build the instance variable list */
    if (*vars) {
        do {

            /* find the next variable name */
            for (p = pname; (ch = *vars) != '\0' && ch != ','; ++vars)
                if (p < &pname[xlSTRMAX])
                    *p++ = ch;
            *p = '\0';

            /* add the variable to the list */
            new = xlCons(xlEnter(pname),xlNil);
            if (last == xlNil)
                xlSetTop(new);
            else
                xlSetCdr(last,new);
            last = new;
            ++iCnt;
        } while (*vars++ == ',');
    }
    
    /* get the number of instance variables in the superclass */
    superCnt = super ? xlGetFixnum(xlGetIVar(super,xlivIVARTOTAL)) : 0;

    /* create the class object */
    xlPush(xlEnter(name));
    class = newobject(c_class,xlCLASSSIZE);
    xlSetValue(xlTop(),class);

    /* initialize the new class */
    xlSetIVar(class,xlivNAME,xlPop());
    xlSetIVar(class,xlivIVARS,xlPop());
    xlSetIVar(class,xlivSUPERCLASS,xlPop());
    xlSetIVar(class,xlivIVARTOTAL,xlMakeFixnum(superCnt + iCnt));
    xlSetIVar(class,xlivIVARCNT,xlMakeFixnum(iCnt));
    xlSetIVar(class,xlivCVARS,xlNewFrame(xlENV,xlNil,xlFIRSTENV + 1));
    xlSetEnvNames(xlGetIVar(class,xlivCVARS),xlCons(xlEnter("%%CLASS"),xlNil));
    xlSetEnvElement(xlGetIVar(class,xlivCVARS),xlFIRSTENV,class);

    /* return the new class */
    return class;
}

/* xlMethod - add a method to a class */
xlEXPORT void xlMethod(xlValue cls,const char *selector,xlValue (*handler)(void))
{
    xlValue binding;

    /* enter the message selector */
    binding = entermsg(cls,xlEnter(selector));

    /* store the method for this message */
    xlSetCdr(binding,xlMakeSubr(selector,handler));
}

/* xlXMethod - add a method returning multiple values to a class */
xlEXPORT void xlXMethod(xlValue cls,const char *selector,void (*handler)(void))
{
    xlValue binding;

    /* enter the message selector */
    binding = entermsg(cls,xlEnter(selector));

    /* store the method for this message */
    xlSetCdr(binding,xlMakeXSubr(selector,handler));
}

/* xlInstanceP - check to see if an object is an instance of a class */
xlEXPORT int xlInstanceP(xlValue class,xlValue obj)
{
    xlValue cls = xlGetClass(obj);
    while (cls != xlNil) {
        if (cls == class)
            return TRUE;
        cls = xlGetIVar(cls,xlivSUPERCLASS);
    }
    return FALSE;
}

/* xlNewInstance - create an instance of a class */
xlEXPORT xlValue xlNewInstance(xlValue cls)
{
    return newobject(cls,getivcnt(cls,xlivIVARTOTAL));
}

/* xlFindIVarOffset - find the offset of an instance variable with an object */
int xlFindIVarOffset(xlValue cls,xlValue sym,int *pOffset)
{
    *pOffset = xlFIRSTIVAR;
    return FindIVarOffset(cls,sym,pOffset);
}

/* FindIVarOffset - helper function for xlFindIVarOffset */
static int FindIVarOffset(xlValue cls,xlValue sym,int *pOffset)
{
    if (cls != xlNil) {
        xlValue superCls = xlGetIVar(cls,xlivSUPERCLASS);
        if (superCls && FindIVarOffset(superCls,sym,pOffset))
            return TRUE;
        else {
            xlValue names = xlGetIVar(cls,xlivIVARS);
            for (; names != xlNil; names = xlCdr(names)) {
                if (sym == xlCar(names))
                    return TRUE;
                ++(*pOffset);
            }
        }
    }
    return FALSE;
}

/* newobject - allocate and initialize a new object */
static xlValue newobject(xlValue cls,int size)
{
    xlValue val;
    xlCPush(cls);
    val = xlNewVector(1 + size); /* class, ivars */
    val->type = xlOBJECT;
    xlSetClass(val,xlPop());
    return val;
}

/* xlSend - send a message to an object */
void xlSend(xlValue obj,xlValue sym)
{
    xlValue msg,cls,p;

    /* look for the message in the class or superclasses */
    for (cls = xlGetClass(obj); cls; cls = xlGetIVar(cls,xlivSUPERCLASS))
        for (p = xlGetIVar(cls,xlivMESSAGES); p; p = xlCdr(p))
            if ((msg = xlCar(p)) != xlNil && xlCar(msg) == sym) {
                xlPush(obj); ++xlArgC; /* insert 'self' argument */
                xlVal = xlCdr(msg);    /* get the method */
                xlNext = xlApply;    /* invoke the method */
                return;
            }

    /* message not found */
    xlFmtError("~S has no method for the message '~S",obj,sym);
}

/* xsendsuper - built-in function 'send-super' */
void xsendsuper(void)
{
    xlValue class,sym,msg,cls,p;

    /* get the method class and the message selector */
    class = xlGetArgObject();
    sym = xlGetArgSymbol();
    
    /* look for the message in the superclasses */
    for (cls = xlGetIVar(class,xlivSUPERCLASS); cls; cls = xlGetIVar(cls,xlivSUPERCLASS))
        for (p = xlGetIVar(cls,xlivMESSAGES); p; p = xlCdr(p))
            if ((msg = xlCar(p)) != xlNil && xlCar(msg) == sym) {
                xlVal = xlCdr(msg); /* get the method */
                xlNext = xlApply;   /* invoke the method */
                return;
            }

    /* message not found */
    xlFmtError("class ~S has no method for the message '~S",class,sym);
}

/* obinitialize - default 'initialize' method */
static xlValue obinitialize(void)
{
    xlValue self;
    self = xlGetArgObject();
    xlLastArg();
    return (self);
}

/* obclass - get the class of an object */
static xlValue obclass(void)
{
    xlValue self;
    self = xlGetArgObject();
    xlLastArg();
    return (xlGetClass(self));
}

/* obgetvariable - get the value of an instance variable */
static xlValue obgetvariable(void)
{
    int offset = xlFIRSTIVAR;
    xlValue self,sym;

    /* parse the argument list */
    self = xlGetArgObject();
    sym = xlGetArgSymbol();
    xlLastArg();

    /* get the instance variable offset */
    if (!xlFindIVarOffset(xlGetClass(self),sym,&offset))
        xlFmtError("no instance variable ~S",sym);

    /* return the value */
    return xlGetIVar(self,offset);
}

/* obsetvariable - set the value of an instance variable */
static xlValue obsetvariable(void)
{
    int offset = xlFIRSTIVAR;
    xlValue self,sym,val;
    
    /* parse the argument list */
    self = xlGetArgObject();
    sym = xlGetArgSymbol();
    val = xlGetArg();
    xlLastArg();

    /* get the instance variable offset */
    if (!xlFindIVarOffset(xlGetClass(self),sym,&offset))
        xlFmtError("no instance variable ~S",sym);

    /* set and return the value */
    xlSetIVar(self,offset,val);
    return val;
}

/* obinstancebindings - get a list of instance variable bindings */
static xlValue obinstancebindings(void)
{
    xlValue self,last;
    
    /* parse the argument list */
    self = xlGetArgObject();
    xlLastArg();

    /* initialize */
    xlVal = last = xlNil;

    /* build the list of bindings */
    instancebindings(xlGetClass(self),self,xlFIRSTIVAR,&last);

    /* return the instance bindings */
    return xlVal;
}

/* instancebindings - helper function for xlFindIVarOffset */
static int instancebindings(xlValue cls,xlValue obj,int offset,xlValue *pLast)
{
    xlValue superCls,names,next;
    if (cls != xlNil) {

        /* handle the superclass variables first */
        if ((superCls = xlGetIVar(cls,xlivSUPERCLASS)) != NULL)
            offset = instancebindings(superCls,obj,offset,pLast);

        /* now handle the variables in this class */
        for (names = xlGetIVar(cls,xlivIVARS); names; names = xlCdr(names)) {
            next = xlCons(xlCons(xlCar(names),xlGetIVar(obj,offset)),xlNil);
            if (*pLast)
                xlSetCdr(*pLast,next);
            else
                xlVal = next;
            *pLast = next;
            ++offset;
        }
    }
    return offset;
}

/* obprint - print an object */
static xlValue obprint(void)
{
    xlValue self,class,fptr;
    char buf[256];
    
    /* get self and the file pointer */
    self = xlGetArgObject();
    fptr = (xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput());
    xlLastArg();
    
    /* print the object */
    if ((class = xlGetClass(self)) != xlNil
    &&  (class = xlGetIVar(class,xlivNAME)) != xlNil) {
        xlPutStr(fptr,"#<Object:");
        xlWrite(class,fptr);
        xlPutStr(fptr," #x");
    }
    else
        xlPutStr(fptr,"#<Object #x");
    sprintf(buf,xlAFMT,self);
    strcat(buf,">");
    xlPutStr(fptr,buf);
    return self;
}

/* obshow - show an object */
static xlValue obshow(void)
{
    xlValue self,fptr;

    /* get self and the file pointer */
    self = xlGetArgObject();
    fptr = (xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput());
    xlLastArg();
    
    /* show the object */
    showobject(self,fptr);
    return self;
}

/* showobject - show an object */
static void showobject(xlValue obj,xlValue fptr)
{
    xlValue cls;

    /* get the object's class */
    cls = xlGetClass(obj);

    /* print the object and class */
    xlPutStr(fptr,"\nObject is ");
    xlWrite(obj,fptr);
    xlPutStr(fptr,", Class is ");
    xlWrite(cls,fptr);

    /* print the object's instance variables */
    ShowIVars(fptr,obj,cls,xlFIRSTIVAR);
}

/* ShowIVars - helper function for showobject */
static int ShowIVars(xlValue fptr,xlValue obj,xlValue cls,int offset)
{
    xlValue superCls,names;

    /* first display the superclass variables */
    if ((superCls = xlGetIVar(cls,xlivSUPERCLASS)) != xlNil)
        offset = ShowIVars(fptr,obj,superCls,offset);

    /* then display our variables */
    names = xlGetIVar(cls,xlivIVARS);
    for (; names != xlNil; names = xlCdr(names)) {
        if (offset == xlFIRSTIVAR)
            xlPutStr(fptr,"\nInstance variables:");
        xlPutStr(fptr,"\n  ");
        xlWrite(xlCar(names),fptr);
        xlPutStr(fptr," = ");
        xlWrite(xlGetIVar(obj,offset),fptr);
        ++offset;
    }

    /* return the updated offset for our subclass */
    return offset;
}

/* clmakeinstance - create a new object instance */
static xlValue clmakeinstance(void)
{
    xlValue self;
    self = xlGetArgObject();
    xlLastArg();
    return newobject(self,getivcnt(self,xlivIVARTOTAL));
}

/* clnew - create a new object instance and initialize */
static void clnew(void)
{
    xlValue self;

    /* create a new object */
    self = xlGetArgObject();
    xlVal = newobject(self,getivcnt(self,xlivIVARTOTAL));

    /* send the 'initialize' message */
    xlSend(xlVal,k_initialize);
}

/* clinitialize - initialize a new class */
static xlValue clinitialize(void)
{
    xlValue self,ivars,cvars,super,name;
    xlFIXTYPE n;

    /* get self, the ivars, cvars and superclass */
    self = xlGetArgObject();
    ivars = xlGetArgList();
    cvars = (xlMoreArgsP() ? xlGetArgList() : xlNil);
    super = (xlMoreArgsP() ? xlGetArgObject() : c_object);
    name = (xlMoreArgsP() ? xlGetArgSymbol() : xlNil);
    xlLastArg();

    /* create the class variable environment */
    xlCheck(5);
    xlPush(self);
    xlPush(name);
    xlPush(super);
    xlPush(ivars);
    xlPush(cvars);
    xlVal = xlNewFrame(xlENV,xlGetIVar(super,xlivCVARS),xlFIRSTENV + xlLength(xlTop()) + 1);
    parse_cvars(xlVal,xlPop());
    xlSetEnvElement(xlVal,xlFIRSTENV,self);

    /* store the instance and class variable lists and the superclass */
    xlSetIVar(self,xlivNAME,name);
    xlSetIVar(self,xlivIVARS,copylist(ivars));
    xlSetIVar(self,xlivCVARS,xlVal);
    xlSetIVar(self,xlivSUPERCLASS,super);
    xlDrop(4);
    
    /* compute the instance variable count */
    n = xlLength(ivars);
    xlSetIVar(self,xlivIVARCNT,xlMakeFixnum(n));
    n += getivcnt(super,xlivIVARTOTAL);
    xlSetIVar(self,xlivIVARTOTAL,xlMakeFixnum(n));

    /* return the new class object */
    return (self);
}

/* parse_cvars - parse class variable declarations */
static void parse_cvars(xlValue frame,xlValue defs)
{
    int i = xlFIRSTENV; /* leave space for %%CLASS */
    xlValue this,last;
    xlCheck(2);
    xlPush(frame);
    xlPush(defs);
    last = xlCons(xlEnter("%%CLASS"),xlNil);
    xlSetEnvNames(frame,last);
    for (; xlConsP(defs); defs = xlCdr(defs)) {
        xlValue def = xlCar(defs), sym = NULL, val = NULL;
        if (xlSymbolP(def)) {
            sym = def;
            val = xlNil;
        }
        else if (xlConsP(def) && xlSymbolP(xlCar(def))) {
            sym = xlCar(def);
            val = xlConsP(xlCdr(def)) ? xlCar(xlCdr(def)) : xlNil;
        }
        else
            xlError("expecting a class variable definition",def);
        this = xlCons(sym,xlNil);
        xlSetCdr(last,this);
        xlSetElement(frame,++i,val);
        last = this;
    }
    xlDrop(2);
}

/* clanswer - define a method for answering a message */
static xlValue clanswer(void)
{
    xlValue self,msg,fargs,code,mptr;

    /* message symbol, formal argument list and code */
    self = xlGetArgObject();
    msg = xlGetArgSymbol();
    fargs = xlGetArg();
    code = xlGetArgList();
    xlLastArg();

    /* protect our working structures */
    xlCheck(4);
    xlPush(self);
    xlPush(msg);
    xlPush(fargs);
    xlPush(code);
    
    /* make a new message list entry */
    mptr = entermsg(self,msg);

    /* compile and store the method */
    xlVal = xlCompileMethod(msg,fargs,code,xlGetIVar(self,xlivCVARS));
    xlSetCdr(mptr,xlMakeClosure(xlVal,xlGetIVar(self,xlivCVARS)));
    xlDrop(4);
    
    /* return the object */
    return (self);
}

/* clshow - show a class */
static xlValue clshow(void)
{
    xlValue self,fptr,env;
    int first = TRUE;
    
    /* get self and the file pointer */
    self = xlGetArgObject();
    fptr = (xlMoreArgsP() ? xlGetOutputPort() : xlCurOutput());
    xlLastArg();
    
    /* show the object */
    showobject(self,fptr);

    /* print the object's class variables */
    for (env = xlGetIVar(self,xlivCVARS); env != xlNil; env = xlGetNextFrame(env)) {
        xlValue names = xlCdr(xlGetEnvNames(env)); /* skip the %%class variable */
        xlFIXTYPE i,maxi = xlGetEnvSize(env);
        for (i = xlFIRSTENV + 1; i < maxi; ++i) {
            if (first) {
                xlPutStr(fptr,"\nClass variables:");
                first = FALSE;
            }
            xlPutStr(fptr,"\n  ");
            xlWrite(xlCar(names),fptr);
            xlPutStr(fptr," = ");
            xlWrite(xlGetEnvElement(env,i),fptr);
            names = xlCdr(names);
        }
    }
    return self;
}

/* entermsg - add a message to a class */
static xlValue entermsg(xlValue cls,xlValue msg)
{
    xlValue lptr,mptr;

    /* lookup the message */
    for (lptr = xlGetIVar(cls,xlivMESSAGES); lptr; lptr = xlCdr(lptr))
        if (xlCar(mptr = xlCar(lptr)) == msg)
            return (mptr);

    /* allocate a new message entry if one wasn't found */
    xlCPush(xlCons(msg,xlNil));
    xlSetIVar(cls,xlivMESSAGES,xlCons(xlTop(),xlGetIVar(cls,xlivMESSAGES)));

    /* return the symbol node */
    return (xlPop());
}

/* getivcnt - get the number of instance variables for a class */
static int getivcnt(xlValue cls,int ivar)
{
    xlValue cnt;
    if ((cnt = xlGetIVar(cls,ivar)) == xlNil || !xlFixnumP(cnt))
        xlError("bad value for instance variable count",cnt);
    return ((int)xlGetFixnum(cnt));
}

/* copylist - make a copy of a list */
static xlValue copylist(xlValue list)
{
    xlValue last,next;
    
    /* initialize */
    xlCPush(xlNil); last = xlNil;
    
    /* copy the list */
    for (; xlConsP(list); list = xlCdr(list)) {
        next = xlCons(xlCar(list),xlNil);
        if (last) xlSetCdr(last,next);
        else xlSetTop(next);
        last = next;
    }
    
    /* return the new list */
    return xlPop();
}

/* xlObSymbols - initialize symbols */
void xlObSymbols(void)
{
    /* enter the object related symbols */
    k_initialize = xlEnter("INITIALIZE");

    /* get the Object and Class symbol values */
    c_object = xlGetValue(xlEnter("OBJECT"));
    c_class  = xlGetValue(xlEnter("CLASS"));
}

/* xlInitObjects - object function initialization routine */
void xlInitObjects(void)
{
    /* create the 'object' and 'class' objects */
    c_object = c_class = NULL;
    c_object = xlClass("OBJECT",xlNil,"");
    c_class = xlClass("CLASS",c_object,"NAME,MESSAGES,IVARS,CVARS,SUPERCLASS,IVARCNT,IVARTOTAL");
    xlSetClass(c_object,c_class);
    xlSetClass(c_class,c_class);

    /* enter the 'Object' methods */
    xlMethod(c_object,"INITIALIZE",obinitialize);
    xlMethod(c_object,"CLASS",obclass);
    xlMethod(c_object,"GET-VARIABLE",obgetvariable);
    xlMethod(c_object,"SET-VARIABLE!",obsetvariable);
    xlMethod(c_object,"INSTANCE-BINDINGS",obinstancebindings);
    xlMethod(c_object,"PRINT",obprint);
    xlMethod(c_object,"SHOW",obshow);

    /* enter the 'Class' methods */
    xlMethod(c_class,"MAKE-INSTANCE",clmakeinstance);
    xlXMethod(c_class,"NEW",clnew);
    xlMethod(c_class,"INITIALIZE",clinitialize);
    xlMethod(c_class,"ANSWER",clanswer);
    xlMethod(c_class,"SHOW",clshow);
}
