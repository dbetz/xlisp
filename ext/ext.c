/* ext.c - a sample xlisp extension dll */

/* Load this library with the command:

        (load-library "ext.dll")
   
   You should then be able to invoke the my-add function:
       
        (my-add 2 3) ==> 5
   
            
*/

#include "xlisp.h"

#ifndef EXPORT
#define EXPORT __declspec(dllexport)
#endif

/* prototypes */
static xlValue myadd(void);

/* initialize - the dll initialization function */
EXPORT int initialize(void)
{
    xlSubr("MY-ADD",myadd);
    return TRUE;
}

/* myadd - my add function */
static xlValue myadd(void)
{
    xlFIXTYPE arg1,arg2;
    
    /* parse the argument list */
    xlVal = xlGetArgFixnum(); arg1 = xlGetFixnum(xlVal);
    xlVal = xlGetArgFixnum(); arg2 = xlGetFixnum(xlVal);
    xlLastArg();

    /* return the result of adding the two arguments */
    return xlMakeFixnum(arg1 + arg2);
}