/* xlitersq.c - routines to iterate over a sequence */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

#define IS_X            0
#define IS_LIST         1
#define IS_KEYFCN       2
#define IS_TESTFCN      3
#define IS_TRESULT      4
#define IS_COUNT        5
#define IS_INDEX        6
#define IS_END          7
#define IS_VALUE        8
#define IS_WORK         9
#define IS_ACTION       10
#define IS_CDISPATCH    11
#define _ISSIZE         12

#define MS_FCN          0
#define MS_ARGC         1
#define MS_VALUE        2
#define MS_WORK         3
#define MS_ACTION       4
#define MS_CDISPATCH    5
#define _MSSIZE         6

/* action routine definitions */
typedef xlValue (*ACTION)(int op,xlValue val,xlValue *d);
typedef xlValue (*MACTION)(xlValue val,xlValue *d);
#define IS_FETCH        1
#define IS_UPDATE       2

/* external variables */
extern xlValue xlVal,k_key,k_count,k_start,k_end;

static void iterseq1(xlValue ivalue,xlValue tresult,ACTION action);
static void iterlist1(xlValue ivalue,xlValue tresult,ACTION action);
static void do_iterseq1(void);
static void seq1key_restore(void);
static void do_seq1key(xlValue *);
static void seq1test_restore(void);
static void do_seq1test(xlValue,xlValue *);

static void iterseq2(xlValue ivalue,ACTION action);
static void iterlist2(xlValue ivalue,ACTION action);
static void do_iterseq2(void);
static void seq2key_restore(void);
static void do_seq2key(xlValue *);
static void seq2test_restore(void);
static void seq2test_check(xlValue *);
static void do_seq2test(xlValue,xlValue *);

static void seqdummy_restore(void);
static xlValue *seqkeytest_mark(xlValue *);
static xlValue *seqkeytest_skipit(xlValue *);
static void seqkeytest_unwind(void);
static xlValue *seqdummy_print(xlValue *);
static xlValue *seqkey_print(xlValue *);
static xlValue *seqtest_print(xlValue *);
static void show_seqcontinuation(xlValue *,char *);

static void mapseq(xlValue ivalue,MACTION action);
static void do_mapseq(void);
static void mapseq_restore(void);
static xlValue *mapseq_mark(xlValue *);
static xlValue *mapseq_skipit(xlValue *);
static void mapseq_unwind(void);
static xlValue *mapseq_print(xlValue *);
static void mapseqdummy_restore(void);
static xlValue *mapseqdummy_print(xlValue *);
static void show_mapseqcontinuation(xlValue *,char *);

static void maplist(xlValue ivalue,ACTION action);
static void do_maplist(xlValue *);
static void maplist_restore(void);
static xlValue *maplist_mark(xlValue *);
static xlValue *maplist_skipit(xlValue *);
static void maplist_unwind(void);
static xlValue *maplist_print(xlValue *);
static void maplistdummy_restore(void);
static xlValue *maplistdummy_print(xlValue *);
static void show_maplistcontinuation(xlValue *,char *);

static xlValue is_find(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return val;
    case IS_UPDATE:
        if (val)
            d[IS_VALUE] = xlCar(d[IS_LIST]);
        return val;
    }
    return xlNil; /* never reached */
}

/* xfind - built-in function 'find' */
void xfind(void)
{
    iterseq2(xlNil,is_find);
}

/* xfindif - built-in function 'find-if' */
void xfindif(void)
{
    iterseq1(xlNil,xlTrue,is_find);
}

/* xfindifnot - built-in function 'find-if-not' */
void xfindifnot(void)
{
    iterseq1(xlNil,xlNil,is_find);
}

static xlValue is_member(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return val;
    case IS_UPDATE:
        if (val)
            d[IS_VALUE] = d[IS_LIST];
        return val;
    }
    return xlNil; /* never reached */
}

/* xmember - built-in function 'member' */
void xmember(void)
{
    iterlist2(xlNil,is_member);
}

/* xmemberif - built-in function 'member-if' */
void xmemberif(void)
{
    iterlist1(xlNil,xlTrue,is_member);
}

/* xmemberifnot - built-in function 'member-if-not' */
void xmemberifnot(void)
{
    iterlist1(xlNil,xlNil,is_member);
}

static xlValue is_assoc(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        if (!xlConsP(val))
            xlError("bad association list entry",val);
        return xlCar(val);
    case IS_UPDATE:
        if (val)
            d[IS_VALUE] = xlCar(d[IS_LIST]);
        return val;
    }
    return xlNil; /* never reached */
}

/* xassoc - built-in function 'assoc' */
void xassoc(void)
{
    iterlist2(xlNil,is_assoc);
}

/* xassocif - built-in function 'assoc-if' */
void xassocif(void)
{
    iterlist1(xlNil,xlTrue,is_assoc);
}

/* xassocifnot - built-in function 'assoc-if-not' */
void xassocifnot(void)
{
    iterlist1(xlNil,xlNil,is_assoc);
}

static xlValue is_rassoc(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        if (!xlConsP(val))
            xlError("bad association list entry",val);
        return xlCdr(val);
    case IS_UPDATE:
        if (val)
            d[IS_VALUE] = xlCar(d[IS_LIST]);
        return val;
    }
    return xlNil; /* never reached */
}

/* xrassoc - built-in function 'rassoc' */
void xrassoc(void)
{
    iterlist2(xlNil,is_rassoc);
}

/* xrassocif - built-in function 'rassoc-if' */
void xrassocif(void)
{
    iterlist1(xlNil,xlTrue,is_rassoc);
}

/* xrassocifnot - built-in function 'rassoc-if-not' */
void xrassocifnot(void)
{
    iterlist1(xlNil,xlNil,is_rassoc);
}

static xlValue is_remove(int op,xlValue val,xlValue *d)
{
    xlFIXTYPE n;
    switch (op) {
    case IS_FETCH:
        return val;
    case IS_UPDATE:
        n = d[IS_COUNT] ? xlGetFixnum(d[IS_COUNT]) : 1;
        if (n <= 0 || !val) {
            if (d[IS_WORK]) {
                xlSetCdr(d[IS_WORK],xlCons(xlCar(d[IS_LIST]),xlNil));
                d[IS_WORK] = xlCdr(d[IS_WORK]);
            }
            else
                d[IS_VALUE] = d[IS_WORK] = xlCons(xlCar(d[IS_LIST]),xlNil);
        }
        if (val && d[IS_COUNT])
            d[IS_COUNT] = xlMakeFixnum(n - 1);
        return xlNil;
    }
    return xlNil; /* never reached */
}

/* xremove - built-in function 'remove' */
void xremove(void)
{
    iterseq2(xlNil,is_remove);
}

/* xremoveif - built-in function 'remove-if' */
void xremoveif(void)
{
    iterseq1(xlNil,xlTrue,is_remove);
}

/* xremoveifnot - built-in function 'remove-if-not' */
void xremoveifnot(void)
{
    iterseq1(xlNil,xlNil,is_remove);
}

static xlValue is_delete(int op,xlValue val,xlValue *d)
{
    xlFIXTYPE n;
    switch (op) {
    case IS_FETCH:
        return val;
    case IS_UPDATE:
        n = d[IS_COUNT] ? xlGetFixnum(d[IS_COUNT]) : 1;
        if (n > 0 && val) {
            if (d[IS_WORK])
                xlSetCdr(d[IS_WORK],xlNil);
        }
        else {
            if (d[IS_WORK]) {
                xlSetCdr(d[IS_WORK],d[IS_LIST]);
                d[IS_WORK] = xlCdr(d[IS_WORK]);
            }
            else
                d[IS_VALUE] = d[IS_WORK] = d[IS_LIST];
        }
        if (val && d[IS_COUNT])
            d[IS_COUNT] = xlMakeFixnum(n - 1);
        return xlNil;
    }
    return xlNil; /* never reached */
}

/* xdelete - built-in function 'delete' */
void xdelete(void)
{
    iterseq2(xlNil,is_delete);
}

/* xdeleteif - built-in function 'delete-if' */
void xdeleteif(void)
{
    iterseq1(xlNil,xlTrue,is_delete);
}

/* xdeleteifnot - built-in function 'delete-if-not' */
void xdeleteifnot(void)
{
    iterseq1(xlNil,xlNil,is_delete);
}

static xlValue is_count(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return val;
    case IS_UPDATE:
        if (val)
            d[IS_VALUE] = xlMakeFixnum(xlGetFixnum(d[IS_VALUE]) + 1);
        return xlNil;
    }
    return xlNil; /* never reached */
}

/* xcount - built-in function 'count' */
void xcount(void)
{
    iterseq2(xlMakeFixnum((xlFIXTYPE)0),is_count);
}

/* xcountif - built-in function 'count-if' */
void xcountif(void)
{
    iterseq1(xlMakeFixnum((xlFIXTYPE)0),xlTrue,is_count);
}

/* xcountifnot - built-in function 'count-if-not' */
void xcountifnot(void)
{
    iterseq1(xlMakeFixnum((xlFIXTYPE)0),xlNil,is_count);
}

static xlValue is_position(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return val;
    case IS_UPDATE:
        if (val)
            d[IS_VALUE] = d[IS_INDEX];
        return val;
    }
    return xlNil; /* never reached */
}

/* xposition - built-in function 'position' */
void xposition(void)
{
    iterseq2(xlNil,is_position);
}

/* xpositionif - built-in function 'position-if' */
void xpositionif(void)
{
    iterseq1(xlNil,xlTrue,is_position);
}

/* xpositionifnot - built-in function 'position-if-not' */
void xpositionifnot(void)
{
    iterseq1(xlNil,xlNil,is_position);
}

static xlValue ms_mapcar(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return xlCar(val);
    case IS_UPDATE:
        if (d[MS_WORK]) {
            xlSetCdr(d[MS_WORK],xlCons(val,xlNil));
            d[MS_WORK] = xlCdr(d[MS_WORK]);
        }
        else
            d[MS_VALUE] = d[MS_WORK] = xlCons(val,xlNil);
        return xlNil;
    }
    return xlNil; /* never reached */
}

/* xmapcar - built-in function 'mapcar' */
void xmapcar(void)
{
    maplist(xlNil,ms_mapcar);
}

static xlValue ms_mapc(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return xlCar(val);
    case IS_UPDATE:
        return xlNil;
    }
    return xlNil; /* never reached */
}

/* xmapc - built-in function 'mapc' */
void xmapc(void)
{
    maplist(xlTrue,ms_mapc);
}

static xlValue ms_mapcan(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return xlCar(val);
    case IS_UPDATE:
        if (xlConsP(val)) {
            if (d[MS_WORK])
                xlSetCdr(d[MS_WORK],val);
            else
                d[MS_VALUE] = d[MS_WORK] = val;
            for (; xlConsP(xlCdr(val)); val = xlCdr(val))
                ;
            d[MS_WORK] = val;
        }
        return xlNil;
    }
    return xlNil; /* never reached */
}

/* xmapcan - built-in function 'mapcan' */
void xmapcan(void)
{
    maplist(xlNil,ms_mapcan);
}

static xlValue ms_maplist(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return val;
    case IS_UPDATE:
        if (d[MS_WORK]) {
            xlSetCdr(d[MS_WORK],xlCons(val,xlNil));
            d[MS_WORK] = xlCdr(d[MS_WORK]);
        }
        else
            d[MS_VALUE] = d[MS_WORK] = xlCons(val,xlNil);
        return xlNil;
    }
    return xlNil; /* never reached */
}

/* xmaplist - built-in function 'maplist' */
void xmaplist(void)
{
    maplist(xlNil,ms_maplist);
}

static xlValue ms_mapl(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return val;
    case IS_UPDATE:
        return xlNil;
    }
    return xlNil; /* never reached */
}

/* xmapl - built-in function 'mapl' */
void xmapl(void)
{
    maplist(xlTrue,ms_mapl);
}

static xlValue ms_mapcon(int op,xlValue val,xlValue *d)
{
    switch (op) {
    case IS_FETCH:
        return val;
    case IS_UPDATE:
        if (xlConsP(val)) {
            if (d[MS_WORK])
                xlSetCdr(d[MS_WORK],val);
            else
                d[MS_VALUE] = d[MS_WORK] = val;
            for (; xlConsP(xlCdr(val)); val = xlCdr(val))
                ;
            d[MS_WORK] = val;
        }
        return xlNil;
    }
    return xlNil; /* never reached */
}

/* xmapcon - built-in function 'mapcon' */
void xmapcon(void)
{
    maplist(xlNil,ms_mapcon);
}

static xlValue ms_some(xlValue val,xlValue *d)
{
    return val ? (d[MS_VALUE] = val, xlTrue) : xlNil;
}

/* xsome - built-in function 'some' */
void xsome(void)
{
    mapseq(xlNil,ms_some);
}

static xlValue ms_every(xlValue val,xlValue *d)
{
    return !val ? (d[MS_VALUE] = xlNil, xlTrue) : xlNil;
}

/* xevery - built-in function 'every' */
void xevery(void)
{
    mapseq(xlTrue,ms_every);
}

static xlValue ms_notany(xlValue val,xlValue *d)
{
    return val ? (d[MS_VALUE] = xlNil, xlTrue) : xlNil;
}

/* xnotany - built-in function 'notany' */
void xnotany(void)
{
    mapseq(xlTrue,ms_notany);
}

static xlValue ms_notevery(xlValue val,xlValue *d)
{
    return !val ? (d[MS_VALUE] = xlTrue, xlTrue) : xlNil;
}

/* xnotevery - built-in function 'notevery' */
void xnotevery(void)
{
    mapseq(xlNil,ms_notevery);
}

/* sequence dummy continuation dispatch table */
static xlCDispatch cd_seqdummy = {
    seqdummy_restore,
    seqkeytest_mark,
    seqkeytest_skipit,
    seqkeytest_unwind,
    seqkeytest_skipit,
    seqdummy_print
};

/* do_iterseq1 - start the next iteration */
static void do_iterseq1(void)
{
    xlValue *d = xlCSP - _ISSIZE;
    if (xlConsP(d[IS_LIST]))
        do_seq1key(d);
    else {
        xlVal = d[IS_VALUE];
        xlCtlDrop(_ISSIZE);
        xlSVReturn();
    }
}

/* seq1key_restore - restore a sequence key continuation */
static void seq1key_restore(void)
{
    xlValue *d;
    xlCtlPush((xlValue)&cd_seqdummy);
    d = xlCSP - _ISSIZE;
    do_seq1test(xlVal,d);
}

/* sequence key continuation dispatch table */
static xlCDispatch cd_seq1key = {
    seq1key_restore,
    seqkeytest_mark,
    seqkeytest_skipit,
    seqkeytest_unwind,
    seqkeytest_skipit,
    seqkey_print
};

/* do_seq1key - setup to call the key function */
static void do_seq1key(xlValue *d)
{
    xlValue val = (*(ACTION)d[IS_ACTION])(IS_FETCH,xlCar(d[IS_LIST]),d);
    if (d[IS_KEYFCN]) {
        d[IS_CDISPATCH] = (xlValue)&cd_seq1key;
        xlCPush(val);
        xlVal = d[IS_KEYFCN];
        xlArgC = 1;
        xlNext = xlApply;
    }
    else
        do_seq1test(val,d);
}

/* seq1test_restore - restore a sequence key continuation */
static void seq1test_restore(void)
{
    xlValue val,*d;
    xlCtlPush((xlValue)&cd_seqdummy);
    d = xlCSP - _ISSIZE;
    val = (d[IS_TRESULT] && xlVal) || (!d[IS_TRESULT] && !xlVal) ? xlTrue : xlNil;
    if ((*(ACTION)d[IS_ACTION])(IS_UPDATE,val,d)) {
        xlVal = d[IS_VALUE];
        xlCtlDrop(_ISSIZE);
        xlSVReturn();
    }
    else {
        d[IS_LIST] = xlCdr(d[IS_LIST]);
        d[IS_INDEX] = xlMakeFixnum(xlGetFixnum(d[IS_INDEX]) + 1);
        xlNext = do_iterseq1;
    }
}

/* sequence test continuation dispatch table */
static xlCDispatch cd_seq1test = {
    seq1test_restore,
    seqkeytest_mark,
    seqkeytest_skipit,
    seqkeytest_unwind,
    seqkeytest_skipit,
    seqtest_print
};

/* do_seq1test - setup to call the test function */
static void do_seq1test(xlValue key,xlValue *d)
{
    d[IS_CDISPATCH] = (xlValue)&cd_seq1test;
    xlCPush(key);
    xlVal = d[IS_TESTFCN];
    xlArgC = 1;
    xlNext = xlApply;
}

/* iterseq1 - iterate over a sequence (list) */
static void iterseq1(xlValue ivalue,xlValue tresult,ACTION action)
{
    xlValue seq,testfcn,keyfcn,count,start,end;
    
    /* parse the argument list */
    testfcn = xlGetArg();
    seq = xlGetArgList();
    xlGetKeyArg(k_key,xlNil,&keyfcn);
    xlGetKeyArg(k_count,xlNil,&count);
    xlGetKeyArg(k_start,xlNil,&start);
    xlGetKeyArg(k_end,xlNil,&end);
    xlPopArgs();
    
    /* setup the continuation frame */
    xlCtlCheck(_ISSIZE);
    xlCtlPush(xlNil);
    xlCtlPush(seq);
    xlCtlPush(keyfcn);
    xlCtlPush(testfcn);
    xlCtlPush(tresult);
    xlCtlPush(count);
    xlCtlPush(xlMakeFixnum((xlFIXTYPE)0));
    xlCtlPush(end);
    xlCtlPush(ivalue);
    xlCtlPush(xlNil);
    xlCtlPush((xlValue)action);
    xlCtlPush((xlValue)&cd_seq1key);
    
    /* start the iteration */
    do_iterseq1();
}

/* iterlist1 - iterate over a list */
static void iterlist1(xlValue ivalue,xlValue tresult,ACTION action)
{
    xlValue list,testfcn,keyfcn;
    
    /* parse the argument list */
    testfcn = xlGetArg();
    list = xlGetArgList();
    xlGetKeyArg(k_key,xlNil,&keyfcn);
    xlPopArgs();
    
    /* setup the continuation frame */
    xlCtlCheck(_ISSIZE);
    xlCtlPush(xlNil);
    xlCtlPush(list);
    xlCtlPush(keyfcn);
    xlCtlPush(testfcn);
    xlCtlPush(tresult);
    xlCtlPush(xlNil);
    xlCtlPush(xlMakeFixnum((xlFIXTYPE)0));
    xlCtlPush(xlNil);
    xlCtlPush(ivalue);
    xlCtlPush(xlNil);
    xlCtlPush((xlValue)action);
    xlCtlPush((xlValue)&cd_seq1key);
    
    /* start the iteration */
    do_iterseq1();
}

/* do_iterseq2 - start the next iteration */
static void do_iterseq2(void)
{
    xlValue *d = xlCSP - _ISSIZE;
    if (xlConsP(d[IS_LIST]))
        do_seq2key(d);
    else {
        xlVal = d[IS_VALUE];
        xlCtlDrop(_ISSIZE);
        xlSVReturn();
    }
}

/* seq2key_restore - restore a sequence key continuation */
static void seq2key_restore(void)
{
    xlValue *d;
    xlCtlPush((xlValue)&cd_seqdummy);
    d = xlCSP - _ISSIZE;
    do_seq2test(xlVal,d);
}

/* sequence key continuation dispatch table */
static xlCDispatch cd_seq2key = {
    seq2key_restore,
    seqkeytest_mark,
    seqkeytest_skipit,
    seqkeytest_unwind,
    seqkeytest_skipit,
    seqkey_print
};

/* do_seq2key - setup to call the key function */
static void do_seq2key(xlValue *d)
{
    xlValue val = (*(ACTION)d[IS_ACTION])(IS_FETCH,xlCar(d[IS_LIST]),d);
    if (d[IS_KEYFCN]) {
        d[IS_CDISPATCH] = (xlValue)&cd_seq2key;
        xlCPush(val);
        xlVal = d[IS_KEYFCN];
        xlArgC = 1;
        xlNext = xlApply;
    }
    else
        do_seq2test(val,d);
}

/* seq2test_restore - restore a sequence key continuation */
static void seq2test_restore(void)
{
    xlValue *d;
    xlCtlPush((xlValue)&cd_seqdummy);
    d = xlCSP - _ISSIZE;
    seq2test_check(d);
}

/* seq2test_check - check for termination */
static void seq2test_check(xlValue *d)
{
    xlValue val;
    val = (d[IS_TRESULT] && xlVal) || (!d[IS_TRESULT] && !xlVal) ? xlTrue : xlNil;
    if ((*(ACTION)d[IS_ACTION])(IS_UPDATE,val,d)) {
        xlVal = d[IS_VALUE];
        xlCtlDrop(_ISSIZE);
        xlSVReturn();
    }
    else {
        d[IS_LIST] = xlCdr(d[IS_LIST]);
        d[IS_INDEX] = xlMakeFixnum(xlGetFixnum(d[IS_INDEX]) + 1);
        xlNext = do_iterseq2;
    }
}

/* sequence test continuation dispatch table */
static xlCDispatch cd_seq2test = {
    seq2test_restore,
    seqkeytest_mark,
    seqkeytest_skipit,
    seqkeytest_unwind,
    seqkeytest_skipit,
    seqtest_print
};

/* do_seq2test - setup to call the test function */
static void do_seq2test(xlValue key,xlValue *d)
{
    if (d[IS_TESTFCN]) {
        d[IS_CDISPATCH] = (xlValue)&cd_seq2test;
        xlCheck(2);
        xlPush(key);
        xlPush(d[IS_X]);
        xlVal = d[IS_TESTFCN];
        xlArgC = 2;
        xlNext = xlApply;
    }
    else {
        xlArgC = 1;
        xlVal = xlEqv(d[IS_X],key) ? xlTrue : xlNil;
        seq2test_check(d);
    }
}

/* iterseq2 - iterate over a sequence (list) */
static void iterseq2(xlValue ivalue,ACTION action)
{
    xlValue arg,seq,keyfcn,testfcn,tresult,count,start,end;
    
    /* parse the argument list */
    arg = xlGetArg();
    seq = xlGetArgList();
    xlGetKeyArg(k_key,xlNil,&keyfcn);
    xlGetKeyArg(k_count,xlNil,&count);
    xlGetKeyArg(k_start,xlNil,&start);
    xlGetKeyArg(k_end,xlNil,&end);
    xlGetTest(xlNil,&testfcn,&tresult);
    xlPopArgs();
    
    /* setup the continuation frame */
    xlCtlCheck(_ISSIZE);
    xlCtlPush(arg);
    xlCtlPush(seq);
    xlCtlPush(keyfcn);
    xlCtlPush(testfcn);
    xlCtlPush(tresult);
    xlCtlPush(count);
    xlCtlPush(xlMakeFixnum((xlFIXTYPE)0));
    xlCtlPush(end);
    xlCtlPush(ivalue);
    xlCtlPush(xlNil);
    xlCtlPush((xlValue)action);
    xlCtlPush((xlValue)&cd_seq2key);
    
    /* start the iteration */
    do_iterseq2();
}

/* iterlist2 - iterate over a list */
static void iterlist2(xlValue ivalue,ACTION action)
{
    xlValue fcn,tresult;
    xlCtlCheck(_ISSIZE);
    xlCtlPush(xlGetArg());
    xlCtlPush(xlGetArgList());
    xlGetKeyArg(k_key,xlNil,&fcn);
    xlCtlPush(fcn);
    xlGetTest(xlNil,&fcn,&tresult);
    xlCtlPush(fcn);
    xlCtlPush(tresult);
    xlPopArgs();
    xlCtlPush(xlNil);
    xlCtlPush(xlMakeFixnum((xlFIXTYPE)0));
    xlCtlPush(xlNil);
    xlCtlPush(ivalue);
    xlCtlPush(xlNil);
    xlCtlPush((xlValue)action);
    xlCtlPush((xlValue)&cd_seq2key);
    do_iterseq2();
}

/* seqdummy_restore - restore a dummy frame (an error) */
static void seqdummy_restore(void)
{
    xlFmtError("shouldn't happen -- seqdummy_restore");
}

/* seqkeytest_mark - mark a sequence key/test continuation */
static xlValue *seqkeytest_mark(xlValue *p)
{
    --p;        /* action */
    xlMark(*--p);       /* work */
    xlMark(*--p);       /* value */
    xlMark(*--p);       /* end */
    xlMark(*--p);       /* index */
    xlMark(*--p);       /* count */
    xlMark(*--p);       /* tresult */
    xlMark(*--p);       /* testfcn */
    xlMark(*--p);       /* keyfcn */
    xlMark(*--p);       /* list */
    xlMark(*--p);       /* x */
    return p;
}

/* seqkeytest_skipit - unmark/unstack a sequence key/test continuation (just skip over it) */
static xlValue *seqkeytest_skipit(xlValue *p)
{
    return p - _ISSIZE + 1;
}

/* seqkeytest_unwind - unwind past a sequence key/test continuation (just skip over it) */
static void seqkeytest_unwind(void)
{
    xlCtlDrop(_ISSIZE - 1);
}

/* seqdummy_print - print a sequence dummy continuation */
static xlValue *seqdummy_print(xlValue *p)
{
    p -= _ISSIZE - 1;
    show_seqcontinuation(p,"SeqDummy");
    return p;
}

/* seqkey_print - print a sequence key continuation */
static xlValue *seqkey_print(xlValue *p)
{
    p -= _ISSIZE - 1;
    show_seqcontinuation(p,"SeqKey");
    return p;
}

/* seqtest_print - print a sequence key continuation */
static xlValue *seqtest_print(xlValue *p)
{
    p -= _ISSIZE - 1;
    show_seqcontinuation(p,"SeqTest");
    return p;
}

/* show_seqcontinuation - show a sequence key or test continuation */
static void show_seqcontinuation(xlValue *d,char *tag)
{
    xlErrPutStr("\n ");
    xlErrPutStr(tag);
    xlErrPutStr("\n  x: ");
    xlErrPrint(d[IS_X]);
    xlErrPutStr("\n  list: ");
    xlErrPrint(d[IS_LIST]);
    xlErrPutStr("\n  keyfcn: ");
    xlErrPrint(d[IS_KEYFCN]);
    xlErrPutStr("\n  testfcn: ");
    xlErrPrint(d[IS_TESTFCN]);
    xlErrPutStr("\n  tresult: ");
    xlErrPrint(d[IS_TRESULT]);
    xlErrPutStr("\n  count: ");
    xlErrPrint(d[IS_COUNT]);
    xlErrPutStr("\n  index: ");
    xlErrPrint(d[IS_INDEX]);
    xlErrPutStr("\n  end: ");
    xlErrPrint(d[IS_END]);
    xlErrPutStr("\n  value: ");
    xlErrPrint(d[IS_VALUE]);
    xlErrPutStr("\n  work: ");
    xlErrPrint(d[IS_WORK]);
}

/* map dummy continuation dispatch table */
static xlCDispatch cd_mapseqdummy = {
    mapseqdummy_restore,
    mapseq_mark,
    mapseq_skipit,
    mapseq_unwind,
    mapseq_skipit,
    mapseqdummy_print
};

/* mapseq_restore - restore a sequence key continuation */
static void mapseq_restore(void)
{
    xlValue *d;
    xlCtlPush((xlValue)&cd_mapseqdummy);
    d = xlCSP - _MSSIZE;
    if ((*(MACTION)d[MS_ACTION])(xlVal,d)) {
        xlDrop((int)xlGetFixnum(d[MS_ARGC]));
        xlVal = d[MS_VALUE];
        xlCtlDrop(_MSSIZE);
        xlSVReturn();
    }
    else
        xlNext = do_mapseq;
}

/* mapseqdummy_restore - restore a dummy frame (an error) */
static void mapseqdummy_restore(void)
{
    xlFmtError("shouldn't happen -- mapdummy_restore");
}

/* mapseq_mark - mark a sequence key/test continuation */
static xlValue *mapseq_mark(xlValue *p)
{
    --p;        /* action */
    xlMark(*--p);       /* work */
    xlMark(*--p);       /* value */
    xlMark(*--p);       /* argc */
    xlMark(*--p);       /* fcn */
    return p;
}

/* mapseq_skipit - unmark/unstack a sequence key/test continuation (just skip over it) */
static xlValue *mapseq_skipit(xlValue *p)
{
    return p - _MSSIZE + 1;
}

/* mapseq_unwind - unwind past a sequence key/test continuation (just skip over it) */
static void mapseq_unwind(void)
{
    xlCtlDrop(_MSSIZE - 1);
}

/* mapseq_print - print a map continuation */
static xlValue *mapseq_print(xlValue *p)
{
    p -= _MSSIZE - 1;
    show_mapseqcontinuation(p,"MapSeq");
    return p;
}

/* mapseqdummy_print - print a dummy map continuation */
static xlValue *mapseqdummy_print(xlValue *p)
{
    p -= _MSSIZE - 1;
    show_mapseqcontinuation(p,"MapSeqDummy");
    return p;
}

/* show_mapseqcontinuation - print a map continuation */
static void show_mapseqcontinuation(xlValue *d,char *tag)
{
    xlErrPutStr("\n ");
    xlErrPutStr(tag);
    xlErrPutStr("\n  fcn: ");
    xlErrPrint(d[MS_FCN]);
    xlErrPutStr("\n  argc: ");
    xlErrPrint(d[MS_ARGC]);
    xlErrPutStr("\n  value: ");
    xlErrPrint(d[MS_VALUE]);
    xlErrPutStr("\n  work: ");
    xlErrPrint(d[MS_WORK]);
}

/* sequence key continuation dispatch table */
static xlCDispatch cd_mapseq = {
    mapseq_restore,
    mapseq_mark,
    mapseq_skipit,
    mapseq_unwind,
    mapseq_skipit,
    mapseq_print
};

/* do_mapseq - start the next iteration */
static void do_mapseq(void)
{
    xlValue *d = xlCSP - _MSSIZE;
    xlValue *finalsp,*argp,x;
    int argc;

    /* initialize */
    xlArgC = argc = (int)xlGetFixnum(d[MS_ARGC]);
    finalsp = argp = xlSP + argc;

    /* get the function to apply */
    xlVal = d[MS_FCN];

    /* build the argument list for the next application */
    xlCheck(argc);
    while (--argc >= 0) {
        x = *--argp;
        if (xlConsP(x)) {
            xlPush(xlCar(x));
            *argp = xlCdr(x);
        }
        else {
            xlSP = finalsp;
            xlVal = d[MS_VALUE];
            xlCtlDrop(_MSSIZE);
            xlSVReturn();
            return;
        }
    }

    /* apply the function to the argument list */
    d[MS_CDISPATCH] = (xlValue)&cd_mapseq;
    xlNext = xlApply;
}

/* mapseq - iterate over a sequence (list) */
static void mapseq(xlValue ivalue,MACTION action)
{
    xlValue fcn;
    fcn = xlGetArg();
    xlCtlCheck(_MSSIZE);
    xlCtlPush(fcn);
    xlCtlPush(xlMakeFixnum((xlFIXTYPE)xlArgC));
    xlCtlPush(ivalue);
    xlCtlPush(xlNil);
    xlCtlPush((xlValue)action);
    xlCtlPush((xlValue)&cd_mapseq);
    do_mapseq();
}

/* map list dummy continuation dispatch table */
static xlCDispatch cd_maplistdummy = {
    maplistdummy_restore,
    maplist_mark,
    maplist_skipit,
    maplist_unwind,
    maplist_skipit,
    maplistdummy_print
};

/* maplist_restore - restore a map list continuation */
static void maplist_restore(void)
{
    xlValue *d;
    xlCtlPush((xlValue)&cd_maplistdummy);
    d = xlCSP - _MSSIZE;
    if ((*(ACTION)d[MS_ACTION])(IS_UPDATE,xlVal,d)) {
        xlDrop((int)xlGetFixnum(d[MS_ARGC]));
        xlVal = d[MS_VALUE];
        xlCtlDrop(_MSSIZE);
        xlSVReturn();
    }
    else
        do_maplist(d);
}

/* maplistdummy_restore - restore a dummy frame (an error) */
static void maplistdummy_restore(void)
{
    xlFmtError("shouldn't happen -- maplistdummy_restore");
}

/* maplist_mark - mark a map list continuation */
static xlValue *maplist_mark(xlValue *p)
{
    --p;        /* action */
    xlMark(*--p);       /* work */
    xlMark(*--p);       /* value */
    xlMark(*--p);       /* argc */
    xlMark(*--p);       /* fcn */
    return p;
}

/* maplist_skipit - unmark/unstack a map list continuation (just skip over it) */
static xlValue *maplist_skipit(xlValue *p)
{
    return p - _MSSIZE + 1;
}

/* maplist_unwind - unwind past a map list continuation (just skip over it) */
static void maplist_unwind(void)
{
    xlCtlDrop(_MSSIZE - 1);
}

/* maplist_print - print a map list continuation */
static xlValue *maplist_print(xlValue *p)
{
    p -= _MSSIZE - 1;
    show_maplistcontinuation(p,"MapList");
    return p;
}

/* maplistdummy_print - print a dummy map list continuation */
static xlValue *maplistdummy_print(xlValue *p)
{
    p -= _MSSIZE - 1;
    show_maplistcontinuation(p,"MapListDummy");
    return p;
}

/* show_maplistcontinuation - print a map list continuation */
static void show_maplistcontinuation(xlValue *d,char *tag)
{
    xlErrPutStr("\n ");
    xlErrPutStr(tag);
    xlErrPutStr("\n  fcn: ");
    xlErrPrint(d[MS_FCN]);
    xlErrPutStr("\n  argc: ");
    xlErrPrint(d[MS_ARGC]);
    xlErrPutStr("\n  value: ");
    xlErrPrint(d[MS_VALUE]);
    xlErrPutStr("\n  work: ");
    xlErrPrint(d[MS_WORK]);
}

/* map list continuation dispatch table */
static xlCDispatch cd_maplist = {
    maplist_restore,
    maplist_mark,
    maplist_skipit,
    maplist_unwind,
    maplist_skipit,
    maplist_print
};

/* do_maplist - start the next iteration */
static void do_maplist(xlValue *d)
{
    xlValue *finalsp,*argp,x;
    int argc;

    /* initialize */
    xlArgC = argc = (int)xlGetFixnum(d[MS_ARGC]);
    finalsp = argp = xlSP + argc;

    /* get the function to apply */
    xlVal = d[MS_FCN];

    /* build the argument list for the next application */
    xlCheck(argc);
    while (--argc >= 0) {
        x = *--argp;
        if (xlConsP(x)) {
            xlPush((*(ACTION)d[MS_ACTION])(IS_FETCH,x,d));
            *argp = xlCdr(x);
        }
        else {
            xlSP = finalsp;
            xlVal = d[MS_VALUE];
            xlCtlDrop(_MSSIZE);
            xlSVReturn();
            return;
        }
    }

    /* apply the function to the argument list */
    d[MS_CDISPATCH] = (xlValue)&cd_maplist;
    xlNext = xlApply;
}

/* maplist - iterate over a list */
static void maplist(xlValue ivalue,ACTION action)
{
    xlValue fcn,*d = xlCSP;
    fcn = xlGetArg();
    if (xlArgC == 0)
        xlTooFew();
    xlCtlCheck(_MSSIZE);
    xlCtlPush(fcn);
    xlCtlPush(xlMakeFixnum((xlFIXTYPE)xlArgC));
    xlCtlPush(ivalue ? *xlSP : xlNil);
    xlCtlPush(xlNil);
    xlCtlPush((xlValue)action);
    xlCtlPush((xlValue)&cd_maplist);
    do_maplist(d);
}
