/* xlftab.c - built-in function table */
/*      Copyright (c) 1984-2002, by David Michael Betz
        All Rights Reserved
        See the included file 'license.txt' for the full license.
*/

#include "xlisp.h"

/* normal functions */
static xlSubrDef subrtab[] = {

        /* list functions */
{       "CONS",                             xcons                       },
{       "ACONS",                            xacons                      },
{       "CAR",                              xcar                        },
{       "FIRST",                            xcar                        },
{       "CDR",                              xcdr                        },
{       "REST",                             xcdr                        },
{       "CAAR",                             xcaar                       },
{       "CADR",                             xcadr                       },
{       "SECOND",                           xcadr                       },
{       "CDAR",                             xcdar                       },
{       "CDDR",                             xcddr                       },
{       "CAAAR",                            xcaaar                      },
{       "CAADR",                            xcaadr                      },
{       "CADAR",                            xcadar                      },
{       "CADDR",                            xcaddr                      },
{       "THIRD",                            xcaddr                      },
{       "CDAAR",                            xcdaar                      },
{       "CDADR",                            xcdadr                      },
{       "CDDAR",                            xcddar                      },
{       "CDDDR",                            xcdddr                      },
{       "CAAAAR",                           xcaaaar                     },
{       "CAAADR",                           xcaaadr                     },
{       "CAADAR",                           xcaadar                     },
{       "CAADDR",                           xcaaddr                     },
{       "CADAAR",                           xcadaar                     },
{       "CADADR",                           xcadadr                     },
{       "CADDAR",                           xcaddar                     },
{       "CADDDR",                           xcadddr                     },
{       "FOURTH",                           xcadddr                     },
{       "CDAAAR",                           xcdaaar                     },
{       "CDAADR",                           xcdaadr                     },
{       "CDADAR",                           xcdadar                     },
{       "CDADDR",                           xcdaddr                     },
{       "CDDAAR",                           xcddaar                     },
{       "CDDADR",                           xcddadr                     },
{       "CDDDAR",                           xcdddar                     },
{       "CDDDDR",                           xcddddr                     },
{       "LIST",                             xlist                       },
{       "LIST*",                            xliststar                   },
{       "PAIRLIS",                          xpairlis                    },
{       "COPY-LIST",                        xcopylist                   },
{       "COPY-TREE",                        xcopytree                   },
{       "COPY-ALIST",                       xcopyalist                  },
{       "APPEND",                           xappend                     },
{       "REVERSE",                          xreverse                    },
{       "LAST-PAIR",                        xlast                       },
{       "LENGTH",                           xlength                     },
{       "MEMBER",                           xxmember                    },
{       "MEMV",                             xxmemv                      },
{       "MEMQ",                             xxmemq                      },
{       "ASSOC",                            xxassoc                     },
{       "ASSV",                             xxassv                      },
{       "ASSQ",                             xxassq                      },
{       "LIST-REF",                         xlistref                    },
{       "LIST-TAIL",                        xlisttail                   },

        /* destructive list functions */
{       "SET-CAR!",                         xsetcar                     },
{       "SET-CDR!",                         xsetcdr                     },
{       "APPEND!",                          xnappend                    },

        /* package functions */
{       "MAKE-PACKAGE",                     xmkpackage                  },
{       "FIND-PACKAGE",                     xfindpackage                },
{       "LIST-ALL-PACKAGES",                xlistallpackages            },
{       "PACKAGE-NAME",                     xpackagename                },
{       "PACKAGE-NICKNAMES",                xpkgnicknames               },
{       "IN-PACKAGE",                       xinpackage                  },
{       "USE-PACKAGE",                      xusepackage                 },
{       "PACKAGE-USE-LIST",                 xpkguselist                 },
{       "UNUSE-PACKAGE",                    xunusepackage               },
{       "PACKAGE-USED-BY-LIST",             xpkgusedbylist              },
{       "EXPORT",                           xexport                     },
{       "UNEXPORT",                         xunexport                   },
{       "IMPORT",                           ximport                     },
{       "UNINTERN",                         xunintern                   },
{       "MAKE-SYMBOL",                      xmksymbol                   },

        /* symbol functions */
{       "BOUND?",                           xboundp                     },
{       "SYMBOL-NAME",                      xsymname                    },
{       "SYMBOL-VALUE",                     xsymvalue                   },
{       "SET-SYMBOL-VALUE!",                xsetsymvalue                },
{       "SYMBOL-PACKAGE",                   xsympackage                 },
{       "SYMBOL-PLIST",                     xsymplist                   },
{       "SET-SYMBOL-PLIST!",                xsetsymplist                },
{       "GENSYM",                           xgensym                     },
{       "GET",                              xget                        },
{       "PUT",                              xput                        },
{       "REMPROP",                          xremprop                    },

        /* environment functions */
{       "PROCEDURE-ENVIRONMENT",            xprocenvironment            },
{       "ENVIRONMENT?",                     xenvp                       },
{       "ENVIRONMENT-BINDINGS",             xenvbindings                },
{       "ENVIRONMENT-PARENT",               xenvparent                  },

        /* object functions */
{       "OBJECT?",                          xobjectp                    },

        /* vector functions */
{       "VECTOR",                           xvector                     },
{       "MAKE-VECTOR",                      xmakevector                 },
{       "VECTOR-LENGTH",                    xvlength                    },
{       "VECTOR-REF",                       xvref                       },
{       "VECTOR-SET!",                      xvset                       },

        /* array functions */
{       "MAKE-ARRAY",                       xmakearray                  },
{       "ARRAY-REF",                        xaref                       },
{       "ARRAY-SET!",                       xaset                       },

        /* array functions */
{       "TABLE?",                           xtablep                     },
{       "MAKE-TABLE",                       xmaketable                  },
{       "TABLE-REF",                        xtableref                   },
{       "TABLE-SET!",                       xtableset                   },
{       "TABLE-REMOVE!",                    xtableremove                },
{       "EMPTY-TABLE!",                     xemptytable                 },
{       "MAP-OVER-TABLE-ENTRIES",           xmapovertableentries        },

        /* conversion functions */
{       "SYMBOL->STRING",                   xsymstr                     },
{       "STRING->SYMBOL",                   xstrsym                     },
{       "VECTOR->LIST",                     xvectlist                   },
{       "LIST->VECTOR",                     xlistvect                   },
{       "STRING->LIST",                     xstrlist                    },
{       "LIST->STRING",                     xliststring                 },
{       "CHAR->INTEGER",                    xcharint                    },
{       "INTEGER->CHAR",                    xintchar                    },

        /* predicate functions */
{       "NULL?",                            xnull                       },
{       "NOT",                              xnull                       },
{       "ATOM?",                            xatom                       },
{       "LIST?",                            xlistp                      },
{       "END?",                             xendp                       },
{       "NUMBER?",                          xnumberp                    },
{       "BOOLEAN?",                         xbooleanp                   },
{       "PAIR?",                            xpairp                      },
{       "SYMBOL?",                          xsymbolp                    },
{       "COMPLEX?",                         xrealp                      }, /*(1)*/
{       "REAL?",                            xrealp                      },
{       "RATIONAL?",                        xintegerp                   }, /*(1)*/
{       "INTEGER?",                         xintegerp                   },
{       "CHAR?",                            xcharp                      },
{       "STRING?",                          xstringp                    },
{       "VECTOR?",                          xvectorp                    },
{       "PROCEDURE?",                       xprocedurep                 },
{       "PORT?",                            xportp                      },
{       "INPUT-PORT?",                      xinputportp                 },
{       "OUTPUT-PORT?",                     xoutputportp                },
{       "EOF-OBJECT?",                      xeofobjectp                 },
{       "DEFAULT-OBJECT?",                  xdefaultobjectp             },
{       "EQ?",                              xeq                         },
{       "EQV?",                             xeqv                        },
{       "EQUAL?",                           xequal                      },

        /* arithmetic functions */
{       "ZERO?",                            xzerop                      },
{       "POSITIVE?",                        xpositivep                  },
{       "NEGATIVE?",                        xnegativep                  },
{       "ODD?",                             xoddp                       },
{       "EVEN?",                            xevenp                      },
{       "EXACT?",                           xexactp                     },
{       "INEXACT?",                         xinexactp                   },
{       "TRUNCATE",                         xtruncate                   },
{       "FLOOR",                            xfloor                      },
{       "CEILING",                          xceiling                    },
{       "ROUND",                            xround                      },
{       "1+",                               xadd1                       },
{       "-1+",                              xsub1                       },
{       "ABS",                              xabs                        },
{       "SET-RANDOM-SEED!",                 xsetrandomseed              },
{       "RANDOM",                           xrandom                     },
{       "ASH",                              xash                        },
{       "LSH",                              xlsh                        },
{       "+",                                xadd                        },
{       "-",                                xsub                        },
{       "*",                                xmul                        },
{       "/",                                xdiv                        },
{       "QUOTIENT",                         xquo                        },
{       "REMAINDER",                        xrem                        },
{       "MODULO",                           xmod                        },
{       "MIN",                              xmin                        },
{       "MAX",                              xmax                        },
{       "SIN",                              xsin                        },
{       "COS",                              xcos                        },
{       "TAN",                              xtan                        },
{       "ASIN",                             xasin                       },
{       "ACOS",                             xacos                       },
{       "ATAN",                             xatan                       },
{       "EXP",                              xxexp                       },
{       "SQRT",                             xsqrt                       },
{       "EXPT",                             xexpt                       },
{       "LOG",                              xxlog                       },

        /* bitwise logical functions */
{       "LOGAND",                           xlogand                     },
{       "LOGIOR",                           xlogior                     },
{       "LOGXOR",                           xlogxor                     },
{       "LOGNOT",                           xlognot                     },

        /* numeric comparison functions */
{       "<",                                xlss                        },
{       "<=",                               xleq                        },
{       "=",                                xeql                        },
{       "/=",                               xneq                        },
{       ">=",                               xgeq                        },
{       ">",                                xgtr                        },

        /* string functions */
{       "MAKE-STRING",                      xmakestring                 },
{       "STRING-LENGTH",                    xstrlen                     },
{       "STRING-NULL?",                     xstrnullp                   },
{       "STRING-APPEND",                    xstrappend                  },
{       "STRING-REF",                       xstrref                     },
{       "STRING-SET!",                      xstrset                     },
{       "SUBSTRING",                        xsubstring                  },
{       "STRING-UPCASE",                    xupcase                     },
{       "STRING-DOWNCASE",                  xdowncase                   },
{       "STRING-UPCASE!",                   xnupcase                    },
{       "STRING-DOWNCASE!",                 xndowncase                  },
{       "STRING-TRIM",                      xtrim                       },
{       "STRING-LEFT-TRIM",                 xlefttrim                   },
{       "STRING-RIGHT-TRIM",                xrighttrim                  },
{       "STRING<?",                         xstrlss                     },
{       "STRING<=?",                        xstrleq                     },
{       "STRING=?",                         xstreql                     },
{       "STRING/=?",                        xstrneq                     },
{       "STRING>=?",                        xstrgeq                     },
{       "STRING>?",                         xstrgtr                     },
{       "STRING-CI<?",                      xstrilss                    },
{       "STRING-CI<=?",                     xstrileq                    },
{       "STRING-CI=?",                      xstrieql                    },
{       "STRING-CI/=?",                     xstrineq                    },
{       "STRING-CI>=?",                     xstrigeq                    },
{       "STRING-CI>?",                      xstrigtr                    },
{       "STRING-SEARCH",                    xstrsearch                  },
{       "STRING-SEARCH-CI",                 xstrisearch                 },
{       "NUMBER->STRING",                   xnumstr                     },
{       "STRING->NUMBER",                   xstrnum                     },

        /* character functions */
{       "CHAR<?",                           xchrlss                     },
{       "CHAR<=?",                          xchrleq                     },
{       "CHAR=?",                           xchreql                     },
{       "CHAR/=?",                          xchrneq                     },
{       "CHAR>=?",                          xchrgeq                     },
{       "CHAR>?",                           xchrgtr                     },
{       "CHAR-CI<?",                        xchrilss                    },
{       "CHAR-CI<=?",                       xchrileq                    },
{       "CHAR-CI=?",                        xchrieql                    },
{       "CHAR-CI/=?",                       xchrineq                    },
{       "CHAR-CI>=?",                       xchrigeq                    },
{       "CHAR-CI>?",                        xchrigtr                    },
{       "CHAR-UPPER-CASE?",                 xuppercasep                 },
{       "CHAR-LOWER-CASE?",                 xlowercasep                 },
{       "CHAR-ALPHABETIC?",                 xbothcasep                  },
{       "CHAR-NUMERIC?",                    xdigitp                     },
{       "CHAR-WHITESPACE?",                 xwhitespacep                },
{       "CHAR-ALPHANUMERIC?",               xalphanumericp              },
{       "STRING",                           xstring                     },
{       "CHAR",                             xchar                       },
{       "CHAR-UPCASE",                      xchupcase                   },
{       "CHAR-DOWNCASE",                    xchdowncase                 },
{       "DIGIT->CHAR",                      xdigitchar                  },

        /* I/O functions */
{       "READ",                             xread                       },
{       "READ-DELIMITED-LIST",              xreaddelimitedlist          },
{       "READ-LINE",                        xreadline                   },
{       "READ-CHAR",                        xrdchar                     },
{       "UNREAD-CHAR",                      xunreadchar                 },
{       "PEEK-CHAR",                        xpkchar                     },
{       "CHAR-READY?",                      xcharready                  },
{       "CLEAR-INPUT",                      xclearinput                 },
{       "READ-BYTE",                        xrdbyte                     },
{       "READ-SHORT",                       xrdshort                    },
{       "READ-SHORT-HIGH-FIRST",            xrdshorthf                  },
{       "READ-SHORT-LOW-FIRST",             xrdshortlf                  },
{       "READ-LONG",                        xrdlong                     },
{       "READ-LONG-HIGH-FIRST",             xrdlonghf                   },
{       "READ-LONG-LOW-FIRST",              xrdlonglf                   },
{       "WRITE",                            xwrite                      },
{       "WRITE-CHAR",                       xwrchar                     },
{       "WRITE-BYTE",                       xwrbyte                     },
{       "WRITE-SHORT",                      xwrshort                    },
{       "WRITE-SHORT-HIGH-FIRST",           xwrshorthf                  },
{       "WRITE-SHORT-LOW-FIRST",            xwrshortlf                  },
{       "WRITE-LONG",                       xwrlong                     },
{       "WRITE-LONG-HIGH-FIRST",            xwrlonghf                   },
{       "WRITE-LONG-LOW-FIRST",             xwrlonglf                   },
{       "DISPLAY",                          xdisplay                    },
{       "PRINT",                            xprint                      },
{       "WRITE-SIZE",                       xwritesize                  },
{       "DISPLAY-SIZE",                     xdisplaysize                },
{       "NEWLINE",                          xnewline                    },
{       "FRESH-LINE",                       xfreshline                  },
{       "FLUSH-OUTPUT",                     xflushoutput                },
{       "FORMAT",                           xformat                     },

        /* print control functions */
{       "PRINT-BREADTH",                    xprbreadth                  },
{       "PRINT-DEPTH",                      xprdepth                    },

        /* file I/O functions */
{       "PARSE-PATH-STRING",                xparsepathstring            },
{       "COMBINE-PATH-WITH-FILENAME",       xcombinepathwithfilename    },
{       "FILE-MODIFICATION-TIME",           xfilemodtime                },
{       "OPEN-INPUT-FILE",                  xopeni                      },
{       "OPEN-OUTPUT-FILE",                 xopeno                      },
{       "OPEN-APPEND-FILE",                 xopena                      },
{       "OPEN-UPDATE-FILE",                 xopenu                      },
{       "CLOSE-PORT",                       xclose                      },
{       "CLOSE-INPUT-PORT",                 xclosei                     },
{       "CLOSE-OUTPUT-PORT",                xcloseo                     },
{       "GET-FILE-POSITION",                xgetfposition               },
{       "SET-FILE-POSITION!",               xsetfposition               },
{       "CURRENT-INPUT-PORT",               xcurinput                   },
{       "CURRENT-OUTPUT-PORT",              xcuroutput                  },
{       "CURRENT-ERROR-PORT",               xcurerror                   },
{       "MAKE-STRING-INPUT-STREAM",         xmkstrinput                 },
{       "MAKE-STRING-OUTPUT-STREAM",        xmkstroutput                },
{       "GET-OUTPUT-STREAM-STRING",         xgetstroutput               },
{       "MAKE-OBJECT-STREAM",               xmkobjstream                },
{       "LOAD-FASL-FILE",                   xloadfaslfile               },

        /* miscellaneous functions */
{       "IDENTITY",                         xidentity                   },

        /* utility functions */
{       "TRANSCRIPT-ON",                    xtranson                    },
{       "TRANSCRIPT-OFF",                   xtransoff                   },
{       "GETARG",                           xgetarg                     },
{       "GET-TIME",                         xgettime                    },
{       "GET-ENVIRONMENT-VARIABLE",         xgetenv                     },
{       "IDLE",                             xidle                       },
{       "EXIT",                             xexit                       },
{       "QUIT",                             xexit                       },
{       "COMPILE",                          xcompile                    },
{       "DECOMPILE",                        xdecompile                  },
{       "SET-DEBUG-MODE!",                  xsetdebugmode               },
{       "FASL-WRITE-PROCEDURE",             xfaslwriteprocedure         },
{       "FASL-READ-PROCEDURE",              xfaslreadprocedure          },
{       "GC",                               xgc                         },
{       "ROOM",                             xroom                       },
{       "SAVE",                             xsave                       },
{       "RESTORE",                          xrestore                    },
{       "ERROR",                            xerror                      },

        /* debugging functions */
{       "TRACE-ON",                         xtraceon                    },
{       "TRACE-OFF",                        xtraceoff                   },

#if 0
        /* crecord functions */
{       "ALLOCATE-CMEMORY",                 xalloccmemory               },
{       "FREE-CMEMORY",                     xfreecmemory                },
{       "FOREIGN-POINTER?",                 xforeignptrp                },
{       "FOREIGN-POINTER-TYPE",             xforeignptrtype             },
{       "SET-FOREIGN-POINTER-TYPE!",        xsetforeignptrtype          },
{       "FOREIGN-POINTER-TYPE?",            xforeignptrtypep            },
{       "FOREIGN-POINTER-EQ?",              xforeignptreqp              },
{       "GET-CRECORD-FIELD",                xgetcrecfield               },
{       "GET-CRECORD-FIELD-ADDRESS",        xgetcrecfieldaddr           },
{       "SET-CRECORD-FIELD!",               xsetcrecfield               },
{       "GET-CRECORD-STRING",               xgetcrecstring              },
{       "SET-CRECORD-STRING!",              xsetcrecstring              },
{       "GET-CRECORD-TYPE-SIZE",            xgetcrectypesize            },
{       "NULL-POINTER?",                    xnullpointerp               },
#endif

        /* internal functions */
{       "%CAR",                             xicar                       },
{       "%CDR",                             xicdr                       },
{       "%SET-CAR!",                        xisetcar                    },
{       "%SET-CDR!",                        xisetcdr                    },
{       "%VECTOR-LENGTH",                   xivlength                   },
{       "%VECTOR-REF",                      xivref                      },
{       "%VECTOR-SET!",                     xivset                      },
{       "%VECTOR-BASE",                     xivbase                     },
{       "%ADDRESS-OF",                      xiaddrof                    },
{       "%FORMAT-ADDRESS",                  xifmtaddr                   },
#if 0
{       "%GET-STACK-POINTER",               xGetStackPointer            },
#endif

{0,0} /* end of table marker */
};

/* functions that call eval or apply */
static xlXSubrDef xsubrtab[] = {

{       "APPLY",                            xapply                      },
{       "CALL-WITH-CURRENT-CONTINUATION",   xcallcc                     },
{       "CALL/CC",                          xcallcc                     },
{       "MAP",                              xmapcar                     },
{       "FOR-EACH",                         xmapc                       },
{       "SPLIT-PATH-FROM-FILENAME",         xsplitpathfromfilename      },
{       "CALL-WITH-INPUT-FILE",             xcallwi                     },
{       "CALL-WITH-OUTPUT-FILE",            xcallwo                     },
{       "LOAD",                             xload                       },
{       "LOAD-NOISILY",                     xloadnoisily                },
{       "FORCE",                            xforce                      },
{       "THROW",                            xthrow                      },
{       "THROW-ERROR",                      xthrowerror                 },
{       "VALUES",                           xvalues                     },
{       "VALUES-LIST",                      xvalueslist                 },
{       "INTERN",                           xintern                     },
{       "FIND-SYMBOL",                      xfindsymbol                 },
{       "MAPCAR",                           xmapcar                     },
{       "MAPC",                             xmapc                       },
{       "MAPCAN",                           xmapcan                     },
{       "MAPLIST",                          xmaplist                    },
{       "MAPL",                             xmapl                       },
{       "MAPCON",                           xmapcon                     },
{       "SOME",                             xsome                       },
{       "EVERY",                            xevery                      },
{       "NOTANY",                           xnotany                     },
{       "NOTEVERY",                         xnotevery                   },
{       "FIND",                             xfind                       },
{       "FIND-IF",                          xfindif                     },
{       "FIND-IF-NOT",                      xfindifnot                  },
{       "XLISP:MEMBER",                     xmember                     },
{       "MEMBER-IF",                        xmemberif                   },
{       "MEMBER-IF-NOT",                    xmemberifnot                },
{       "XLISP:ASSOC",                      xassoc                      },
{       "ASSOC-IF",                         xassocif                    },
{       "ASSOC-IF-NOT",                     xassocifnot                 },
{       "RASSOC",                           xrassoc                     },
{       "RASSOC-IF",                        xrassocif                   },
{       "RASSOC-IF-NOT",                    xrassocifnot                },
{       "REMOVE",                           xremove                     },
{       "REMOVE-IF",                        xremoveif                   },
{       "REMOVE-IF-NOT",                    xremoveifnot                },
{       "DELETE",                           xdelete                     },
{       "DELETE-IF",                        xdeleteif                   },
{       "DELETE-IF-NOT",                    xdeleteifnot                },
{       "COUNT",                            xcount                      },
{       "COUNT-IF",                         xcountif                    },
{       "COUNT-IF-NOT",                     xcountifnot                 },
{       "POSITION",                         xposition                   },
{       "POSITION-IF",                      xpositionif                 },
{       "POSITION-IF-NOT",                  xpositionifnot              },

        /* read macros */
{       "%RM-HASH",                         xrmhash                     },
{       "%RM-QUOTE",                        xrmquote                    },
{       "%RM-DOUBLE-QUOTE",                 xrmdquote                   },
{       "%RM-BACKQUOTE",                    xrmbquote                   },
{       "%RM-COMMA",                        xrmcomma                    },
{       "%RM-LEFT-PAREN",                   xrmlparen                   },
{       "%RM-RIGHT-PAREN",                  xrmrparen                   },
{       "%RM-SEMICOLON",                    xrmsemi                     },

        /* internal functions */
{       "%SEND-SUPER",                      xsendsuper                  },
#if 0
{       "%GET-STACK-FRAME",                 xGetStackFrame              },
#endif

        /* system functions */
{       "SHOW-STACK",                       xshowstack                  },
{       "SHOW-CONTROL-STACK",               xshowcontrolstack           },
{       "SHOW-VALUE-STACK",                 xshowvaluestack             },

{0,0} /* end of table marker */
};

/* Notes:

   (1)  This version only supports integers and reals.

*/

/* xlInitFunctions - initialize the built-in functions */
void xlInitFunctions(void)
{
    xlSubrDef *sdp;
    xlXSubrDef *xsdp;
    for (sdp = subrtab; sdp->subr != NULL; ++sdp)
        xlSubr(sdp->name,sdp->subr);
    for (xsdp = xsubrtab; xsdp->subr != NULL; ++xsdp)
        xlXSubr(xsdp->name,xsdp->subr);
}

/* xlFindSubr - find a subr in the internal table */
xlValue (*xlFindSubr(const char *name))(void)
{
    xlSubrDef *sdp;
    xlXSubrDef *xsdp;
    for (sdp = subrtab; sdp->name != NULL; ++sdp)
        if (strcmp(sdp->name,name) == 0)
            return sdp->subr;
    for (xsdp = xsubrtab; xsdp->name != NULL; ++xsdp)
        if (strcmp(xsdp->name,name) == 0)
            return (xlValue (*)(void))xsdp->subr;
    return NULL;
}
