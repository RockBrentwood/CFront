/* @(#) cfront.h 1.4 1/27/86 17:48:33 */
/*ident	"@(#)cfront:src/cfront.h	1.4" */
/***********************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

	When reading cfront code please remember that C++ was not available
	when it was originally written. Out of necessity cfront is written
	in a style that takes advantage of only few of C++'s more advanced
	features.

cfront.h:

	Here is all the class definitions for cfront, and most of the externs

***********************************************************************/

/*	WARNING:
	This program relies on non-initialized class members being ZERO.
	This will be true as long as they are allocated using the "new" operator
*/

#include "token.h"
#include "lib.h"
#include "typedef.h"

extern const char *prog_name; /* compiler Id and version */
extern bit old_fct_accepted; /* if set:
                                old style function definitions are legal,
                                implicit declarations are legal
                              */
extern bit fct_void; /* if set:
                        int f(); ... f(1); gives a warning per file
                        undeclared(); gives a warning per file
                        if not:
                        int f(); ... f(1); is an error
                        undeclared(); is an error                                                            (currently only a warning)

                      */
extern int inline_restr; /* inline expansion restrictions */
/*	free lists */
extern IdP name_free;
extern ExP expr_free;
extern StP stmt_free;

extern int Nspy, Nn, Nbt, Nt, Ne, Ns, Nstr, Nc, Nl;

extern Token lex(void);
extern IdP syn(void);
extern bit print_mode;

        /* stage initializers: */
extern void init_print(void);
extern void init_lex(void);
extern void int_syn(void);
extern void ext(int);

extern char *make_name(Token);

struct Loc { /* a source file location */
   short file; /* index into file_name[], or zero */
   short line;
};
void putLoc(LocP this, FILE *);
void putline(LocP this);

extern struct Loc curloc;
extern int curr_file;

// overload error;
extern int errorTL(int, LocP, const char *, ...);
extern int errorT(int, const char *, ...);
extern int errorL(LocP, const char *, ...);
extern int error(const char *, ...);
extern int error_count;
extern bit debug;
extern TypeP outlined;

extern FILE *in_file;
extern FILE *out_file;
extern char scan_started;
extern bit warn;
extern int br_level;
extern int bl_level;
extern TableP ktbl; /* keywords and typedef names */
extern const char *oper_name(Token);
extern TableP gtbl; /* global names */
extern ClassP ccl;
extern BaseP defa_type;
extern BaseP moe_type;

extern StP Cstmt; /* current statement, or 0 */
extern IdP Cdcl; /* Id currently being declared, or 0 */
extern void put_dcl_context(void);

extern TableP any_tbl; /* Table of undefined struct members */
extern BaseP any_type;
extern BaseP int_type;
extern BaseP char_type;
extern BaseP short_type;
extern BaseP long_type;
extern BaseP uchar_type;
extern BaseP ushort_type;
extern BaseP uint_type;
extern BaseP ulong_type;
extern TypeP Pchar_type;
extern TypeP Pint_type;
extern TypeP Pfctvec_type;
extern BaseP float_type;
extern BaseP double_type;
extern BaseP void_type;
extern TypeP Pvoid_type;
extern BaseP zero_type;

extern int byte_offset;
extern int bit_offset;
extern int max_align;
extern int stack_size;
extern int enum_count;
extern int const_save;

extern IdP class_name(TableP, char *, bit);
extern IdP gen_find(IdP, FunP);
extern char *gen_name(char *, char);

extern ExP dummy; /* the empty expression */
extern ExP zero;
extern ExP one;
extern IdP sta_name; /* qualifier for unary :: */

#define DEL(T, p) if (p && (p->permanent==0)) del##T(p)
#define PERM(p) p->permanent=1
#define UNPERM(p) p->permanent=0

struct Node {
#define DecNode \
   Token base; \
   Token n_key; /* for names in Table: class */ \
   bit permanent
// public:
   DecNode;
};

extern ClassP Ebase, Epriv; /* lookc return values */

struct Table {
   DecNode;
/*	a Table is a Node only to give it a "base" for debugging */
// private:
   short size;
   short hashsize;
   IdP *entries;
   short *hashtbl;
   short free_slot; /* next free slot in entries */
// public:
   short init_stat; /* ==0 if Block(s) of Table not simplified,
                       ==1 if simplified but had no initializers,
                       ==2 if simplified and had initializers.
                     */
   StP real_block; /* the last Block the user wrote,
                        not one of the ones cfront created
                      */
   TableP next; /* Table for enclosing scope */
   IdP t_name; /* Id of the Table */
};
TableP MakeTable(short, TableP, IdP);
IdP look(TableP this, const char *, Token);
IdP insert(TableP this, IdP, Token);
void grow(TableP this, int);
static inline void set_scopeTable(TableP this, TableP t) {
   this->next = t;
};
static inline void set_name(TableP this, IdP n) {
   this->t_name = n;
};
IdP get_mem(TableP this, int);
static inline int max(TableP this) {
   return this->free_slot - 1;
};
void dcl_printTable(TableP this, Token, Token);
IdP lookc(TableP this, const char *, Token);
ExP find_name(TableP this, IdP, bit, ExP);
void delTable(TableP this);

extern bit Nold;
extern bit vec_const;
extern void restore(void);
extern void set_scope(IdP);
extern IdListP modified_tn;
extern BaseP start_cl(Token, IdP, IdP);
extern void end_cl(void);
extern BaseP end_enum(IdP, IdP);

/************ types : basic types, aggregates, declarators ************/

extern bit new_type;
extern IdP cl_obj_vec;
extern IdP eobj;

#define DEFINED 01 /* definition fed through dclType() */
#define SIMPLIFIED 02 /* in simplType() */
#define DEF_SEEN 04 /* definition seen, but not processed */
                        /*   used for class members in norm.c */
#define IN_ERROR 010

struct Type {
#define DecType \
   DecNode; \
   bit defined; /* flags DEF_SEEN, DEFINED, SIMPLIFIED, IN_ERROR \
                   not used systematically yet \
                 */
// public:
   DecType;
};
void printType(TypeP this);
void dcl_printType(TypeP this, IdP);
void base_print(TypeP this);
void delType(TypeP this);

IdP is_cl_obj(TypeP this); /* sets cl_obj_vec */
int is_ref(TypeP this);
void dclType(TypeP this, TableP);
int tsizeof(TypeP this);
bit tconst(TypeP this);
Token set_const(TypeP this, bit);
int align(TypeP this);
Token kind(TypeP this, Token, Token);
static inline Token integral(TypeP this, Token oo) {
   return kind(this, oo, I);
};
static inline Token numeric(TypeP this, Token oo) {
   return kind(this, oo, N);
};
static inline Token num_ptr(TypeP this, Token oo) {
   return kind(this, oo, P);
};
bit fct_type(TypeP this);
bit vec_type(TypeP this);
bit checkType(TypeP this, TypeP, Token);
TypeP deref(TypeP this);
static inline PtrP addrof(TypeP this);

char *signature(TypeP this, char *);

extern bit vrp_equiv;

struct Enum { /* ENUM */
// public:
   DecType;
   IdP mem;
   bit e_body;
   int no_of_enumerators;
};
static inline EnumP MakeEnum(IdP n) {
   EnumP this = _new(sizeof *this);
   this->base = ENUM;
   this->mem = n;
   return this;
};
void printEnum(EnumP this);
void dcl_printEnum(EnumP this, IdP);
void dclEnum(EnumP this, IdP, TableP);
void simplEnum(EnumP this);

struct Class { /* CLASS */
// public:
   DecType;
   IdP clbase;
   bit pubbase;
   bit c_body; /* print definition only once */
   Token csu; /* CLASS, STRUCT, UNION, or ANON */
   const char *string; /* Id of class */
   IdP privmem;
   IdP pubmem;
   TableP memtbl;
   short obj_size;
   short real_size; /* obj_size - alignment waste */
   char obj_align;
   char bit_ass; // 1 if no member has operator=()

   IdListP friend_list;
   IdP pubdef;
   IdListP tn_list; // list of member names hiding Type names
   ClassP in_class; /* enclosing class, or 0 */
   TypeP this_type;
   char virt_count; /* number of virtual functions
                       incl. virtuals in base classes
                     */
   IdP *virt_init; /* vector of jump Table initializers */
   IdP itor; /* constructor X(X&) */
   IdP conv; /* operator T() chain */
};
ClassP MakeClass(Token, IdP);

void printClass(ClassP this);
void dcl_printClass(ClassP this, IdP);
void simplClass(ClassP this);

void print_members(ClassP this);
void dclClass(ClassP this, IdP, TableP);
bit has_friend(ClassP this, IdP);
static inline Token is_simple(ClassP this) {
   return (this->csu == CLASS) ? 0 : this->csu;
};
IdP has_oper(ClassP this, Token);
static inline IdP has_ctor(ClassP this) {
   return look(this->memtbl, "_ctor", 0);
}
static inline IdP has_dtor(ClassP this) {
   return look(this->memtbl, "_dtor", 0);
}
static inline IdP has_itor(ClassP this) {
   return this->itor;
}
IdP has_ictor(ClassP this);

struct Base {
        /*      ZTYPE CHAR SHORT INT LONG FLOAT DOUBLE
           FIELD EOBJ COBJ TYPE ANY
         */
        /*      used for gathering all the attributes
           for a list of declarators

           ZTYPE is the (generic) Type of ZERO
           ANY is the generic Type of an undeclared Id
         */
// public:
   DecType;
   bit b_unsigned;
   bit b_const;

   bit b_typedef;
   bit b_inline;
   bit b_virtual;
   bit b_short;
   bit b_long;

   char b_offset;
   Token b_sto; /* AUTO STATIC EXTERN REGISTER 0 */
   IdP b_name; /* Id of non-basic Type */
   ExP b_field; /* field size expression for a field */
   char b_bits; /* number of bits in field */
   TableP b_table; /* memtbl for b_name, or 0 */
   union {
      IdP b_xname; /* extra Id */
      TypeP b_fieldtype;
   };
};
inline void delBase(BaseP this) { return delType((TypeP)this); }
BaseP MakeBase(Token, IdP);

BaseP type_adj(BaseP this, Token);
BaseP base_adj(BaseP this, BaseP);
BaseP name_adj(BaseP this, IdP);
BaseP checkBase(BaseP this, IdP);
IdP aggr(BaseP this);
void normalizeBase(BaseP this);
void dcl_printBase(BaseP this);
BaseP arit_conv(BaseP this, BaseP);

struct Fun { /* FCT */
// public:
   DecType;
   TypeP returns;
   IdP argtype;
   TypeP s_returns;
   IdP f_this;
   BlockP body;
   IdP f_init; /* base/member initializers
                    null Id => base class init;
                    ids => member classes (with ctors)
                  */
   ExP b_init; /* base class initializer
                    ctor call after dclFun()
                  */
   short frame_size;
   Token nargs;
   Token nargs_known; /* KNOWN, ELLIPSIS, or 0 */
   char f_virtual; /* 1+index in virtual Table, or 0 */
   char f_inline; /* 1 if inline, 2 if being expanded, else 0 */
   ExP f_expr; /* body expanded into an expression */
   ExP last_expanded;
};
FunP MakeFun(TypeP, IdP, Token);

void argdcl(FunP this, IdP, IdP);
TypeP normalizeFun(FunP this, TypeP);
void dcl_printFun(FunP this);
void dclFun(FunP this, IdP);
ExP base_init(FunP this, IdP, ExP, TableP);
ExP mem_init(FunP this, IdP, ExP, TableP);
static inline bit declared(FunP this) {
   return this->nargs_known;
};
void simplFun(FunP this);
ExP expandFun(FunP this, IdP, TableP, ExP);

struct IdList {
// public:
   IdP f;
   IdListP l;
};
static inline IdListP MakeIdList(IdP ff, IdListP ll) {
   IdListP this = _new(sizeof *this);
   this->f = ff;
   this->l = ll;
   return this;
};

struct Gen { /* OVERLOAD */
// public:
   DecType;
   IdListP fct_list;
   const char *string;
};
GenP MakeGen(const char *);
IdP addGen(GenP this, IdP, int);
IdP find(GenP this, FunP);

struct Vec { /* VEC */
        /*      typ [ dim ] */
// public:
   DecType;
   TypeP typ;
   ExP dim;
   int size;

};
static inline VecP MakeVec(TypeP t, ExP e) {
   VecP this = _new(sizeof *this);
   Nt++;
   this->base = VEC;
   this->typ = t;
   this->dim = e;
   return this;
};

TypeP normalizeVec(VecP this, TypeP);
void printVec(VecP this);

struct Ptr { /* PTR RPTR */
// public:
   DecType;
   TypeP typ;
   bit rdo; // for "*const"
};

static inline PtrP MakePtr(Token b, TypeP t, bit r/* = 0*/) {
   PtrP this = _new(sizeof *this);
   Nt++;
   this->base = b;
   this->typ = t;
   this->rdo = r;
   return this;
};
TypeP normalizePtr(PtrP this, TypeP);

static inline PtrP addrof(TypeP this) {
   return MakePtr(PTR, this, 0);
}

/****************************** constants ********************************/

                /* STRING ZERO ICON FCON CCON ID */
                /* IVAL FVAL LVAL */

/***************************** expressions ********************************/

extern ExP next_elem(void);
extern void new_list(ExP);
extern void list_check(IdP, TypeP, ExP);
extern ExP ref_init(PtrP, ExP, TableP);
extern ExP class_init(ExP, TypeP, ExP, TableP);
extern ExP check_cond(ExP, Token, TableP);

struct Ex { /* PLUS, MINUS, etc. */
                /* IMPORTANT:   all expression nodes are of sizeof(struct Ex) */
                /*      DEREF           =>      *e1 (e2==0) OR e1[e2]
                   UMINUS               =>      -e2
                   INCR (e1==0) =>      ++e2
                   INCR (e2==0) =>      e1++
                   CM           =>      e1 , e2
                   ILIST                =>      LC e1 RC   (an initializer list)
                   a ExP may denote a Id
                 */
#define DecEx \
   DecNode; \
   union { \
      TypeP tp; \
      int syn_class; \
   }; \
   union { \
      ExP e1; \
      const char *string; \
   }; \
   union { \
      ExP e2; \
      ExP n_initializer; \
      const char *string2; \
   }; \
   union { /* used by the derived classes */ \
      TypeP tp2; \
      IdP fct_name; \
      ExP cond; \
      IdP mem; \
      TypeP as_type; \
      TableP n_table; \
      InLineP il; \
   }
// public:
   DecEx;
};

ExP MakeEx(Token, ExP, ExP);
void FreeEx(ExP this);

void delEx(ExP this);
void printEx(ExP this);
ExP typ(ExP this, TableP);
int eval(ExP this);
int lval(ExP this, Token);
TypeP fct_call(ExP this, TableP);
ExP address(ExP this);
ExP contents(ExP this);
void simplEx(ExP this);
ExP expandEx(ExP this);
bit not_simple(ExP this);

extern const char *Neval;

struct ObjEx {
// public:
   DecEx;
};
static inline ObjExP MakeObjEx(Token t, char *s) {
   return (ObjExP)MakeEx(t, (ExP) s, 0);
}

struct TEx { // NEW CAST VALUE
// public:
   DecEx;
};
static inline TExP MakeTEx(Token bb, TypeP tt, ExP ee) {
   TExP this = (TExP)MakeEx(bb, ee, 0);
   this->tp2 = tt;
   return this;
}

struct Call { // CALL
// public:
   DecEx;
};
static inline CallP MakeCall(ExP aa, ExP bb) {
   return (CallP)MakeEx(CALL, aa, bb);
}
void simplCall(CallP this);
ExP expandCall(CallP this, TableP);

struct QEx { // QUEST
        /* cond ? e1 : e2 */
// public:
   DecEx;
};
static inline QExP MakeQEx(ExP ee, ExP ee1, ExP ee2) {
   QExP this = (QExP)MakeEx(QUEST, ee1, ee2);
   this->cond = ee;
   return this;
}

struct Ref { // REF DOT
        /* e1->mem OR e1.mem */
// public:
   DecEx;
};
static inline RefP MakeRef(Token ba, ExP a, IdP b) {
   RefP this = (RefP)MakeEx(ba, a, 0);
   this->mem = b;
   return this;
}

struct StrEx { // TEXT
// public:
   DecEx;
};
static inline StrExP MakeStrEx(const char *a, const char *b) {
   StrExP this = (StrExP)MakeEx(TEXT, 0, 0);
   this->string = a;
   this->string2 = b;
   return this;
}

/************************* names (are expressions) ****************************/

struct Id {
/* NAME TNAME and the keywords in the ktbl */
// public:
   DecEx;
/*      ExP   n_initializer;  */
   int n_val; /* the value of n_initializer */
   Token n_oper; /* Id of operator or 0 */
   Token n_sto; /* STO keyword: EXTERN, STATIC, AUTO, REGISTER, ENUM */
   Token n_stclass; /* STATIC AUTO REGISTER 0 */
   Token n_scope; /* EXTERN STATIC FCT ARG PUBLIC 0 */
   short n_offset; /* byte offset in frame or struct */
   IdP n_list;
   IdP n_tbl_list;
/*      TableP  n_table;        */
   short n_used;
   short n_addr_taken;
   short n_assigned_to;
   char n_union; /* 0 or union index */
   bit n_evaluated; /* 0 or n_val holds the value */
   short lex_level;
   struct Loc where;
   union {
      IdP n_qualifier; /* Id of containing class */
      TableP n_realscope; /* for labels (always entered in
                             function Table) the Table for the
                             actual scope in which label occurred.
                           */
   };
};

IdP MakeId(const char *s/* = 0*/);
void FreeId(IdP this);

void delId(IdP this);
void printId(IdP this);
void dcl_printId(IdP this, Token);
IdP normalizeId(IdP this, BaseP, BlockP, bit);
IdP tdef(IdP this);
IdP tname(IdP this, Token);
IdP dclId(IdP this, TableP, Token);
int no_of_names(IdP this);
void hide(IdP this);
static inline void unhide(IdP this) {
   this->n_key = 0;
   this->n_list = 0;
};
static inline void use(IdP this) {
   this->n_used++;
};
void assign(IdP this);
static inline void called(IdP this) {
   this->n_used++;
};
static inline void take_addr(IdP this) {
   this->n_addr_taken++;
};
void check_oper(IdP this, IdP);
void simplId(IdP this);

/******************** statements *********************************/

struct St { /* BREAK CONTINUE DEFAULT */
/*      IMPORTANT: all statement nodes have sizeof(struct St) */
#define DecSt \
   DecNode; \
   StP s; \
   StP s_list; \
   struct Loc where; \
   union { \
      IdP d; \
      ExP e2; \
      StP has_default; \
      int case_value; \
   }; \
   union { \
      ExP e; \
      bit own_tbl; \
      StP s2; \
   }; \
   TableP memtbl; \
   union { \
      StP for_init; \
      StP else_stmt; \
      StP case_list; \
      bit empty; \
   }
// public:
   DecSt;
};

StP MakeSt(Token, struct Loc, StP);
void FreeSt(StP this);

void delSt(StP this);
void printSt(StP this);
void dclSt(StP this);
void reached(StP this);
StP simplSt(StP this);
StP expandSt(StP this);
StP copy(StP this);

extern IdP dcl_temp(TableP, IdP);
extern const char *temp(const char *, const char *, const char *);
extern TableP scope;
extern TableP expand_tbl;
extern IdP expand_fn;

struct ESt { /* SM WHILE DO SWITCH RETURN CASE */
                /* SM (e!=0)    =>      e;
                   in particular assignments and function calls
                   SM (e==0)    =>      ;       (the null statement)

                   CASE         =>      case e : s ;
                 */
// public:
   DecSt;
};
static inline EStP MakeESt(Token t, struct Loc ll, ExP ee, StP ss) {
   EStP this = (EStP)MakeSt(t, ll, ss);
   this->e = ee;
   return this;
}

struct IfSt { /* IF */
                // else_stme==0 =>      if (e) s
                // else_stmt!=0 =>      if (e) s else else_stmt
// public:
   DecSt;
};
static inline IfStP MakeIfSt(struct Loc ll, ExP ee, StP ss1, StP ss2) {
// this = 0;
   IfStP this = (IfStP)MakeSt(IF, ll, ss1);
   this->e = ee;
   this->else_stmt = ss2;
   return this;
};

struct LSt { /* LABEL GOTO */
                /*
                   d : s
                   goto d
                 */
// public:
   DecSt;
};
static inline LStP MakeLSt(Token bb, struct Loc ll, IdP nn, StP ss) {
   LStP this = (LStP)MakeSt(bb, ll, ss);
   this->d = nn;
   return this;
}

struct ForSt { /* FOR */
// public:
   DecSt;
};
static inline ForStP MakeForSt(struct Loc ll, StP fss, ExP ee1, ExP ee2, StP ss) {
   ForStP this = (ForStP)MakeSt(FOR, ll, ss);
   this->for_init = fss;
   this->e = ee1;
   this->e2 = ee2;
   return this;
}

struct Block { /* BLOCK */
                /* { d s } */
// public:
   DecSt;
};
static inline void printBlock(BlockP this) { printSt((StP)this); }
static inline StP expandBlock(BlockP this) { expandSt((StP)this); }
static inline void delBlock(BlockP this) { delSt((StP)this); }
static inline BlockP MakeBlock(struct Loc ll, IdP nn, StP ss) {
   BlockP this = (BlockP)MakeSt(BLOCK, ll, ss);
   this->d = nn;
   return this;
}
void dclBlock(BlockP this, TableP);
StP simplBlock(BlockP this);

struct Pair { /* PAIR */
// public:
   DecSt;
};
static inline PairP MakePair(struct Loc ll, StP a, StP b) {
   PairP this = (PairP)MakeSt(PAIR, ll, a);
   this->s2 = b;
   return this;
}

struct Ids {
// public:
   IdP head;
   IdP tail;
};
IdsP MakeIds(IdP);
static inline void addIds(IdsP this, IdP n) {
   this->tail->n_list = n;
   this->tail = n;
};
void add_list(IdsP this, IdP);

extern IdP name_unlist(IdsP);

struct Sts {
// public:
   StP head;
   StP tail;
};
static inline StsP MakeSts(StP s) {
   StsP this = _new(sizeof *this);
   Nl++;
   this->head = this->tail = s;
   return this;
};
static inline void addSts(StsP this, StP s) {
   this->tail->s_list = s;
   this->tail = s;
};

extern StP stmt_unlist(StsP);

struct Exs {
// public:
   ExP head;
   ExP tail;
};
static inline ExsP MakeExs(ExP e) {
   ExsP this = _new(sizeof *this);
   Nl++;
   this->head = this->tail = e;
   return this;
};
static inline void addExs(ExsP this, ExP e) {
   this->tail->e2 = e;
   this->tail = e;
};

extern ExP expr_unlist(ExsP);

extern ScopeP cc;

struct Scope {
// public:
   IdP c_this; /* current Fun's "this" */
   TypeP tot; /* Type of "this" or 0 */
   IdP Not; /* Id of "this"'s class or 0 */
   ClassP cot; /* the definition of "this"'s class */
   TableP ftbl; /* current Fun's symbol Table */
   IdP nof; /* current Fun's Id */
};

static inline void stack(ScopeP cc) {
   cc++;
   *cc = *(cc - 1);
};
static inline void unstack(ScopeP cc) {
   cc--;
};

#define MAXCONT	20
extern struct Scope ccvec[MAXCONT];

extern bit can_coerce(TypeP, TypeP);
extern void yyerror(const char *);
extern Token back;

                /* "spy" counters: */
extern int Nspy;
extern int Nfile, Nline, Ntoken, Nname, Nfree_store, Nalloc, Nfree;
extern int NFn, NFtn, NFpv, NFbt, NFf, NFs, NFc, NFe, NFl;
extern const char *line_format;

extern IdListP isf_list;
extern StP st_ilist;
extern StP st_dlist;
extern TableP sti_tbl;
extern TableP std_tbl;

extern TypeP np_promote(Token, Token, Token, TypeP, TypeP, Token);
extern void new_key(const char *, Token, Token);

extern IdP dcl_list;
extern int over_call(IdP, ExP);
extern IdP Nover;
extern IdP Ncoerce;
extern int Nover_coerce;

#define MIA 8
struct InLine {
// public:
   IdP fct_name; /* Fun called */
   InLineP i_next;
   TableP i_table;
   IdP local[MIA]; /* local variable for arguments */
   ExP arg[MIA]; /* actual arguments for call */
   TypeP tp[MIA]; /* Type of formal arguments */
};
static inline InLineP MakeInLine(void) {
   InLineP this = _new(sizeof *this);
   return this;
}

extern ExP curr_expr;
extern InLineP curr_icall;
#define FUDGE111 111

extern StP curr_loop;
extern BlockP curr_block;
extern StP curr_switch;
extern bit arg_err_suppress;
extern struct Loc last_line;

extern int no_of_undcl;
extern int no_of_badcall;
extern IdP undcl, badcall;

#include <string.h> /* For strlen(), strcpy() and strcmp() */
extern int str_to_int(const char *);
extern int c_strlen(const char *s);

extern IdP vec_new_fct;
extern IdP vec_del_fct;

extern IdP find_hidden(IdP);
extern Token lalex(void);
extern int Nstd; // standard coercion used (derived* =>base* or int=>long or ...)

extern int stcount; // number of names generated using make_name()
/* end */
