// 1985 Feb 08 12:50
/* %Z% %M% %I% %H% %T% */

/*************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T Technologies, Inc. All rigths Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T TECHNOLOGIES, INC.

	If you ignore this notice the ghost of Ma Bell will haunt you forever.
gram.y:

	This is the syntax analyser.

	Old C features not recognized:
	(1) "+ =" as the operator "+="
	(2) any construct using one of the new keywords as an identifier
	(3) initializers without "=" operator
	(4) structure tags used as identifier names

	Additions:
	(1) Classes (keywords: CLASS THIS PUBLIC FRIEND and VIRTUAL)
		(classes incorporate STRUCT and UNION)
	(2) the new and delete operators (keywords: NEW DELETE)
	(3) inline functions (keyword INLINE)
	(4) overloaded function names (keyword OVERLOAD)
	(5) overloaded operators (keyword OPERATOR)
	(6) constructors and destructors
	(7) constant types (keyword: CONST)
	(8) argument types part of function function type (token: ...)
	(9) new argument syntax ( e.g. char f(int a, char b) { ... })
	(10) names can be left out of argument lists

	Syntax extensions for error handling:
	(1) nested functions
	(2) any expression can be empty
	(3) any expression can be a constant_expression

	note that a call to error*() does not change the parser's state
*/

%{
#include "cfront.h"
#include "size.h"

#define YYMAXDEPTH 300

BaseP defa_type;
BaseP moe_type;
ExP dummy;
ExP zero;

ClassP ccl;
int cdi = 0;
IdP cd = 0, cd_vec[BLMAX];
char stmt_seen = 0, stmt_vec[BLMAX];
IdListP modified_tn = 0, tn_vec[BLMAX];

IdP sta_name = (IdP)&sta_name;

bit cm_warn;

extern Token back;
Token back;
#define LexUnGet(X) back = X

#define NormData(A, B)		normalizeId((IdP)B, (BaseP)A, 0, 0)
#define NormCast(A, B)		normalizeId((IdP)B, (BaseP)A, 0, 1)
#define NormFun(A, B, C)	normalizeId((IdP)B, (BaseP)A, (BlockP)C, 0)
#define IdType(P)		((IdP)P)->tp
#define IdCopy(N)		(((IdP)N)->base == TNAME? MakeId(((IdP)N)->string): (IdP)N) //(#) Clipped at "string) :" and wrapped only "(IdP)" over onto the next line.
#define Idhide(N)		hide((IdP)N)

#define NewField(E)		MakeBase(FIELD, (IdP)E)
#define FunInit(P)		((FunP)P)->f_init
#define FunArgDec(P, Q)		argdcl((FunP)P, Q)
#define RetType(P)		((FunP)P)->returns
#define NewVec(E)		MakeVec(0, E)
#define VecType(V)		((VecP)V)->typ
#define PtrType(P)		((PtrP)P)->typ

#define NewCon(T, V)		MakeEx(T, (ExP)V, 0)

#define NewIds(N)		(NodeP)MakeIds((IdP)N)
#define AddId(L, N)		addIds((IdsP)L, (IdP)N)
#define AddList(L, N)		add_list((IdsP)L, (IdP)N)
#define DelId(L)		name_unlist((IdsP)L)
#define NewSts(S)		(NodeP)MakeSts((StP)S)
#define AddSt(L, S)		addSts((StsP)L, (StP)S)
#define DelSt(L)		stmt_unlist((StsP)L)
#define AddEx(L, E)		addExs((ExsP)L, (ExP)E)
#define DelEx(L)		expr_unlist((ExsP)L)

		/* avoid redefinitions */
#undef EOFTOK
#undef ASM
#undef BREAK
#undef CASE
#undef CONTINUE
#undef DEFAULT
#undef DELETE
#undef DO
#undef ELSE
#undef ENUM
#undef FOR
#undef FORTRAN
#undef GOTO
#undef IF
#undef NEW
#undef OPERATOR
#undef PUBLIC
#undef RETURN
#undef SIZEOF
#undef SWITCH
#undef THIS
#undef WHILE
#undef LP
#undef RP
#undef LB
#undef RB
#undef REF
#undef DOT
#undef NOT
#undef COMPL
#undef MUL
#undef AND
#undef PLUS
#undef MINUS
#undef ER
#undef OR
#undef ANDAND
#undef OROR
#undef QUEST
#undef COLON
#undef ASSIGN
#undef CM
#undef SM
#undef LC
#undef RC
#undef ID
#undef STRING
#undef ICON
#undef FCON
#undef CCON
#undef ZERO
#undef ASOP
#undef RELOP
#undef EQUOP
#undef DIVOP
#undef SHIFTOP
#undef ICOP
#undef TYPE
#undef TNAME
#undef EMPTY
#undef NO_ID
#undef NO_EXPR
#undef ELLIPSIS
#undef AGGR
#undef MEM
#undef CAST

IdP syn(void) { return (IdP) yyparse(); }

%}

%union {
   char *s;
   Token t;
   int i;
   struct Loc l;
   IdP pn;
   TypeP pt;
   ExP pe;
   StP ps;
   BaseP pb;
   NodeP p;	/* fudge: pointer to all struct Node objects
		neccessary only because unions of class
		pointers are not implemented by cpre
		*/
}
%{
extern YYSTYPE yylval;
%}
/*
	the token definitions are copied from token.h,
	and all %token replaced by %token
*/
			/* keywords in alphabetical order */
%token EOFTOK		0
%token ASM		1
%token BREAK		3
%token CASE		4
%token CONTINUE		7
%token DEFAULT		8
%token DELETE		9
%token DO		10
%token ELSE		12
%token ENUM		13
%token FOR		16
%token FORTRAN		17
%token GOTO		19
%token IF		20
%token NEW		23
%token OPERATOR		24
%token PUBLIC		25
%token RETURN		28
%token SIZEOF		30
%token SWITCH		33
%token THIS		34
%token WHILE		39

			/* operators in priority order (sort of) */
%token LP		40
%token RP		41
%token LB		42
%token RB		43
%token REF		44
%token DOT		45
%token NOT		46
%token COMPL		47
%token MUL		50
%token AND		52
%token PLUS		54
%token MINUS		55
%token ER		64
%token OR		65
%token ANDAND		66
%token OROR		67
%token QUEST		68
%token COLON		69
%token ASSIGN		70
%token CM		71
%token SM		72
%token LC		73
%token RC		74
%token CAST		113

			/* constants etc. */
%token ID		80
%token STRING		81
%token ICON		82
%token FCON		83
%token CCON		84

%token ZERO		86

			/* groups of tokens */
%token ASOP		90	/* op= */
%token RELOP		91	/* LE GE LT GT */
%token EQUOP		92	/* EQ NE */
%token DIVOP		93	/* DIV MOD */
%token SHIFTOP		94	/* LS RS */
%token ICOP		95	/* INCR DECR */

%token TYPE		97	/*	INT FLOAT CHAR DOUBLE
					REGISTER STATIC EXTERN AUTO
					CONST INLINE VIRTUAL FRIEND
					LONG SHORT UNSIGNED
					TYPEDEF */
%token TNAME		123
%token EMPTY		124
%token NO_ID		125
%token NO_EXPR		126
%token ELLIPSIS		155	/* ... */
%token AGGR		156	/* CLASS STRUCT UNION */
%token MEM		160	/* :: */


%type <p>	ExtDef FunDec FunDef AttFunDef DataDecs
		BaseInit
		DataDec ExtItem Vec Ptr
		TSps TSp EnumDec Enums Enum
		Tag ClassHead ClassDec ClassBody Members Member IDec IDecs
		FunName Dec Init Sts
		Block St St0 Inits Exs Ex Ex1 Ex0
		CastDec CastType CDec CType CTp
		ArgDec OptArgType ArgType Args ArgTypes
		NewDec NewType
		Cond
		TNAME Name
%type <l>	LC RC SWITCH CASE DEFAULT FOR IF DO WHILE GOTO RETURN DELETE
		BREAK CONTINUE
%type <t>	Op
		EQUOP DIVOP SHIFTOP ICOP RELOP ASOP
		ANDAND OROR PLUS MINUS MUL ASSIGN OR ER AND
		LP LB NOT COMPL AGGR
		TYPE
%type <s>	ID CCON ZERO ICON FCON STRING

%left	EMPTY
%left	NO_ID
%left	RC LC ID BREAK CONTINUE RETURN GOTO DELETE DO IF WHILE FOR CASE DEFAULT
	AGGR ENUM TYPE
%left	NO_EXPR

%left	CM
%right	ASOP ASSIGN
%right	QUEST COLON
%left	OROR
%left	ANDAND
%left	OR
%left	ER
%left	AND
%left	EQUOP
%left	RELOP
%left	SHIFTOP
%left	PLUS MINUS
%left	MUL DIVOP
%right	NOT COMPL NEW
%right	CAST ICOP SIZEOF
%left	LB LP DOT REF MEM

%start ExtItem

%%
/* This parser handles declarations one by one, NOT a complete .c file */

/* === Declarations in the outermost scope: returns IdP === */
ExtItem: ExtDef { return $<i>1; };
ExtItem: SM { return 1; };
ExtItem: EOFTOK { return 0; };

ExtDef: DataDec {
   modified_tn = 0;
   if ($<pn>1 == 0) $<i>$ = 1;
};
ExtDef: AttFunDef { goto Mod; };
ExtDef: FunDef { goto Mod; };
ExtDef: FunDec {
Mod:
   if (modified_tn) {
      restore();
      modified_tn = 0;
   }
};
ExtDef: ASM LP STRING RP SM {
   IdP n = MakeId(make_name('A'));
   n->tp = (TypeP)MakeBase(ASM, $<pn>3);
   $$ = (NodeP)n;
};

FunDec: Dec SM {
   IdP n = $<pn>1;
   switch (n->tp->base) {
      case FCT:
         $$ = (NodeP)NormFun(defa_type, n, 0);
         break;
      default:
         error("TX for%n", n);
         $$ = (NodeP)NormData(defa_type, $1);
   }
};

AttFunDef: TSps Dec DataDecs BaseInit Block {
   $$ = (NodeP)NormFun($1, $2, $5);
   FunArgDec(IdType($$), DelId($3));
   FunInit(IdType($$)) = $<pe>4;
};

FunDef: Dec DataDecs BaseInit Block {
   $$ = (NodeP)NormFun(defa_type, $1, $4);
   FunArgDec(IdType($$), DelId($2));
   FunInit(IdType($$)) = $<pe>3;
};

BaseInit: COLON LP Exs RP { $$ = $3; };
BaseInit: %prec EMPTY { $$ = 0; };

/* === Declarations: returns IdP === */
DataDecs: DataDecs DataDec {
   if ($<pn>2 == 0)
      error("badAD");
   else if ($<pn>2->tp->base == FCT)
      error("FD inAL (%n)", $<pn>2);
   else if ($1)
      AddList($1, $2);
   else
      $$ = NewIds($2);
};
DataDecs: %prec EMPTY { $$ = 0; };

IDec: Dec;
IDec: ID COLON Ex %prec CM {
   $$ = (NodeP)MakeId($<s>1);
   IdType($$) = (TypeP)NewField($<pe>3);
};
IDec: COLON Ex %prec CM {
   $$ = (NodeP)MakeId(0);
   IdType($$) = (TypeP)NewField($<pe>2);
};
IDec: Dec ASSIGN Init { $<pn>1->n_initializer = $<pe>3; };

IDecs: IDec {
   if ($1) $$ = NewIds($1);
};
IDecs: IDecs CM IDec {
   if ($1)
      if ($3) AddId($1, $3); else error("DL syntax");
   else {
      if ($3) $$ = NewIds($3);
      error("DL syntax");
   }
};

DataDec: TSps IDecs SM { $$ = (NodeP)NormData($1, DelId($2)); };
DataDec: TSps SM { $$ = (NodeP)aggr($<pb>1); };

TSp: TYPE { $$ = (NodeP)MakeBase($<t>1, 0); };
TSp: TNAME { $$ = (NodeP)MakeBase(TYPE, $<pn>1); };
TSp: ClassDec;
TSp: EnumDec;
TSp: AGGR Tag {
   IdP n = $<pn>2;
   Token t = $<t>1;
   if (n->base == NAME) { /* implicit dcl */
      n = tname(n, t);
      modified_tn = modified_tn->l;	/* not loca... */ //(#) Clipped at "not loca".
      n->lex_level = 0;
   }
   $$ = (NodeP)n->tp;
};
TSp: ENUM Tag {
   IdP n = $<pn>2;
   if (n->base == NAME) { /* implicit dcl */
      n = tname(n, ENUM);
      modified_tn = modified_tn->l;
      n->lex_level = 0;
   }
   $$ = (NodeP)n->tp;
};

TSps: TSp;
TSps: TSps TYPE { $$ = (NodeP)type_adj($<pb>1, $<t>2); };
TSps: TSps TNAME { $$ = (NodeP)name_adj($<pb>1, $<pn>2); };
TSps: TSps ClassDec { $$ = (NodeP)base_adj($<pb>1, $<pb>2); };
TSps: TSps EnumDec { $$ = (NodeP)base_adj($<pb>1, $<pb>2); };
TSps: TSps AGGR Tag {
   IdP n = $<pn>3;
   Token t = $<t>2;
   if (n->base == NAME) { /* implicit dcl */
      n = tname(n, t);
      modified_tn = modified_tn->l;
      n->lex_level = 0;
   }
   $$ = (NodeP)base_adj($<pb>1, (BaseP)n->tp);
};
TSps: TSps ENUM Tag {
   IdP n = $<pn>3;
   if (n->base == NAME) { /* implicit dcl */
      n = tname(n, ENUM);
      modified_tn = modified_tn->l;
      n->lex_level = 0;
   }
   $$ = (NodeP)base_adj($<pb>1, (BaseP)n->tp);
};

/* === Aggregate: returns IdP === */
EnumDec: ENUM LC Enums RC { $$ = (NodeP)end_enum(0, $<pn>3); };
EnumDec: ENUM Tag LC Enums RC { $$ = (NodeP)end_enum($<pn>2, $<pn>4); };

Enums: Enum {
   if ($1) $$ = NewIds($1);
};
Enums: Enums CM Enum {
   if ($3)
      if ($1) AddId($1, $3); else $$ = NewIds($3);
}; //(#) Clipped at "NewIds($3); ".

Enum: ID { $$ = (NodeP)MakeId($<s>1); IdType($$) = (TypeP)moe_type; };
Enum: ID ASSIGN Ex {
   $$ = (NodeP)MakeId($<s>1);
   IdType($$) = (TypeP)moe_type;
   $<pn>$->n_initializer = $<pe>3;
};
Enum: /* empty */ { $$ = 0; };


ClassDec: ClassHead ClassBody RC { end_cl(); };
ClassDec: ClassHead ClassBody RC TYPE {
   end_cl();
   error("`;' or declaratorX afterCD");
   LexUnGet($4);
/* LexUnGet($4); but only one unget, sorry */
};

ClassHead: AGGR LC { $$ = (NodeP)start_cl($<t>1, 0, 0); };
ClassHead: AGGR Tag LC { $$ = (NodeP)start_cl($<t>1, $<pn>2, 0); };
ClassHead: AGGR Tag COLON Tag LC { $$ = (NodeP)start_cl($<t>1, $<pn>2, $<pn>4); };
ClassHead: AGGR Tag COLON PUBLIC Tag LC {
   $$ = (NodeP)start_cl($<t>1, $<pn>2, $<pn>5);
   ccl->pubbase = 1;
};

Tag: ID { $$ = (NodeP)MakeId($<s>1); };
Tag: TNAME;

ClassBody: Members {
   IdP n = DelId($1);
   if (is_simple(ccl)) ccl->pubmem = n; else ccl->privmem = n;
   $$ = 0;
};
ClassBody: Members PUBLIC Members {
   error("``:'' missing after ``public''");
   ccl->pubmem = DelId($3);
   goto priv;
};
ClassBody: Members PUBLIC COLON Members {
   Token t;
   ccl->pubmem = DelId($4);
priv:
   t = is_simple(ccl);
   if (t) error("public in%k", t);
   ccl->privmem = DelId($1);
   $$ = 0;
};

Members: Members Member {
   if ($2)
      if ($1) AddList($1, $2); else $$ = NewIds($2);
}; //(#) Clipped at "nlist(".
Members: %prec EMPTY { $$ = 0; };

Member: DataDec;
Member: AttFunDef SM;
Member: AttFunDef;
Member: FunDef SM;
Member: FunDef;
Member: FunDec;
Member: Name Tag SM /* public declaration */ {
   IdP n = IdCopy($2);
   n->n_qualifier = (IdP)$1;
   n->n_list = ccl->pubdef;
   ccl->pubdef = n;
   $$ = 0;
};

/* === Declarators: returns IdP === */
/* a Dec is used for function and data declarations, and for member declarations (it has a name) */
/* an ArgDec is used for argument declarations (it may or may not have a name) */
/* an CastDec is used for casts (it does not have a name) */
/* a NewDec is used for type specifiers for the NEW operator (it does not have a name, and PtoF and PtoV cannot be expressed) */

FunName: ID { $$ = (NodeP)MakeId($<s>1); };
FunName: COMPL TNAME { $$ = (NodeP)IdCopy($2); $<pn>$->n_oper = DTOR; };
FunName: DELETE {
   if (fct_void == 0) error("deleteF (use destructor)");
   $$ = (NodeP)MakeId("_dtor");
   $<pn>$->n_oper = DTOR;
};
FunName: NEW {
   if (fct_void == 0) error("newF (use constructor)");
   $$ = (NodeP)MakeId("_ctor");
   $<pn>$->n_oper = CTOR;
};
FunName: OPERATOR Op {
   $$ = (NodeP)MakeId(oper_name($2));
   $<pn>$->n_oper = $<t>2;
};
FunName: OPERATOR CType {
   IdP n = $<pn>2;
   n->string = "_type";
   n->n_oper = TYPE;
   n->n_initializer = (ExP)n->tp;
   n->tp = 0;
   $$ = (NodeP)n;
};

Op: PLUS | MINUS | MUL | AND | OR | ER | SHIFTOP | EQUOP | DIVOP | RELOP | ANDAND | OROR;
Op: LP RP { $$ = CALL; };
Op: LB RB { $$ = DEREF; };
Op: NOT | COMPL | ICOP | ASOP | ASSIGN;
Op: NEW { $$ = NEW; };
Op: DELETE { $$ = DELETE; };

Name: TNAME DOT;
Name: Name TNAME DOT { errorT('s',"MF of nestedC"); };
Name: Name ID DOT { errorT('s',"MF of nestedC"); };

Dec: Dec Args {
   RetType($2) = IdType($1);
   IdType($1) = (TypeP)$2;
};
Dec: TNAME Args {
   IdP n = (IdP)$1;
   $$ = (NodeP)IdCopy(n);
   if (ccl && strcmp(n->string, ccl->string)) Idhide(n);
   $<pn>$->n_oper = TNAME;
   RetType($2) = IdType($$);
   IdType($$) = (TypeP)$2;
};
Dec: Dec LP Exs RP /* may be class object initializer, class object vector initializer, if not Exs will be a CM or an ID */ {
   Token k = 1;
   IdP l = $<pn>3;
   if (fct_void && l == 0) k = 0;
   IdType($1) = (TypeP)MakeFun(IdType($1), l, k);
};
Dec: TNAME LP Exs RP {
   Token k = 1;
   IdP l = $<pn>3;
   if (fct_void && l == 0) k = 0;
   $$ = (NodeP)IdCopy($1);
   $<pn>$->n_oper = TNAME;
   IdType($$) = (TypeP)MakeFun(0, l, k);
};
Dec: FunName;
Dec: ID DOT FunName {
   $$ = (NodeP)IdCopy($3);
   $<pn>$->n_qualifier = MakeId($<s>1);
};
Dec: Name FunName {
   $$ = $2;
   set_scope($<pn>1);
   $<pn>$->n_qualifier = $<pn>1;
};
Dec: Name TNAME {
   $$ = (NodeP)IdCopy($2);
   set_scope($<pn>1);
   $<pn>$->n_oper = TNAME;
   $<pn>$->n_qualifier = $<pn>1;
};
Dec: Ptr Dec %prec MUL {
   PtrType($1) = IdType($2);
   IdType($2) = (TypeP)$1;
   $$ = $2;
};
Dec: Ptr TNAME %prec MUL {
   $$ = (NodeP)IdCopy($2);
   $<pn>$->n_oper = TNAME;
   Idhide($2);
   IdType($$) = (TypeP)$1;
};
Dec: TNAME Vec %prec LB {
   $$ = (NodeP)IdCopy($1);
   $<pn>$->n_oper = TNAME;
   Idhide($1);
   IdType($$) = (TypeP)$2;
};
Dec: Dec Vec %prec LB {
   VecType($2) = IdType($1);
   IdType($1) = (TypeP)$2;
};
Dec: LP Dec RP Args /* xxxxx need a CAST here? */ {
   RetType($4) = IdType($2);
   IdType($2) = (TypeP)$4;
   $$ = $2;
};
Dec: LP Dec RP Vec /* xxx */ {
   VecType($4) = IdType($2);
   IdType($2) = (TypeP)$4;
   $$ = $2;
};

ArgDec: ID { $$ = (NodeP)MakeId($<s>1); };
ArgDec: %prec NO_ID { $$ = (NodeP)MakeId(0); };
ArgDec: Ptr ArgDec %prec MUL {
   PtrType($1) = IdType($2);
   IdType($2) = (TypeP)$1;
   $$ = $2;
};
ArgDec: ArgDec Vec %prec LB {
   VecType($2) = IdType($1);
   IdType($1) = (TypeP)$2;
};
ArgDec: LP ArgDec RP Args {
   RetType($4) = IdType($2);
   IdType($2) = (TypeP)$4;
   $$ = $2;
};
ArgDec: LP ArgDec RP Vec {
   VecType($4) = IdType($2);
   IdType($2) = (TypeP)$4;
   $$ = $2;
};

NewDec: %prec NO_ID { $$ = (NodeP)MakeId(0); };
NewDec: Ptr NewDec %prec MUL {
   PtrType($1) = IdType($2);
   IdType($2) = (TypeP)$1;
   $$ = $2;
};
NewDec: NewDec Vec %prec LB {
   VecType($2) = IdType($1);
   IdType($1) = (TypeP)$2;
};

CastDec: %prec NO_ID { $$ = (NodeP)MakeId(0); };
CastDec: Ptr CastDec %prec MUL {
   PtrType($1) = IdType($2);
   IdType($2) = (TypeP)$1;
   $$ = $2;
};
CastDec: CastDec Vec %prec LB {
   VecType($2) = IdType($1);
   IdType($1) = (TypeP)$2;
};
CastDec: LP CastDec RP Args {
   RetType($4) = IdType($2);
   IdType($2) = $<pt>4;
   $$ = $2;
};
CastDec: LP CastDec RP Vec {
   VecType($4) = IdType($2);
   IdType($2) = $<pt>4;
   $$ = $2;
};

CDec: %prec NO_ID { $$ = (NodeP)MakeId(0); };
CDec: Ptr CDec %prec MUL {
   PtrType($1) = IdType($2);
   IdType($2) = (TypeP)$1;
   $$ = $2;
};

/* === Statements: returns StP === */
Sts: Sts St {
   if ($2)
      if ($1) AddSt($1, $2);
      else {
         $$ = NewSts($2);
         stmt_seen = 1;
      }
};
Sts: St {
   if ($1) {
      $$ = NewSts($1);
      stmt_seen = 1;
   }
};

Cond: LP Ex RP {
   $$ = $2;
   if ($<pe>$ == dummy) error("empty condition");
   stmt_seen = 1;
};

Block: LC {
   cd_vec[cdi] = cd;
   stmt_vec[cdi] = stmt_seen;
   tn_vec[cdi++] = modified_tn;
   cd = 0;
   stmt_seen = 0;
   modified_tn = 0;
} Sts RC {
   IdP n = DelId(cd);
   StP ss = DelSt($3);
   $$ = (NodeP)MakeBlock($<l>1, n, ss);
   if (modified_tn) restore();
   cd = cd_vec[--cdi];
   stmt_seen = stmt_vec[cdi];
   modified_tn = tn_vec[cdi];
   if (cdi < 0) errorT('i',"block level(%d)", cdi);
};
Block: LC RC { $$ = (NodeP)MakeBlock($<l>1, 0, 0); };
Block: LC error RC { $$ = (NodeP)MakeBlock($<l>1, 0, 0); };

St0: Ex { $$ = (NodeP)MakeESt(SM, curloc, $<pe>1, 0); };
St0: BREAK { $$ = (NodeP)MakeSt(BREAK, $<l>1, 0); };
St0: CONTINUE { $$ = (NodeP)MakeSt(CONTINUE, $<l>1, 0); };
St0: RETURN Ex { $$ = (NodeP)MakeESt(RETURN, $<l>1, $<pe>2, 0); };
St0: GOTO ID {
   IdP n = MakeId($<s>2);
   $$ = (NodeP)MakeLSt(GOTO, $<l>1, n, 0);
};
St0: DELETE Ex { $$ = (NodeP)MakeESt(DELETE, $<l>1, $<pe>2, 0); };
St0: DO { stmt_seen = 1; } St WHILE Cond { $$ = (NodeP)MakeESt(DO, $<l>1, $<pe>5, $<ps>3); };

St: St0 SM;
St: ASM LP STRING RP SM {
   if (stmt_seen) $$ = (NodeP)MakeESt(ASM, curloc, (ExP)$<s>3, 0);
   else {
      IdP n = MakeId(make_name('A'));
      n->tp = (TypeP)MakeBase(ASM, (IdP)$<s>3);
      if (cd) AddList(cd, n); else cd = (IdP)NewIds(n); //(#) Clipped at "(IdP)nli".
      $$ = 0;
   }
};
/* St: St0 { error("';' missing after simpleS"); }; */
St: DataDec {
   if ($<pn>1)
      if (stmt_seen) {
         IdP n = $<pn>1;
         $$ = (NodeP)MakeBlock(n->where, n, 0);
         $<ps>$->base = DCL;
      } else goto dddd;
};
St: AttFunDef {
   LexUnGet(RC);
   error("nestedFD (did you forget a ``}''?)");
dddd:
   if (cd) AddList(cd, $1); else cd = (IdP)NewIds($1); //(#) Clipped at "NewIds($".
   $$ = 0;
};
St: Block;
St: IF Cond St { $$ = (NodeP)MakeIfSt($<l>1, $<pe>2, $<ps>3, 0); };
St: IF Cond St ELSE St { $$ = (NodeP)MakeIfSt($<l>1, $<pe>2, $<ps>3, $<ps>5); };
St: WHILE Cond St { $$ = (NodeP)MakeESt(WHILE, $<l>1, $<pe>2, $<ps>3); };
/* St: FOR LP { stmt_seen = 1; cm_warn++; } Ex SM Ex SM Ex RP St {
   $$ = MakeForSt($<l>1, $<pe>4, $<pe>6, $<pe>8, $<ps>10); //(#) Clipped at "$<ps>10".
   cm_warn--;
}; */
St: FOR LP { stmt_seen = 1; cm_warn++; } St Ex SM Ex RP St {
   $$ = (NodeP)MakeForSt($<l>1, $<ps>4, $<pe>5, $<pe>7, $<ps>9); //(#) Clipped at "$<ps>9)".
   cm_warn--;
};
St: FOR CAST { stmt_seen = 1; cm_warn++; } St Ex SM Ex RP St { //(#) Clipped at "stateme".
   $$ = (NodeP)MakeForSt($<l>1, $<ps>4, $<pe>5, $<pe>7, $<ps>9); //(#) Clipped at "$<ps>9)".
   cm_warn--;
};
St: SWITCH Cond St { $$ = (NodeP)MakeESt(SWITCH, $<l>1, $<pe>2, $<ps>3); };
St: ID COLON { $$ = (NodeP)MakeId($<s>1); stmt_seen = 1; } St {
   IdP n = $<pn>3;
   $$ = (NodeP)MakeLSt(LABEL, n->where, n, $<ps>4);
};
St: CASE { stmt_seen = 1; } Ex COLON St {
   if ($<pe>3 == dummy) error("empty case label");
   $$ = (NodeP)MakeESt(CASE, $<l>1, $<pe>3, $<ps>5);
};
St: DEFAULT COLON { stmt_seen = 1; } St { $$ = (NodeP)MakeSt(DEFAULT, $<l>1, $<ps>4); };

/* === Expressions: returns ExP === */
Exs: Inits {
   ExP e = DelEx($1);
   while (e && e->e1 == dummy) {
      if (e->e2) error("EX inEL");
      FreeEx(e);
      e = e->e2;
   }
   $$ = (NodeP)e;
}; //(#) The semicolon was missing or clipped from the original.

Inits: Init %prec CM {
   ExP e = MakeEx(ELIST, $<pe>1, 0);
   $$ = (NodeP)MakeExs(e);
};
Inits: Inits CM Init {
   ExP e = MakeEx(ELIST, $<pe>3, 0);
   AddEx($1, e);
};

Init: Ex %prec CM;
Init: LC Exs RC {
   ExP e;
   if ($2) e = $<pe>2; else e = MakeEx(ELIST, dummy, 0);
   $$ = (NodeP)MakeEx(ILIST, e, 0);
};

Ex: Ex ASSIGN Ex {
BinOp:
   $$ = (NodeP)MakeEx($<t>2, $<pe>1, $<pe>3);
};
Ex: Ex PLUS Ex { goto BinOp; };
Ex: Ex MINUS Ex { goto BinOp; };
Ex: Ex MUL Ex { goto BinOp; };
Ex: Ex AND Ex { goto BinOp; };
Ex: Ex OR Ex { goto BinOp; };
Ex: Ex ER Ex { goto BinOp; };
Ex: Ex SHIFTOP Ex { goto BinOp; };
Ex: Ex EQUOP Ex { goto BinOp; };
Ex: Ex DIVOP Ex { goto BinOp; };
Ex: Ex RELOP Ex { goto BinOp; };
Ex: Ex ANDAND Ex { goto BinOp; };
Ex: Ex OROR Ex { goto BinOp; };
Ex: Ex ASOP Ex { goto BinOp; };
Ex: Ex CM Ex {
   if (cm_warn == 0) errorT('w',"comma not in parentheses"); //(#) Clipped at "parentheses".
   goto BinOp;
};
Ex: Ex QUEST Ex COLON Ex { $$ = (NodeP)MakeQEx($<pe>1, $<pe>3, $<pe>5); };
Ex: Ex1;

Ex1: TYPE LP Exs RP {
   Token b = $<t>1;
   TypeP t;
   switch (b) {
      case CHAR: t = (TypeP)char_type; break;
      case SHORT: t = (TypeP)short_type; break;
      case INT: t = (TypeP)int_type; break;
      case LONG: t = (TypeP)long_type; break;
      case UNSIGNED: t = (TypeP)uint_type; break;
      case FLOAT: t = (TypeP)float_type; break;
      case DOUBLE: t = (TypeP)double_type; break;
      case VOID: t = (TypeP)void_type; break;
      default:
         error("illegal constructor:%k", b);
         t = (TypeP)int_type;
   }
   $$ = (NodeP)MakeTEx(VALUE, t, $<pe>3);
};
Ex1: TNAME LP Exs RP {
   TypeP t = IdType($1);
   $$ = (NodeP)MakeTEx(VALUE, t, $<pe>3);
};
Ex1: NEW NewType { TypeP t = IdType($2); $$ = (NodeP)MakeTEx(NEW, t, 0); };
Ex1: NEW LP NewType RP { TypeP t = IdType($3); $$ = (NodeP)MakeTEx(NEW, t, 0); };
/*
Ex1: NEW NewType LP Exs RP { TypeP t = IdType($2); $$ = MakeTEx(NEW, t, $<pe>4); };
Ex1: NEW LP NewType LP Exs RP RP { TypeP t = IdType($3); $$ = MakeTEx(NEW, t, $<pe>5); };
*/
Ex1: Ex1 ICOP { $$ = (NodeP)MakeEx($<t>2, $<pe>1, 0); };
Ex1: CAST CastType RP Ex1 /* lex() returns CAST instead of LP */ {
   TypeP t = IdType($2);
   $$ = (NodeP)MakeTEx(CAST, t, $<pe>4);
};
Ex1: MUL Ex1 { $$ = (NodeP)MakeEx(DEREF, $<pe>2, 0); };
Ex1: AND Ex1 { $$ = (NodeP)MakeEx(ADDROF, 0, $<pe>2); };
Ex1: MINUS Ex1 { $$ = (NodeP)MakeEx(UMINUS, 0, $<pe>2); };
Ex1: NOT Ex1 { $$ = (NodeP)MakeEx(NOT, 0, $<pe>2); };
Ex1: COMPL Ex1 { $$ = (NodeP)MakeEx(COMPL, 0, $<pe>2); };
Ex1: ICOP Ex1 { $$ = (NodeP)MakeEx($<t>1, 0, $<pe>2); };
Ex1: SIZEOF Ex1 {
   ExP e = $<pe>2;
   if (e->base == CAST) {
      ExP ee = e->e1;
      Token k = ee->base;
      switch (k) {
         case UMINUS:
            ee = MakeEx(MINUS, e, ee->e2);
            goto kk;
         case DEREF:
            if (ee->e2) goto dd;
            ee = MakeEx(MUL, e, ee->e1);
            goto kk;
         case ADDROF:
            ee = MakeEx(AND, e, ee->e2);
          kk:
            e->base = SIZEOF;
            e->e1 = 0;
            $$ = (NodeP)ee;
            break;
         default:
            dd:
            e->base = SIZEOF;
            $$ = $2;
      }
   } else $$ = (NodeP)MakeTEx(SIZEOF, 0, e);
};
Ex1: Ex1 LB Ex RB { $$ = (NodeP)MakeEx(DEREF, $<pe>1, $<pe>3); };
Ex1: Ex1 LP Exs RP {
   ExP ee = $<pe>3;
   ExP e = $<pe>1;
   if (e->base == NEW) e->e1 = ee; else $$ = (NodeP)MakeCall(e, ee);
};
Ex1: Ex1 REF Ex0 { $$ = (NodeP)MakeRef(REF, $<pe>1, $<pn>3); };
Ex1: Ex1 REF TNAME { IdP n = IdCopy($3); $$ = (NodeP)MakeRef(REF, $<pe>1, n); };
Ex1: Ex1 DOT Ex0 { $$ = (NodeP)MakeRef(DOT, $<pe>1, $<pn>3); };
Ex1: Ex1 DOT TNAME { IdP n = IdCopy($3); $$ = (NodeP)MakeRef(DOT, $<pe>1, n); };
Ex1: MEM Tag { $$ = (NodeP)IdCopy($2); $<pn>$->n_qualifier = sta_name; };
Ex1: Ex0;
Ex1: LP { cm_warn++; } Ex RP { $$ = $3; cm_warn--; };
Ex1: ZERO { $$ = (NodeP)zero; };
Ex1: ICON { $$ = (NodeP)NewCon(ICON, $1); };
Ex1: FCON { $$ = (NodeP)NewCon(FCON, $1); };
Ex1: STRING { $$ = (NodeP)NewCon(STRING, $1); };
Ex1: CCON { $$ = (NodeP)NewCon(CCON, $1); };
Ex1: THIS { $$ = (NodeP)NewCon(THIS, 0); };
Ex1: %prec NO_EXPR { $$ = (NodeP)dummy; };

Ex0: ID { $$ = (NodeP)MakeId($<s>1); };
Ex0: TNAME MEM Tag {
   $$ = (NodeP)IdCopy($3);
   $<pn>$->n_qualifier = $<pn>1;
};
Ex0: OPERATOR Op {
   $$ = (NodeP)MakeId(oper_name($2));
   $<pn>$->n_oper = $<t>2;
};
Ex0: TNAME MEM OPERATOR Op {
   $$ = (NodeP)MakeId(oper_name($4));
   $<pn>$->n_oper = $<t>4;
   $<pn>$->n_qualifier = $<pn>1;
};

/* === Abstract types (return type IdP) === */
CastType: TSps CastDec { $$ = (NodeP)NormCast($1, $2); };
CTp: TYPE { $$ = (NodeP)MakeBase($<t>1, 0); };
CTp: TNAME { $$ = (NodeP)MakeBase(TYPE, $<pn>1); };
CType: CTp CDec { $$ = (NodeP)NormCast($1, $2); };

NewType: TSps NewDec { $$ = (NodeP)NormCast($1, $2); };
ArgType: TSps ArgDec { $$ = (NodeP)NormData($1, $2); };
ArgType: TSps ArgDec ASSIGN Init {
   $$ = (NodeP)NormData($1, $2);
   $<pn>$->n_initializer = $<pe>4;
};

Args: CAST ArgTypes RP {
   Token k = 1;
   IdP l = $<pn>2;
   if (fct_void && l == 0) k = 0;
   $$ = (NodeP)MakeFun(0, DelId(l), k);
};
Args: CAST ArgTypes ELLIPSIS RP {
   Token k = ELLIPSIS;
   IdP l = $<pn>2;
   if (fct_void && l == 0) k = 0;
   $$ = (NodeP)MakeFun(0, DelId(l), k);
};
Args: CAST ArgTypes CM ELLIPSIS RP {
   Token k = ELLIPSIS;
   IdP l = $<pn>2;
   if (fct_void && l == 0) k = 0;
   errorT('w',"syntax error: comma before ellipsis");
   $$ = (NodeP)MakeFun(0, DelId(l), k);
};
Args: LP ArgTypes RP {
   Token k = 1;
   IdP l = $<pn>2;
   if (fct_void && l == 0) k = 0;
   $$ = (NodeP)MakeFun(0, DelId(l), k);
};
Args: LP ArgTypes ELLIPSIS RP {
   Token k = ELLIPSIS;
   IdP l = $<pn>2;
   if (fct_void && l == 0) k = 0;
   $$ = (NodeP)MakeFun(0, DelId(l), k);
};
Args: LP ArgTypes CM ELLIPSIS RP {
   Token k = ELLIPSIS;
   IdP l = $<pn>2;
   if (fct_void && l == 0) k = 0;
   errorT('w',"syntax error: comma before ellipsis");
   $$ = (NodeP)MakeFun(0, DelId(l), k);
};

ArgTypes: ArgTypes CM OptArgType {
   if ($3)
      if ($1) AddId($1, $3);
      else {
         error("AD syntax");
         $$ = NewIds($3);
      }
   else error("AD syntax");
};
ArgTypes: OptArgType %prec CM {
   if ($1) $$ = NewIds($1);
};

OptArgType: ArgType;
OptArgType: %prec EMPTY { $$ = 0; }; //(#) The semicolon was missing or clipped from the original.

Ptr: MUL { $$ = (NodeP)MakePtr(PTR, 0, 0); };
Ptr: AND { $$ = (NodeP)MakePtr(RPTR, 0, 0); };
Ptr: MUL TYPE {
   Token c = $<t>2;
   if (c == CONST) $$ = (NodeP)MakePtr(PTR, 0, 1);
   else {
      $$ = (NodeP)MakePtr(PTR, 0, 0);
      error("syntax error: *%k", c);
   }
};
Ptr: AND TYPE {
   Token c = $<t>2;
   if (c == CONST) $$ = (NodeP)MakePtr(RPTR, 0, 1);
   else {
      $$ = (NodeP)MakePtr(RPTR, 0, 0);
      error("syntax error: &%k", c);
   }
};

Vec: LB Ex RB {
   ExP d = $<pe>2;
   $$ = (NodeP)NewVec(d != dummy? d: 0);
};
%%
