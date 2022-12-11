// 1985 Feb 08 12:48
/* %Z% %M% %I% %H% %T% */
/***************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T Technologies, Inc. All rigths Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T TECHNOLOGIES, INC.

	If you ignore this notice the ghost of Ma Bell will haunt you forever.

	
lex.c:
	lexical analyser based on pcc's and cpre's scanners
	modified to handle classes:
	new keywords:	class
			public
			call
			etc.
	names are not entered in the symbol table by lex()
	names can be of arbitrary length
	error() is used to report errors
	{} and () must match
	numeric constants are not converted into internal representation
	but stored as strings

****************************************************************************/

#include "cfront.h"
#include "yystype.h"
#include "size.h"

# define  CCTRANS(x) x

	/* lexical actions */

#define A_ERR	0		/* illegal character */
#define A_LET	1		/* saw a letter */
#define A_DIG	2		/* saw a digit */
#define A_1C	3		/* return a single character */
#define A_STR 	4		/* string */
#define A_CC	5		/* character constant */
#define A_BCD	6		/* GCOS BCD constant */
#define A_SL	7		/* saw a / */
#define A_DOT 	8		/* saw a . */
#define A_2C	9		/* possible two character symbol */
#define A_WS	10		/* whitespace (not \n) */
#define A_NL	11		/* \n */
#define A_LC	12		/* { */
#define A_RC	13		/* } */
#define A_L	14		/* ( */
#define A_R	15		/* ) */
#define A_EOF	16
#define A_ASS	17
#define A_LT	18
#define A_GT	19		/* > */
#define A_ER	20
#define A_OR	21
#define A_AND	22
#define A_MOD	23
#define A_NOT	24
#define A_MIN	25
#define A_MUL	26
#define A_PL	27
#define A_COL	28		/* : */

	/* character classes */

# define LEXLET 01
# define LEXDIG 02
/* no LEXOCT because 8 and 9 used to be octal digits */
# define LEXHEX 010
# define LEXWS 020
# define LEXDOT 040

	/* text buffer */
char inbuf[TBUFSZ];
char* txtmax = &inbuf[TBUFSZ-1];
char* txtstart;
char* txtfree;
#define pch(c) ((txtmax<=txtfree)?error('i',"input buffer overflow"):(*txtfree++=c))
#define start_txt()	txtstart = txtfree
#define del_txt()	txtfree = txtstart

char* file_name[MAXFILE];	/* stack of source file names */
				/* file_name[0] == 0 means stdin */
class loc curloc;
FILE * out_file = stdout;
FILE * in_file = stdin;
Ptable ktbl;
int br_level = 0;		/* number of unmatched ``(''s */
int bl_level = 0;		/* number of unmatched ``{''s */

# ifdef ibm

# define CSMASK 0377
# define CSSZ 256

# else

# define CSMASK 0177
# define CSSZ 128

# endif

short lxmask[CSSZ+1];

int saved;	/* putback character, avoid ungetchar */
int lastseen;	/* last token returned */
extern int lxtitle();

#define get(c)		(c=getc(in_file))
#define unget(c)	ungetc(c,in_file)

#define reti(a,b)	{ yylval.t = b; return lastseen=a; }
#define retn(a,b)	{ yylval.p = (Pnode)b; return lastseen=a; }
#define rets(a,b)	{ yylval.s = b; return lastseen=a; }
#define retl(a)		{ yylval.l = curloc; return lastseen=a; }

void ktbl_init()
/*
	enter keywords into keyword table for use by lex()
	and into keyword representation table used for output
*/
{
	ktbl = new table(KTBLSIZE,0,0);

	new_key("asm",ASM,0);
	new_key("auto",AUTO,TYPE);
	new_key("break",LOC,BREAK);
	new_key("case",LOC,CASE);
	new_key("continue",LOC,CONTINUE);
	new_key("char",CHAR,TYPE);
	new_key("do",LOC,DO);
	new_key("double",DOUBLE,TYPE);
	new_key("default",LOC,DEFAULT);
	new_key("enum",ENUM,0);
/*	new_key("fortran",FORTRAN);	*/
	new_key("else",LOC,ELSE);
	new_key("extern",EXTERN,TYPE);
	new_key("float",FLOAT,TYPE);
	new_key("for",LOC,FOR);
	new_key("fortran",FORTRAN,0);
	new_key("goto",LOC,GOTO);
	new_key("if",LOC,IF);
	new_key("int",INT,TYPE);
	new_key("long",LONG,TYPE);
	new_key("return",LOC,RETURN);
	new_key("register",REGISTER,TYPE);
	new_key("static",STATIC,TYPE);
	new_key("struct",STRUCT,AGGR);
	new_key("sizeof",SIZEOF,0);
	new_key("short",SHORT,TYPE);
	new_key("switch",LOC,SWITCH);
	new_key("typedef",TYPEDEF,TYPE);
	new_key("unsigned",UNSIGNED,TYPE);
	new_key("union",UNION,AGGR);
	new_key("void",VOID,TYPE);
	new_key("while",LOC,WHILE);
}

extern char* src_file_name;
extern char* line_format;
loc last_line;

void loc.putline()
{
	if (file==0 && line==0) return;
	if (0<=file && file<MAXFILE) {
		char* f = file_name[file];
		if (f==0) f = (src_file_name) ? src_file_name : "";
		fprintf(out_file,line_format,line,f);
		last_line = *this;
	}
}

void loc.put(FILE* p)
{
	if (0<=file && file<MAXFILE) {
		char* f = file_name[file];
		if (f==0) f = (src_file_name) ? src_file_name : "";
		fprintf(p,"\"%s\", line %d: ",f,line);
	}
}	

void lxenter( s, m ) register char *s; register short m;
/* enter a mask into lxmask */
{
	register c;

	while( c= *s++ ) lxmask[c+1] |= m;

}


void lxget(c,m) register c, m;
/*
	put 'c' back then scan for members of character class 'm'
	terminate the string read with \0
	txtfree points to the character position after that \0
*/
{
	pch(c);
	while ( (get(c), lxmask[c+1]&m) ) pch(c);
	unget(c);
	pch('\0');
}

struct LXDOPE {
	short lxch;	/* the character */
	short lxact;	/* the action to be performed */
	TOK   lxtok;	/* the token number to be returned */
} lxdope[] = {
	'$',	A_ERR,	0,	/* illegal characters go here... */
	'_',	A_LET,	0,	/* letters point here */
	'0',	A_DIG,	0,	/* digits point here */
	' ',	A_WS,	0,	/* whitespace goes here */
	'\n',	A_NL,	0,
	'"',	A_STR,	0,	/* character string */
	'\'',	A_CC,	0,	/* ASCII character constant */
	'`',	A_BCD,	0,	/* 'foreign' character constant, e.g. BCD */
	'(',	A_L,	LP,
	')',	A_R,	RP,
	'{',	A_LC,	LC,
	'}',	A_RC,	RC,
	'[',	A_1C,	LB,
	']',	A_1C,	RB,
	'*',	A_MUL,	MUL,
	'?',	A_1C,	QUEST,
	':',	A_COL,	COLON,
	'+',	A_PL,	PLUS,
	'-',	A_MIN,	MINUS,
	'/',	A_SL,	DIV,
	'%',	A_MOD,	MOD,
	'&',	A_AND,	AND,
	'|',	A_OR,	OR,
	'^',	A_ER,	ER,
	'!',	A_NOT,	NOT,
	'~',	A_1C,	COMPL,
	',',	A_1C,	CM,
	';',	A_1C,	SM,
	'.',	A_DOT,	DOT,
	'<',	A_LT,	LT,
	'>',	A_GT,	GT,
	'=',	A_ASS,	ASSIGN,
	EOF,	A_EOF,	EOFTOK
	};
/* note: EOF is used as sentinel, so must be <=0 and last entry in table */

struct LXDOPE *lxcp[CSSZ+1];

extern void lex_init();
void lex_init()
{
	register struct LXDOPE *p;
	register i;
	register char *cp;
	/* set up character classes */

	/* first clear lexmask */
	for(i=0; i<=CSSZ; i++) lxmask[i] = 0;

	lxenter( "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_", LEXLET );
	lxenter( "0123456789", LEXDIG );
	lxenter( "0123456789abcdefABCDEF", LEXHEX );
		/* \013 should become \v someday; \013 is OK for ASCII and EBCDIC */ //(#) Clipped at "EBCDIC *".
	lxenter( " \t\r\b\f\013", LEXWS );
	lxmask['.'+1] |= LEXDOT;

	/* make lxcp point to appropriate lxdope entry for each character */

	/* initialize error entries */

	for( i= 0; i<=CSSZ; ++i ) lxcp[i] = lxdope;

	/* make unique entries */

	for( p=lxdope; ; ++p ) {
		lxcp[p->lxch+1] = p;
		if( p->lxch < 0 ) break;
		}

	/* handle letters, digits, and whitespace */
	/* by convention, first, second, and third places */

	cp = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
	while( *cp ) lxcp[*cp++ + 1] = &lxdope[1];
	cp = "123456789";
	while( *cp ) lxcp[*cp++ + 1] = &lxdope[2];
	cp = "\t\b\r\f\013";
	while( *cp ) lxcp[*cp++ + 1] = &lxdope[3];

	file_name[0] = src_file_name;
	curloc.file = 0;
	curloc.line = 1;

	ktbl_init();

	lex_clear();

	saved = lxtitle();
}

void lex_clear()
{
	txtstart = txtfree = inbuf;
}

char * chconst()
/*
	read a character constant into inbuf
*/
{
	register c;
	int nch = 0;

	pch('\'');

	forever {
		if (SZ_INT < nch++) {
			error("char constant too long");
			goto ex;
		}

		switch (get(c)) {
		case '\'':
			goto ex;
		case EOF:
			error("eof in char constant");
			goto ex;
		case '\n':
			error("newline in char constant");
			goto ex;
		case '\\':
			pch(c);
			switch (get(c)){
			case '\n':
				++curloc.line;
			default:
				pch(c);
				break;
			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9':
				pch(c);
				get(c);  /* try for 2 */
				if( lxmask[c+1] & LEXDIG ){
					pch(c);
					get(c);  /* try for 3 */
					if (lxmask[c+1] & LEXDIG) pch(c);
					else unget(c);
				}
				else unget(c);
				break;
			};
			break;
		default:
			pch(c);
		}
	}
ex:
	pch('\'');
	pch('\0');
	return txtstart;
}

void lxcom()
/* process a "block comment" */
{
	register c;

	forever
	switch (get(c)) {
	case EOF:
		error("eof in comment");
		return;
	case '\n':
		curloc.line++;
		Nline++;
		break;
	case '*':
		if (get(c) == '/') return;
		unget(c);
		break;
	case '/':
		if (get(c) == '*') error('w',"``/*'' in comment");
		unget(c);
		break;
	}
}


void linecom()
/* process a "line comment" */
{
	register c;

	forever
	switch (get(c)) {
	case EOF:
		error("eof in comment");
		return;
	case '\n':
		curloc.line++;
		Nline++;
		saved = lxtitle();
		return;
	}
}

struct xyzzy {
	TOK t;
	int y;	/* fake for yystype */
};
xyzzy bck;

TOK lex()
{
	TOK ret;
	Pname n;

	if( bck.t ) {
		xyzzy tmp = bck;
		bck.t = 0;
		if( tmp.t==LC || tmp.t==RC )
			retl( tmp.t )
		else
			rets( tmp.t, (char *)tmp.y )
	}

	Ntoken++;

	forever {
		register lxchar;
		register struct LXDOPE *p;

		start_txt();

		if (saved) {
			lxchar = saved;
			saved = 0;
		}
		else
			get(lxchar);

		switch( (p=lxcp[lxchar+1])->lxact ){
		case A_1C:
			/* eat up a single character, and return an opcode */

			reti(p->lxtok,p->lxtok);

		case A_EOF:
			if (br_level || bl_level)
				error("'%s' missing at end of input",(bl_level) ? "}" : ")"); //(#) Clipped at '(bl_level) ? "'.
			reti(EOFTOK,0);

		case A_ERR:
			error("illegal character (0%o)",lxchar);
			break;

		case A_LET:
			/* collect an identifier, check for reserved word, and return */ //(#) Clipped at "and retu".
			lxget( lxchar, LEXLET|LEXDIG );

			if (n = ktbl->look(txtstart,0)) {
				TOK x;
				del_txt();
				switch (x=n->base) {
				case TNAME:
					retn(TNAME,n);
					break;
				case LOC:
					retl(n->syn_class);
				default:
					reti(n->syn_class,x);
				}
			}
			else {
				rets(ID,txtstart);
			}

		case A_DIG:

			ret = ICON;

			if (lxchar=='0') {	/* octal or hexadecimal number */
				pch('0');
				switch (get(lxchar)) {
				case 'l':
				case 'L':
					pch('L');
					pch(0);
					rets(ICON,txtstart);
				case 'x':
				case 'X':
					lxget('X',LEXHEX);
					switch (get(lxchar)) {
					case 'l':
					case 'L':
						txtfree--;
						pch('L');
						pch(0);
						break;
					default:
						saved = lxchar;
					}
					rets(ICON,txtstart);
				case '8':
				case '9':
					error("8 or 9 used as octal digit");
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
					pch(lxchar);
				ox:
					switch (get(lxchar)) {
					case '8':
					case '9':
						error("8 or 9 used as octal digit"); //(#) Clipped at 'digit")'.
					case '0':
					case '1':
					case '2':
					case '3':
					case '4':
					case '5':
					case '6':
					case '7':
						pch(lxchar);
						goto ox;
					case 'l':
					case 'L':
						pch('L');
						pch(0);
						break;
					default:
						pch(0);
						saved = lxchar;
					}
					rets(ICON,txtstart);
				case '.':
					lxget('.',LEXDIG);
					goto getfp;
				default:
					saved = lxchar;
					reti(ZERO,0);
				}
			}
			else
				lxget(lxchar,LEXDIG);

			if (get(lxchar) == '.') {
				txtfree--;
				lxget('.', LEXDIG );
		getfp:
				ret = FCON;
				get(lxchar);
			};

			switch (lxchar) {
			case 'e':
			case 'E':
				txtfree--;
				switch (get(lxchar)) {
				case '-':
				case '+':
					pch('e');
					break;
				default:
					unget(lxchar);
					lxchar = 'e';
				};
				lxget( lxchar, LEXDIG );
				ret = FCON;
				break;
			case 'l':
			case 'L':
				txtfree--;
				pch('L');
				break;
			default:
				saved = lxchar;
			};

			pch(0);
			rets(ret,txtstart);

		case A_DOT:
			if (get(lxchar) == '.') {	/* look for ellipsis */
				if (get(lxchar) != '.') {
					error("token .. ?");
					saved = lxchar;
				}
				reti(ELLIPSIS,0);
			}
			if( lxmask[lxchar+1] & LEXDIG ){/* look for floating constant */ //(#) Clipped at "consta".
				unget(lxchar);
				lxget( '.', LEXDIG );
				goto getfp;
			}
			saved = lxchar;
			reti(DOT,0);

		case A_STR:
			/* save string constant in buffer */
			forever
			switch (get(lxchar)) {
			case '\\':
				pch('\\');
				get(lxchar);
				pch(lxchar);
				break;
			case '"':
				pch(0);
				rets(STRING,txtstart);
			case '\n':
				error("newline in string");
				pch(0);
				rets(STRING,txtstart);
			case EOF:
				error("eof in string");
				pch(0);
				rets(STRING,txtstart);
			default:
				pch(lxchar);
			}

		case A_CC:
			/* character constant */
			rets(CCON,chconst());

		case A_BCD:
			{
				register i;
				int j;
	
				pch('`');
	
				for (i=0; i<7; ++i) {
					pch(get(j));
					if (j == '`' ) break;
				}
				pch(0);
				if (6<i)
					error("bcd constant exceeds 6 characters" ); //(#) Clipped at ")".
				rets(CCON,txtstart);
			}

		case A_SL:	/* / */
			switch (get(lxchar))  {
			case '*':
				lxcom();
				break;
			case '/':
				linecom();
				break;
			case '=':
				reti(ASOP,ASDIV);
			default:
				saved = lxchar;
				reti(DIVOP,DIV);
			}

		case A_WS:
			continue;

		case A_NL:
			++curloc.line;
			Nline++;
			saved = lxtitle();
			continue;

		case A_LC:
			if (BLMAX <= bl_level++) {
				error('s',"blocks too deaply nested");
				ext(3);
			}
			retl(LC);

		case A_RC:
			if (bl_level-- <= 0) {
				error("unX '}'");
				bl_level = 0;
			}
			retl(RC);

		case A_L:
		/*
			return CAST if the LP is the start of a cast LP otherwise
			only
				( type-name (
			is a real problem
		*/
			br_level++;
			switch (lastseen) {	/* f( => all bets are off */
			case NAME:
			case TNAME:
			case TYPE:
				reti(LP,0);
			}

			if( saved )
				lxchar = saved;
			else
				get( lxchar );
			while( ( p = lxcp[ lxchar+1 ] )->lxact == A_WS )
				get( lxchar );
			saved = lxchar;
			if( p->lxact != A_LET ) reti( LP,  0 );

			bck.t = lex();
			bck.y = int( yylval.s );
			switch (bck.t) {
			case TYPE:
			case TNAME:
				break;
			case AGGR:
			case ENUM:
				reti(CAST,0);
			default:
				reti(LP,0);
			}

			if( saved )
				lxchar = saved;
			else
				get( lxchar );
			while( ( p = lxcp[ lxchar+1 ] )->lxact == A_WS )
				get( lxchar );
			saved = lxchar;
			switch (lxchar) {
			case ':':	reti(LP,0);	/* (classname::memname */
			case '(':	break;
			default:	reti(CAST,0);
			}

			/*	here is the real prblem:
				CAST:	( int ( * ) ( ) ) p;
				LP:	( int ( * p ) )

				ignore
					( int ( &
				and	( int ( [
				and	( int ( (	problems
			*/
			get( lxchar );
			while( ( p = lxcp[ lxchar+1 ] )->lxact == A_WS ) get( lxchar ); //(#) Clipped at "get( lxcha".
			if (lxchar != '*') {
				unget(lxchar);
				reti(LP,0);
			}

			get( lxchar );
			while( ( p = lxcp[ lxchar+1 ] )->lxact == A_WS ) get( lxchar ); //(#) Clipped at "get( lxcha".
			unget(lxchar);
			unget('*');
			if (lxchar == ')') reti(CAST,0);

			reti(LP,0);

		case A_R:
			if (br_level-- <= 0) {
				error("unX ')'");
				br_level = 0;
			}
			reti(RP,0);
		case A_ASS:
			switch (get(lxchar)) {
			case '=':
				reti(EQUOP,EQ);
			default:
				saved = lxchar;
				reti(ASSIGN,ASSIGN);
			}
		case A_COL:
			switch (get(lxchar)) {
			case ':':
				reti(MEM,0);
			case '=':
				error("':=' is not a c++ operator");
				reti(ASSIGN,ASSIGN);
			default:
				saved = lxchar;
				reti(COLON,COLON);
			}
		case A_NOT:
			switch (get(lxchar)) {
			case '=':
				reti(EQUOP,NE);
			default:
				saved = lxchar;
				reti(NOT,NOT);
			}
		case A_GT:
			switch(get(lxchar)) {
			case '>':
				switch (get(lxchar)) {
				case '=':
					reti(ASOP,ASRS);
					break;
				default:
					saved = lxchar;
					reti(SHIFTOP,RS);
				}
			case '=':
				reti(RELOP,GE);
			default:
				saved = lxchar;
				reti(RELOP,GT);
			}
		case A_LT:
			switch (get(lxchar)) {
			case '<':
				switch (get(lxchar)) {
				case '=':
					reti(ASOP,ASLS);
				default:
					saved = lxchar;
					reti(SHIFTOP,LS);
				}
			case '=':
				reti(RELOP,LE);
			default:
				saved = lxchar;
				reti(RELOP,LT);
			}
		case A_AND:
			switch (get(lxchar)) {
			case '&':
				reti(ANDAND,ANDAND);
			case '=':
				reti(ASOP,ASAND);
			default:
				saved = lxchar;
				reti(AND,AND);
			}
		case A_OR:
			switch (get(lxchar)) {
			case '|':
				reti(OROR,OROR);
			case '=':
				reti(ASOP,ASOR);
			default:
				saved = lxchar;
				reti(OR,OR);
			}
		case A_ER:
			switch (get(lxchar)) {
			case '=':
				reti(ASOP,ASER);
			default:
				saved = lxchar;
				reti(ER,ER);
			}
		case A_PL:
			switch (get(lxchar)) {
			case '=':
				reti(ASOP,ASPLUS);
			case '+':
				reti(ICOP,INCR);
			default:
				saved = lxchar;
				reti(PLUS,PLUS);
			}
		case A_MIN:
			switch (get(lxchar)) {
			case '=':
				reti(ASOP,ASMINUS);
			case '-':
				reti(ICOP,DECR);
			case '>':
				reti(REF,REF);
			default:
				saved = lxchar;
				reti(MINUS,MINUS);
			}
		case A_MUL:
			switch (get(lxchar)) {
			case '=':
				reti(ASOP,ASMUL);
			case '/':
				error('w',"*/ not as end of comment");
			default:
				saved = lxchar;
				reti(MUL,MUL);
			}
		case A_MOD:
			switch (get(lxchar)) {
			case '=':
				reti(ASOP,ASMOD);
			default:
				saved = lxchar;
				reti(DIVOP,MOD);
			}
		default:
			error('i',"lex act==%d getc()->%d",p,lxchar);

		}

		error('i',"lex, main switch");

	}

}

int lxtitle()
/*
	called after a newline; set linenumber and file name
*/
{
	register c;

	forever
	switch ( get(c) ) {
	default:
		return c;
/*	case EOF:
		return EOF;	*/
	case '\n':
		curloc.line++;
		Nline++;
		break;
	ll:
		break;
	case '#': 	/* # lineno "filename" */
		curloc.line = 0;
		forever 
		switch (get(c)) {
		case '"':
			start_txt();
			forever
			switch (get(c)) {
			case '"':
				pch('\0');
				if (get(c) != '\n') error("unX eol on # line");
				if (*txtstart) {
					/*	maintain stack of file names */
					int f = curloc.file;

					char* fn;
					if (f == 0) goto push;
					if ( (fn=file_name[f]) && (strcmp(txtstart,fn)==0) ) { //(#) Clipped at "strcmp(txtstart,".
						/* same file: ignore */
					}
					else if ( (fn=file_name[f-1]) && (strcmp(txtstart,fn)==0) ) { //(#) Clipped at "strcmp(tx".
						/* previous file: pop */
					/*	delete(file_name[f]);*/
						curloc.file--;
					}
					else {
						/* new file name: push */
						char * p;
				push:
						Nfile++;
						p = new char[txtfree-txtstart+1];

						if (MAXFILE<=++f) error('i',"fileN stack overflow"); //(#) Clipped at "fileN ".
						file_name[curloc.file=f] = p;
						(void) strcpy(p,txtstart);
						Nstr++;
					}
				}
				else {
					/* back to the original .c file: "" */
/*
					int f = curloc.file;
					if (1<f) error('i',"fileN buffer (%d)",f);
					if (f) delete file_name[f];
*/
					curloc.file = 0;
				}
				del_txt();
/*
				curloc.putline();
*/
				goto ll;
			case '\n':
				error("unX end of line on '# line'");
			default:
				pch(c);
			}
		case ' ':
			break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			curloc.line = curloc.line*10+c-'0'; 
			break;
		case 'c':	/* ignore #class */
			if (get(c) == 'l')
				while (get(c) != '\n') ;
			curloc.line++;
			Nline++;
			goto ll;
		case '\n':
			curloc.putline();
			goto ll;
		default:
			error("unX character on '# line'");
		}
	}
} 
