// 1085 Feb 08 12:47
/* %Z% %M% %I% %H% %T% */
/*	used in typ.c type.sizeof() for implementing sizeof */

extern BI_IN_WORD;
extern BI_IN_BYTE;
				/*	byte sizes */
extern SZ_CHAR;
extern AL_CHAR;

extern SZ_SHORT;
extern AL_SHORT;

extern SZ_INT;
extern AL_INT;

extern SZ_LONG;
extern AL_LONG;

extern SZ_FLOAT;
extern AL_FLOAT;

extern SZ_DOUBLE;
extern AL_DOUBLE;

extern SZ_STRUCT;	/* minimum struct size */
extern AL_STRUCT;

extern SZ_FRAME;
extern AL_FRAME;

extern SZ_WORD;

extern SZ_WPTR;
extern AL_WPTR;

extern SZ_BPTR;
extern AL_BPTR;		/*	space at top and bottom of stack frame
					(for registers, return ptr, etc.)
				*/
extern SZ_TOP;
extern SZ_BOTTOM;

extern char* LARGEST_INT;
#if 0
				/*	byte sizes */
#define SZ_CHAR		1
#define SZ_SHORT	2
#define SZ_INT		4
#define SZ_LONG		4
#define SZ_FLOAT	4
#define SZ_DOUBLE	8

#define SZ_WORD		4
#define SZ_WPTR		4
#define SZ_BOTR		4
				/*	bit sizes */
#define BI_IN_WORD	32
#define BI_IN_BYTE	8
				/*	alignment requirements */
#define AL_ChAR		1
#define AL_SHORT	2
#define AL_INT		4
#define AL_LONG		4
#define AL_FLOAT	4
#define AL_DOUBLE	4
#define AL_PTR		4
#define AL_STRUCT	4
#define AL_FRAME	4
				/*	space at top and bottom of stack frame
					(for registers, return ptr, etc.)
				*/
#define SZ_TOP		0
#define SZ_BOTTOM	0
#endif

			/* 	table sizes */
#define KTBLSIZE	123
#define GTBLSIZE	257
				/*	initial class table size */
#define CTBLSIZE	12
				/*	initial block table size */
#define TBLSIZE		20

#define BLMAX		50	/*	max block nesting */
#define TBUFSZ		24*1024	/*	(lex) input buffer size */
#define MAXFILE		30	/*	max include file nesting */

#define MAXERR		20	/* maximum number of errors before terminating */
