// 1085 Feb 08 12:47
/* %Z% %M% %I% %H% %T% */
/*	used in typ.c sizeof() for implementing sizeof */

extern int BI_IN_WORD;
extern int BI_IN_BYTE;
                                /*      byte sizes */
extern int SZ_CHAR;
extern int AL_CHAR;

extern int SZ_SHORT;
extern int AL_SHORT;

extern int SZ_INT;
extern int AL_INT;

extern int SZ_LONG;
extern int AL_LONG;

extern int SZ_FLOAT;
extern int AL_FLOAT;

extern int SZ_DOUBLE;
extern int AL_DOUBLE;

extern int SZ_STRUCT; /* minimum struct size */
extern int AL_STRUCT;

extern int SZ_FRAME;
extern int AL_FRAME;

extern int SZ_WORD;

extern int SZ_WPTR;
extern int AL_WPTR;

extern int SZ_BPTR;
extern int AL_BPTR;	/*      space at top and bottom of stack frame
			(for registers, return ptr, etc.)
			*/
extern int SZ_TOP;
extern int SZ_BOTTOM;

extern char *LARGEST_INT;
#if 0
                                /*      byte sizes */
#   define SZ_CHAR	1
#   define SZ_SHORT	2
#   define SZ_INT	4
#   define SZ_LONG	4
#   define SZ_FLOAT	4
#   define SZ_DOUBLE	8

#   define SZ_WORD	4
#   define SZ_WPTR	4
#   define SZ_BOTR	4
                                /*      bit sizes */
#   define BI_IN_WORD	32
#   define BI_IN_BYTE	8
                                /*      alignment requirements */
#   define AL_ChAR	1
#   define AL_SHORT	2
#   define AL_INT	4
#   define AL_LONG	4
#   define AL_FLOAT	4
#   define AL_DOUBLE	4
#   define AL_PTR	4
#   define AL_STRUCT	4
#   define AL_FRAME	4
                                /*      space at top and bottom of stack frame
                                   (for registers, return ptr, etc.)
                                 */
#   define SZ_TOP	0
#   define SZ_BOTTOM	0
#endif

                        /*      table sizes */
#define KTBLSIZE	123
#define GTBLSIZE	257
                                /*      initial struct Table size */
#define CTBLSIZE	12
                                /*      initial block table size */
#define TBLSIZE		20

#define BLMAX		50 /*      max block nesting */
#define TBUFSZ		24*1024 /*      (lex) input buffer size */
#define MAXFILE		30 /*      max include file nesting */

#define MAXERR		20 /* maximum number of errors before terminating */
