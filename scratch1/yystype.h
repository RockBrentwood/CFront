/* @(#) yystype.h 1.3 1/27/86 17:49:37 */
/*ident	"@(#)cfront:src/yystype.h	1.3" */
typedef union {
   const char *s;
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
} YYSTYPE;
extern YYSTYPE yylval;
