/* @(#) tqueue.h 1.3 1/27/86 17:49:34 */
/*ident	"@(#)cfront:src/tqueue.h	1.3" */
#ifndef EOF
#   include <stdio.h> /* For printf() and fprintf() */
#endif

typedef struct TokNode *TokNodeP;
struct TokNode {
   Token tok; /* token for parser */
   YYSTYPE retval; /* $arg */
   TokNodeP next;
   TokNodeP last;
};
TokNodeP MakeTokNode(Token, YYSTYPE);
static inline void FreeTokNode(TokNodeP this);
extern TokNodeP front;
extern TokNodeP rear;

extern void addtok(Token, YYSTYPE); /* add tok to rear of Q */
extern Token deltok(void); /* take tok from front of Q */

// extern char* image(Token);
extern void tlex(void);
extern Token lalex(void);
extern void *MAlloc(unsigned);

extern YYSTYPE yylval;
extern Token tk; // last token returned;

extern char *image(Token);
