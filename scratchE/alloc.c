// 1985 Feb 08 12:48
/* %Z% %M% %I% %H% %T% */
#include "cfront.h"
#include <stdlib.h> // For free() and malloc().

extern void print_free(void);

void print_free(void) {
   fprintf(stderr, "print_free (no longer supported)\n");
}

void *MAlloc(unsigned nbytes) { /* general-purpose storage allocator */
   Nalloc++;
   void *X = malloc(nbytes); if (X == 0) return 0;
   return X;
}

int NFn, NFtn, NFbt, NFpv, NFf, NFe, NFs, NFc;

void Free(void *ap) { /* put block on free list */
   if (ap == 0) return;

   Nfree++;

   if (Nspy) {
      IdP pp = (IdP) ap;
      Token t = pp->base;
      char *s = 0;

      switch (t) {
/*
         case TNAME:
         case NAME:
            NFn++;
            fprintf(stderr, "??name %d %d sz=%d\n", pp, t, p->size); fflush(stderr); //(#) Clipped at "(stderr)".
            break;
*/
         case INT:
         case CHAR:
         case TYPE:
         case VOID:
         case SHORT:
         case LONG:
         case FLOAT:
         case DOUBLE:
         case COBJ:
         case EOBJ:
         case FIELD:
            NFbt++;
            break;

         case PTR:
         case VEC:
            NFpv++;
            break;

         case FCT:
            NFf++;
            break;
/*
         case INCR: case DECR: case ASSIGN: case CALL: case PLUS: case MINUS:
         case DEREF: case MUL: case DIV: case ASPLUS: case MOD: case UMINUS:
         case DOT: case REF: case CAST: case NEW: case NOT: case COMPL: case ER:
         case EQ: case NE: case GT: case LT: case LE: case GE:
         case ANDAND: case AND: case OR: case OROR: case SIZEOF:
         case ILIST: case ELIST: case CM: case QUEST: case RS: case LS:
         case TEXT: case IVAL: case FVAL:
            NFe++;
            fprintf(stderr, "??expr %d %d sz=%d\n", pp, t, p->size); fflush(stderr); //(#) Clipped at "(stderr)".
            break;
*/
         case ICON:
         case CCON:
         case STRING:
         case FCON:
         case THIS:
            NFc++;
            break;
/*
         case IF: case SM: case FOR: case WHILE: case DO: case BLOCK:
         case BREAK: case CONTINUE: case DEFAULT: case SWITCH: case CASE:
         case PAIR: case LABEL: case GOTO: case RETURN: case DELETE: case ASM:
            NFs++;
            break;
*/
/*
         default:
            if (0 < t && t < 140) fprintf(stderr, "delete tok %d\n", t);
*/
      }
   }
   free(ap);
}
