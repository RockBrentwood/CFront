/* @(#) alloc.c 1.3 1/27/86 17:48:32 */
/*ident	"@(#)cfront:src/alloc.c	1.3" */
#include "cfront.h"
#include <stdlib.h> /* For free() and malloc() */

extern void print_free(void);

void print_free(void) {
   fprintf(stderr, "print_free (no longer supported)\n");
}

void *MAlloc(unsigned nbytes) {
   Nalloc++;
   void *X = malloc(nbytes); if (X == 0) errorT('i', "free store exhausted");
// Nfree_store is no longer used: it was the number of bits allocated by morecore() from the free store.
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

         case ICON:
         case CCON:
         case STRING:
         case FCON:
         case THIS:
            NFc++;
            break;
      }
   }
   free(ap);
}
