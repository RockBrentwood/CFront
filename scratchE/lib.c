#include "lib.h"

static PFVV _new_handler = 0;

extern PFVV set_new_handler(PFVV handler) {
   PFVV rr = _new_handler;
   _new_handler = handler;
   return rr;
}

extern void *_new(unsigned long size) {
   void *p;

   while ((p = malloc(size)) == 0) {
      if (_new_handler)
         (*_new_handler) ();
      else
         return 0;
   }
   return p;
}

extern void _delete(void *p) {
   if (p) free(p);
}

extern void *_vec_new(void *X, unsigned n, unsigned sz, PF f) {
/*
	allocate a vector of "n" elements of size "sz"
	and initialize each by a call of "f"
*/
   register unsigned i;
   register char *p = (char *)X;
   if (p == 0) p = _new(n*sz*sizeof *p);
   for (i = 0; i < n; i++) (*f)(p + i*sz);
   return (void *)p;
}

void _vec_delete(void *X, unsigned n, unsigned sz, PF f, int _free) {
   register unsigned i;
   register char *p = (char *)X;
   for (i = 0; i < n; i++) (*f)(p + i*sz);
}

PFVV _ctors[] = { 0 }, _dtors[] = { 0 };

extern void Exit(int i) {
   static int ddone;
   if (ddone == 0) { // once only
      ddone = 1;
      PFVV *pf = _dtors;
      while (*pf) pf++;
      while (_dtors < pf) (**--pf) ();
   }
   exit(i);
}

extern void Enter(void) {
   typedef void (*PFVV)();
   for (PFVV * pf = _ctors; *pf; pf++) (**pf) ();
}
