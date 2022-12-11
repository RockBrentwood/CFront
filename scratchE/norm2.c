// 1985 Feb 08 12:48
/* %Z% %M% %I% %H% %T% */
/************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T Technologies, Inc.
		All rigths Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T TECHNOLOGIES, INC.

	If you ignore this notice the ghost of Ma Bell will haunt you forever.

norm2.c:

	"normalization handles problems which could have been handled
	by the syntax analyser; but has not been done. The idea is
	to simplify the grammar and the actions associated with it,
	and to get a more robust error handling

****************************************************************************/

#include "cfront.h"
#include "size.h"
extern void *MAlloc(unsigned);

FunP MakeFun(TypeP t, IdP arg, Token known) {
   FunP this = (FunP)_new(sizeof *this);
   Nt++;
   this->base = FCT;
   this->nargs_known = known;
   this->returns = t;
   this->argtype = arg;
/*fprintf(stderr,"MakeFun t %d %d arg %d %d -> %d\n",t, t?t->base:0, arg, arg?arg->base:0, this);*///(#) Clipped at "arg->base:".

   if (arg == 0 || arg->base == ELIST) return this;

   register IdP n;
   for (n = arg; n; n = n->n_list) {
      switch (n->tp->base) {
         case VOID:
            this->argtype = 0;
            this->nargs = 0;
            this->nargs_known = 1;
            if (n->string)
               error("voidFA%n", n);
            else if (this->nargs || n->n_list) {
               error("voidFA");
               this->nargs_known = 0;
            }
            break;
         case CLASS:
         case ENUM:
            break;
         default:
            this->nargs++;
      }
   }
   return this;
}

ExP expr_free;
#define EBITE 250

static void InitEx(ExP this, Token ba, ExP a, ExP b) {
   Ne++;
   this->base = ba;
   this->e1 = a;
   this->e2 = b;
}

ExP MakeEx(Token ba, ExP a, ExP b) {
   register ExP p;

   if ((p = expr_free) == 0) {
      register ExP q = (ExP) MAlloc(EBITE * sizeof *q);
      for (p = expr_free = &q[EBITE - 1]; q < p; p--) p->e1 = p - 1;
      (p + 1)->e1 = 0;
   /*fprintf(stderr, "MAlloc %d expr_free=%d p+1=%d\n", EBITE*sizeof *p, expr_free, p+1);*///(#) Clipped at "expr_".
   } else
      expr_free = p->e1;

/* beware of alignment differences */
   if (sizeof *p & 1) {
      register char *pp = (char *)(p + 1);
      while ((char *)p < pp) *--pp = 0;
   } else if (sizeof *p & 2) {
      register short *pp = (short *)(p + 1);
      while ((short *)p < pp) *--pp = 0;
   } else {
      register int *pp = (int *)(p + 1);
      while ((int *)p < pp) *--pp = 0;
   }

   ExP this = p;
/*fprintf(stderr,"MakeEx(%d,%d,%d)->%d\n",ba,a,b,this); fflush(stderr);*/
   InitEx(this, ba, a, b);
   return this;
}

void FreeEx(ExP this) {
   NFe++;
/*fprintf(stderr,"FreeEx(%d, %d %d %d)\n",this,this->base,this->e1,this->e2); */
   this->e1 = expr_free;
   expr_free = this;
// this = 0;
}

StP stmt_free;
#define SBITE 250

StP MakeSt(Token ba, struct Loc ll, StP a) {
   register StP p;

   if ((p = stmt_free) == 0) {
      register StP q = (StP) MAlloc(SBITE * sizeof *q);
      for (p = stmt_free = &q[SBITE - 1]; q < p; p--) p->s_list = p - 1;
      (p + 1)->s_list = 0;
   } else
      stmt_free = p->s_list;

/* beware of alignment differences */
   if (sizeof *p & 1) {
      register char *pp = (char *)(p + 1);
      while ((char *)p < pp) *--pp = 0;
   } else if (sizeof *p & 2) {
      register short *pp = (short *)(p + 1);
      while ((short *)p < pp) *--pp = 0;
   } else {
      register int *pp = (int *)(p + 1);
      while ((int *)p < pp) *--pp = 0;
   }

   StP this = p;

   Ns++;
   this->base = ba;
   this->where = ll;
   this->s = a;
   return this;
}

void FreeSt(StP this) {
   NFs++;
   this->s_list = stmt_free;
   stmt_free = this;
// this = 0;
}

ClassP MakeClass(Token b, IdP n) {
   ClassP this = (ClassP)_new(sizeof *this);
   this->base = CLASS;
   this->csu = b;
   this->pubmem = n;
   this->memtbl = MakeTable(CTBLSIZE, 0, 0);
   return this;
}

BaseP MakeBase(Token b, IdP n) {
/*fprintf(stderr,"%d->MakeBase(%d %d)\n",this,b,n);*/
   BaseP this = (BaseP)_new(sizeof *this);
   Nbt++;
   switch (b) {
      case 0:
         break;
      case TYPEDEF:
         this->b_typedef = 1;
         break;
      case INLINE:
         this->b_inline = 1;
         break;
      case VIRTUAL:
         this->b_virtual = 1;
         break;
      case CONST:
         this->b_const = 1;
         break;
      case UNSIGNED:
         this->b_unsigned = 1;
         break;
      case FRIEND:
      case OVERLOAD:
      case EXTERN:
      case STATIC:
      case AUTO:
      case REGISTER:
         this->b_sto = b;
         break;
      case SHORT:
         this->b_short = 1;
         break;
      case LONG:
         this->b_long = 1;
         break;
      case ANY:
      case ZTYPE:
      case VOID:
      case CHAR:
      case INT:
      case FLOAT:
      case DOUBLE:
         this->base = b;
         break;
      case TYPE:
      case COBJ:
      case EOBJ:
      case FIELD:
      case ASM:
         this->base = b;
         this->b_name = n;
         break;
      default:
         errorT('i', "badBT:%k", b);
   }
   return this;
}

#define NBITE 250
IdP name_free;

IdP MakeId(char *s) {
   register IdP p;

   if ((p = name_free) == 0) {
      register IdP q = (IdP) MAlloc(NBITE * sizeof *q);
      for (p = name_free = &q[NBITE - 1]; q < p; p--) p->n_tbl_list = p - 1;
      (p + 1)->n_tbl_list = 0;
   /*fprintf(stderr, "MAlloc %d name_free=%d p+1=%d\n", NBITE*sizeof *p, name_free, p+1); *///(#) Clipped at "name_".
   } else
      name_free = p->n_tbl_list;

/* beware of alignment differences */
   if (sizeof *p & 1) {
      register char *pp = (char *)(p + 1);
      while ((char *)p < pp) *--pp = 0;
   } else if (sizeof *p & 2) {
      register short *pp = (short *)(p + 1);
      while ((short *)p < pp) *--pp = 0;
   } else {
      register int *pp = (int *)(p + 1);
      while ((int *)p < pp) *--pp = 0;
   }

   IdP this = p;
   InitEx((ExP)this, NAME, (ExP) s, 0);
/*fprintf(stderr,"%d: MakeId %s %d\n",this,s,this->base); fflush(stderr);*/

   Nn++;
   this->where = curloc;
   this->lex_level = bl_level;
   return this;
}

void FreeId(IdP this) {
   NFn++;
/*fprintf(stderr,"delete %d: %s %d\n",this,this->string,this->base);*/
   this->n_tbl_list = name_free;
   name_free = this;
// this = 0;
}

IdsP MakeIds(IdP n) {
   IdsP this = (IdsP)_new(sizeof *this);
   IdP nn;

   if (n == 0) errorT('i', "MakeIds(0)");

   this->head = n;
   for (nn = n; nn->n_list; nn = nn->n_list);
   this->tail = nn;
   Nl++;
   return this;
}

void add_list(IdsP this, IdP n) {
   IdP nn;

   this->tail->n_list = n;
   for (nn = n; nn->n_list; nn = nn->n_list);
   this->tail = nn;
}

int NFl;

IdP name_unlist(IdsP l) {
   IdP n;
   if (l == 0) return 0;
   n = l->head;
   NFl++;
   _delete(l);
   return n;
}

StP stmt_unlist(StsP l) {
   StP s;
   if (l == 0) return 0;
   s = l->head;
   NFl++;
   _delete(l);
   return s;
}

ExP expr_unlist(ExsP l) {
   ExP e;
   if (l == 0) return 0;
   e = l->head;
   NFl++;
   _delete(l);
   return e;
}
