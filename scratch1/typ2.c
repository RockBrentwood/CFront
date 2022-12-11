/* @(#) typ2.c 1.3 1/27/86 17:49:36 */
/*ident	"@(#)cfront:src/typ2.c	1.3" */
/**************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

typ2.c:

***************************************************************************/

#include "cfront.h"
#include "size.h"

extern void typ_init(void);
void typ_init(void) {
   defa_type = int_type = MakeBase(INT, 0);
   PERM(int_type);

   moe_type = MakeBase(INT, 0);
   PERM(moe_type);
   moe_type->b_const = 1;
   checkBase(moe_type, 0);

   uint_type = MakeBase(INT, 0);
   PERM(uint_type);
   type_adj(uint_type, UNSIGNED);
   checkBase(uint_type, 0);

   long_type = MakeBase(LONG, 0);
   PERM(long_type);
   checkBase(long_type, 0);

   ulong_type = MakeBase(LONG, 0);
   PERM(ulong_type);
   type_adj(ulong_type, UNSIGNED);
   checkBase(ulong_type, 0);

   short_type = MakeBase(SHORT, 0);
   PERM(short_type);
   checkBase(short_type, 0);

   ushort_type = MakeBase(SHORT, 0);
   PERM(ushort_type);
   type_adj(ushort_type, UNSIGNED);
   checkBase(ushort_type, 0);

   float_type = MakeBase(FLOAT, 0);
   PERM(float_type);

   double_type = MakeBase(DOUBLE, 0);
   PERM(double_type);

   zero_type = MakeBase(ZTYPE, 0);
   PERM(zero_type);
   zero->tp = (TypeP)zero_type;

   void_type = MakeBase(VOID, 0);
   PERM(void_type);

   char_type = MakeBase(CHAR, 0);
   PERM(char_type);

   uchar_type = MakeBase(CHAR, 0);
   PERM(uchar_type);
   type_adj(uchar_type, UNSIGNED);
   checkBase(uchar_type, 0);

   Pchar_type = (TypeP)MakePtr(PTR, (TypeP)char_type, 0);
   PERM(Pchar_type);

   Pint_type = (TypeP)MakePtr(PTR, (TypeP)int_type, 0);
   PERM(Pint_type);

   Pvoid_type = (TypeP)MakePtr(PTR, (TypeP)void_type, 0);
   PERM(Pvoid_type);

   Pfctvec_type = (TypeP)MakeFun((TypeP)int_type, 0, 0); /* must be last, see normalizeBase() */
   Pfctvec_type = (TypeP)MakePtr(PTR, Pfctvec_type, 0);
   Pfctvec_type = (TypeP)MakePtr(PTR, Pfctvec_type, 0);
   PERM(Pfctvec_type);

   any_tbl = MakeTable(TBLSIZE, 0, 0);
   gtbl = MakeTable(GTBLSIZE, 0, 0);
   gtbl->t_name = MakeId("global");
}

/*
	perform the "usual arithmetic conversions" C ref Manual 6.6
	on "this" op "t"
	"this" and "t" are integral or floating
	"t" may be 0
*/
BaseP arit_conv(BaseP this, BaseP t) {
   bit l;
   bit u;
   bit f;
   bit l1 = (this->base == LONG);
   bit u1 = this->b_unsigned;
   bit f1 = (this->base == FLOAT || this->base == DOUBLE);
   if (t) {
      bit l2 = (t->base == LONG);
      bit u2 = t->b_unsigned;
      bit f2 = (t->base == FLOAT || t->base == DOUBLE);
      l = l1 || l2;
      u = u1 || u2;
      f = f1 || f2;
   } else {
      l = l1;
      u = u1;
      f = f1;
   }

   if (f) return double_type;
   if (l & u) return ulong_type;
   if (l & !u) return long_type;
   if (u) return uint_type;
   return int_type;
}

bit vec_const = 0;

/*
	is this type a constant
*/
bit tconst(TypeP this) {
   TypeP t = this;
   vec_const = 0;
 xxx:
   switch (t->base) {
      case TYPE:
         if (ToBaseP(t)->b_const) return 1;
         t = ToBaseP(t)->b_name->tp;
         goto xxx;
      case VEC:
         vec_const = 1;
         return 1;
      case PTR:
      case RPTR:
         return ToPtrP(t)->rdo;
      default:
         return ToBaseP(t)->b_const;
   }
}

/*
	make someting a constant or variable, return 0 if no problem
*/
Token set_const(TypeP this, bit mode) {
   TypeP t = this;
 xxx:
   switch (t->base) {
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xxx;
      case ANY:
      case RPTR:
      case VEC:
         return t->base; // constant by definition
      case PTR:
         ToPtrP(t)->rdo = mode;
         return 0;
      default:
         ToBaseP(t)->b_const = mode;
         return 0;
   }
}

int is_ref(TypeP this) {
   TypeP t = this;
 xxx:
   switch (t->base) {
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xxx;
      case RPTR:
         return 1;
      default:
         return 0;
   }
}

int align(TypeP this) {
   TypeP t = this;
 xx:
/*fprintf(stderr,"align %d %d\n",t,t->base);*/
   switch (t->base) {
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xx;
      case COBJ:
         t = ToBaseP(t)->b_name->tp;
         goto xx;
      case VEC:
         t = ToVecP(t)->typ;
         goto xx;
      case ANY:
         return 1;
      case CHAR:
         return AL_CHAR;
      case SHORT:
         return AL_SHORT;
      case INT:
         return AL_INT;
      case LONG:
         return AL_LONG;
      case FLOAT:
         return AL_FLOAT;
      case DOUBLE:
         return AL_DOUBLE;
      case PTR:
      case RPTR:
         return AL_WPTR;
      case CLASS:
         return ToClassP(t)->obj_align;
      case ENUM:
      case EOBJ:
         return AL_INT;
      case VOID:
         error("illegal use of void");
         return AL_INT;
      default:
         errorT('i', "align(%d,%k)", t, t->base);
   }
}

/*
	the sizeof type operator
	return the size in bytes of the types representation
*/
int tsizeof(TypeP this) {
   TypeP t = this;
 zx:
   if (t == 0) errorT('i', "tsizeof(t==0)");
   switch (t->base) {
      case TYPE:
      case COBJ:
/*fprintf(stderr,"tsizeof %d %d %s%s\n",t,t->base,((BaseP)t)->b_name->string,(t->permanent)?" PERM":"");fflush(stderr);*/
         t = ((BaseP) t)->b_name->tp;
         goto zx;
      case ANY:
         return 1;
      case VOID:
         return 0;
      case ZTYPE:
         return SZ_WPTR; /* assume pointer */
      case CHAR:
         return SZ_CHAR;
      case SHORT:
         return SZ_SHORT;
      case INT:
         return SZ_INT;
      case LONG:
         return SZ_LONG;
      case FLOAT:
         return SZ_FLOAT;
      case DOUBLE:
         return SZ_DOUBLE;
      case VEC:
      {
         VecP v = (VecP) t;
         if (v->size == 0) return SZ_WPTR;
         return v->size * tsizeof(v->typ);
      }
      case PTR:
      case RPTR:
         t = ((PtrP) t)->typ;
       xxx:
         switch (t->base) {
            default:
               return SZ_WPTR;
            case CHAR:
               return SZ_BPTR;
            case TYPE:
               t = ((BaseP) t)->b_name->tp;
               goto xxx;
         }
      case FIELD:
      {
         BaseP b = (BaseP) t;
         return b->b_bits / BI_IN_BYTE + 1;
      }
      case CLASS:
      {
         ClassP cl = (ClassP) t;
         int sz = cl->obj_size;
         if ((cl->defined & (DEFINED | SIMPLIFIED)) == 0) {
            error("%sU, size not known", cl->string);
            return SZ_INT;
         }
         return sz;
      }
      case EOBJ:
      case ENUM:
         return SZ_INT;
      default:
         errorT('i', "sizeof(%d)", t->base);
   }
}

bit fct_type(TypeP this) {
   return 0;
}

bit vec_type(TypeP this) {
   TypeP t = this;
 xx:
   switch (t->base) {
      case ANY:
      case VEC:
      case PTR:
      case RPTR:
         return 1;
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xx;
      default:
         return 0;
   }
}

/*	index==1:	*p
	index==0:	p[Ex]
*/
TypeP deref(TypeP this) {
   TypeP t = this;
 xx:
   switch (t->base) {
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xx;
      case PTR:
      case RPTR:
      case VEC:
         if (t == Pvoid_type) error("void* dereferenced");
         return ToVecP(t)->typ;
      case ANY:
         return t;
      default:
         error("nonP dereferenced");
         return (TypeP)any_type;
   }
}
