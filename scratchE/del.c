// 1985 Feb 08 12:48
/* %Z% %M% %I% %H% %T% */
/************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T Technologies, Inc.
		All rigths reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T TECHNOLOGIES, INC.

	If you ignore this notice the ghost of Ma Bell will haunt you forever.

del.c:

	walk the trees to reclaim storage

**************************************************************/

#include "cfront.h"

void delId(IdP this) {
/*fprintf(stderr,"%d->delId: %s %d\n",this,(this->string)?this->string:"?",this->base);fflush(stderr);*///(#) Clipped at "fflush(stder".
   ExP i = this->n_initializer;

   NFn++;
   DEL(Type, this->tp);
   if (i && i != (ExP) 1) DEL(Ex, i);
   this->n_tbl_list = name_free;
   name_free = this;
}

void delType(TypeP this) {
/*fprintf(stderr,"DEL(Type=%d %d)\n",this,this->base);*/
   this->permanent = 3; /* do not delete twice */
   switch (this->base) {
      case TNAME:
      case NAME:
         errorT('i', "%d->delType():N %s %d", this, ((IdP) this)->string, this->base);
      case TYPE:
      {
         BaseP b = (BaseP) this;
         break;
      }
      case FCT:
      {
         FunP f = (FunP) this;
         DEL(Type, f->returns);
      /* DEL(Id, f->argtype);
       */
         break;
      }
      case VEC:
      {
         VecP v = (VecP) this;
         DEL(Ex, v->dim);
         DEL(Type, v->typ);
         break;
      }
      case PTR:
      case RPTR:
      {
         PtrP p = (PtrP) this;
         DEL(Type, p->typ);
         break;
      }
/*    case CLASS:
      {
         ClassP cl = (ClassP)this;
         delTable(&this->memtbl);
         break;
      }
      case ENUM:
      case OVERLOAD:
         break;*/
   }

   _delete(this);
}

void delEx(ExP this) {
/*fprintf(stderr,"DEL(Ex=%d: %d %d %d)\n",this,this->base,this->e1,this->e2); fflush(stderr);*/
   this->permanent = 3;
   switch (this->base) {
      case IVAL:
         if (this == one) return;
      case ICON:
      case FCON:
      case CCON:
      case THIS:
      case STRING:
      case TEXT:
      case FVAL:
         goto dd;
      case DUMMY:
      case ZERO:
      case NAME:
         return;
      case CAST:
      case SIZEOF:
      case NEW:
      case VALUE:
         DEL(Type, this->tp2);
         break;
      case REF:
      case DOT:
         DEL(Ex, this->e1);
         DEL(Id, this->mem);
         goto dd;
      case QUEST:
         DEL(Ex, this->cond);
         break;
      case ICALL:
         _delete(this->il);
         goto dd;
   }

   DEL(Ex, this->e1);
   DEL(Ex, this->e2);
/* DEL(Type, this->tp);*/
 dd:
   this->e1 = expr_free;
   expr_free = this;
   NFe++;
}

void delSt(StP this) {
/*fprintf(stderr,"DEL(St=%d %s)\n",this,keys[this->base]); fflush(stderr);*/
   this->permanent = 3;
   switch (this->base) {
      case SM:
      case WHILE:
      case DO:
      case DELETE:
      case RETURN:
      case CASE:
      case SWITCH:
         DEL(Ex, this->e);
         break;
      case PAIR:
         DEL(St, this->s2);
         break;
      case BLOCK:
         DEL(Id, this->d);
         DEL(St, this->s);
         if (this->own_tbl) DEL(Table, this->memtbl);
         DEL(St, this->s_list);
         goto dd;
      case FOR:
         DEL(Ex, this->e);
         DEL(Ex, this->e2);
         DEL(St, this->for_init);
         break;
      case IF:
         DEL(Ex, this->e);
         DEL(St, this->else_stmt);
         break;
   }

   DEL(St, this->s);
   DEL(St, this->s_list);
 dd:
   this->s_list = stmt_free;
   stmt_free = this;
   NFs++;
}

void delTable(TableP this) {
   register int i;
/*fprintf(stderr,"tbl.del %s %d size=%d used=%d)\n", (this->t_name)?this->t_name->string:"?", this, this->size, this->free_slot-1); fflush(stderr);*///(#) Clipped at ", th".

   for (i = 1; i < this->free_slot; i++) {
      IdP n = this->entries[i];
      if (n == 0) errorT('i', "delTable(0)");
      switch (n->n_scope) {
         case ARG:
         case ARGT:
            break;
         default:
         {
            char *s = n->string;
            if (s && (s[0] != '_' || s[1] != 'X')) _delete(s);
         /* FreeId(n); */
            delId(n);
         }
      }
   }
   _delete(this->entries);
   _delete(this->hashtbl);
   _delete(this);
}
