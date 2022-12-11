/* @(#) expand.c 1.3 1/27/86 17:48:48 */
/*ident	"@(#)cfront:src/expand.c	1.3" */
/*****************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

expand.c:

	expand inline functions

******************************************************************/

#include "cfront.h"

/*
	make the name of the temporary: _X_vn_fn_cn
*/
const char *temp(const char *vn, const char *fn, const char *cn) {
   if (vn[0] != '_' || vn[1] != 'X') {
      int vnl = strlen(vn);
      int fnl = strlen(fn);
      int cnl = (cn) ? strlen(cn) : 0;
      char *s = _new((vnl + fnl + cnl + 6)*sizeof *s);

      s[0] = '_';
      s[1] = 'X';
      strcpy(s + 2, vn);
      s[vnl + 2] = '_';
      strcpy(s + vnl + 3, fn);
      if (cnl) {
         s[vnl + fnl + 3] = '_';
         strcpy(s + vnl + fnl + 4, cn);
      }
      return s;
   } else
      return vn;

}

IdP dcl_local(TableP scope, IdP an, IdP fn) {
   if (scope == 0) {
      errorT('s', "cannot expand inlineF needing temporary variable in nonF context");
      return an;
   }
   if (an->n_stclass == STATIC) errorT('s', "static%n in inlineF", an);
   IdP cn = fn->n_table->t_name;
   const char *s = temp(an->string, fn->string, (cn) ? cn->string : 0);
   IdP nx = MakeId(s);
/*errorT('d',"%n: dcl_local(%d, %s)",fn,scope,s); */
   nx->tp = an->tp;
   PERM(nx->tp);
   nx->n_used = an->n_used;
   nx->n_assigned_to = an->n_assigned_to;
   nx->n_addr_taken = an->n_addr_taken;
   IdP r = insert(scope, nx, 0);
   FreeId(nx);
   return r;
}

/*
	copy the statements with the formal arguments replaced by ANAMES

	called once only per inline function
	expand_tbl!=0 if the function should be transformed into an expression
	and expand_tbl is the table for local variables
*/
StP expandSt(StP this) {
   if (this == 0) errorT('i', "expandSt(0) for%n", expand_fn);
/*errorT('d',"St %d:%k s=%d e=%d l=%d",this,this->base,this->s,this->e,this->s_list);*/

   if (this->memtbl) { /* check for static variables */
      register TableP t = this->memtbl;
      register int i;
      for (register IdP n = get_mem(t, i = 1); n; n = get_mem(t, ++i))
         if (n->n_stclass == STATIC) {
            errorT('s', "static%n in inlineF", n);
            n->n_stclass = AUTO;
         }
   }

   if (expand_tbl) { /* make expression */
      ExP ee;
      static int ret_seen = 0;

      if (this->memtbl && this->base != BLOCK) { /* temporaries */
         int i;
         IdP n;
         TableP tbl = this->memtbl;
         for (n = get_mem(tbl, i = 1); n; n = get_mem(tbl, ++i)) {
/*errorT('d',"%n: %n",expand_fn,n);*/
            IdP nn = dcl_local(scope, n, expand_fn);
            nn->base = NAME;
            n->string = nn->string;
         }
      }

      switch (this->base) {
         default:
            errorT('s', "%kS in inline%n", this->base, expand_fn);
            return (StP) dummy;

         case BLOCK:
            if (this->s_list) {
               ee = (ExP) expandSt(this->s_list);
               if (this->s) {
                  ee = MakeEx(CM, (ExP) expandSt(this->s), ee);
                  PERM(ee);
               }
               return (StP) ee;
            }

            if (this->s) return expandSt(this->s);

            return (StP) zero;

         case PAIR:
            ee = this->s2 ? (ExP) expandSt(this->s2) : 0;
            ee = MakeEx(CM, this->s ? (ExP) expandSt(this->s) : 0, ee);
            if (this->s_list) ee = MakeEx(CM, ee, (ExP) expandSt(this->s_list));
            PERM(ee);
            return (StP) ee;

         case RETURN:
            ret_seen = 1;
            this->s_list = 0;
            return (StP) expandEx(this->e);

         case SM:
            ee = (this->e == 0 || this->e->base == DUMMY) ? zero : expandEx(this->e);
            if (this->s_list) {
               ee = MakeEx(CM, ee, (ExP) expandSt(this->s_list));
               PERM(ee);
            }
            return (StP) ee;

         case IF:
         {
            ret_seen = 0;
            ExP qq = MakeEx(QUEST, (ExP) expandSt(this->s), 0);
            qq->cond = expandEx(this->e);
            qq->e2 = this->else_stmt ? (ExP) expandSt(this->else_stmt) : zero;
            if (ret_seen && this->s_list) errorT('s', "S after \"return\" inIF");
            ret_seen = 0;
            if (this->s_list) qq = MakeEx(CM, qq, (ExP) expandSt(this->s_list));
            PERM(qq);
            return (StP) qq;
         }
      }
   }

   switch (this->base) {
      default:
         if (this->e) this->e = expandEx(this->e);
         break;
      case PAIR:
         if (this->s2) this->s2 = expandSt(this->s2);
         break;
      case BLOCK:
         break;
      case FOR:
         if (this->for_init) this->for_init = expandSt(this->for_init);
         if (this->e2) this->e2 = expandEx(this->e2);
         break;
      case LABEL:
      case GOTO:
      case RETURN:
      case BREAK:
      case CONTINUE:
         errorT('s', "%kS in inline%n", this->base, expand_fn);
   }

   if (this->s) this->s = expandSt(this->s);
   if (this->s_list) this->s_list = expandSt(this->s_list);
   PERM(this);
   return this;
}

ExP expandEx(ExP this) {
   if (this == 0) errorT('i', "expandEx(0)");
/*fprintf(stderr,"%s(): Ex %d: b=%d e1=%d e2=%d\n",expand_fn->string,this,this->base,this->e1,this->e2); fflush(stderr);*/
   switch (this->base) {
      case NAME:
         if (expand_tbl && ((IdP) this)->n_scope == FCT) {
            IdP n = (IdP) this;
            const char *s = n->string;
            if (s[0] == '_' && s[1] == 'X') break;
            IdP cn = expand_fn->n_table->t_name;
            n->string = temp(s, expand_fn->string, (cn) ? cn->string : 0);
         }
      case DUMMY:
      case ICON:
      case FCON:
      case CCON:
      case IVAL:
      case FVAL:
      case LVAL:
      case STRING:
      case ZERO:
      case SIZEOF:
      case TEXT:
      case ANAME:
         break;
      case ICALL:
         if (expand_tbl && this->e1 == 0) {
            IdP fn = this->il->fct_name;
            FunP f = (FunP) fn->tp;
            if (f->returns == (TypeP)void_type && fn->n_oper != CTOR)
               errorT('s', "non-value-returning inline%n called in value-returning inline%n", fn, expand_fn);
            else
               error("inline%n called before defined", fn);
         }
         break;
      case QUEST:
         this->cond = expandEx(this->cond);
      default:
         if (this->e2) this->e2 = expandEx(this->e2);
      case REF:
      case DOT:
         if (this->e1) this->e1 = expandEx(this->e1);
         break;
      case CAST:
         PERM(this->tp2);
         this->e1 = expandEx(this->e1);
         break;
   }

   PERM(this);
   return this;
}

/*
	is a temporary variable needed to hold the value of this expression
	as an argument for an inline expansion?
	return 1; if side effect
	return 2; if modifies expression
*/
bit not_simple(ExP this) {
   int s;
/*errorT('d',"not_simple%k",this->base);*/
   switch (this->base) {
      default:
         return 2;
      case ZERO:
      case IVAL:
      case FVAL:
      case ICON:
      case CCON:
      case FCON:
      case STRING:
      case NAME: /* unsafe (alias) */
         return 0;
      case SIZEOF:
         return this->e1 == dummy ? 0 : not_simple(this->e1);
      case G_ADDROF:
      case ADDROF:
         return not_simple(this->e2);
      case CAST:
      case DOT:
      case REF:
         return not_simple(this->e1);
      case UMINUS:
      case NOT:
      case COMPL:
         return not_simple(this->e2);
      case DEREF:
         s = not_simple(this->e1);
         if (1 < s) return 2;
         if (this->e2 == 0) return s;
         return s |= not_simple(this->e2);
      case MUL:
      case DIV:
      case MOD:
      case PLUS:
      case MINUS:
      case LS:
      case RS:
      case AND:
      case OR:
      case ER:
      case LT:
      case LE:
      case GT:
      case GE:
      case EQ:
      case NE:
      case ANDAND:
      case OROR:
      case CM:
         s = not_simple(this->e1);
         if (1 < s) return 2;
         return s |= not_simple(this->e2);
      case QUEST:
         s = not_simple(this->cond);
         if (1 < s) return 2;
         s |= not_simple(this->e1);
         if (1 < s) return 2;
         return s |= not_simple(this->e2);
      case ANAME:
         if (curr_icall) {
            IdP n = (IdP) this;
            int argno = n->n_val;
            InLineP il;
            for (il = curr_icall; il; il = il->i_next)
               if (n->n_table == il->i_table) goto aok;
            goto bok;
          aok:
            return (il->local[argno]) ? 0 : not_simple(il->arg[argno]);
         }
       bok:errorT('i', "expand aname%n", this);
      case VALUE:
      case NEW:
      case CALL:
      case G_CALL:
      case ICALL:
      case ASSIGN:
      case INCR:
      case DECR:
      case ASPLUS:
      case ASMINUS:
      case ASMUL:
      case ASDIV:
      case ASMOD:
      case ASAND:
      case ASOR:
      case ASER:
      case ASLS:
      case ASRS:
         return 2;
   }
}

/*
	expand call to (previously defined) inline function in "scope"
	with the argument list "ll"
	(1) declare variables in "scope"
	(2) initialize argument variables
	(3) link to body
*/
ExP expandFun(FunP this, IdP fn, TableP scope, ExP ll) {
//errorT('d',"expand%n inline=%d body %d this->defined %d f_expr %d last_exp=%d curr_expr=%d",fn,this->f_inline,this->body,this->defined ,this->f_expr,this->last_expanded,curr_expr);
   if ((this->body == 0 && this->f_expr == 0) // before defined
      || ((this->defined & SIMPLIFIED) == 0) // before simplified
      || (ToFunP(fn->tp)->body->memtbl == scope) // while defining
      || (this->f_inline == 2) // recursive call
      || (this->last_expanded && this->last_expanded == curr_expr) // twice in an expression
      ) {
      take_addr(fn); /* so don't expand */
      if (fn->n_addr_taken == 1) {
         IdP nn = MakeId(0); /* but declare it */
         *nn = *fn;
         nn->n_list = dcl_list;
         nn->n_sto = STATIC;
         dcl_list = nn;
      }
      return 0;
   }

   this->f_inline = 2;

   InLineP il = MakeInLine();
   ExP ic = (ExP)MakeTEx(ICALL, 0, 0);
   il->fct_name = fn;
   ic->il = il;
   ic->tp = this->returns;
   IdP n;
   IdP at = (this->f_this) ? this->f_this : this->argtype;
   if (at) il->i_table = at->n_table;
   int i = 0;
   int NotSimple = 0; /* is a temporary argument needed? */

   for (n = at; n; n = n->n_list, i++) {
   /*      check formal/actual argument pairs
      and generate temporaries as necessary
    */
      if (ll == 0) errorT('i', "simpl.call:AX for %n", fn);
      ExP ee;

      if (ll->base == ELIST) {
         ee = ll->e1;
         ll = ll->e2;
      } else {
         ee = ll;
         ll = 0;
      }

      int s; /* could be avoided when expanding into a block */

//errorT('d',"n=%n addr %d ass %d used %d ee %k => %d",n,n->n_addr_taken,n->n_assigned_to,n->n_used,ee->base,not_simple(ee));
      if (n->n_assigned_to == FUDGE111) { /* constructor's this */
         if (ee != zero && not_simple(ee) == 0) { /* automatic or static
                                                       then we can use the
                                                       actual variable
                                                     */
            il->local[i] = 0;
            goto zxc;
         }
      }

      if (n->n_addr_taken || n->n_assigned_to) {
         IdP nn = dcl_local(scope, n, fn);
         nn->base = NAME;
         il->local[i] = nn;
         ++NotSimple;
      } else if (n->n_used && (s = not_simple(ee))
         && (1 < s || 1 < n->n_used)) { /* not safe */
         IdP nn = dcl_local(scope, n, fn);
         nn->base = NAME;
         il->local[i] = nn;
         ++NotSimple;
      } else
         il->local[i] = 0;
    zxc:
      il->arg[i] = ee;
      il->tp[i] = n->tp;
   }

   TableP tbl = this->body->memtbl;
   if (this->f_expr) { /* generate comma expression */
      char loc_var = 0;
   /* look for local variables needing declaration: */
//errorT('d',"fn%n tbl %d",fn,tbl);
      for (n = get_mem(tbl, i = 1); n; n = get_mem(tbl, ++i)) {
//errorT('d',"?%s: b=%d u%d =%d &=%d",n->string,n->base,n->n_used,n->n_assigned_to,n->n_addr_taken);
         if (n->base == NAME && (n->n_used || n->n_assigned_to || n->n_addr_taken)) {
            IdP nn = dcl_local(scope, n, fn);
            nn->base = NAME;
            n->string = nn->string;
            loc_var++;
         }
      }
/*errorT('d',"NotSimple=%d loc_var=%d last_expanded=%d curr_expr=%d",NotSimple,loc_var,this->last_expanded,curr_expr);*/
      if (NotSimple || loc_var) this->last_expanded = curr_expr;

      ExP ex;
      if (NotSimple) {
         ExP etail = ex = MakeEx(CM, 0, 0);
         for (i = 0; i < MIA; i++) {
            IdP n = il->local[i];
            if (n == 0) continue;
            ExP e = il->arg[i];
            etail->e1 = MakeEx(ASSIGN, (ExP)n, e);
/*errorT('d',"%n = %k",n,e->base);*/
            if (--NotSimple)
               etail = etail->e2 = MakeEx(CM, 0, 0);
            else
               break;
         }
         etail->e2 = this->f_expr;
      } else {
         ex = this->f_expr;
      }
      ic->e1 = ex;
   } else { /* generate block: */
      for (n = get_mem(tbl, i = 1); n; n = get_mem(tbl, ++i)) {
      // mangle local names
//errorT('d',"?%s: b=%d u%d =%d &=%d",n->string,n->base,n->n_used,n->n_assigned_to,n->n_addr_taken);
         if (n->base == NAME && (n->n_used || n->n_assigned_to || n->n_addr_taken)) {
            IdP cn = fn->n_table->t_name;
            n->string = temp(n->string, fn->string, (cn) ? cn->string : 0);
         }
      }
      StP ss;
      if (NotSimple) {
         this->last_expanded = curr_expr;
         StP st = (StP)MakeESt(SM, curloc, 0, 0);
         StP stail = st;
         for (i = 0; i < MIA; i++) {
            IdP n = il->local[i];
            if (n == 0) continue;
            ExP e = il->arg[i];
            stail->e = MakeEx(ASSIGN, (ExP)n, e);
            if (--NotSimple)
               stail = stail->s_list = (StP)MakeESt(SM, curloc, 0, 0);
            else
               break;
         }
         stail->s_list = (StP)this->body;
         ss = (StP)MakeBlock(curloc, 0, st);
      } else {
         ss = (StP)this->body;
      }
      ic->e2 = (ExP) ss;
   }

   this->f_inline = 1;
   return ic;
}
