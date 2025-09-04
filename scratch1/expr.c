/* @(#) expr.c 1.6 1/27/86 17:48:51 */
/*ident	"@(#)cfront:src/expr.c	1.6" */
/***************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

expr.c:

	type check expressions

************************************************************************/

#include "cfront.h"
#include "size.h"

int const_save;

ExP address(ExP this) {
   if (this->base == DEREF && this->e2 == 0) return this->e1; /* &* */
   if (this->base == CM) {
      this->e2 = address(this->e2);
      return this;
   }
   register ExP ee = MakeEx(G_ADDROF, 0, this);
   ee->tp = (TypeP)MakePtr(PTR, this->tp, 0);
   if (this->base == NAME) take_addr(ToIdP(this));
   return ee;
}

ExP contents(ExP this) {
   if (this->base == ADDROF || this->base == G_ADDROF) return this->e2; /* *& */
   register ExP ee = MakeEx(DEREF, this, 0);
   if (this->tp) ee->tp = ToPtrP(this->tp)->typ; /* this->tp==0 ??? */
   return ee;
}

/*
	find the true name for "n", implicitly define if undefined
	if "n" was called f==1 and "args" were its argument list
	if n was qualified r->n or o.n  f==2
*/
ExP find_name(TableP this, register IdP n, bit f, ExP args) {
   IdP q = n->n_qualifier;
   register IdP qn = 0;
   register IdP nn;
   ClassP cl; /* class specified by q */

   if (n->n_table) {
      nn = n;
      n = 0;
      goto xx;
   }

   if (q) {
      TableP tbl;
//errorT('d',"qq %n %n",q,n);
      if (q == sta_name)
         tbl = gtbl;
      else {
         TypeP t = (TypeP)ToClassP(q->tp);
         if (t == 0) errorT('i', "Qr%n'sT missing", q);

         if (q->base == TNAME) {
            if (t->base != COBJ) {
               error("badT%k forQr%n", t->base, q);
               goto nq;
            }
            t = ToBaseP(t)->b_name->tp;
         }
         if (t->base != CLASS) {
            error("badQr%n(%k)", q, t->base);
            goto nq;
         }
         cl = ToClassP(t);
         tbl = cl->memtbl;
      }

      qn = look(tbl, n->string, 0);
//errorT('d',"qn == %d",qn);
      if (qn == 0) {
         n->n_qualifier = 0;
         nn = 0;
         goto def;
      }

      if (q == sta_name) { /* explicitly global */
         use(qn);
         FreeId(n);
         return (ExP)qn;
      }
   /* else check visibility */
   }

 nq:
   if (cc->tot) {
      {
         for (TableP tbl = this;;) { // loop necessary to get past
         // local re-definitions
            nn = lookc(tbl, n->string, 0);
//errorT('d',"cc->tot:%n nn=%n sto%k sco%k tbl=%d",n,nn,nn->n_stclass,nn->n_scope,tbl);
            if (nn == 0) goto qq; /* try for friend */

            switch (nn->n_scope) {
               case 0:
               case PUBLIC:
                  if (nn->n_stclass == ENUM) break;

                  if (nn->tp->base == OVERLOAD) break;

                  if (Ebase && cc->cot->clbase && Ebase != (ClassP)cc->cot->clbase->tp && !has_friend(Ebase, cc->nof))
                     error("%n is from a privateBC", n);

                  if (Epriv && Epriv != cc->cot && !has_friend(Epriv, cc->nof))
                     error("%n is private", n);
            }

            if (qn == 0 || qn == nn) break;

            if ((tbl = tbl->next) == 0) { /* qn/cl test necessary? */
               if ( /* (qn->n_stclass==STATIC
                       || qn->tp->base==FCT
                       || qn->tp->base==OVERLOAD)
                       &&  */ (qn->n_scope == PUBLIC
                     || has_friend(cl, cc->nof))) {
               /*use(qn);
                  FreeId(n);
                  return qn;
                */
                  nn = qn;
                  break;
               } else {
                  error("QdN%n not in scope", n);
                  goto def;
               }
            }
         }
      }
    xx:
//errorT('d',"xx: nn=%n qn=%n n=%n f=%d",nn,qn,n,f);
      if (nn == 0) goto def;
      use(nn);
      if (f == 2) {
         if (qn && nn->n_stclass == 0)
            switch (nn->n_scope) {
               case 0:
               case PUBLIC: /* suppress virtual */
                  switch (qn->tp->base) {
                     case FCT:
                     case OVERLOAD:
                        *n = *qn;
                        n->n_qualifier = q;
                        return (ExP)n;
                  }
            }
         if (nn->n_table == gtbl) error("MF%n not found", n);
         if (n) FreeId(n);
         return (ExP)nn;
      }

      switch (nn->n_scope) {
         case 0:
         case PUBLIC:
//errorT('d',"st %d th %d",nn->n_stclass,cc->c_this);
            if (nn->n_stclass == 0) {
               if (qn) { /* suppress virtual */
                  switch (qn->tp->base) {
                     case FCT:
                     case OVERLOAD:
                        *n = *qn;
                        n->n_qualifier = q;
                     /*return n; */
                        nn = n;
                        n = 0;
                  }
               }

               if (cc->c_this == 0) {
                  switch (nn->n_oper) {
                     case CTOR:
                     case DTOR:
                        break;
                     default:
                     /* in static member initializer */
                        error("%n cannot be used here", nn);
                        return (ExP)nn;
                  }
               }

               RefP r = MakeRef(REF, (ExP)cc->c_this, nn);
               use(cc->c_this);
               r->tp = nn->tp;
               if (n) FreeId(n);
               return (ExP)r;
            }
         default:
            if (n) FreeId(n);
            return (ExP)nn;
      }
   }
 qq:
//errorT('d',"qq: n%n qn%d",n,qn);
   if (qn) {
   // check for p->base::mem :
   // nasty where derived::mem is public
   // and base::mem is private
   // NOT DONE

   /* static member? */
      if (qn->n_scope == 0 && !has_friend(cl, cc->nof)) {
         error("%n is private", qn);
         if (n) FreeId(n);
         return (ExP)qn;
      }

      switch (qn->n_stclass) {
         case STATIC:
            break;
         default:
            switch (qn->tp->base) {
               case FCT:
               case OVERLOAD: /* suppress virtual */
                  if (f == 1) error("O missing for%n", qn);
                  *n = *qn;
                  n->n_qualifier = q;
                  return (ExP)n;
               default:
                  if (f != 2) error("O missing for%n", qn);
            }
      }

      if (n) FreeId(n);
      return (ExP)qn;
   }

   if (nn = lookc(this, n->string, 0)) {
      switch (nn->n_scope) {
         case 0:
         case PUBLIC:
            if (nn->n_stclass == ENUM) break;

            if (nn->tp->base == OVERLOAD) break;
            if (Ebase && !has_friend(Ebase, cc->nof))
               error("%n is from privateBC", n);

            if (Epriv && !has_friend(Epriv, cc->nof))
               error("%n is private", n);
      }
   }

   if (nn) {
//errorT('d',"found %n",nn);
      if (f == 2 && nn->n_table == gtbl) error("MF%n not found", n);
      use(nn);
      if (n) FreeId(n);
      return (ExP)nn;
   }

 def: /* implicit declaration */
//errorT('d',"implicit f %d",f);
   n->n_qualifier = 0;
   if (f == 1) { /* function */
      if (n->tp) errorT('i', "find_name(fct_type?)");
      n->tp = (TypeP)MakeFun((TypeP)defa_type, 0, 0);
      n->n_sto = EXTERN;
   /*      if (fct_void) {
      n->tp = (TypeP)MakeFun(defa_type,0,0);
      }
      else {
      ExP e;
      IdP at = 0;
      IdP att;

      for (e=args; e; e=e->e2) {
      IdP ar = MakeId;
      if (e->base != ELIST) errorT('i',"badA %k",e->base);
      e->e1 = typ(e->e1, this);
      ar->tp = e->e1->base==STRING ? Pchar_type : e->e1->tp;
      switch (ar->tp->base) {
      case ZTYPE:
      ar->tp = defa_type;
      break;
      case FIELD:
      ar->tp = int_type;
      break;
      case ANY:
      default:
      PERM(ar->tp);
      }
      if (at)
      att->n_list = ar;
      else
      at = ar;
      att = ar;
      }
      n->tp = (TypeP)MakeFun(defa_type,at,1);

      }
    */
   } else {
      n->tp = (TypeP)any_type;
      if (this != any_tbl)
         if (cc->Not && (cc->cot->defined & (DEFINED | SIMPLIFIED)) == 0)
            error("C%n isU", cc->Not);
         else
            error("%n isU", n);
   }

   nn = dclId(n, gtbl, EXTERN);
   nn->n_list = 0;
   use(nn);
   use(nn); /* twice to cope with "undef = 1;" */
   if (n) FreeId(n);

   if (f == 1)
      if (fct_void) {
         if (no_of_undcl++ == 0) undcl = nn;
      } else
         errorT('w', "undeclaredF%n called", nn);

   return (ExP)nn;
}

/*
	find the type of "this" and place it in this->tp;
	return the typechecked version of the expression:
	"tbl" provides the scope for the names in "this"
*/
ExP typ(ExP this, TableP tbl) {
   if (this == 0) errorT('i', "typ(0)");
   IdP n;
   TypeP t = 0;
   TypeP t1, t2;
   Token b = this->base;
   Token r1, r2;

#define nppromote(b)	t=np_promote(b,r1,r2,t1,t2,1)
#define npcheck(b)	(void)np_promote(b,r1,r2,t1,t2,0)
   if (tbl->base != TABLE) errorT('i', "typ(%d)", tbl->base);
//if (b == NAME) errorT('d',"name %d %d %s",this,this->string,this->string?this->string:"?");
   if (this->tp) {
/*errorT('d',"typ %d (checked) tbl=%d",this,tbl);*/
      if (b == NAME) use(ToIdP(this));
      return this;
   }
//errorT('d',"typ %d%k e1 %d%k e2 %d%k tbl %d\n",this,this->base,this->e1,this->e1?this->e1->base:0,this->e2,this->e2?this->e2->base:0,tbl);
   switch (b) { /* is it a basic type */
      case DUMMY:
         error("emptyE");
         this->tp = (TypeP)any_type;
         return this;
      case ZERO:
         this->tp = (TypeP)zero_type;
         return this;
      case IVAL:
         this->tp = (TypeP)int_type;
         return this;
      case FVAL:
         this->tp = (TypeP)float_type;
         return this;
      case ICON:
      /*      is it long?
         explicit long?
         decimal larger than largest signed int
         octal or hexadecimal larger than largest unsigned int
       */
      {
         int ll = strlen(this->string);
         switch (this->string[ll - 1]) {
            case 'l':
            case 'L':
             lng:
               this->tp = (TypeP)long_type;
               goto save;
         }

         if (this->string[0] == '0') { /* assume 8 bits in byte */
            switch (this->string[1]) {
               case 'x':
               case 'X':
                  if (SZ_INT + SZ_INT < ll - 2) goto lng;
                  goto nrm;
               default:
                  if (BI_IN_BYTE * SZ_INT < (ll - 1) * 3) goto lng;
                  goto nrm;
            }
         } else {
            if (ll < /*sizeof(LARGEST_INT)-1 */ 10) {
             nrm:
               this->tp = (TypeP)int_type;
               goto save;
            }
            if (ll > 10) goto lng;
            const char *p = this->string;
            const char *q = LARGEST_INT;
            do
               if (*p++ > *q++) goto lng;
            while (*p);
         }

         goto nrm;
      }
      case CCON:
         this->tp = (TypeP)char_type;
         goto save;
      case FCON:
         this->tp = (TypeP)double_type;
         goto save;
      case STRING: // type of "as\tdf" is char[6]
      // c_strlen counts the terminating '\0'
      {
         int ll = c_strlen(this->string);
         VecP v = MakeVec((TypeP)char_type, 0);
         v->size = ll;
         this->tp = (TypeP)v;
         goto save;
      }
       save:
/*errorT('d',"%s const_save %d",this->string,const_save);*/
         if (const_save) {
            int ll = c_strlen(this->string);
            char *p = _new(ll*sizeof *p);
            strcpy(p, this->string);
            this->string = p;
         }
         return this;

      case THIS:
         FreeEx(this);
         if (cc->tot) {
            use(cc->c_this);
            return (ExP)cc->c_this;
         }
         error("this used in nonC context");
         n = MakeId("this");
         n->tp = (TypeP)any_type;
         return (ExP)insert(tbl, n, 0);

      case NAME:
/*errorT('d',"name %s",this->string);*/
      {
         ExP ee = find_name(tbl, (IdP) this, 0, 0);
         if (ee->tp->base == RPTR) return contents(ee);
         return ee;
      }
      case SIZEOF:
         t = this->tp2;
         if (t) {
            dclType(t, tbl);
            if (this->e1 && this->e1 != dummy) {
               this->e1 = typ(this->e1, tbl);
               DEL(Ex, this->e1);
               this->e1 = dummy;
            }
         } else {
            this->e1 = typ(this->e1, tbl);
            this->tp2 = this->e1->tp;
         }
         this->tp = (TypeP)int_type;
         return this;

      case CAST:
      {
         TypeP tt = t = this->tp2;
         dclType(tt, tbl);
       zaq: /* is the cast legal? */
//errorT('d',"tt %d %d",tt,tt?tt->base:0);
         switch (tt->base) {
            case TYPE:
               tt = ToBaseP(tt)->b_name->tp;
               goto zaq;
            case RPTR: // necessary?
            case PTR:
               if (ToPtrP(tt)->rdo) error("*const in cast");
               tt = ToPtrP(tt)->typ;
               goto zaq;
            case VEC:
               tt = ToVecP(tt)->typ;
               goto zaq;
            case FCT:
               tt = ToFunP(tt)->returns;
               goto zaq;
            default:
               if (ToBaseP(tt)->b_const) error("const in cast");
         }

      /* now check cast against value, INCOMPLETE */

//errorT('d',"cast e1 %d %k",this->e1,this->e1->base);
         tt = t;

         if (this->e1 == dummy) {
            error("expression missing for cast");
            this->tp = (TypeP)any_type;
            return this;
         }
         this->e1 = typ(this->e1, tbl);
         TypeP etp = this->e1->tp;
         while (etp->base == TYPE) etp = ToBaseP(etp)->b_name->tp;

         if (etp->base == COBJ) {
            int i = can_coerce(tt, etp);
//errorT('d',"cast%t->%t -- %d%n",tt,etp,i,Ncoerce);
            if (i == 1 && Ncoerce) {
               IdP cn = ToBaseP(etp)->b_name;
               ClassP cl = ToClassP(cn->tp);
               RefP r = MakeRef(DOT, this->e1, Ncoerce);
               ExP rr = typ((ExP)r, tbl);
               ExP c = MakeEx(G_CALL, rr, 0);
               c->fct_name = Ncoerce;
               c->tp = tt;
               *this = *ToExP(c);
               FreeEx(c);
               return this;
            }
         }

         switch (etp->base) {
            case VOID:
               if (tt->base == VOID) {
                  this->tp = t;
                  return this;
               }
               error("cast of void value");
            case ANY:
               this->tp = (TypeP)any_type;
               return this;
         }

       legloop:
         switch (tt->base) {
            case TYPE:
               tt = ToBaseP(tt)->b_name->tp;
               goto legloop;
            case VOID:
               switch (etp->base) {
                  case COBJ:
                     switch (this->e1->base) {
                        case VALUE:
                        case CALL:
                        case G_CALL:
                        {
                           IdP cln = is_cl_obj(etp);
                           ClassP cl = (ClassP) cln->tp;
                           if (has_dtor(cl)) errorT('s', "cannot castCO to void");
                        }
                     }
                     break;
               }
               break;
            case PTR:
               switch (etp->base) {
                  case COBJ:
                     error("cannot castCO toP");
                     break;
               }
               break;

            case RPTR: // (x&)e: pretend e is an x
               if ((this->e1->base == CALL || this->e1->base == G_CALL || lval(this->e1, 0))
                  && tsizeof(ToPtrP(tt)->typ) <= tsizeof(etp)) {
//errorT('d',"%t(%t)",t,etp);
                  this->e1 = address(this->e1); // *(x*)&e
                  this->tp = t;
                  return contents(this);
               } else
                  error("cannot cast%t to%t", etp, t);
               break;

            case COBJ:
//errorT('d',"%n ctor %d",cn,ctor);
               this->base = VALUE; // (x)e => x(e): construct an x from e
               this->e1 = MakeEx(ELIST, this->e1, 0);
               return typ(this, tbl);

            case CHAR:
            case INT:
            case SHORT:
            case LONG:
            case FLOAT:
            case DOUBLE:
               switch (etp->base) {
                  case COBJ:
                     error("cannot castCO to%k", tt->base);
                     break;
               }
               break;

         }
         this->tp = t;
         return this;
      }

      case VALUE:
      {
         TypeP tt = this->tp2;
         ClassP cl;
         IdP cn;
//errorT('d',"value %d %d (%d %k)",tt,tt?tt->base:0,this->e1,this->e1?this->e1->base:0);

         dclType(tt, tbl);
       vv:
//errorT('d',"vv %d %d",tt,tt?tt->base:0);
         switch (tt->base) {
            case TYPE:
               tt = ToBaseP(tt)->b_name->tp;
               goto vv;
            case EOBJ:
            default:
               if (this->e1 == 0) {
                  error("value missing in conversion to%t", tt);
                  this->tp = (TypeP)any_type;
                  return this;
               }
               this->base = CAST;
               this->e1 = this->e1->e1; // strip ELIST
               return typ(this, tbl);
            case CLASS:
               cl = (ClassP) tt;
               goto nn;
            case COBJ:
               cn = ToBaseP(tt)->b_name;
               cl = ToClassP(cn->tp);
             nn:
               if (this->e1 && this->e1->e2 == 0) { /* single argument */
                  this->e1->e1 = typ(this->e1->e1, tbl);
                  IdP acn = is_cl_obj(this->e1->e1->tp);
//errorT('d',"acn%n itor%d",acn,acn?cl->itor:0);
                  if (acn && acn->tp == (TypeP)cl && has_itor(cl) == 0) {
                     if (this->e2) { // x(x_obj) => this->e2=x_obj
                        this->base = ASSIGN;
                        ExP ee = this->e1->e1;
                        this->e1 = this->e2;
                        this->e2 = ee;
                        this->tp = this->tp2;
                        return this;
                     } else // x(x_obj) => x_obj
                        return this->e1->e1;
                  }
               }

               { /* x(a) => obj.ctor(a); where this->e1==obj */
                  ExP ee;
                  ExP a = this->e1;
                  IdP ctor = has_ctor(cl);
                  if (ctor == 0) {
                     error("cannot make a%n", cn);
                     this->base = SM;
                     this->e1 = dummy;
                     this->e2 = 0;
                     return this;
                  }
//errorT('d',"value %n.%n",this->e2,ctor);
                  int tv = 0;
                  if (this->e2 == 0) { /*  x(a) => x temp; (temp.x(a),temp) */
                     TableP otbl = tbl;
                     if (Cstmt) { /* make Cstmt into a block */
                        if (Cstmt->memtbl == 0) Cstmt->memtbl = MakeTable(4, tbl, 0);
                        tbl = Cstmt->memtbl;
                     }
                     char *s = make_name('V');
//errorT('d',"%s: %d %d",s,otbl,tbl);
                     IdP n = MakeId(s);
                     n->tp = this->tp2;
                     n = dclId(n, tbl, ARG); /* no init! */
                     n->n_scope = FCT;
                     assign(n);
                     this->e2 = (ExP)n;
                     ee = MakeEx(CM, this, (ExP)n);
                     tbl = otbl;
                     tv = 1;
                  } else
                     ee = this;

                  this->base = G_CALL;
                  this->e1 = (ExP)MakeRef(DOT, this->e2, ctor);
                  this->e2 = a;
                  ee = typ(ee, tbl);
//errorT('d',"ee %t",ee->tp);
                  if (tv == 0) { // deref value returned by constructor
                     ee = MakeEx(DEREF, ee, 0);
                     ee->tp = ee->e1->tp;
                  }
                  return ee;
               }
         }
      }

      case NEW:
      {
         TypeP tt = this->tp2;
         TypeP tx = tt;
         bit v = 0;
         bit old = new_type;
         new_type = 1;
/*errorT('d',"new%t this->e1 %d %d",tt,this->e1,this->e1?this->e1->base:0);*/
         dclType(tt, tbl);
         new_type = old;
         if (this->e1) this->e1 = typ(this->e1, tbl);
       ll:
//errorT('d',"tt %d %d",tt,tt?tt->base:0);
         switch (tt->base) {
            default:
               if (this->e1) {
                  error("Ir for nonCO created using \"new\"");
                  this->e1 = 0;
               }
               break;
            case VEC:
               v = 1;
               tt = ToVecP(tt)->typ;
               goto ll;
            case TYPE:
               tt = ToBaseP(tt)->b_name->tp;
               goto ll;
            case COBJ:
            {
               IdP cn = ToBaseP(tt)->b_name;
               ClassP cl = (ClassP) cn->tp;

               if ((cl->defined & (DEFINED | SIMPLIFIED)) == 0) {
                  error("new%n;%n isU", cn, cn);
               } else {
                  IdP ctor = has_ctor(cl);
                  Token su;
                  if (ctor) {
/*errorT('d',"cobj%n this->tp%t",ctor,ctor->tp);*/
                     if (v) {
                        IdP ic;
                        if (this->e1)
                           errorT('s', "Ir forvector ofCO created using \"new\"");
                        else if ((ic = has_ictor(cl)) == 0)
                           error("vector ofC%n that do not have a constructor taking noAs", cn);
                        else if (ToFunP(ic->tp)->nargs)
                           errorT('s', "defaultAs for constructor for vector ofC%n", cn);
                     }
                     this->e1 = (ExP)MakeCall((ExP)ctor, this->e1);
                     this->e1 = typ(this->e1, tbl);
                  /*(void) fct_call(this->e1, tbl); */
                  } else if (su = is_simple(cl)) {
/*errorT('d',"simple cobj%k",su);*/
                     if (this->e1) error("new%n withIr", cn);
                  } else {
/*errorT('d',"not simple and no constructor?");*/
                  }
               }
            }
         }
//errorT('d',"v==%d",v);
         this->tp = (v) ? (TypeP) tx : (TypeP) MakePtr(PTR, tx, 0);
         return this;
      }

      case DELETE: // delete this->e1 OR delete[this->e2] this->e1
      {
         int i;
         if (this->e1->base == ADDROF) errorT('w', "delete &E");
         this->e1 = typ(this->e1, tbl);
         i = num_ptr(this->e1->tp, DELETE);
         if (i != P) error("nonP deleted");
         if (this->e2) {
            this->e2 = typ(this->e2, tbl);
            integral(this->e2->tp, DELETE);
         }
         this->tp = (TypeP)void_type;
         return this;
      }
   }

   if (this->e1 == 0 && this->e2 == 0) errorT('i', "no operands for%k", b);

   switch (b) {
      case ILIST: /* an ILIST is pointer to an ELIST */
         this->e1 = typ(this->e1, tbl);
         this->tp = (TypeP)any_type;
         return this;

      case ELIST:
      {
         ExP e;
         ExP ex;

         if (this->e1 == dummy && this->e2 == 0) {
            error("emptyIrL");
            this->tp = (TypeP)any_type;
            return this;
         }

         for (e = this; e; e = ex) {
            ExP ee = e->e1;
/*errorT('d',"e %d %d ee %d %d",e,e?e->base:0,ee,ee?ee->base:0);*/
            if (e->base != ELIST) errorT('i', "Exs%k", e->base);
            if (ex = e->e2) { /* look ahead for end of list */
               if (ee == dummy) error("EX in EL");
               if (ex->e1 == dummy && ex->e2 == 0) {
               /* { ... , } */
                  DEL(Ex, ex);
                  e->e2 = ex = 0;
               }
            }
            e->e1 = typ(ee, tbl);
            t = e->e1->tp;

         }
         this->tp = t;
         return this;
      }

      case DOT:
      case REF:
      {
         BaseP b;
         TableP atbl;
         IdP nn;
         const char *s;
         ClassP cl;

         this->e1 = typ(this->e1, tbl);
         t = this->e1->tp;

         if (this->base == REF) {
          xxx:
//errorT('d',"xxx %t",t);
            switch (t->base) {
               case TYPE:
                  t = ToBaseP(t)->b_name->tp;
                  goto xxx;
               default:
                  error("nonP ->%n", this->mem);
               case ANY:
                  atbl = any_tbl;
                  goto mm;
               case PTR:
               case VEC:
                  b = ToBaseP(ToPtrP(t)->typ);
                  break;
            }
         } else {
          qqq:
            switch (t->base) {
               case TYPE:
                  t = ToBaseP(t)->b_name->tp;
                  goto qqq;
               default:
                  error("nonO .%n", this->mem);
               case ANY:
                  atbl = any_tbl;
                  goto mm;
               case COBJ:
                  break;
            }
//errorT('d',"dot this->e1 %k %d",this->e1->base,this->e1->base);
            switch (this->e1->base) { /* FUDGE, but cannot use lval (consts) */
               case CM:
               /* ( ... , x). => ( ... , &x)-> */
               {
                  ExP ex = this->e1;
                cfr:switch (ex->e2->base) {
                     case NAME:
                        this->base = REF;
                        ex->e2 = address(ex->e2);
                        goto xde;
                     case CM:
                        ex = ex->e2;
                        goto cfr;
                  }
               }
               case CALL:
               case G_CALL:
//errorT('d',"call%d, %n %d %d",this->e1->fct_name,this->e1->fct_name,ToFunP(this->e1->fct_name->tp)->f_inline,ToFunP(this->e1->fct_name->tp)->f_virtual);
#ifdef BSD
                  if (this->e1->fct_name && ToFunP(this->e1->fct_name->tp)->f_inline && ToFunP(this->e1->fct_name->tp)->f_virtual == 0) break;
#else
               //      if (this->e1->fct_name==0
               //      || ToFunP(this->e1->fct_name->tp)->f_inline==0)
#endif
               {
               /* f(). => (tmp=f(),&tmp)-> */
                  TableP otbl = tbl;
                  if (Cstmt) { /* make Cstmt into a block */
                     if (Cstmt->memtbl == 0) Cstmt->memtbl = MakeTable(4, tbl, 0);
                     tbl = Cstmt->memtbl;
                  }
                  char *s = make_name('T');
                  IdP tmp = MakeId(s);
                  tmp->tp = this->e1->tp;
                  tmp = dclId(tmp, tbl, ARG); /* no init! */
                  tmp->n_scope = FCT;
                  this->e1 = MakeEx(ASSIGN, (ExP)tmp, this->e1);
                  this->e1->tp = tmp->tp;
                  ExP aa = address((ExP)tmp);
                  this->e1 = MakeEx(CM, this->e1, aa);
                  this->e1->tp = aa->tp;
                  this->base = REF;
                  tbl = otbl;
               }
                  break;
               //      case QUEST:
               //              error("non-lvalue .%n",this->mem);
               //              break;
               //      case NAME:
               //              take_addr(ToIdP(this->e1));
               //              use(ToIdP(this->e1));
            }
          xde:
            b = ToBaseP(t);
         }

       xxxx:
         switch (b->base) {
            case TYPE:
               b = (BaseP) b->b_name->tp;
               goto xxxx;
            default:
               error("(%t) before %k%n (%n not aM)", this->e1->tp, this->base, this->mem, this->mem);
            case ANY:
               atbl = any_tbl;
               goto mm;

            case COBJ:
               if (atbl = b->b_table) goto mm;
               s = b->b_name->string; /* lookup the class name */
               if (s == 0) errorT('i', "%kN missing", CLASS);
//errorT('d',"lookup %s",s);
               nn = look(tbl, s, CLASS);
               if (nn == 0) errorT('i', "%k %sU", CLASS, s);
               if (nn != b->b_name) b->b_name = nn;
               cl = (ClassP) nn->tp;
               PERM(cl);
               if (cl == 0) errorT('i', "%k %s'sT missing", CLASS, s);
               b->b_table = atbl = cl->memtbl;
             mm:
               if (atbl->base != TABLE) errorT('i', "atbl(%d)", atbl->base);
               nn = (IdP) find_name(atbl, this->mem, 2, 0);
//errorT('d',"nn%n %d %d",nn,nn->n_stclass,nn->n_scope);
               switch (nn->n_stclass) {
                  case 0:
                     this->mem = nn;
                     this->tp = nn->tp;
                     return this;
                  case STATIC:
                     return (ExP)nn;
               }
         }
      }

      case CALL: /* handle undefined function names */
         if (this->e1->base == NAME && this->e1->tp == 0) {
//errorT('d',"call %d %s",this->e1,this->e1->string);
            this->e1 = find_name(tbl, ToIdP(this->e1), 1, this->e2);
         }
         break;
      case QUEST:
         this->cond = typ(this->cond, tbl);
   }

   if (this->e1) {
      this->e1 = typ(this->e1, tbl);
      if (this->e1->tp->base == RPTR) this->e1 = contents(this->e1);
      t1 = this->e1->tp;
   } else
      t1 = 0;

   if (this->e2) {
      this->e2 = typ(this->e2, tbl);
      if (this->e2->tp->base == RPTR) this->e2 = contents(this->e2);
      t2 = this->e2->tp;
   } else
      t2 = 0;

   Token bb;
   switch (b) { /* filter non-overloadable operators out */
      default:
         bb = b;
         break;
      case DEREF:
         bb = (this->e2) ? DEREF : MUL;
         break;
      case CM:
      case QUEST:
      case G_ADDROF:
      case G_CALL:
         goto not_overloaded;
   }

   IdP n1;
   if (this->e1) {
      TypeP tx = t1;
      while (tx->base == TYPE) tx = ToBaseP(tx)->b_name->tp;
      n1 = is_cl_obj(tx);
   } else
      n1 = 0;

   IdP n2;
   if (this->e2) {
      TypeP tx = t2;
      while (tx->base == TYPE) tx = ToBaseP(tx)->b_name->tp;
      n2 = is_cl_obj(tx);
   } else
      n2 = 0;
//errorT('d',"overload %k: %s %s\n", bb, n1?n1->string:"1", n2?n2->string:"2");
   if (n1 == 0 && n2 == 0) goto not_overloaded;
   {
   /* first try for non-member function:   op(this->e1,this->e2) or op(this->e2) or op(this->e1) */
      ExP oe2 = this->e2;
      ExP ee2 = (this->e2 && this->e2->base != ELIST) ? this->e2 = MakeEx(ELIST, this->e2, 0) : 0;
      ExP ee1 = (this->e1) ? MakeEx(ELIST, this->e1, this->e2) : ee2;
      const char *obb = oper_name(bb);
      IdP gname = look(gtbl, obb, 0);
      int go = gname ? over_call(gname, ee1) : 0;
      int nc = Nover_coerce; // first look at member functions                                               // then if necessary check for ambiguities
      if (go) gname = Nover;
//errorT('d',"global%n go=%d nc=%d",gname,go,nc);

      if (n1) { /* look for member of n1 */
         TableP ctbl = ToClassP(n1->tp)->memtbl;
         IdP mname = look(ctbl, obb, 0);
         if (mname == 0) goto glob;
         switch (mname->n_scope) {
            default:
               goto glob;
            case 0:
            case PUBLIC:
               break; /* try this->e1.op(?) */
         }

         int mo = over_call(mname, this->e2);
//errorT('d',"n1%n %d",mname,mo);
         switch (mo) {
            case 0:
               if (go == 2) goto glob;
               if (1 < Nover_coerce) goto am1;
               goto glob;
            case 1:
               if (go == 2) goto glob;
               if (go == 1) {
                am1:
                  error("ambiguous operandTs%n%t for%k", n1, t2, b);
                  this->tp = (TypeP)any_type;
                  return this;
               } else {
                  ClassP cl = (ClassP) n1->tp;
                  if (cl->conv) errorT('w', "overloaded%k may be ambiguous", bb);
               }
               break;
            case 2:
               if (go == 2) error("%k defined both as%n and%n", bb, gname, Nover);
         }
//errorT('d',"%k mtbl %d ctbl %d",bb,mname->n_table,ctbl);
         if (bb == ASSIGN && mname->n_table != ctbl) { /* inherited = */
         //if (tsizeof(n1)!=mname->?)
            error("assignment not defined for class%n", n1);
            this->tp = (TypeP)any_type;
            return this;
         }
//errorT('d',"Nover %n %k",Nover,Nover->tp->base);
         this->base = G_CALL; /* this->e1.op(this->e2) or this->e1.op() */
         this->e1 = (ExP)MakeRef(DOT, this->e1, Nover);
         if (ee1) FreeEx(ee1);
         return typ(this, tbl);
      }

      if (n2 && this->e1 == 0) { /* look for unary operator */
         TableP ctbl = ToClassP(n2->tp)->memtbl;
         IdP mname = look(ctbl, obb, 0);
         if (mname == 0) goto glob;
         switch (mname->n_scope) {
            default:
               goto glob;
            case 0:
            case PUBLIC:
               break; /* try this->e2.op() */
         }

         int mo = over_call(mname, 0);
/*errorT('d',"n2%n %d",mname,mo);*/
         switch (mo) {
            case 0:
               if (1 < Nover_coerce) goto am2;
               goto glob;
            case 1:
               if (go == 2) goto glob;
               if (go == 1) {
                am2:
                  error("ambiguous operandT%n for%k", n2, b);
                  this->tp = (TypeP)any_type;
                  return this;
               }
               break;
            case 2:
               if (go == 2) error("%k defined both as%n and%n", bb, gname, Nover);
         }

         this->base = G_CALL; /* this->e2.op() */
         this->e1 = (ExP)MakeRef(DOT, oe2, Nover);
         this->e2 = 0;
         if (ee2) FreeEx(ee2);
         if (ee1 && ee1 != ee2) FreeEx(ee1);
         return typ(this, tbl);

      }

    glob:
//errorT('d',"glob %d",nc);
      if (1 < nc) {
         error("ambiguous operandTs%t%t for%k", t1, t2, b);
         this->tp = (TypeP)any_type;
         return this;
      }
      if (go) {
         if (go == 1) { /* conversion necessary => binary */
            if (n1) {
               ClassP cl = (ClassP) n1->tp;
               if (cl->conv) errorT('w', "overloaded%k may be ambiguous", bb);
//errorT('d',"go n1");
            } else if (n2) {
               ClassP cl = (ClassP) n2->tp;
               if (cl->conv) errorT('w', "overloaded%k may be ambiguous", bb);
//errorT('d',"bb%k go n2 %n",bb,gname);
            }
         }
         this->base = G_CALL; /* op(this->e1,this->e2) or op(this->e1) or op(this->e2) */
         this->e1 = (ExP)gname;
         this->e2 = ee1;
         return typ(this, tbl);
      }

      if (ee2) FreeEx(ee2);
      if (ee1 && ee1 != ee2) FreeEx(ee1);
      this->e2 = oe2;
//errorT('d',"bb%k",bb);
      switch (bb) {
         case ASSIGN:
         case ADDROF:
            break;
         case CALL:
         case DEREF:
            if (n1 == 0) break;
         default: /* look for conversions to basic types */
         {
            int found = 0;
            if (n1) {
               int val = 0;
               ClassP cl = (ClassP) n1->tp;
               for (IdP on = cl->conv; on; on = on->n_list) {
//errorT('d',"oper_coerce n1%n %t",on,(on)?ToFunP(on->tp)->returns:0);
                  FunP f = (FunP) on->tp;
                  if (bb == ANDAND || bb == OROR) {
                     this->e1 = check_cond(this->e1, bb, tbl);
                     goto not_overloaded;
                  }
                  if (n2 || (t2 && checkType(f->returns, t2, ASSIGN) == 0)
                     || (t2 && checkType(t2, f->returns, ASSIGN) == 0)) {
                     Ncoerce = on;
                     val++;
                  }
               }
               switch (val) {
                  case 0:
                     break;
                  case 1:
                  {
                     RefP r = MakeRef(DOT, this->e1, Ncoerce);
                     ExP rr = typ(this->e1, tbl);
                     this->e1 = MakeEx(G_CALL, rr, 0);
                     found = 1;
                     break;
                  }
                  default:
                     errorT('s', "ambiguous coercion of%n to basicT", n1);
               }
            }
            if (n2) {
               int val = 0;
               ClassP cl = (ClassP) n2->tp;
               for (IdP on = cl->conv; on; on = on->n_list) {
//errorT('d',"oper_coerce n2%n %t",on,(on)?on->tp:0);
                  FunP f = (FunP) on->tp;
                  if (bb == ANDAND || bb == OROR || bb == NOT) {
                     this->e2 = check_cond(this->e2, bb, tbl);
                     goto not_overloaded;
                  }
                  if (n1 || (t1 && checkType(f->returns, t1, ASSIGN) == 0)
                     || (t1 && checkType(t1, f->returns, ASSIGN) == 0)) {
                     Ncoerce = on;
                     val++;
                  }
               }
               switch (val) {
                  case 0:
                     break;
                  case 1:
                  {
                     RefP r = MakeRef(DOT, this->e2, Ncoerce);
                     ExP rr = typ((ExP)r, tbl);
                     this->e2 = MakeEx(G_CALL, rr, 0);
                     found++;
                     break;
                  }
                  default:
                     errorT('s', "ambiguous coercion of%n to basicT", n2);
               }
            }
            if (found) {
            /*      if (found == 2) errorT('w',"coercions of operands of%k may be ambiguous",b); */
               return typ(this, tbl);
            }
            if (t1 && t2)
               error("bad operandTs%t%t for%k", t1, t2, b);
            else
               error("bad operandT%t for%k", t1 ? t1 : t2, b);
            this->tp = (TypeP)any_type;
            return this;
         }
      }
   }
 not_overloaded:
   t = (t1 == 0) ? t2 : (t2 == 0) ? t1 : 0;
/*fprintf(stderr,"%s: e1 %d %d e2 %d %d\n",oper_name(b),this->e1,this->e1?this->e1->base:0,this->e2,this->e2?this->e2->base:0);*/
   switch (b) { /* are the operands of legal types */
      case G_CALL:
      case CALL:
         this->tp = fct_call(this, tbl); /* two calls of use() for this->e1's names */
         if (this->tp->base == RPTR) return contents(this);
         return this;

      case DEREF:
//errorT('d',"deref %t",t?t:t1);
         if (this->e1 == dummy) error("O missing before []\n");
         if (t) { /*      *t      */
            vec_type(t);
            this->tp = deref(t);
         } else { // this->e1[this->e2] that is *(this->e1+this->e2)
            if (vec_type(t1)) { // this->e1[this->e2]
               integral(t2, b);
               this->tp = deref(t1);
            } else if (vec_type(t2)) { // really this->e2[this->e1]
               integral(t1, b);
               this->tp = deref(t2);
            } else {
               error("[] applied to nonPT:%t[%t]", t1, t2);
               this->tp = (TypeP)any_type;
            }
         }
         if (this->tp->base == RPTR) return contents(this);
         return this;

      case G_ADDROF:
      case ADDROF:
         if (lval(this->e2, b) == 0) {
            this->tp = (TypeP)any_type;
            return this;
         }
         this->tp = (TypeP)addrof(t);
      /* look for &p->member_function */
         switch (this->e2->base) {
            case DOT:
            case REF:
            {
               IdP m = this->e2->mem;
               FunP f = (FunP) m->tp;
               if (f->base == FCT && (f->f_virtual == 0 || m->n_qualifier)) {
                  DEL(Ex, this->e2);
                  this->e2 = (ExP)m;
               }
            }
         }
         return this;

      case UMINUS:
         numeric(t, b);
         this->tp = t;
         return this;
      case UPLUS:
         num_ptr(t, b);
         errorT('s', "unary + (ignored)");
         this->tp = t;
         this->base = PLUS;
         this->e1 = zero;
         return this;
      case NOT:
         this->e2 = check_cond(this->e2, NOT, tbl);
         this->tp = (TypeP)int_type;
         return this;
      case COMPL:
         integral(t, b);
         this->tp = t;
         return this;

      case INCR:
      case DECR:
         if (this->e1) lval(this->e1, b);
         if (this->e2) lval(this->e2, b);
         r1 = num_ptr(t, b);
         this->tp = t;
         return this;

   }

   if (this->e1 == dummy || this->e2 == dummy || this->e1 == 0 || this->e2 == 0) error("operand missing for%k", b);
   switch (b) {
      case MUL:
      case DIV:
         r1 = numeric(t1, b);
         r2 = numeric(t2, b);
         nppromote(b);
         break;
      case MOD:
         r1 = integral(t1, b);
         r2 = integral(t2, b);
         nppromote(b);
         break;
      case PLUS:
         r2 = num_ptr(t2, b);
         r1 = num_ptr(t1, b);
         if (r1 == P && r2 == P) error("P +P");
         nppromote(b);
         this->tp = t;
         break;
      case MINUS:
         r2 = num_ptr(t2, b);
         r1 = num_ptr(t1, b);
         if (r2 == P && r1 != P && r1 != A) error("P - nonP");
         nppromote(b);
         this->tp = t;
         break;
      case LS:
      case RS:
      case AND:
      case OR:
      case ER:
         switch (this->e1->base) {
            case LT:
            case LE:
            case GT:
            case GE:
            case EQ:
            case NE:
               errorT('w', "%kE as operand for%k", this->e1->base, b);
         }
         switch (this->e2->base) {
            case LT:
            case LE:
            case GT:
            case GE:
            case EQ:
            case NE:
               errorT('w', "%kE as operand for%k", this->e2->base, b);
         }
         r1 = integral(t1, b);
         r2 = integral(t2, b);
         nppromote(b);
         break;
      case LT:
      case LE:
      case GT:
      case GE:
      case EQ:
      case NE:
         r1 = num_ptr(t1, b);
         r2 = num_ptr(t2, b);
         npcheck(b);
         t = (TypeP)int_type;
         break;
      case ANDAND:
      case OROR:
         this->e1 = check_cond(this->e1, b, tbl);
         this->e2 = check_cond(this->e2, b, tbl);
//              if (st2 != st3) errorT('s',"needs temporaryV to evaluateE after \"%k\" (please use ifS)",b);
         t = (TypeP)int_type;
         break;
      case QUEST:
      {
         IdP c1, c2;
         this->cond = check_cond(this->cond, b, tbl);
//              if (st1 != st2) errorT('s',"needs temporaryV to evaluateE after \"?\" (please use ifS)");
//              if (st2 != st3) errorT('s',"needs temporaryV to evaluateE after \":\" (please use ifS)");
      // still doesn't do complete checking for possible conversions...
         if (t1 == t2 || ((c1 = is_cl_obj(t1))
               && (c2 = is_cl_obj(t2))
               && (c1->tp == c2->tp)
            ))
            t = t1;
         else {
            r1 = num_ptr(t1, b);
            r2 = num_ptr(t2, b);
//errorT('d',"r1 %d r2 %d",r1,r2);
            if (r1 == FCT && r2 == FCT) { // fudge
               if (checkType(t1, t2, ASSIGN)) error("badTs in ?:E: %t and %t", t1, t2);
               t = t1;
            } else
               nppromote(b);
//errorT('d',"t: %d %t   t1: %d %t   t2: %d %t",t,t,t1,t1,t2,t2);
            if (t != t1 && checkType(t, t1, 0)) {
               this->e1 = (ExP)MakeTEx(CAST, t, this->e1);
               this->e1->tp = t;
            }
            if (t != t2 && checkType(t, t2, 0)) {
               this->e2 = (ExP)MakeTEx(CAST, t, this->e2);
               this->e2->tp = t;
            }

         }
      }
         break;
      case ASPLUS:
         r1 = num_ptr(t1, b);
         r2 = num_ptr(t2, b);
         if (r1 == P && r2 == P) error("P +=P");
         nppromote(b);
         goto ass;
      case ASMINUS:
         r1 = num_ptr(t1, b);
         r2 = num_ptr(t2, b);
         if (r2 == P && r1 != P && r1 != A) error("P -= nonP");
         nppromote(b);
         goto ass;
      case ASMUL:
      case ASDIV:
         r1 = numeric(t1, b);
         r2 = numeric(t1, b);
         nppromote(b);
         goto ass;
      case ASMOD:
         r1 = integral(t1, b);
         r2 = integral(t2, b);
         nppromote(b);
         goto ass;
      case ASAND:
      case ASOR:
      case ASER:
      case ASLS:
      case ASRS:
         r1 = integral(t1, b);
         r2 = integral(t2, b);
         npcheck(b);
         t = (TypeP)int_type;
         goto ass;
       ass:
         this->as_type = t; /* the type of the rhs */
         t2 = t;
      case ASSIGN:
         if (lval(this->e1, b) == 0) {
            this->tp = (TypeP)any_type;
            return this;
         }
       lkj:
         switch (t1->base) {
            case INT:
            case CHAR:
            case SHORT:
               if (this->e2->base == ICON && this->e2->tp == (TypeP)long_type)
                  errorT('w', "long constant assigned to%k", t1->base);
            case LONG:
               if (b == ASSIGN && ToBaseP(t1)->b_unsigned && this->e2->base == UMINUS && this->e2->e2->base == ICON)
                  errorT('w', "negative assigned to unsigned");
               break;
            case TYPE:
               t1 = ToBaseP(t1)->b_name->tp;
               goto lkj;
            case COBJ:
            {
               IdP c1 = is_cl_obj(t1);

               if (c1) {
                  IdP c2 = is_cl_obj(t2);
//errorT('d',"%t=%t %d %d",t1,t2,c1,c2);
                  if (c1 != c2) {
                     this->e2 = MakeEx(ELIST, this->e2, 0);
                     this->e2 = (ExP)MakeTEx(VALUE, t1, this->e2);
                     this->e2->e2 = this->e1;
                     this->e2 = typ(this->e2, tbl);
                     *this = *this->e2;
                     this->tp = t1;
                     return this;
                  } else { // check for bitwise copy
                     ClassP cl = ToClassP(c1->tp);
//errorT('d',"bit %d",cl->bit_ass);
                     if (cl->bit_ass == 0)
                        errorT('s', "bitwise copy: %s has a member with operator=()", cl->string);
                     else if (cl->itor && has_dtor(cl))
                        errorT('w', "bitwise copy: %s has destructor and %s(%s&) but not assignment", cl->string, cl->string, cl->string);
                  }
               }
               break;
            }
            case PTR:
/*errorT('d',"ptr %d %d",t1,t1?t1->base:0);*/
            {
               FunP ef = ToFunP(ToPtrP(t1)->typ);
               if (ef->base == FCT) {
                  FunP f;
                  IdP n = 0;
                  switch (this->e2->base) {
                     case NAME:
                        f = (FunP) this->e2->tp;
                        n = ToIdP(this->e2);
                        switch (f->base) {
                           case FCT:
                           case OVERLOAD:
                              this->e2 = MakeEx(G_ADDROF, 0, this->e2);
                              this->e2->tp = (TypeP)f;
                        }
                        goto ad;
                     case DOT:
                     case REF:
/*errorT('d',"dot %d %d",this->e2->mem->tp,this->e2->mem->tp?this->e2->mem->tp->base:0);*/
                        f = (FunP) this->e2->mem->tp;
                        switch (f->base) {
                           case FCT:
                           case OVERLOAD:
                              n = ToIdP(this->e2->mem);
                              this->e2 = MakeEx(G_ADDROF, 0, this->e2);
                              this->e2 = typ(this->e2, tbl);
                        }
                        goto ad;
                     case ADDROF:
                     case G_ADDROF:
                        f = (FunP) this->e2->e2->tp;
                      ad:
                        if (f->base == OVERLOAD) {
                           GenP g = (GenP) f;
                           n = find(g, ef);
                           if (n == 0) {
                              error("cannot deduceT for &overloaded %s()", g->string);
                              this->tp = (TypeP)any_type;
                           } else
                              this->tp = t1;
                           this->e2->e2 = (ExP)n;
                           lval((ExP)n, ADDROF);
                           return this;
                        }
                        if (n) lval((ExP)n, ADDROF);
                  }
               }
               break;
            }
         }
         {
            IdP cn;
            int i;
            if ((cn = is_cl_obj(t2))
               && (i = can_coerce(t1, t2))
               && Ncoerce) {
               if (1 < i) error("%d possible conversions for assignment", i);
//errorT('d',"%t =%t",t1,t2);
               ClassP cl = (ClassP) cn->tp;
               RefP r = MakeRef(DOT, this->e2, Ncoerce);
               ExP rr = typ((ExP)r, tbl);
               ExP c = MakeEx(G_CALL, rr, 0);
               c->fct_name = Ncoerce;
               c->tp = t1;
               this->e2 = c;
               this->tp = t1;
               return this;
            }
         }
//errorT('d',"checkType(%t,%t)",this->e1->tp,t2);
         if (checkType(this->e1->tp, t2, ASSIGN)) error("bad assignmentT:%t =%t", this->e1->tp, t2);
         t = this->e1->tp; /* the type of the lhs */
         break;
      case CM:
         t = t2;
         break;
      default:
         errorT('i', "unknown operator%k", b);
   }

   this->tp = t;
   return this;
}
