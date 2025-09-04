/* @(#) expr2.c 1.4 1/27/86 17:48:56 */
/*ident	"@(#)cfront:src/expr2.c	1.4" */
/***************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

expr2.c:

	type check expressions

************************************************************************/

#include "cfront.h"
#include "size.h"

void assign(IdP this) {
   if (this->n_assigned_to++ == 0) {
      switch (this->n_scope) {
         case FCT:
            if (this->n_used && this->n_addr_taken == 0) {
               TypeP t = this->tp;
             ll:
               switch (t->base) {
                  case TYPE:
                     t = ToBaseP(t)->b_name->tp;
                     goto ll;
                  case VEC:
                     break;
                  default:
                     if (curr_loop)
                        errorT('w', "%n may have been used before set", this);
                     else
                        errorT('w', "%n used before set", this);
               }
            }
      }
   }
}

int lval(ExP this, Token oper) {
   register ExP ee = this;
   register IdP n;
   int deref = 0;
   const char *es;

   if (this == 0 || this->tp == 0) errorT('i', "lval(%d, 0)", this);

   switch (oper) {
      case ADDROF:
      case G_ADDROF:
         es = "address of";
         break;
      case INCR:
      case DECR:
         es = "increment of";
         goto def;
      case DEREF:
         es = "dereference of";
         break;
      default:
         es = "assignment to";
       def:
         if (tconst(this->tp)) {
            if (oper) {
               if (this->base == NAME)
                  error("%s constant%n", es, this);
               else
                  error("%s constant", es);
            }
            return 0;
         }
   }
//errorT('d',"lval %s",es);
   while (1) {
//errorT('d',"ee %d %k",ee->base,ee->base);
      switch (ee->base) {
         case G_CALL:
         case CALL:
            if (deref == 0) {
               switch (oper) {
                  case ADDROF:
                  case G_ADDROF:
                  case 0:
                     if (ee->fct_name && ToFunP(ee->fct_name->tp)->f_inline) return 1;
               }
            }
         default:
            if (deref == 0) {
               if (oper) error("%s %k (not an lvalue)", es, ee->base);
               return 0;
            }
            return 1;
         case ZERO:
         case CCON:
         case ICON:
         case FCON:
            if (oper) error("%s numeric constant", es);
            return 0;
         case STRING:
            if (oper) errorT('w', "%s string constant", es);
            return 1;

         case DEREF:
         {
            ExP ee1 = ee->e1;
            if (ee1->base == ADDROF) /* *& vanishes */
               ee = ee1->e2;
            else {
               ee = ee1;
               deref = 1;
            }
            break;
         }
         // OK, except I cannot generate old C for (i?a:b) = c
         //      case QUEST:
         //              return lval(this->e1, oper) && lval(this->e2, oper);
         //      case INCR:
         //      case DECR:
         //              ee = (ee->e1) ? ee->e1 : ee->e2;
         //              break;

         case DOT:
//errorT('d',"lval dot: %k",ee->e1->base);
            switch (ee->e1->base) { // update use counts, etc.
               case NAME:
//errorT('d',"lval dot: %n (oper %d)",ToIdP(ee->e1),oper);
                  switch (oper) {
                     case ADDROF:
                     case G_ADDROF:
                        take_addr(ToIdP(ee->e1));
                     case 0:
                        break;
                     case ASSIGN:
                        ToIdP(ee->e1)->n_used--;
                     default:
                        assign(ToIdP(ee->e1)); // asop
                  }
               case DOT: {
                  ExP e = ee->e1;
                  do
                     e = e->e1;
                  while (e->base == DOT);
                  if (e->base == NAME) {
//errorT('d',"lval dot.dot: %n (oper %d)",ToIdP(e),oper);
                     switch (oper) {
                        case ADDROF:
                        case G_ADDROF:
                           take_addr(ToIdP(e));
                        case 0:
                           break;
                        case ASSIGN:
                           ToIdP(e)->n_used--;
                        default:
                           assign(ToIdP(e)); // asop
                     }
                  }
               }
            }
            n = ee->mem;
            if (deref == 0 && tconst(ee->e1->tp)) {
               if (oper) error("%sM%n of%t", es, n, ee->e1->tp);
               return 0;
            }
            goto xx;

         case REF:
            n = ee->mem;
            if (deref == 0) {
               TypeP p = ee->e1->tp;
             zxc:
               switch (p->base) {
                  case TYPE:
                     p = ToBaseP(p)->b_name->tp;
                     goto zxc;
                  case PTR:
                     break;
                  default:
                     errorT('i', "%t->%n", p, n);
               }
               if (tconst(ToPtrP(p)->typ)) {
                  if (oper) error("%sM%n of%t", es, n, ToPtrP(p)->typ);
                  return 0;
               }
            }
            goto xx;
         case NAME:
            n = (IdP) ee;
          xx:
            if (deref || oper == 0) return 1;

            if (n->tp->base == FIELD && ToBaseP(n->tp)->b_bits == 0) {
               error("%s 0-length field%n", es, n);
               return 0;
            }
            switch (oper) {
               case ADDROF:
               case G_ADDROF:
               {
                  FunP f = (FunP) n->tp;
                  if (n->n_sto == REGISTER) {
                     error("& register%n", n);
                     return 0;
                  }
                  if (f == 0) {
                     error("& label%n", n);
                     return 0;
                  }
                  if (n->n_stclass == ENUM) {
                     error("& enumerator%n", n);
                     return 0;
                  }
                  if (n->tp->base == FIELD) {
                     error("& field%n", es, n);
                     return 0;
                  }
                  n->n_used--;
                  take_addr(n);
                  if ((n->n_evaluated && n->n_scope != ARG)
                     || (f->base == FCT && f->f_inline)) {
                  /* address of const or inline: allocate it */
                     IdP nn = MakeId(0);
                     if (n->n_evaluated && n->n_scope != ARG) {
                        n->n_evaluated = 0; /* use allocated version */
                        n->n_initializer = MakeEx(IVAL, (ExP) n->n_val, 0);
                     }
                     *nn = *n;
                     nn->n_sto = STATIC;
                     nn->n_list = dcl_list;
                     dcl_list = nn;
                  }
                  break;
               }
               case ASSIGN:
                  n->n_used--;
                  assign(n);
                  break;
               default: /* incr ops, and asops */
                  if (cc->tot && n == cc->c_this) {
                     error("%n%k", n, oper);
                     return 0;
                  }
                  assign(n);
            }
            return 1;
      }
   }
}

ExP Ninit; // default argument used;
int Nstd; // standard coercion used (derived* =>base* or int=>long or ...)

/*
	look for an exact match between "n" and the argument list "arg"
*/
bit gen_match(IdP n, ExP arg) {
   FunP f = ToFunP(n->tp);
   register ExP e;
   register IdP nn;

   for (e = arg, nn = f->argtype; e; e = e->e2, nn = nn->n_list) {
      ExP a = e->e1;
      TypeP at = a->tp;
      if (at->base == ANY) return 0;
      if (nn == 0) return f->nargs_known == ELLIPSIS;

      TypeP nt = nn->tp;

//errorT('d',"nt %t at %t",nt,at);
      switch (nt->base) {
         case RPTR:
            if (at == (TypeP)zero_type) return 0; //break;
            if (checkType(nt, at, COERCE)) {
               PtrP pt = addrof(at);
               nt->base = PTR; // handle derived classes
//errorT('d',"ptr nt %t pt %t",nt,pt);
               if (checkType(nt, (TypeP)pt, COERCE)) {
                  nt->base = RPTR;
                  _delete(pt);
                  return 0;
               }
               nt->base = RPTR;
               _delete(pt);
            }
            break;
         default:
            if (checkType(nt, at, COERCE)) return 0;
      }
   }
//errorT('d',"nn %d init %d",nn,nn?nn->n_initializer:0);
   if (nn) {
      Ninit = nn->n_initializer;
      return Ninit != 0;
   }

   return 1;
}

IdP Ncoerce;

/*	return number of possible coercions of t2 into t1,
	Ncoerce holds a coercion function (not constructor), if found
*/
bit can_coerce(TypeP t1, TypeP t2) {
//errorT('d',"can_coerce %t<-%t",t1,t2);
   Ncoerce = 0;
   if (t2->base == ANY) return 0;
   switch (t1->base) {
      case RPTR:
       rloop:
         switch (t2->base) {
            case TYPE:
               t2 = ToBaseP(t2)->b_name->tp;
               goto rloop;
            //      case VEC:
            //      case PTR:
            //      case RPTR:
            //              if (checkType(t1, t2,COERCE) == 0) return 1;
            default:
            {
               TypeP tt2 = (TypeP)addrof(t2);
//errorT('d',"t1%t tt2%t =>%d",t1,tt2,checkType(t1, tt2,COERCE));
               if (checkType(t1, tt2, COERCE) == 0) return 1;
               TypeP tt1 = ToPtrP(t1)->typ;
               int i = can_coerce(tt1, t2);
               return i;
            }
         }
   }

   IdP c1 = is_cl_obj(t1);
   IdP c2 = is_cl_obj(t2);
   int val = 0;
//errorT('d',"c1 %s c2 %s",c1?c1->string:"0",c2?c2->string:"0");
   if (c1) {
      ClassP cl = (ClassP) c1->tp;
      if (c2 && c2->tp == (TypeP)cl) return 1;

   /*      look for constructor
      with one argument
      or with default for second argument
      of acceptable type
    */
      IdP ctor = has_ctor(cl);
      if (ctor == 0) goto oper_coerce;
      register FunP f = (FunP) ctor->tp;
//errorT('d',"f %k",f->base);
      switch (f->base) {
         case FCT:
            switch (f->nargs) {
               case 1:
                one:
               {
                  TypeP tt = f->argtype->tp;
//errorT('d',"one: f->argtype->tp %t t2 %t",tt,t2);
                  if (checkType(tt, t2, COERCE) == 0) val = 1;
                  if (tt->base == RPTR) {
                     PtrP pt = addrof(t2); // handle derived classed
                     tt->base = PTR;
                     if (checkType((TypeP)tt, (TypeP)pt, COERCE) == 0) val = 1;
                     tt->base = RPTR;
                     _delete(pt);
                  }
                  goto oper_coerce;
               }
               default:
                  if (f->argtype->n_list->n_initializer) goto one;
               case 0:
                  goto oper_coerce;
            }
         case OVERLOAD:
         {
            register IdListP gl;

            for (gl = ToGenP(f)->fct_list; gl; gl = gl->l) { // look for match
               IdP nn = gl->f;
               FunP ff = (FunP) nn->tp;
               switch (ff->nargs) {
                  case 0:
                     break;
                  case 1:
                   over_one:
                  {
                     TypeP tt = ff->argtype->tp;
//errorT('d',"over_one: ff->argtype->tp %t t2 %t",tt,t2);
                     if (checkType(tt, t2, COERCE) == 0) val = 1;
                     if (tt->base == RPTR) {
                        PtrP pt = addrof(t2); // handle derived classed
                        tt->base = PTR;
                        if (checkType((TypeP)tt, (TypeP)pt, COERCE) == 0) {
                           tt->base = RPTR;
                           _delete(pt);
                           val = 1;
                           goto oper_coerce;
                        }
                        tt->base = RPTR;
                        _delete(pt);
                     }
                     break;
                  }
                  default:
                     if (ff->argtype->n_list->n_initializer) goto over_one;
               }
            }
            goto oper_coerce;
         }
         default:
            errorT('i', "cannot_coerce(%k)\n", f->base);
      }
   }
 oper_coerce:
   if (c2) {
      ClassP cl = (ClassP) c2->tp;
      int std = 0;
      for (register IdP on = cl->conv; on; on = on->n_list) {
//errorT('d',"oper_coerce%n %t %d",on,(on)?on->tp:0,on);
         FunP f = (FunP) on->tp;
         Nstd = 0;
         if (checkType(t1, f->returns, COERCE) == 0) {
//errorT('d',"nstd %d std %d",Nstd,std);
            if (Nstd == 0) { // forget solutions involving standard conversions
               if (std) { // forget
                  val = 1;
                  std = 0;
               } else
                  val++;
               Ncoerce = on;
            } else { // take note only if no exact match seen
               if (val == 0 || std) {
                  Ncoerce = on;
                  val++;
                  std = 1;
               }
            }
         }
      }
   }
//errorT('d',"val %d",val);
   if (val) return val;
   if (c1 && has_itor(ToClassP(c1->tp))) return 0;
   if (checkType(t1, t2, COERCE)) return 0;
   return 1;
}

/*
	look to see if the argument list "arg" can be coerced into a call of "n"
	1: it can
	0: it cannot or it can be done in more than one way
*/
int gen_coerce(IdP n, ExP arg) {
   FunP f = (FunP) n->tp;
   register ExP e;
   register IdP nn;
//errorT('d',"gen_coerce%n %d",n,arg);
   for (e = arg, nn = f->argtype; e; e = e->e2, nn = nn->n_list) {
      if (nn == 0) return f->nargs_known == ELLIPSIS;
      ExP a = e->e1;
      TypeP at = a->tp;
      int i = can_coerce(nn->tp, at);
//errorT('d',"a1 %k at%t argt%t -> %d",a->base,at,nn->tp,i);
      if (i != 1) return 0;
   }
   if (nn && nn->n_initializer == 0) return 0;
   return 1;
}

IdP Nover;
int Nover_coerce;

/*
	return 2 if n(arg) can be performed without user defined coercion of arg
	return 1 if n(arg) can be performed only with user defined coercion of arg
	return 0 if n(arg) is an error
	Nover is the function found, if any
*/
int over_call(IdP n, ExP arg) {
   register IdListP gl;
   GenP g = (GenP) n->tp;
   if (arg && arg->base != ELIST) errorT('i', "ALX");
//errorT('d',"over_call%n this->base%k arg %d%k", n, g->base, arg, arg?arg->tp->base:0);
   Nover_coerce = 0;
   switch (g->base) {
      default:
         errorT('i', "over_call(%t)\n", g);
      case OVERLOAD:
         break;
      case FCT:
         Nover = n;
         Ninit = 0;
         if (gen_match(n, arg) && Ninit == 0) return 2;
         if (gen_coerce(n, arg)) return 1;
         return 0;
   }

   IdP exact = 0;
   int no_exact = 0;
   for (gl = g->fct_list; gl; gl = gl->l) { /* look for match */
      Nover = gl->f;
      Ninit = 0;
      Nstd = 0;
//errorT('d',"exact? %n",Nover);
      if (gen_match(Nover, arg) && Ninit == 0) {
//errorT('d',"%n: nstd %d",Nover,Nstd);
         if (Nstd == 0) return 2;
         if (exact)
            no_exact++;
         else
            exact = Nover;
      }

   }

   if (exact) {
//errorT('d',"exact%n %d",exact,no_exact);
      if (no_exact) errorT('w', "more than one standard conversion possible for%n", n);
      Nover = exact;
      return 2;
   }
//errorT('d',"exact == 0");
   Nover = 0;
   for (gl = g->fct_list; gl; gl = gl->l) { /* look for coercion */
      IdP nn = gl->f;
//errorT('d',"over_call: gen_coerce(%n,%k) %d",nn,arg->e1->base,gen_coerce(nn,arg));
      if (gen_coerce(nn, arg)) {
         if (Nover) {
            Nover_coerce = 2;
            return 0; /* ambiguous */
         }
         Nover = nn;
      }
   }

   return Nover ? 1 : 0;
}

/*
	check "this" call:
		 e1(e2)
	typ(e1) and typ(e2) has been done
*/
TypeP fct_call(ExP this, TableP tbl) {
   FunP f;
   IdP fn;
   int x;
   int k;
   IdP nn;
   ExP e;
   TypeP t;
   ExP arg = this->e2;
   TypeP t1;
   int argno;
   ExP etail = 0;
   IdP no_virt; // set if explicit qualifier was used: c::f()
//errorT('d',"fct_call");
   switch (this->base) {
      case CALL:
      case G_CALL:
         break;
      default:
         errorT('i', "fct_call(%k)", this->base);
   }

   if (this->e1 == 0 || (t1 = this->e1->tp) == 0) errorT('i', "fct_call(e1=%d,e1->tp=%t)", this->e1, t1);
   if (arg && arg->base != ELIST) errorT('i', "badAL%d%k", arg, arg->base);

   switch (this->e1->base) {
      case NAME:
         fn = (IdP) this->e1;
         no_virt = fn->n_qualifier;
         break;
      case REF:
      case DOT:
         fn = this->e1->mem;
         no_virt = fn->n_qualifier;
         break;
      default:
         fn = 0;
         no_virt = 0;
   };
//errorT('d',"fn%n t1%k",fn,t1->base);
 lll:
   switch (t1->base) {
      case TYPE:
         t1 = ToBaseP(t1)->b_name->tp;
         goto lll;

      case PTR: // pf() allowed as shorthand for (*pf)()
         if (ToPtrP(t1)->typ->base == FCT) {
            t1 = ToPtrP(t1)->typ;
            fn = 0;
            goto case_fct;
         }

      default:
         error("call of%n;%n is a%t", fn, fn, this->e1->tp);

      case ANY:
         return (TypeP)any_type;

      case OVERLOAD:
      {
         register IdListP gl;
         GenP g = (GenP) t1;
         IdP found = 0;

//              for (gl=g->fct_list; gl; gl=gl->l) {    /* look for match */
//                      register IdP nn = gl->f;
//errorT('d',"gen_match %s %d",nn->string?nn->string:"?",arg->base);
//                      if (gen_match(nn,arg)) {
//                              found = nn;
//                              goto fnd;
//                      }
//              }
         IdP exact = 0;
         int no_exact = 0;
         for (gl = g->fct_list; gl; gl = gl->l) { /* look for match */
            register IdP nn = gl->f;
            Ninit = 0;
            Nstd = 0;

            if (gen_match(nn, arg)) {
               if (Nstd == 0) {
                  found = nn;
                  goto fnd;
               }
               if (exact)
                  no_exact++;
               else
                  exact = nn;
            }

         }
         if (exact) {
            if (no_exact) errorT('w', "more than one standard conversion possible for%n", fn);
            found = exact;
            goto fnd;
         }
//errorT('d',"exact == 0");
         for (gl = g->fct_list; gl; gl = gl->l) { /* look for coercion */
            register IdP nn = gl->f;
//errorT('d',"gen_coerce %s %d\n",nn->string?nn->string:"?",arg->base);
            if (gen_coerce(nn, arg)) {
               if (found) {
                  error("ambiguousA for overloaded%n", fn);
                  goto fnd;
               }
               found = nn;
            }
         }

       fnd:
//errorT('d',"found%n",found);
         if (found) {
            BaseP b;
            TableP tblx;

            f = (FunP) found->tp;
            this->fct_name = found;

         /* is this->fct_name visible? */
//errorT('d',"e1 %d%k",this->e1,this->e1?this->e1->base:0);
            switch (this->e1->base) {
               default:
                  if (no_virt) this->e1 = (ExP)found; // instead of using this->fct_name
                  break;
               case REF: {
                  if (no_virt) this->e1->mem = found; // instead of using this->fct_name
                  if (this->e1->e1 == 0) break; // constructor: this==0
                  TypeP pt = this->e1->e1->tp;
                  for (; pt->base == TYPE; pt = ToBaseP(pt)->b_name->tp);
                  b = ToBaseP(ToPtrP(pt)->typ);
                  goto xxxx;
               }
               case DOT:
                  if (no_virt) this->e1->mem = found; // instead of using this->fct_name
                  b = ToBaseP(this->e1->e1->tp);
                xxxx:
                  switch (b->base) {
                     case TYPE:
                        b = ToBaseP(b->b_name->tp);
                        goto xxxx;
                     case ANY:
                        break;
                     case COBJ:
                        tblx = b->b_table;
                        if (tblx->base != TABLE) errorT('i', "tblx %d %d", tblx, tblx->base);
                        break;
                     default:
                        errorT('i', "no tblx %d", b);
                  }

                  if (lookc(tblx, g->string, 0) == 0)
                     errorT('i', "fct_call overload check");
//errorT('d',"scope %d epriv %d ebase %d cc %d",found->n_scope,Epriv,Ebase,cc);
                  switch (found->n_scope) {
                     case 0:
                        if (Epriv && Epriv != cc->cot && !has_friend(Epriv, cc->nof)) {
                           error("%n is private", found);
                           break;
                        }
                     /* no break */
                     case PUBLIC:
                        if (Ebase && (cc->cot == 0 || (Ebase != (ClassP)cc->cot->clbase->tp && !has_friend(Ebase, cc->nof)))
                           ) {
                           error("%n is from a privateBC", found);
                        }
                  }
            }
         } else {
            error("badAL for overloaded%n", fn);
            return (TypeP)any_type;
         }
         break;
      }
      case FCT:
       case_fct:
         f = (FunP) t1;
         if (fn) this->fct_name = fn;
         break;
   }

   if (no_virt) this->fct_name = 0;

   t = f->returns;
   x = f->nargs;
   k = f->nargs_known;
//errorT('d',"fct_name%n",this->fct_name);

   if (k == 0) {
      if (fct_void && fn && x == 0 && arg)
         if (no_of_badcall++ == 0) badcall = fn;
      return t;
   }

   for (e = arg, nn = f->argtype, argno = 1; e || nn; nn = nn->n_list, e = etail->e2, argno++) {
      ExP a;

      if (e) {
         a = e->e1;
//errorT('d',"e %d%k a %d%k e2 %d",e,e->base,a,a->base,e->e2);
         etail = e;

         if (nn) { /* type check */
            TypeP t1 = nn->tp;
//errorT('d',"argname %n (%t)",nn,nn->tp);
          lx:
/*errorT('d',"lx: t1%t a->tp%t",t1,a->tp);*/
            switch (t1->base) {
               case TYPE:
                  t1 = ToBaseP(t1)->b_name->tp;
                  goto lx;
               case RPTR:
                  e->e1 = ref_init(ToPtrP(t1), a, tbl);
                  break;
               case COBJ:
                  e->e1 = class_init(0, t1, a, tbl);
                  break;
               case ANY:
                  return t;
               case PTR:
               {
                  FunP ef = (FunP) ToPtrP(t1)->typ;
                  if (ef->base == FCT) {
                     FunP f;
                     IdP n = 0;
                     switch (a->base) {
                        case NAME:
                           f = (FunP) a->tp;
                           switch (f->base) {
                              case FCT:
                              case OVERLOAD:
                                 e->e1 = MakeEx(G_ADDROF, 0, a);
                                 e->e1->tp = (TypeP)f;
                           }
                           n = ToIdP(a);
                           goto ad;
                        case DOT:
                        case REF:
                           f = (FunP) a->mem->tp;
                           switch (f->base) {
                              case FCT:
                              case OVERLOAD:
                                 n = ToIdP(a->mem);
                                 a = MakeEx(G_ADDROF, 0, a);
                                 e->e1 = typ(a, tbl);
                           }
                           goto ad;
                        case ADDROF:
                        case G_ADDROF:
                           f = (FunP) a->e2->tp;
                         ad:
                           if (f->base == OVERLOAD) {
                              GenP g = (GenP) f;
                              n = find(g, ef);
                              if (n == 0) {
                                 error("cannot deduceT for &overloaded %s()", g->string);
                                 return (TypeP)any_type;
                              }
                              e->e1->e2 = (ExP)n;
                           }
                           if (n) lval((ExP)n, ADDROF);
                     }
                     break;

                  }
                  goto def;
               }
               case CHAR:
               case SHORT:
               case INT:
                  if (a->base == ICON && a->tp == (TypeP)long_type)
                     errorT('w', "long constantA for%n,%kX", fn, t1->base);
               case LONG:
                  if (((BaseP) t1)->b_unsigned && a->base == UMINUS && a->e2->base == ICON)
                     errorT('w', "negativeA for%n, unsignedX", fn);
               default:
                def:
               {
                  IdP cn;
                  int i;
                  if ((cn = is_cl_obj(a->tp))
                     && (i = can_coerce(t1, a->tp))
                     && Ncoerce) {
                     if (1 < i) error("%d possible conversions for%nA%d", i, fn, argno);
//errorT('d',"%t<-%t",t1,a->tp);
                     ClassP cl = (ClassP) cn->tp;
                     RefP r = MakeRef(DOT, a, Ncoerce);
                     ExP rr = typ((ExP)r, tbl);
                     ExP c = MakeEx(G_CALL, rr, 0);
                     c->fct_name = Ncoerce;
                     c->tp = t1;
                     e->e1 = c;
                     break;
                  }
               }
                  if (checkType(t1, a->tp, ARG)) {
                     if (arg_err_suppress == 0) error("badA %dT for%n:%t (%tX)", argno, fn, a->tp, nn->tp);
                     return (TypeP)any_type;
                  }
            }
         } else {
            if (k != ELLIPSIS) {
               if (arg_err_suppress == 0) error("unX %dA for%n", argno, fn);
               return (TypeP)any_type;
            }
            return t;
         }
      } else { /* default argument? */
         a = nn->n_initializer;
//errorT('d',"arg missing: %n %d as %d",nn,a,arg_err_suppress);
         if (a == 0) {
            if (arg_err_suppress == 0) error("A %d ofT%tX for%n", argno, nn->tp, fn);
            return (TypeP)any_type;
         }
//errorT('d',"%n: perm=%d",nn,a->permanent);
         a->permanent = 2; // ought not be necessary, but it is
         e = MakeEx(ELIST, a, 0);
         if (etail)
            etail->e2 = e;
         else
            this->e2 = e;
         etail = e;
      }
   }

   return t;
}

int refd;

/*
	initialize the "p" with the "init"
*/
ExP ref_init(PtrP p, ExP init, TableP tbl) {
   register TypeP it = init->tp;
   TypeP p1 = p->typ;
   IdP c1;
   ExP a;
//errorT('d',"init %d",tconst(it));
 rloop:
//errorT('d',"rloop: %d%k",it,it->base);
   switch (it->base) {
      case TYPE:
         it = ToBaseP(it)->b_name->tp;
         goto rloop;
      default:
      {
         TypeP tt = (TypeP)addrof(it);
         p->base = PTR; // allow &x for y& when y : public x
      // but not &char for int&
         int x = checkType((TypeP)p, tt, COERCE);
         p->base = RPTR;
//errorT('d',"p%t tt%t => %d (nstd %d)",p,tt,x,Nstd);
         if (x == 0) {
            if (lval(init, 0)) return address(init);
            if (init->base == G_CALL // &inline function call?
               && init->fct_name && ToFunP(init->fct_name->tp)->f_inline)
               return address(init);
            p1 = p->typ;
            goto xxx;
         }
      }
   }

   c1 = is_cl_obj(p1);

   if (c1) {
//errorT('d',"c1%n",c1);
      refd = 1; /* disable itor */
      a = class_init(0, p1, init, tbl);
      refd = 0;
//errorT('d',"a %d init %d",a,init);
      if (a == init && init->tp != (TypeP)any_type) goto xxx;
      switch (a->base) {
         case G_CALL:
         case CM:
            init = a;
            goto xxx;
      }
      return address(a);
   }

   if (checkType(p1, it, 0)) {
      error("badIrT:%t (%tX)", it, p);
      if (init->base != NAME) init->tp = (TypeP)any_type;
      return init;
   }

 xxx:
//errorT('d',"xxx: %k",init->base);
   switch (init->base) {
      case NAME:
      case DEREF:
      case REF:
      case DOT: // init => &init
         if (tconst(it) && vec_const == 0) goto def;
         lval(init, ADDROF);
         return address(init);
      case CM:
/*errorT('d',"cm%k",init->e2->base);*/
         switch (init->e2->base) { /* (a, b) => (a, &b) */
            case NAME:
            case DEREF:
               return address(init);
         }
      default:
       def: /* init = > ( temp=init, &temp) */
      {
         TableP otbl = tbl;
         if (Cstmt) { /*      make Cstmt into a block */
            if (Cstmt->memtbl == 0) Cstmt->memtbl = MakeTable(4, tbl, 0);
            tbl = Cstmt->memtbl;
         }
         char *s = make_name('I');
         IdP n = MakeId(s);

//errorT('d',"ref_init tmp %s n=%d tbl %d init=%d%k",s,n,tbl,init,init->base);
         if (tbl == gtbl) errorT('s', "Ir for static reference not an lvaue");
         n->tp = p1;
         n = dclId(n, tbl, ARG); /* no initialization! */
         n->n_scope = FCT;
         assign(n);
         a = address((ExP)n);
//errorT('d',"tp %t init->tp %t",n->tp,init->tp);
         ExP as = MakeEx(ASSIGN, (ExP)n, init);
         a = MakeEx(CM, as, a);
         a->tp = a->e2->tp;
         tbl = otbl;
         return a;
      }
   }
}

/*
	initialize "nn" of type "tt" with "init"
	if nn==0 make a temporary,
	nn may not be a name
*/
ExP class_init(ExP nn, TypeP tt, ExP init, TableP tbl) {
   IdP c1 = is_cl_obj(tt);
   IdP c2 = is_cl_obj(init->tp);

//errorT('d',"class_init%n%n%n refd=%d",nn,c1,c2,refd);
   if (c1) {
      if (c1 != c2 || (refd == 0 && has_itor(ToClassP(c1->tp)))) {
      /*      really ought to make a temp if refd,
         but ref_init can do that
       */
         int i = can_coerce(tt, init->tp);
//errorT('d',"i %d Ncoerce %d",i,Ncoerce);
         switch (i) {
            default:
               error("%d ways of making a%n from a%t", i, c1, init->tp);
               init->tp = (TypeP)any_type;
               return init;
            case 0:
               error("cannot make a%n from a%t", c1, init->tp);
               init->tp = (TypeP)any_type;
               return init;
            case 1:
               if (Ncoerce == 0) {
                  ExP a = MakeEx(ELIST, init, 0);
                  a = (ExP)MakeTEx(VALUE, tt, a);
                  a->e2 = nn;
                  return typ(a, tbl);
               }
               switch (init->base) {
#ifdef BSD
                  case CALL:
                  case G_CALL:
#endif
                  case CM:
                  case NAME: /* init.coerce() */
                  {
                     RefP r = MakeRef(DOT, init, Ncoerce);
                     ExP rr = typ((ExP)r, tbl);
                     ExP c = MakeEx(G_CALL, rr, 0);
                     c->fct_name = Ncoerce;
                     init = c;
                     break;
                  }
                  default: /* (temp=init,temp.coerce()) */
                  {
                     TableP otbl = tbl;
                     if (Cstmt) { /* make Cstmt into a block */
                        if (Cstmt->memtbl == 0) Cstmt->memtbl = MakeTable(4, tbl, 0);
                        tbl = Cstmt->memtbl;
                     }
                     char *s = make_name('U');
                     IdP tmp = MakeId(s);
                     tmp->tp = init->tp;
                     tmp = dclId(tmp, tbl, ARG); /* no init! */
                     tmp->n_scope = FCT;
                     ExP ass = MakeEx(ASSIGN, (ExP)tmp, init);
                     ass->tp = tt;
                     RefP r = MakeRef(DOT, (ExP)tmp, Ncoerce);
                     ExP rr = typ((ExP)r, tbl);
                     ExP c = MakeEx(G_CALL, rr, 0);
                     c->fct_name = Ncoerce;
                     init = MakeEx(CM, ass, c);
                     tbl = otbl;
                  }
               }
         }
         return typ(init, tbl);
      } else if (refd == 0) { // bitwise copy, check for dtor & operator=
         ClassP cl = ToClassP(c1->tp);
         if (cl->itor == 0) {
            if (cl->bit_ass == 0)
               errorT('w', "bitwise copy: %s has a member with operator=()", cl->string);
            else if (has_dtor(cl) && has_oper(cl, ASSIGN))
               errorT('w', "bitwise copy: %s has assignment and destructor but not %s(%s&)", cl->string, cl->string, cl->string);
         }
      }
//errorT('d',"class_init%n: init %d %d:%t",nn,init->tp,init->tp->base,init->tp);
      return init;
   }

   if (checkType(tt, init->tp, ASSIGN) && refd == 0) {
      error("badIrT:%t (%tX)", init->tp, tt);
      init->tp = (TypeP)any_type;
   }
   return init;
}

/*	assume s points to a string:
		'c'
	or	'\c'
	or	'\0'
	or	'\ddd'
	or multi-character versions of the above
	(hex constants have been converted to octal by the parser)
*/
int char_to_int(const char *s) {
   register int i = 0;
   register char c, d, e;

   switch (*s) {
      default:
         errorT('i', "char constant store corrupted");
      case '`':
         errorT('s', "bcd constant");
         return 0;
      case '\'':
         break;
   }

   while (1) /* also handle multi-character constants */
      switch (c = *++s) {
      case '\'':
         return i;
      case '\\': /* special character */
         switch (c = *++s) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7': /* octal representation */
               c -= '0';
               switch (d = *++s) { /* try for 2 */

                  case '0':
                  case '1':
                  case '2':
                  case '3':
                  case '4':
                  case '5':
                  case '6':
                  case '7':
                     d -= '0';
                     switch (e = *++s) { /* try for 3 */

                        case '0':
                        case '1':
                        case '2':
                        case '3':
                        case '4':
                        case '5':
                        case '6':
                        case '7':
                           c = c * 64 + d * 8 + e - '0';
                           break;
                        default:
                           c = c * 8 + d;
                           s--;
                     }
                     break;
                  default:
                     s--;
               }
               break;
            case 'b':
               c = '\b';
               break;
            case 'f':
               c = '\f';
               break;
            case 'n':
               c = '\n';
               break;
            case 'r':
               c = '\r';
               break;
            case 't':
               c = '\t';
               break;
            case '\\':
               c = '\\';
               break;
            case '\'':
               c = '\'';
               break;
         }
      /* no break */
      default:
         if (i) i <<= BI_IN_BYTE;
         i += c;
   }
}

const int A10 = 'A' - 10;
const int a10 = 'a' - 10;

/*
	read decimal, octal, or hexadecimal integer
*/
int str_to_int(const char *p) {
   register int c;
   register int i = 0;

   if ((c = *p++) == '0') {
      switch (c = *p++) {
         case 0:
            return 0;

         case 'l':
         case 'L': /* long zero */
            return 0;

         case 'x':
         case 'X': /* hexadecimal */
            while (c = *p++)
               switch (c) {
                  case 'l':
                  case 'L':
                     return i;
                  case 'A':
                  case 'B':
                  case 'C':
                  case 'D':
                  case 'E':
                  case 'F':
                     i = i * 16 + c - A10;
                     break;
                  case 'a':
                  case 'b':
                  case 'c':
                  case 'd':
                  case 'e':
                  case 'f':
                     i = i * 16 + c - a10;
                     break;
                  default:
                     i = i * 16 + c - '0';
               }
            return i;

         default: /* octal */
            do
               switch (c) {
                  case 'l':
                  case 'L':
                     return i;
                  default:
                     i = i * 8 + c - '0';
               }
            while (c = *p++);
            return i;
      }
   }
/* decimal */
   i = c - '0';
   while (c = *p++)
      switch (c) {
         case 'l':
         case 'L':
            return i;
         default:
            i = i * 10 + c - '0';
      }
   return i;

}

const char *Neval;

int eval(ExP this) {
   if (Neval) return 1;

   switch (this->base) {
      case ZERO:
         return 0;
      case IVAL:
         return (int)this->e1;
      case ICON:
         return str_to_int(this->string);
      case CCON:
         return char_to_int(this->string);
      case FCON:
         Neval = "float in constant expression";
         return 1;
      case STRING:
         Neval = "string in constant expression";
         return 1;
      case EOBJ:
         return ToIdP(this)->n_val;
      case SIZEOF:
         return tsizeof(this->tp2);
      case NAME:
      {
         IdP n = (IdP) this;
         if (n->n_evaluated && n->n_scope != ARG) return n->n_val;
         Neval = "cannot evaluate constant";
         return 1;
      }
      case ICALL:
         if (this->e1) {
            this->il->i_next = curr_icall;
            curr_icall = this->il;
            int i = eval(this->e1);
            curr_icall = this->il->i_next;
            return i;
         }
         Neval = "void inlineF";
         return 1;
      case ANAME:
      {
         IdP n = (IdP) this;
         int argno = n->n_val;
         InLineP il;
         for (il = curr_icall; il; il = il->i_next)
            if (il->i_table == n->n_table) goto aok;
         goto bok;
       aok:
         if (il->local[argno]) {
          bok:
            Neval = "inlineF call too complicated for constant expression";
            return 1;
         }
         ExP aa = il->arg[argno];
         return eval(aa);
      }
      case CAST:
      {
         int i = eval(this->e1);
      /*
         ignore cast and rely on error message from evaluation.
         this will allow redundant casts only.
         In, particular: case (int)0:
       */
      // Neval = "cast in constant expression";
         return i;
      }
      case UMINUS:
      case UPLUS:
      case NOT:
      case COMPL:
      case PLUS:
      case MINUS:
      case MUL:
      case LS:
      case RS:
      case NE:
      case LT:
      case LE:
      case GT:
      case GE:
      case AND:
      case OR:
      case ER:
      case DIV:
      case MOD:
      case QUEST:
      case EQ:
      case ANDAND:
      case OROR:
         break;
      default:
         Neval = "bad operator in constant expression";
         return 1;
   }

   int i1 = (this->e1) ? eval(this->e1) : 0;
   int i2 = (this->e2) ? eval(this->e2) : 0;

   switch (this->base) {
      case UMINUS:
         return -i2;
      case UPLUS:
         return i2;
      case NOT:
         return !i2;
      case COMPL:
         return ~i2;
      case CAST:
         return i1;
      case PLUS:
         return i1 + i2;
      case MINUS:
         return i1 - i2;
      case MUL:
         return i1 * i2;
      case LS:
         return i1 << i2;
      case RS:
         return i1 >> i2;
      case NE:
         return i1 != i2;
      case EQ:
         return i1 == i2;
      case LT:
         return i1 < i2;
      case LE:
         return i1 <= i2;
      case GT:
         return i1 > i2;
      case GE:
         return i1 >= i2;
      case AND:
         return i1 & i2;
      case OR:
         return i1 | i2;
      case OROR:
         return i1 || i2;
      case ER:
         return i1 ^ i2;
      case MOD:
         return (i2 == 0) ? 1 : i1 % i2;
      case QUEST:
         return (eval(this->cond)) ? i1 : i2;
      case DIV:
         if (i2 == 0) {
            Neval = "divide by zero";
            errorT('w', "divide by zero");
            return 1;
         }
         return i1 / i2;
   }
}

/*
	does this class have function "f" as its friend?
*/
bit has_friend(ClassP this, IdP f) {
   IdListP l;
   TableP ctbl = f->n_table;
/*fprintf(stderr,"has_friend((%d %s), %d %s)\n",this,this->string,f,(f)?f->string:""); fflush(stderr);*/
   for (l = this->friend_list; l; l = l->l) {
      IdP fr = l->f;
/*fprintf(stderr,"fr %d %d %d\n",fr,fr->tp,fr->tp->base); fflush(stderr);*/
      switch (fr->tp->base) {
         case CLASS:
            if (ToClassP(fr->tp)->memtbl == ctbl) return 1;
            break;
         case COBJ:
            if (ToBaseP(fr->tp)->b_table == ctbl) return 1;
            break;
         case FCT:
            if (fr == f) return 1;
            break;
         case OVERLOAD:
         { /*     GenP g = (GenP)fr->tp;
              IdListP ll;
              for (ll=g->fct_list; ll; ll=ll->l) {
              if (ll->f == f) return 1;
              } */
            l->f = fr = ((GenP) fr->tp)->fct_list->f; /* first Fun */
            if (fr == f) return 1;
            break;
         }
         default:
            errorT('i', "bad friend %k", fr->tp->base);
      }
   }
   return 0;
}
