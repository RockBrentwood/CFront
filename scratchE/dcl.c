// 1985 Feb 08 12:48
/* %Z% %M% %I% %H% %T% */
/**************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T Technologies, Inc. All rigths Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T TECHNOLOGIES, INC.

	If you ignore this notice the ghost of Ma Bell will haunt you forever.

dcl.c:

	``declare'' all names, that is insert them in the appropriate symbol tables. //(#) Clipped at "tables".

	Calculate the size for all objects (incl. stack frames),
	and find store the offsets for all members (incl. auto variables).
	"size.h" holds the constants needed for calculating sizes.

	Note that (due to errors) functions may nest

*****************************************************************************/

#include "cfront.h"
#include "size.h"

struct Scope ccvec[MAXCONT], *cc = ccvec;
int byte_offset;
int bit_offset;
int max_align;
int stack_size;
int enum_count;
int friend_in_class;

void check_oper(IdP this, IdP cn) {
   switch (this->n_oper) {
      case CALL:
         if (cn == 0) error("operator() must be aM");
         break;
      case DEREF:
         if (cn == 0) error("operator[] must be aM");
         break;
      case 0:
      case TNAME: /* may be a constructor */
         if (cn && strcmp(cn->string, this->string) == 0) {
            if (this->tp->base == FCT) {
               FunP f = (FunP) this->tp;
               if (f->returns != (TypeP)defa_type && fct_void == 0)
                  error("%s::%s() with returnT", this->string, this->string); //(#) Clipped at "this->string,this->string".
               f->returns = (TypeP)void_type;
               this->string = "_ctor";
               this->n_oper = CTOR;
            } else
               errorT('s', "struct%cnM%n", cn, cn);
         } else
            this->n_oper = 0;
         break;
      case DTOR: /* must be a destructor */
         if (cn == 0) {
            this->n_oper = 0;
            error("destructor ~%s() not inC", this->string);
         } else if (strcmp(cn->string, this->string) == 0) {
            FunP f = (FunP) this->tp;
            this->string = "_dtor";
            if (this->tp->base != FCT) {
               error("%s::~%s notF", cn->string, cn->string);
               this->tp = (TypeP)MakeFun((TypeP)void_type, 0, 1);
            } else if (f->returns != (TypeP)defa_type && fct_void == 0)
               error("%s::~%s() with returnT", cn->string, cn->string); //(#) Clipped at "cn->string,cn->strin".
            if (f->argtype) {
               if (fct_void == 0) error("%s::~%s() withAs", cn->string, cn->string); //(#) Clipped at 'withAs",cn->strin'.
               f->nargs = 0;
               f->nargs_known = 1;
               f->argtype = 0;
            }
            f->returns = (TypeP)void_type;
         } else {
            error("~%s in%s", this->string, cn->string);
            this->n_oper = 0;
         }
         break;
      case TYPE:
         if (cn == 0) {
            error("operator%t() not aM", (TypeP) this->n_initializer);
            this->n_oper = 0;
            this->n_initializer = 0;
         } else {
            FunP f = (FunP) this->tp;
            TypeP tx = (TypeP) this->n_initializer;
/*errorT('d',"operator%t()",tx);*/
            this->n_initializer = 0;
            if (f->base != FCT) error("badT for%n::operator%t()", cn, tx);
            if (f->returns != (TypeP)defa_type) {
               if (checkType(f->returns, tx, 0)) error("bad resultT for%n::operator%t()", cn, tx); //(#) Clipped at "returnT for".
               DEL(Type, f->returns);
            }
            if (f->argtype) {
               error("%n::operator%t() withAs", cn, tx);
               f->argtype = 0;
            }
            f->returns = tx;
            IdP nx = is_cl_obj(tx);
            if (nx && can_coerce(tx, cn->tp)) error("both %n::%n(%n) and %n::operator%t()", cn, cn, nx, tx); //(#) Clipped at "%n(%n) and".
            char buf[128];
            char *bb = signature(tx, buf);
            int l2 = bb - buf - 1;
            char *p = _new((l2 + 1)*sizeof *p);
            strcpy(p, buf);
            this->string = p;
         }
         break;
   }
}

/*
	enter a copy of this name into symbol table "tbl";
		- create local symbol tables as needed

	"scope" gives the scope in which the declaration was found
		- EXTERN, FCT, ARG, PUBLIC, or 0
	Compare "scope" with the specified storage class "n_sto"
		- AUTO, STATIC, REGISTER, EXTERN, OVERLOAD, FRIEND, or 0

	After dclId()
	n_stclass ==	0		class or enum member
			REGISTER	auto variables declared register
			AUTO		auto variables not registers
			STATIC		statically allocated object
	n_scope ==	0		private class member
			PUBLIC		public class member
			EXTERN		name valid in this and other files
			STATIC		name valid for this file only
			FCT		name local to a function
			ARG		name of a function argument
			ARGT		name of a type defined in an argument list

	typecheck function bodies;
	typecheck initializers;

	note that functions (error recovery) and classes (legal) nest

	The return value is used to chain symbol table entries, but cannot
	be used for printout because it denotes the sum of all type information
	for the name

	names of typenames are marked with this->n_oper==TNAME

	WARNING: The handling of scope and storage class is cursed!
*/
IdP dclId(IdP this, TableP tbl, Token scope) {
   IdP nn;
   TypeP nnt = 0;
   IdP odcl = Cdcl;

   if (this == 0) errorT('i', "0->dclId()");
   if (tbl == 0) errorT('i', "%n->dclId(tbl=0,%k)", this, scope);
   if (tbl->base != TABLE) errorT('i', "%n->dclId(tbl=%d,%k)", this, tbl->base, scope); //(#) Clipped at "this,tbl->base,".
   if (this->tp == 0) errorT('i', "dclId(%n,%k)T missing", this, scope);
/*fprintf(stderr,"(%d %s)->dclId(tbl=%d,scope=%d) this->tp = (%d %d)\n",this,this->string,tbl,scope,this->tp,this->tp->base); fflush(stderr);*///(#) Clipped at "tbl,scop".
   Cdcl = this;
   switch (this->base) {
      case TNAME:
         dclType(this->tp, tbl);
         PERM(this->tp);
         nn = MakeId(this->string);
         nn->base = TNAME;
         nn->tp = this->tp;
         insert(tbl, nn, 0);
         FreeId(nn);
         Cdcl = odcl;
         return this;
      case NAME:
         switch (this->n_oper) {
            case TNAME:
               if (this->tp->base != FCT) this->n_oper = 0;
               break;
            case COMPL:
               if (this->tp->base != FCT) {
                  error("~%s notF", this->string);
                  this->n_oper = 0;
               }
               break;
         }
         break;
      default:
         errorT('i', "NX in dclId()");
   }

   if (this->n_qualifier) { /*      class function: c.f(); */
      if (this->tp->base != FCT) {
         error("QdN%n inD of nonF", this);
         Cdcl = odcl;
         return 0;
      }

      IdP cn = this->n_qualifier;
      switch (cn->base) {
         case TNAME:
            break;
         case NAME:
            cn = look(gtbl, cn->string, 0);
            if (cn && cn->base == TNAME) break;
         default:
            error("badQr%n for%n", this->n_qualifier, this);
            Cdcl = odcl;
            return 0;
      }
      cn = ((BaseP) cn->tp)->b_name;
      if (this->n_oper) check_oper(this, cn);

      ClassP cl = (ClassP) cn->tp;
      if (cl == cc->cot) {
         this->n_qualifier = 0;
         goto xdr;
      } else if (cl->defined == 0) {
         error("C%nU", cn);
         Cdcl = odcl;
         return 0;
      }

      TableP etbl = cl->memtbl;
      IdP x = look(etbl, this->string, 0);
      if (x == 0 || x->n_table != etbl) {
         error("%n is not aM of%n", this, cn);
         Cdcl = odcl;
         return 0;
      }
   }
 xdr:
   if (this->n_oper && this->tp->base != FCT && this->n_sto != OVERLOAD)
      error("operator%k not aF", this->n_oper);

/*      if a storage class was specified
   check that it is legal in the scope
   else
   provide default storage class
   some details must be left until the type of the object is known
 */

   this->n_stclass = this->n_sto;
   this->n_scope = scope; /* default scope & storage class */

   if (this->n_sto == 0 && scope == EXTERN) {
      if (scope_default == STATIC) {
         switch (this->tp->base) {
            case FCT:
            {
               FunP f = (FunP) this->tp;
               if (strcmp(this->string, "main"))
                  this->n_scope = (f->body) ? STATIC : EXTERN;
               break;
            }
            case CLASS:
            case ENUM:
            default:
               this->n_scope = STATIC;
         }
      }
   }

   switch (this->n_sto) {
      default:
         errorT('i', "unX %k", this->n_sto);
      case FRIEND:
      {
         ClassP cl = cc->cot;

         switch (scope) {
            case 0:
            case PUBLIC:
               break;
            default:
               error("friend%n not in classD(%k)", this, scope);
               this->base = 0;
               Cdcl = odcl;
               return 0;
         }

         switch (this->n_oper) {
            case 0:
            case NEW:
            case DELETE:
            case CTOR:
            case DTOR:
               this->n_sto = 0;
               break;
            default:
               this->n_sto = OVERLOAD;
         }

         switch (this->tp->base) {
         /* case INT:        undefined: implicitly define as class
               nn = tname(this, CLASS);
               dclType(nn->tp, gtbl);
               break;
          */
            case COBJ:
               nn = ((BaseP) this->tp)->b_name;
               break;
            case CLASS:
               nn = this;
               break;
            case FCT:
               stack(cc);
               cc->not = 0;
               cc->tot = 0;
               cc->cot = 0;
               friend_in_class++;
               nn = dclId(this, gtbl, EXTERN);
               friend_in_class--;
/*fprintf(stderr,"ff %s %d\n",nn->string,nn->tp->base);*/
               unstack(cc);
               if (nn->tp->base == OVERLOAD) {
                  GenP g = (GenP) nn->tp;
                  nn = find(g, (FunP) this->tp);
               }
               break;
            default:
               error("badT%t of friend%n", this->tp, this);
         }
         PERM(nn);
         cl->friend_list = MakeIdList(nn, cl->friend_list);
         Cdcl = odcl;
         return nn;
      }
      case OVERLOAD:
         this->n_sto = 0;
         switch (scope) {
            case 0:
            case PUBLIC:
               errorT('w', "overload inCD (ignored)");
               switch (this->tp->base) {
                  case INT:
                     this->base = 0;
                     Cdcl = odcl;
                     return this;
                  case FCT:
                     return dclId(this, tbl, scope);
               }
         }
         if (this->n_oper && this->tp->base == FCT) break;
         nn = insert(tbl, this, 0);

         if (Nold) {
            if (nn->tp->base != OVERLOAD) {
               error("%n redefined as overloaded", this);
               nn->tp = (TypeP)MakeGen(this->string);
            }
         } else {
            nn->tp = (TypeP)MakeGen(this->string);
         }

         switch (this->tp->base) {
            case INT:
               this->base = 0;
               Cdcl = odcl;
               return nn;
            case FCT:
               break;
            default:
               error("N%n ofT%k cannot be overloaded", this, this->tp->base);
               Cdcl = odcl;
               return nn;
         }
         break;
      case REGISTER:
         if (this->tp->base == FCT) {
            errorT('w', "%n: register (ignored)", this);
            goto ddd;
         }
      case AUTO:
         switch (scope) {
            case 0:
            case PUBLIC:
            case EXTERN:
               error("%k not inF", this->n_sto);
               goto ddd;
         }
         break;
      case EXTERN:
         switch (scope) {
            case ARG:
               error("externA");
               goto ddd;
            case 0:
            case PUBLIC:
            /* extern is provided as a default for functions without body *///(#) Clipped at "without bo".
               if (this->tp->base != FCT) error("externM%n", this);
               goto ddd;
         }
         this->n_stclass = STATIC;
         this->n_scope = EXTERN; /* avoid FCT scoped externs to allow better checking *///(#) Clipped at "to allow better".
         break;
      case STATIC:
         switch (scope) {
            case ARG:
               error("static used forA%n", this);
               goto ddd;
            case 0:
            case PUBLIC:
               this->n_stclass = STATIC;
               this->n_scope = scope;
               break;
            default:
               this->n_scope = STATIC;
         }
         break;
      case 0:
       ddd:
         switch (scope) { /* default storage classes */
            case EXTERN:
               switch (this->tp->base) {
                  case FCT: /* anomaly:     f(int); => extern f(int); *///(#) Clipped at "f(int); *".
                     break;
                  default:
                     this->n_scope = scope_default;
               }
               this->n_stclass = STATIC;
               break;
            case FCT:
               if (this->tp->base == FCT) {
                  this->n_stclass = STATIC;
                  this->n_scope = EXTERN;
               } else
                  this->n_stclass = AUTO;
               break;
            case ARG:
               if (this->tp->base == FCT) error("%n asA", this);
               this->n_stclass = AUTO;
               break;
            case 0:
            case PUBLIC:
               this->n_stclass = 0;
               break;
         }
   }

/*
   now insert the name into the appropriate symbol table,
   and compare types with previous declarations of that name

   do type dependent adjustments of the scope
 */

   switch (this->tp->base) {
      case ASM:
      {
         BaseP b = (BaseP) this->tp;
         IdP n = insert(tbl, this, 0);
         assign(n);
         use(n);
         return this;
      }

      case CLASS:
      {
         ClassP cl;
         BaseP bt;
         IdP bn;
         ClassP nest;
         IdP nx = look(ktbl, this->string, 0); /* TNAME */
/*fprintf(stderr,"%s: nx %d\n",this->string,nx);*/
         if (nx == 0) {
         /*      search for hidden name for
            (1) nested class declaration
            (2) local class declaration
          */
            for (nx = look(ktbl, this->string, HIDDEN); nx; nx = nx->n_tbl_list) {
               if (nx->n_key != HIDDEN) continue;
               if (nx->tp->base != COBJ) continue;
               bt = (BaseP) nx->tp;
               bn = bt->b_name;
               cl = (ClassP) bn->tp;
               if (cl == 0) continue;
               if ((nest = cl->in_class) && nest == cc->cot)
                  goto bbb;
               else if (cc->nof /* fudge */
                  && cc->nof->where.line < nx->where.line)
                  goto bbb;
            }
            errorT('i', "%n is not aTN", this);
         } else {
            bt = (BaseP) nx->tp; /* COBJ */
            bn = bt->b_name;
            nest = 0;
         }
       bbb:
/*fprintf(stderr,"bbb: bt %d %d\n",bt,bt->base); fflush(stderr);*/
         bn->where = nx->where;
         IdP bnn = insert(tbl, bn, CLASS); /*copy for member lookup */
         cl = (ClassP) bn->tp;
      /* CLASS */
/*fprintf(stderr,"cl %d %d\n",cl,cl->base); fflush(stderr);*/
         if (cl->defined)
            error("C%n defined twice", this);
         else {
            if (bn->n_scope == ARG) bn->n_scope = ARGT;
            dclClass(cl, bn, tbl);
            if (nest) {
               int l1 = strlen(cl->string);
               int l2 = strlen(nest->string);
               char *s = _new((l1 + l2 + 2)*sizeof *s);
               strcpy(s, nest->string);
               s[l2] = '_';
               strcpy(s + l2 + 1, cl->string);
               cl->string = s;
            /*      cl->memtbl->t_name->string = s; */
            }
         }
         this->tp = (TypeP)cl;
         Cdcl = odcl;
         return bnn;
      }

      case ENUM:
      {
         IdP nx = look(ktbl, this->string, 0); /* TNAME */
         if (nx == 0) {
            nx = look(ktbl, this->string, HIDDEN); /* hidden TNAME */
         }
         BaseP bt = (BaseP) nx->tp; /* EOBJ */
         IdP bn = bt->b_name;
         IdP bnn = insert(tbl, bn, CLASS);
         EnumP en = (EnumP) bn->tp; /* ENUM */
         if (en->defined)
            error("enum%n defined twice", this);
         else {
            if (bn->n_scope == ARG) bn->n_scope = ARGT;
            dclEnum(en, bn, tbl);
         }
         this->tp = (TypeP)en;
         Cdcl = odcl;
         return bnn;
      }

      case FCT:
      {
         FunP f = (FunP) this->tp;
         IdP class_name;
         TableP etbl;
         int can_overload;
         int in_class_dcl = (int)cc->not;
         int just_made = 0;

         if (f->f_inline) this->n_sto = STATIC;

         if (f->argtype) {
            IdP a;
            int oo = const_save;
            const_save = 1;
            for (a = f->argtype; a; a = a->n_list) {
               ExP init;
               if (init = a->n_initializer) {
                  int i = 0;
                  init = typ(init, tbl);
                  if (checkType(a->tp, init->tp, ARG) == 0 || (i = can_coerce(a->tp, init->tp))) {
                     if (1 < i) error("%d possible conversions for defaultA", i); //(#) Clipped at "possible conve".
                     if (Ncoerce) {
                        IdP cn = is_cl_obj(init->tp); //(#) Clipped at "init->tp->is_cl_".
                        ClassP cl = (ClassP) cn->tp;
                        RefP r = MakeRef(DOT, init, Ncoerce); //(#) Clipped at "(DOT,init,N".
                        init = MakeEx(G_CALL, (ExP)r, 0); //(#) Clipped at "(G_CALL,r,0)".
                        init->fct_name = Ncoerce;
                        init->tp = a->tp;
                     }
                     simplEx(init);
                     init->permanent = 2;
                     a->n_initializer = init;
                  } else {
                     error("badIrT%t forA%n", init->tp, a);
                     DEL(Ex, init);
                     a->n_initializer = 0;
                  }
               }

             flatten1:
               switch (a->tp->base) {
                  case TYPE:
                     a->tp = ((BaseP) a->tp)->b_name->tp;
                     goto flatten1;
                  case CHAR:
                  case SHORT:
                  /* errorT('w',"A ofT%k (becomes int)",a->tp->base); *///(#) Clipped at "a->tp->ba".
                     a->tp = (TypeP)int_type;
                     break;
                  case FLOAT:
                  /* errorT('w',"A ofT float (becomes double)");      *///(#) Clipped at 'double)"); '.
                     a->tp = (TypeP)double_type;
                     break;
               }
            }
            const_save = oo;
         }

         dclType(this->tp, tbl); /* must be done before the type check */

         if (this->n_qualifier) { /* qualified name: c.f() checked above */
            if (in_class_dcl) {
               error("unXQN%n", this);
               Cdcl = odcl;
               return 0;
            }
            class_name = ((BaseP) this->n_qualifier->tp)->b_name;
            etbl = ((ClassP) class_name->tp)->memtbl;
         } else {
            class_name = cc->not;
         /* beware of local function declarations in member functions *///(#) Clipped at "member function".
            if (class_name && tbl != cc->cot->memtbl) {
               class_name = 0;
               in_class_dcl = 0;
            }
            if (this->n_oper) check_oper(this, class_name);
            etbl = tbl;
         }

         if (etbl == 0 || etbl->base != TABLE) errorT('i', "N.dcl: etbl=%d", etbl);

         switch (this->n_oper) {
            case NEW:
            case DELETE:
               switch (scope) {
                  case 0:
                  case PUBLIC:
                     error("%nMF", this);
               }
            case 0:
               can_overload = in_class_dcl;
               break;
            case CTOR:
               if (f->f_virtual) {
                  error("virtual constructor");
                  f->f_virtual = 0;
               }
            case DTOR:
               if (fct_void) this->n_scope = PUBLIC;
               can_overload = in_class_dcl;
               break;
            default:
               can_overload = 1; /* all operators are overloaded */
         }

         switch (scope) {
            case FCT:
            case ARG:
            {
               IdP nx = insert(gtbl, this, 0);
               this->n_table = 0;
               this->n_tbl_list = 0;
            /* no break */
            }
            default:
               nn = insert(etbl, this, 0);
               assign(nn);
               this->n_table = etbl;
               break;
         }

         if (Nold) {
            FunP nf = (FunP) nn->tp;
/*errorT('d',"%n: tp%t nf%t",nn,this->tp,nf);*/
            if (nf->base == ANY || f->base == ANY);
            else if (nf->base == OVERLOAD) {
               GenP g = (GenP) nf;
               nn = addGen(g, this, 0);
               this->string = nn->string;
               if (Nold == 0) {
                  if (f->body) {
                     if (this->n_qualifier) {
                        errorT(0, "badAL for overloaded %n::%s()", this->n_qualifier, g->string); //(#) Clipped at "for overload".
                        Cdcl = odcl;
                        return 0;
                     } else if (f->f_inline == 0 && this->n_oper == 0) //(#) Clipped at "this->n_oper==".
                        errorT('w', "overloaded %n defined without being previously declared", nn); //(#) Clipped at "%n de".
                  }
                  goto thth;
               } else {
                  if (f->body == 0 && friend_in_class == 0) errorT('w', "overloaded%n redeclared", nn); //(#) Clipped at "error".
               }

               nf = (FunP) nn->tp;

               if (f->body && nf->body) {
                  error("two definitions of overloaded%n", nn);
                  Cdcl = odcl;
                  return 0;
               }

               if (f->body) goto bdbd;

               goto stst;
            } else if (nf->base != FCT) {
               error("%n declared both as%t and asF", this, nf);
               f->body = 0;
            } else if (can_overload) {
               if (checkType((TypeP)nf, (TypeP)f, OVERLOAD) || vrp_equiv) {
                  if (f->body && this->n_qualifier) {
                     error("badAT for%n", nn);
                     Cdcl = odcl;
                     return 0;
                  }
                  GenP g = MakeGen(this->string);
                  IdP n1 = addGen(g, nn, in_class_dcl);
                  IdP n2 = addGen(g, this, 0);
/*errorT('d',"n1%n n2%n\n",n1,n2);*/
                  nn->tp = (TypeP) g;
                  nn->string = g->string;
                  nn = n2;
                  goto thth;
               }

               if (in_class_dcl) {
                  error("two declarations of%n", this);
                  f->body = 0;
                  Cdcl = odcl;
                  return 0;
               }

               if (nf->body && f->body) {
                  error("two definitions of%n", this);
                  f->body = 0;
                  Cdcl = odcl;
                  return 0;
               }

               if (f->body) goto bdbd;

               goto stst;
            } else if (checkType((TypeP)nf, (TypeP)f, 0)) {
               switch (this->n_oper) {
                  case CTOR:
                  case DTOR:
                     f->s_returns = nf->s_returns;
               }
               error("%nT mismatch:%t and%t", this, nf, f);
               f->body = 0;
            } else if (nf->body && f->body) {
               error("two definitions of%n", this);
               f->body = 0;
            } else if (f->body) {
               IdP a1, a2;
             bdbd:
               if (f->nargs_known && nf->nargs_known)
                  for (a1 = f->argtype, a2 = nf->argtype; a1; a1 = a1->n_list, a2 = a2->n_list) { //(#) Clipped at "a1=a1->n_li".
                     int i1 = a1->n_initializer || a1->n_evaluated; //(#) Clipped at "a1->n_evaluat".
                     int i2 = a2->n_initializer || a2->n_evaluated; //(#) Clipped at "a2->n_evaluat".
                     if (i1) {
                        if (i2 && (a1->n_evaluated == 0 || a2->n_evaluated == 0 || a1->n_val != a2->n_val)
                           )
                           error("twoIrs for%nA%n", nn, a1); //(#) Clipped at ",nn,".
                     } else if (i2) {
                        a1->n_initializer = a2->n_initializer; //(#) Clipped at "n_initializ".
                        a1->n_evaluated = a2->n_evaluated;
                        a1->n_val = a2->n_val;
                     }
                  }
               f->f_virtual = nf->f_virtual;
               f->f_this = nf->f_this;
            /*fprintf(stderr,"bdbd %s: f %d inl %d nf %d inl %d\n",this->string,f,f->f_inline,nf,nf->f_inline);*///(#) Clipped at "nf,nf->f".
               nn->tp = (TypeP)f;
               if (f->f_inline) {
                  if (nf->f_inline == 0 && nn->n_used) error("%n called before defined as inline", nn); //(#) Clipped at 'error("%'.
                  nf->f_inline = 1;
                  nn->n_sto = STATIC;
               } else if (nf->f_inline) {
               /*error("%n defined as inline but not declared as inline",this); *///(#) Clipped at "but not decla".
                  f->f_inline = 1;
               }
               goto stst2;
            } else { /* two declarations */
               IdP a1, a2;
               f->f_this = nf->f_this;
             stst:
               if (f->nargs_known && nf->nargs_known)
                  for (a1 = f->argtype, a2 = nf->argtype; a1; a1 = a1->n_list, a2 = a2->n_list) { //(#) Clipped at "a1=a1->n_li".
                     int i1 = a1->n_initializer || a1->n_evaluated; //(#) Clipped at "a1->n_evaluat".
                     int i2 = a2->n_initializer || a2->n_evaluated; //(#) Clipped at "a2->n_evaluat".
                     if (i1) {
                        if (i2) {
                           if (a1->n_evaluated == 0 || a2->n_evaluated == 0 || a1->n_val != a2->n_val)
                              error("twoIrs for%nA%n", nn, a1); //(#) Clipped at "twoIrs for%n".
                        } else if (class_name)
                           error("defaultA for%n", nn);
                     } else if (i2) {
                        a1->n_initializer = a2->n_initializer; //(#) Clipped at "a2->n_initializ".
                        a1->n_evaluated = a2->n_evaluated;
                        a1->n_val = a2->n_val;
                     }
                  }
             stst2:
               if (f->f_inline) this->n_sto = STATIC;
               if (this->n_sto) {
                  if (nn->n_scope != this->n_sto && f->f_inline == 0)
                     error("%n both%k and%k", this, this->n_sto, nn->n_scope); //(#) Clipped at "this,this->n_sto,".
               } else {
                  if (nn->n_scope == STATIC && this->n_scope == EXTERN) error("%n both static and extern", this); //(#) Clipped at "==EXTERN)". Not verified.
               }
               this->n_scope = nn->n_scope; /* first specifier wins */
            /* this->n_sto = nn->n_sto; */

            }
         /*      ((FunP)nn->tp)->nargs_known = nf->nargs_known;  */
         } else { /* new function: make f_this for member functions */
          thth:
            just_made = 1;
            if (f->f_inline) nn->n_sto = STATIC;
/*fprintf(stderr,"thth %s: f %d nn->tp %d inl %d\n",this->string,f,nn->tp,f->f_inline);*/
            if (class_name && etbl != gtbl) { /* beware of implicit declaration *///(#) Clipped at "implicit decla".
               IdP cn = nn->n_table->t_name;
               IdP tt = MakeId("this");
               tt->n_scope = ARG;
               tt->n_sto = REGISTER;
               tt->tp = ((ClassP) class_name->tp)->this_type;
               PERM(tt);
               ((FunP) nn->tp)->f_this = f->f_this = tt;
               tt->n_list = f->argtype;
            }

            if (f->f_virtual) {
               switch (nn->n_scope) {
                  default:
                     error("nonC virtual%n", this);
                     break;
                  case 0:
                  case PUBLIC:
                     cc->cot->virt_count = 1;
                     ((FunP) nn->tp)->f_virtual = 1;
                     break;
               }
            }
         }

      /*      an operator must take at least one class object or
         reference to class object argument
       */
         switch (this->n_oper) {
            case CTOR:
               if (f->nargs == 1) { /* check for X(X) and X(X&) */
                  TypeP t = f->argtype->tp;
                clll:
                  switch (t->base) {
                     case TYPE:
                        t = ((BaseP) t)->b_name->tp;
                        goto clll;
                     case RPTR: /* X(X&) ? */
                        t = ((PtrP) t)->typ;
                      cxll:
                        switch (t->base) {
                           case TYPE:
                              t = ((BaseP) t)->b_name->tp;
                              goto cxll;
                           case COBJ:
                              if (class_name == ((BaseP) t)->b_name) //(#) Clipped at "->b_nam".
                                 ((ClassP) class_name->tp)->itor = nn; //(#) Clipped at "->i".
                        }
                        break;
                     case COBJ: /* X(X) ? */
                        if (class_name == ((BaseP) t)->b_name)
                           error("impossible constructor: %s(%s)", class_name->string, class_name->string); //(#) Clipped at "%s(%".
                  }
               }
               break;
            case TYPE:
/*errorT('d',"just_made %d %n",just_made,this);*/
               if (just_made) {
                  nn->n_list = ((ClassP) class_name->tp)->conv;
                  ((ClassP) class_name->tp)->conv = nn;
               }
               break;
            case DTOR:
            case NEW:
            case DELETE:
            case CALL:
            case 0:
               break;
            default:
               if (f->nargs_known != 1) {
                  error("ATs must be fully specified for%n", nn);
               } else if (class_name == 0) {
                  IdP a;
                  switch (f->nargs) {
                     case 1:
                     case 2:
                        for (a = f->argtype; a; a = a->n_list) {
                           TypeP tx = a->tp;
                           if (tx->base == RPTR) tx = ((PtrP) tx)->typ; //(#) Clipped at "((PtrP)t".
                           if (is_cl_obj(tx)) goto cok;
                        }
                        error("%n must take at least oneCTA", nn);
                        break;
                     default:
                        error("%n must take 1 or 2As", nn);
                  }
               } else {
                  switch (f->nargs) {
                     case 0:
                     case 1:
                        break;
                     default:
                        error("%n must take 0 or 1As", nn);
                  }
               }
             cok:;
         }

      /*
         the body cannot be checked until the name
         has been checked and entered into its table
       */
         if (f->body) dclFun(f, nn);
         break;
      }

      case FIELD:
      {
         BaseP fld = (BaseP) this->tp;
         char x;

         if (cc->not == 0 || cc->cot->csu == UNION) {
            if (cc->not)
               error("field in union");
            else
               error("field not inC");
            PERM(this->tp);
            Cdcl = odcl;
            return this;
         }

         if (this->string) {
            nn = insert(tbl, this, 0);
            this->n_table = nn->n_table;
            if (Nold) error("twoDs of field%n", this);
         }

         dclType(this->tp, tbl);
         if (fld->b_bits == 0) { /* force word alignment */
            int b;
            if (bit_offset)
               fld->b_bits = BI_IN_WORD - bit_offset;
            else if (b = byte_offset % SZ_WORD)
               fld->b_bits = b * BI_IN_BYTE;
         }
         x = bit_offset += fld->b_bits;
         if (BI_IN_WORD < x) {
            fld->b_offset = 0;
            byte_offset += SZ_WORD;
            bit_offset = fld->b_bits;
         } else {
            fld->b_offset = bit_offset;
            if (BI_IN_WORD == x) {
               bit_offset = 0;
               byte_offset += SZ_WORD;
            } else
               bit_offset = x;
         }
         this->n_offset = byte_offset;
         break;
      }

      case COBJ:
      {
         ClassP cl = (ClassP) ((BaseP) this->tp)->b_name->tp;
      /*fprintf(stderr,"COBJ %d %s -> (%d %d)\n",this->tp,((BaseP)this->tp)->b_name->string,cl,cl->base); fflush(stderr);*///(#) Clipped at "cl->bas".
         if (cl->csu == ANON) { /* export member names to enclosing scope *///(#) Clipped at "scope *".
            IdP nn;
            int i;
            int uindex;
            TableP mtbl = cl->memtbl;
            char *p = cl->string;

            if (tbl == gtbl) errorT('s', "global anonymous union");
            while (*p++ != 'C'); /* UGH!!! */
            uindex = str_to_int(p);
            for (nn = get_mem(mtbl, i = 1); nn; nn = get_mem(mtbl, ++i)) {
               TableP tb = nn->n_table;
               nn->n_table = 0;
               IdP n = insert(tbl, nn, 0);
               n->n_union = uindex;
               nn->n_table = tb;
            }
         }
         goto cde;
      }

      case VEC:
      case PTR:
      case RPTR:
         dclType(this->tp, tbl);

      default:
       cde:
         nn = insert(tbl, this, 0);

         this->n_table = nn->n_table;
/*errorT('d',"Nold %d tbl %d nn %d%n tp%t",Nold,tbl,nn,nn,nn->tp);*/
         if (Nold) {
            if (nn->tp->base == ANY) goto zzz;
            if (checkType(this->tp, nn->tp, 0)) {
               error("twoDs of%n;Ts:%t and%t", this, nn->tp, this->tp);
               Cdcl = odcl;
               return 0;
            }

            if (this->n_sto && this->n_sto != nn->n_scope)
               error("%n both%k and%k", this, this->n_sto, nn->n_scope);
            else if (nn->n_scope == STATIC && this->n_scope == EXTERN)
               error("%n both%k and%k", this, this->n_sto, nn->n_scope);
            else if (nn->n_scope == STATIC)
               error("static%n declared twice", this);

         /* this->n_sto = nn->n_sto;       first scope specifier wins */
            this->n_scope = nn->n_scope;

            switch (scope) {
               case FCT:
                  if (nn->n_stclass == STATIC && this->n_stclass == STATIC) break; //(#) Clipped at "bre". Not verified.
                  error("twoDs of%n", this);
                  Cdcl = odcl;
                  return 0;
               case ARG:
                  error("two arguments%n", this);
                  Cdcl = odcl;
                  return 0;
               case 0:
               case PUBLIC:
                  error("twoDs ofM%n", this);
                  Cdcl = odcl;
                  return 0;
            }
/* n_val */
            if (this->n_initializer) {
               if (nn->n_initializer) error("twoIrs for%n", this);
               nn->n_initializer = this->n_initializer;
            }
         }

       zzz:
         if (this->base != TNAME) {
            TypeP t = nn->tp;
         /*fprintf(stderr,"tp %d %d nn->tp %d %d\n",this->tp,this->tp->base,nn->tp,nn->tp?nn->tp->base:0); fflush(stderr);*///(#) Clipped at "nn->tp->base;0)".
            switch (nn->n_stclass) {
               default:
                  switch (t->base) {
                     case FCT:
                     case OVERLOAD:
                        break;
                     default:
                     {
                        int x = align(t);
                        int y = tsizeof(t);

                        if (max_align < x) max_align = x;

                        while (0 < bit_offset) {
                           byte_offset++;
                           bit_offset -= BI_IN_BYTE;
                        }
                        bit_offset = 0;

                        if (byte_offset && 1 < x) byte_offset = ((byte_offset - 1) / x) * x + x; //(#) Clipped at "= ((byt".
                        nn->n_offset = byte_offset;
                        byte_offset += y;
                     }
                  }
                  break;
               case STATIC:
                  switch (t->base) {
                     case FCT:
                     case OVERLOAD:
                        break;
                     default:
                        tsizeof(t); /* check that size is known *///(#) Clipped at "is known".
                  }
                  break;
            }
         }

         {
            TypeP t = nn->tp;
            int const_old = const_save;
            bit vec_seen = 0;
            ExP init = this->n_initializer;

            if (init) {
               switch (this->n_scope) {
                  case 0:
                  case PUBLIC:
                     if (this->n_stclass != STATIC) error("Ir forM%n", this);
                     break;
               }
            }

         /* if (this->n_scope == EXTERN) break;           */

          lll:
            switch (t->base) {
               case RPTR:
/*fprintf(stderr,"RPTR init=%d\n",init);*/
                  if (init) {
                     init = typ(init, tbl);
                     nn->n_initializer = this->n_initializer = ref_init((PtrP) t, init, tbl); //(#) Clipped at "ref_init((PtrP)".
                     assign(nn);
                  } else {
                     switch (nn->n_scope) {
                        default:
                           error("unId reference%n", this);
                           break;
                        case ARG:
                        case PUBLIC:
                        case 0:
                           break;
                     }
                  }
                  break;
               case COBJ:
               /*fprintf(stderr,"COBJ %s init=%d scope %d n_scope %d\n",this->string,init,scope,nn->n_scope);*///(#) Clipped at "nn->n_sco". Not verified.
               /*      TEMPORARY fudge
                  to allow initialization of
                  global objects
                */
                  if (init && st_init == 0)
                     switch (nn->n_scope) {
                        case EXTERN:
                        case STATIC:
                           if (init->base == ILIST) goto str;
                     }
                  {
                     IdP cn = ((BaseP) t)->b_name;
                     ClassP cl = (ClassP) cn->tp;
                     IdP ctor = has_ctor(cl);
                     IdP dtor = has_dtor(cl);
                     if (dtor) {
                        StP dls;
                        switch (nn->n_scope) {
                           case EXTERN:
                              if (this->n_sto == EXTERN) break;
                           case STATIC:
                              if (st_init == 0) {
                                 if (ctor == 0) errorT('s', "staticO %n", this); //(#) Clipped at "staticO %n". Not verified.
                                 break;
                              }
                              if (vec_seen) { /*  _vec_delete(vec,noe,sz,dtor,0); *///(#) Clipped at "noe,sz,".
                                 int esz = tsizeof((TypeP)cl);
                                 ExP noe = MakeEx(IVAL, (ExP) (tsizeof(nn->tp) / esz), 0); //(#) Clipped at "IVAL, (ExP)(".
                                 ExP sz = MakeEx(IVAL, (ExP) esz, 0); //(#) Clipped at "(ExP)esz".
                                 ExP arg = MakeEx(ELIST, (ExP)dtor, 0);
                                 lval((ExP)dtor, ADDROF);
                                 arg = MakeEx(ELIST, sz, arg);
                                 arg = MakeEx(ELIST, noe, arg);
                                 arg = MakeEx(ELIST, (ExP)nn, arg);
                                 arg = (ExP)MakeCall((ExP)vec_del_fct, arg);
                                 arg->base = G_CALL;
                                 arg->fct_name = vec_del_fct;
                                 dls = (StP)MakeESt(SM, nn->where, arg, 0);
                              } else { /* nn->cl.~cl(0); */
                                 RefP r = MakeRef(DOT, (ExP)nn, dtor);
                                 ExP ee = MakeEx(ELIST, zero, 0);
                                 CallP dl = MakeCall((ExP)r, ee);
                                 dls = (StP)MakeESt(SM, nn->where, (ExP)dl, 0);
                                 dl->base = G_CALL;
                                 dl->fct_name = dtor;
                              }
                              if (st_dlist) dls->s_list = st_dlist;
                              st_dlist = dls;
                        }
                     }
                     if (ctor) {
                        ExP oo = (vec_seen) ? contents((ExP)nn) : (ExP)nn;
/*errorT('d',"ctor init=%d n_scope=%d",init,nn->n_scope);*/
                        switch (nn->n_scope) {
                           case EXTERN:
                              if (init == 0 && this->n_sto == EXTERN) goto ggg;
                           case STATIC:
                              if (st_init == 0) {
                                 errorT('s', "staticO%n ofC%n that has a constructor", this, nn); //(#) Clipped at "that has". Not verified.
                                 nn->n_initializer = this->n_initializer = 0; //(#) Clipped at "= this->n_initializer =". Not verified.
                                 goto ggg;
                              }
                           default:
                              if (vec_seen && init) error("Ir forCO%n[]", this); //(#) Clipped at "%n[]".
                              break;
                           case ARG:
                              if (init == 0) goto ggg;
                           case PUBLIC:
                           case 0:
                              init = (ExP)MakeTEx(VALUE, (TypeP)cl, 0);
                              init->e2 = oo;
                              nn->n_initializer = this->n_initializer = init = 0; //(#) Clipped at "this->n_initializer = init = ". Not verified.
                              goto ggg;
                        }
                        const_save = 1;
                        assign(nn);
                        if (init) {
                           if (init->base == VALUE && init->tp2 == (TypeP)cl) {
                              init->e2 = oo;
                              init = typ(init, tbl);
                           } else {
                              init = typ(init, tbl);
                              init = class_init((ExP)nn, nn->tp, init, tbl); //(#) Clipped at "nn->tp,init,tb".
                           }
                        } else {
                           init = (ExP)MakeTEx(VALUE, (TypeP)cl, 0);
                           init->e2 = oo;
                           init = typ(init, tbl);
                        }
                        if (init && st_init) {
                           switch (nn->n_scope) {
                              case EXTERN:
                              case STATIC:
                                 if (vec_seen) { /*  _vec_new(vec,noe,sz,ctor); *///(#) Clipped at "(vec,no".
                                    IdP c = has_ictor(cl);
                                    if (c == 0) error("vector ofC%n that do not have a constructor taking noAs", cn); //(#) Clipped at "vector o". Not verified.
                                    int esz = tsizeof((TypeP)cl);
                                    ExP noe = MakeEx(IVAL, (ExP) (tsizeof(nn->tp) / esz), 0); //(#) Clipped at "(IVAL,(".
                                    ExP sz = MakeEx(IVAL, (ExP) esz, 0); //(#) Clipped at "(IVAL,(P".
                                    ExP arg = MakeEx(ELIST, (ExP)c, 0); //(#) Clipped at "(ELIST,".
                                    lval((ExP)c, ADDROF);
                                    arg = MakeEx(ELIST, sz, arg); //(#) Clipped at "sz,arg".
                                    arg = MakeEx(ELIST, noe, arg); //(#) Clipped at "noe,ar".
                                    arg = MakeEx(ELIST, (ExP)nn, arg); //(#) Clipped at "nn,arg".
                                    init = (ExP)MakeCall((ExP)vec_new_fct, arg); //(#) Clipped at "vec_new_fct".
                                    init->base = G_CALL;
                                    init->fct_name = vec_new_fct; //(#) Clipped at "vec_new_fc".
                                 }
                                 {
                                    StP ist = (StP)MakeESt(SM, nn->where, init, 0); //(#) Clipped at "nn->where,".
                                    static StP itail = 0;
                                    if (st_ilist == 0)
                                       st_ilist = ist;
                                    else
                                       itail->s_list = ist;
                                    itail = ist;
                                    init = 0;
                                 }
                           }
                        }
                        nn->n_initializer = this->n_initializer = init;
                        const_save = const_old;
                     } else if (init == 0) /* no initializer */
                        goto str;
                     else if (is_simple(cl)) /* struct */
                        goto str;
                     else { /* bitwise copy ok? */
                        init = typ(init, tbl);
                        if (checkType(nn->tp, init->tp, ASSIGN) == 0)
                           goto str;
                        else
                           error("cannotI%n:C %s has privateMs but no constructor", nn, cl->string); //(#) Clipped at "but no ".
                     }
                     break;
                  }
               case VEC:
                  t = ((VecP) t)->typ;
                  vec_seen = 1;
                  goto lll;
               case TYPE:
                  t = ((BaseP) t)->b_name->tp;
                  goto lll;
               default:
                str:
                  if (init == 0) {
                     switch (this->n_scope) {
                        case ARG:
                        case 0:
                        case PUBLIC:
                           break;
                        default:
                           if (this->n_sto != EXTERN && tconst(t))
                              errorT('w', "unId const%n", this);
                     }

                     break;
                  }

                  const_save = const_save || this->n_scope == ARG || (tconst(t) && vec_const == 0); //(#) Clipped at " && ".
                  nn->n_initializer = this->n_initializer = init = typ(init, tbl);
                  if (const_save) PERM(init);
                  assign(nn);
                  const_save = const_old;

                  switch (init->base) {
                     case ILIST:
                        new_list(init);
                        list_check(nn, nn->tp, 0);
                        if (next_elem()) error("IrL too long");
                        break;
                     case STRING:
                        if (nn->tp->base == VEC) {
                           VecP v = (VecP) nn->tp;
                           if (v->typ->base == CHAR) {
                           /* errorT('w',"\"char[] = this->string\""); */
                              v->size = ToVecP(init->tp)->size;
                              break;
                           }
                        }
                     default:
                     {
                        TypeP nt = nn->tp;

                        if (vec_seen) {
                           error("badIr for vector%n", nn);
                           break;
                        }
                      tlx:
                        switch (nt->base) {
                           case TYPE:
                              nt = ((BaseP) nt)->b_name->tp;
                              goto tlx;
                           case INT:
                           case CHAR:
                           case SHORT:
                              if (init->base == ICON && init->tp == (TypeP)long_type) //(#) Clipped at "==long_type".
                                 errorT('w', "longIr constant for%k%n", nn->tp->base, nn); //(#) Clipped at '"longIr constant for%k%n"'.
                           case LONG:
                              if (((BaseP) nt)->b_unsigned && init->base == UMINUS && init->e2->base == ICON)
                                 errorT('w', "negativeIr for unsigned%n", nn); //(#) Clipped at "unsigned%".
                              if (((BaseP) nt)->b_const) {
                                 int i;
                                 Neval = 0;
                                 i = eval(init);
                                 if (Neval == 0) {
                                    DEL(Ex, init);
                                    nn->n_evaluated = this->n_evaluated = 1; //(#) Clipped at "= n_evaluat".
                                    nn->n_val = this->n_val = i;
                                    nn->n_initializer = this->n_initializer = 0; //(#) Clipped at "= n_initi".
                                 /* if (i) {
                                       nn->n_initializer = 0; //(#) Clipped at "this->n_initializer =". Not verified.
                                       nn->n_val = i;
                                       this->n_initializer = 0;
                                       this->n_val = i;
                                    } else {
                                       nn->n_initializer = 0; //(#) Clipped at "this->n_initializer =". Not verified.
                                       this->n_initializer = zero; //(#) Clipped at "= zer". Not verified.
                                    }
                                  */
                                 }
                              }
                              goto cvcv;
                           case PTR:
                           {
                              FunP ef = (FunP) ((PtrP) nt)->typ;
                              if (ef->base == FCT) {
                                 FunP f;
                                 IdP n = 0;
                                 switch (init->base) {
                                    case NAME:
                                       f = (FunP) init->tp;
                                       n = ToIdP(init);
                                       switch (f->base) {
                                          case FCT:
                                          case OVERLOAD:
                                             init = MakeEx(G_ADDROF, 0, init);
                                             init->tp = (TypeP)f;
                                       }
                                       goto ad;
                                    case DOT:
                                    case REF:
                                       f = (FunP) init->mem->tp;
                                       switch (f->base) {
                                          case FCT:
                                          case OVERLOAD:
                                             n = ToIdP(init->mem);
                                             init = MakeEx(G_ADDROF, 0, init);
                                             init = typ(init, tbl);
                                       }
                                       goto ad;
                                    case ADDROF:
                                    case G_ADDROF:
                                       f = (FunP) init->e2->tp;
                                     ad:
                                       if (f->base == OVERLOAD) {
                                          GenP g = (GenP) f;
                                          n = find(g, ef);
                                          if (n == 0) {
                                             error("cannot deduceT for &overloaded %s()", g->string); //(#) Clipped at ""deduceT for &".
                                          }
                                          init->e2 = (ExP)n;
                                          this->n_initializer = init;
                                          lval((ExP)n, ADDROF);
                                          goto stgg;
                                       }
                                       if (n) lval((ExP)n, ADDROF);
                                 }
                              }
                           }
                        }
                      cvcv:
                        {
                           IdP cn;
                           int i;
                           if ((cn = is_cl_obj(init->tp))
                              && (i = can_coerce(nt, init->tp))
                              && Ncoerce) {
                              if (1 < i) error("%d possible conversions forIr");
/*errorT('d',"dcl %t<-%t",nt,init->tp);*/
                              ClassP cl = (ClassP) cn->tp;
                              RefP r = MakeRef(DOT, init, Ncoerce);
                              ExP c = MakeEx(G_CALL, (ExP)r, 0);
                              c->fct_name = Ncoerce;
                              c->tp = nt;
                              this->n_initializer = c;
                              goto stgg;
                           }
                        }
                        if (checkType(nt, init->tp, ASSIGN))
                           error("badIrT%t for%n (%tX)", init->tp, this, nn->tp); //(#) Clipped at "init->tp,this,".
                        else {
                         stgg:
                           if (init && this->n_stclass == STATIC) {
                           /* check if non-static variables are used *///(#) Clipped at "are used *".
                           /* INCOMPLETE */
                              switch (init->base) {
                                 case NAME:
                                    if (tconst(init->tp) == 0) error("v%n used inIr for%n", init, nn); //(#) Clipped at 'error("v'. Not verified.
                                    break;
                                 case DEREF:
                                 case DOT:
                                 case REF:
                                 case CALL:
                                 case G_CALL:
                                    error("%k inIr of static%n", init->base, nn); //(#) Clipped at ",init->b".
                              }
                           }
                        }
                     }
                  } /* switch */
            } /* block */
         } /* default */

   } /* switch */
 ggg:
   PERM(nn);
   switch (this->n_scope) {
      case FCT:
         nn->n_initializer = this->n_initializer;
         break;
      default:
      { /*     ExP ii = nn->n_initializer; */
         TypeP t = nn->tp;
      /*      if (ii) PERM(ii); */
       px:
         PERM(t);
         switch (t->base) {
            case PTR:
            case RPTR:
               t = ((PtrP) t)->typ;
               goto px;
            case VEC:
               t = ((VecP) t)->typ;
               goto px;
            case TYPE:
               t = ((BaseP) t)->b_name->tp;
               goto px;
            case FCT:
               t = ((FunP) t)->returns;
               goto px; /* args? */
         }
      }
   }

   Cdcl = odcl;
   return nn;
}

int inline_restr; /* report use of constructs that the inline expanded cannot
                     handle here
                   */

void dclFun(FunP this, IdP n) {
   int nmem = TBLSIZE;
   IdP a;
   IdP ll;
   TableP ftbl;

   PtrP cct = 0;
   int const_old = const_save;

   int bit_old = bit_offset;
   int byte_old = byte_offset;
   int max_old = max_align;
   int stack_old = stack_size;

   if (this->base != FCT) errorT('i', "dclFun(%d)", this->base);
   if (this->body == 0 || this->body->memtbl) errorT('i', "dclFun(body=%d)", this->body);
   if (n == 0 || n->base != NAME) errorT('i', "dclFun(name=%d %d)", n, (n) ? n->base : 0);

   this->body->memtbl = ftbl = MakeTable(nmem + 3, n->n_table, 0);
   this->body->own_tbl = 1;

   max_align = AL_FRAME;
   stack_size = byte_offset = SZ_BOTTOM;
   bit_offset = 0;

   stack(cc);
   cc->nof = n;
   cc->ftbl = ftbl;

   switch (n->n_scope) {
      case 0:
      case PUBLIC:
         cc->not = n->n_table->t_name;
         cc->cot = (ClassP) cc->not->tp;
         cc->tot = cc->cot->this_type;
         if (this->f_this == 0 || cc->tot == 0) errorT('i', "dclFun(%n): f_this=%d cc->tot=%d", n, this->f_this, cc->tot); //(#) Clipped at "cc->".
         this->f_this->n_table = ftbl; /* fake for inline printout */
         cc->c_this = this->f_this;

   }

   IdP ax;
   for (a = this->argtype, ll = 0; a; a = ax) {
      ax = a->n_list;
      IdP nn = dclId(a, ftbl, ARG);
      nn->n_assigned_to = nn->n_used = nn->n_addr_taken = 0;
      nn->n_list = 0;
      switch (a->tp->base) {
         case CLASS:
         case ENUM: /* unlink types declared in arg list */
            a->n_list = dcl_list;
            dcl_list = a;
            break;
         default:
            if (ll)
               ll->n_list = nn;
            else {
               this->argtype = nn;
               if (this->f_this) this->f_this->n_list = this->argtype;
            }
            ll = nn;
            FreeId(a);
      }
   }

/* handle initializer for base class constructor */
   if (n->n_oper == CTOR) {
      IdP bn = cc->cot->clbase;

      if (bn) {
         ClassP bcl = (ClassP) bn->tp;
         IdP bnw = has_ctor(bcl);

         if (bnw) {
            TypeP bnwt = bnw->tp;
            FunP bnwf = (FunP) ((bnwt->base == FCT) ? bnwt : ((GenP) bnwt)->fct_list->f->tp); //(#) Clipped at "? bnwt : ((Pg". Not verified.
            TypeP ty = bnwf->f_this->tp;
            ExP v = (ExP)MakeTEx(VALUE, (TypeP)bcl, this->f_init);
            ExP th = (ExP)MakeTEx(CAST, ty, (ExP)this->f_this);
            v->e2 = MakeEx(DEREF, th, 0);
            const_save = 1;
            this->f_init = typ(v, ftbl);
            const_save = const_old;
         } else if (this->f_init)
            errorT(0, "unXAL: noBC constructor");
      } else if (this->f_init)
         error("unXAL: noBC");
   } else if (this->f_init)
      errorT(0, "unXAL: not a constructor");

   PERM(this->returns);
   if (this->returns->base != VOID) {
      IdP rv = MakeId("_result");
      rv->tp = this->returns;
      insert(ftbl, rv, 0);
      FreeId(rv);
   }

   const_save = this->f_inline ? 1 : 0;
   inline_restr = 0;
   dclBlock(this->body, ftbl);
   if (this->f_inline && inline_restr) {
      this->f_inline = 0;
      errorT('w', "\"inline\" ignored, %n contains%s%s%s%s", n, (inline_restr & 8) ? " loop" : "", (inline_restr & 4) ? " switch" : "", (inline_restr & 2) ? " goto" : "", (inline_restr & 1) ? " label" : "");
   }
   const_save = const_old;

   if (this->f_inline) {
      isf_list = MakeIdList(n, isf_list);
   }

   this->defined = 1;

   this->frame_size = stack_size + SZ_TOP;
   this->frame_size = ((this->frame_size - 1) / AL_FRAME) * AL_FRAME + AL_FRAME;
   bit_offset = bit_old;
   byte_offset = byte_old;
   max_align = max_old;
   stack_size = stack_old;

   unstack(cc);
}
