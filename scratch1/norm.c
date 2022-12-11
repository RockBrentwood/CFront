/* @(#) norm.c 1.3 1/27/86 17:49:14 */
/*ident	"@(#)cfront:src/norm.c	1.3" */
/************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

norm.c:

	"normalization" handles problems which could have been handled
	by the syntax analyser; but has not been done. The idea is
	to simplify the grammar and the actions accociated with it,
	and to get a more robust error handling

****************************************************************************/

#include "cfront.h"
#include "size.h"

extern void syn_init(void);
void syn_init(void) {
   any_type = MakeBase(ANY, 0);
   PERM(any_type);
   dummy = MakeEx(DUMMY, 0, 0);
   PERM(dummy);
   dummy->tp = (TypeP)any_type;
   zero = MakeEx(ZERO, 0, 0);
   PERM(zero);
}

int stcount;

char *make_name(Token c) {

   char *s = _new(8*sizeof *s); /* as it happens: fits in two words */

   if (99999 <= ++stcount) errorT('i', "too many generated names");

   s[0] = '_';
   s[1] = c;
   int count = stcount;
   int i = 2;
   if (10000 <= count) {
      s[i++] = '0' + count / 10000;
      count %= 10000;
   }
   if (1000 <= count) {
      s[i++] = '0' + count / 1000;
      count %= 1000;
   } else if (2 < i) s[i++] = '0';

   if (100 <= count) {
      s[i++] = '0' + count / 100;
      count %= 100;
   } else if (2 < i) s[i++] = '0';

   if (10 <= count) {
      s[i++] = '0' + count / 10;
      count %= 10;
   } else if (2 < i) s[i++] = '0';

   s[i++] = '0' + count;
   s[i] = 0;

   return s;
}

BaseP type_adj(BaseP this, Token t) {
   switch (this->base) {
      case COBJ:
      case EOBJ:
      {
         BaseP bt = MakeBase(0, 0);
         *bt = *this;
         DEL(Base, this);
         this = bt;
      }
   }

   if (this->b_xname) {
      if (this->base)
         error("badBT:%n%k", this->b_xname, t);
      else {
         this->base = TYPE;
         this->b_name = this->b_xname;
      }
      this->b_xname = 0;
   }

   switch (t) {
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
      case SHORT:
         this->b_short = 1;
         break;
      case LONG:
         this->b_long = 1;
         break;
      case FRIEND:
      case OVERLOAD:
      case EXTERN:
      case STATIC:
      case AUTO:
      case REGISTER:
         if (this->b_sto)
            error("badBT:%k%k", this->b_sto, t);
         else
            this->b_sto = t;
         break;
      case VOID:
      case CHAR:
      case INT:
      case FLOAT:
      case DOUBLE:
         if (this->base)
            error("badBT:%k%k", this->base, t);
         else
            this->base = t;
         break;
      default:
         errorT('i', "type_adj(this, %k)", t);
   }
   return this;
}

BaseP name_adj(BaseP this, IdP n) {
   if (this->b_xname) {
      if (this->base)
         error("badBT:%n%n", this->b_xname, n);
      else {
         this->base = TYPE;
         this->b_name = this->b_xname;
      }
      this->b_xname = 0;
   }
   this->b_xname = n;
   return this;
}

BaseP base_adj(BaseP this, BaseP b) {
   IdP bn = b->b_name;

   switch (this->base) {
      case COBJ:
      case EOBJ:
         error("NX after%k%n", this->base, this->b_name);
         return this;
   }

   if (this->base) {
      if (this->b_name)
         error("badBT:%k%n%k%n", this->base, this->b_name, b->base, bn);
      else
         error("badBT:%k%k%n", this->base, b->base, bn);
   } else {
      this->base = b->base;
      this->b_name = bn;
      this->b_table = b->b_table;
   }
   return this;
}

/*
	"n" is the first name to be declared using "this"
	check the consistency of "this"
	and use "this->b_xname" for "n->string" if possible and needed
*/
BaseP checkBase(BaseP this, IdP n) {
   this->b_inline = 0;
   this->b_virtual = 0;
//fprintf(stderr,"check n: %d %s n_oper %d b: %d %d %s\n",n,(n)?n->string:"",n?n->n_oper:0,this,this->base,(this->b_name)?this->b_name->string:"");fflush(stderr);
   if (this->b_xname && (n->tp || n->string)) {
      if (this->base)
         error("badBT:%k%n", this->base, this->b_xname);
      else {
         this->base = TYPE;
         this->b_name = this->b_xname;
      }
      this->b_xname = 0;
   }

   if (this->b_xname) {
      if (n->string)
         error("twoNs inD:%n%n", this->b_xname, n);
      else {
         n->string = this->b_xname->string;
         hide(this->b_xname);
      }
      this->b_xname = 0;
   }

   if (ccl == 0 && n && n->n_oper == TNAME && n->n_qualifier == 0 && n->string) { // hide type name
      IdP nx = look(ktbl, n->string, 0);
      if (nx) hide(nx);
   }

   switch (this->base) {
      case 0:
         this->base = INT;
         break;
      case EOBJ:
      case COBJ:
         if (this->b_name->base == TNAME)
            errorT('i', "TN%n inCO %d", this->b_name, this);
   }

   if (this->b_long || this->b_short) {
      Token sl = (this->b_short) ? SHORT : LONG;
      if (this->b_long && this->b_short) error("badBT:long short%k%n", this->base, n);
      if (this->base != INT)
         error("badBT:%k%k%n", sl, this->base, n);
      else
         this->base = sl;
      this->b_short = this->b_long = 0;
   }

   if (this->b_typedef && this->b_sto) error("badBT:typedef%k%n", this->b_sto, n);
   this->b_typedef = this->b_sto = 0;

   if (Pfctvec_type == 0) return this;

   if (this->b_const) {
      if (this->b_unsigned) {
         switch (this->base) {
            default:
               error("badBT: unsigned const %k%n", this->base, n);
               this->b_unsigned = 0;
            case LONG:
            case SHORT:
            case INT:
            case CHAR:
               return this;
         }
      }
      return this;
   } else if (this->b_unsigned) {
      switch (this->base) {
         case LONG:
            _delete(this);
            return ulong_type;
         case SHORT:
            _delete(this);
            return ushort_type;
         case INT:
            _delete(this);
            return uint_type;
         case CHAR:
            _delete(this);
            return uchar_type;
         default:
            error("badBT: unsigned%k%n", this->base, n);
            this->b_unsigned = 0;
            return this;
      }
   } else {
      switch (this->base) {
         case LONG:
            _delete(this);
            return long_type;
         case SHORT:
            _delete(this);
            return short_type;
         case INT:
            if (this != int_type) _delete(this);
            return int_type;
         case CHAR:
            _delete(this);
            return char_type;
         case VOID:
            _delete(this);
            return void_type;
         case TYPE:
         /* use a single this->base saved in the keyword */
//fprintf(stderr,"type %d bn %d %s q %d\n",this,this->b_name,this->b_name->string,this->b_name->n_qualifier);
            if (this->b_name->n_qualifier) {
               _delete(this);
               return (BaseP) this->b_name->n_qualifier;
            } else {
               PERM(this);
               this->b_name->n_qualifier = (IdP) this;
               return this;
            }
         default:
            return this;
      }
   }
}

/*
	"type SM" seen e.g.	struct s {};
				class x;
				enum e;
				int tname;
				friend cname;
				friend class x;
				int;

	convert
		union { ... };
	into
		union name { ... } name ;
*/
IdP aggr(BaseP this) {
   IdP n;

   if (this->b_xname) {
      if (this->base) {
         IdP n = MakeId(this->b_xname->string);
         hide(this->b_xname);
         this->b_xname = 0;
         return normalizeId(n, this, 0, 0);
      } else {
         this->base = TYPE;
         this->b_name = this->b_xname;
         this->b_xname = 0;
      }
   }

   switch (this->base) {
      case COBJ:
      {
         ClassP cl = (ClassP) this->b_name->tp;
         const char *s = cl->string;
/*fprintf(stderr,"COBJ (%d %s) -> (%d %d) ->(%d %d)\n",this,this->b_name->string,this->b_name,this->b_name->base,cl,cl->base);*/
         if (this->b_name->base == TNAME) errorT('i', "TN%n inCO", this->b_name);
         if (this->b_const) error("const%k%n", cl->csu, this->b_name);

         if (cl->c_body == 2) { /* body seen */
            if (s[0] == '_' && s[1] == 'C') {
               char *ss = _new(5*sizeof *ss);
               IdP obj = MakeId(ss);
               if (cl->csu == UNION) {
                  strcpy(ss, s);
                  ss[1] = 'O';
                  cl->csu = ANON;
                  return normalizeId(obj, this, 0, 0);
               }
               errorT('w', "un-usable%k ignored", cl->csu);
            }
            cl->c_body = 1;
            return this->b_name;
         } else { /* really a typedef for cfront only: class x; */
            if (this->b_sto == FRIEND) goto frr;
            return 0;
         }
      }

      case EOBJ:
      {
         EnumP en = (EnumP) this->b_name->tp;
/*fprintf(stderr,"EOBJ (%d %s) -> (%d %d) ->(%d %d)\n",this,this->b_name->string,this->b_name,this->b_name->base,en,en->base);*/
         if (this->b_name->base == TNAME) errorT('i', "TN%n in enumO", this->b_name);
         if (this->b_const) error("const enum%n", this->b_name);
         if (en->e_body == 2) {
            en->e_body = 1;
            return this->b_name;
         } else {
            if (this->b_sto == FRIEND) goto frr;
            return 0;
         }
      }

      default:
         if (this->b_typedef) errorT('w', "illegal typedef ignored");

         if (this->b_sto == FRIEND) {
          frr: ;
            IdP fr = look(ktbl, this->b_name->string, 0);
            if (fr == 0) errorT('i', "cannot find friend%n", this->b_name);
            n = MakeId(this->b_name->string);
            n->n_sto = FRIEND;
            n->tp = fr->tp;
            return n;
         } else {
            n = MakeId(make_name('D'));
            n->tp = (TypeP)any_type;
            errorT('w', "NX inDL");
            return n;
         }
   }
}

/*
	hide "this": that is, "this" should not be a keyword in this scope
*/
void hide(IdP this) {
//errorT('d',"hide%n %d %k tp %d",this,this,this->base,this->tp);
   if (this->base != TNAME) return;
   if (this->n_key == 0) {
//errorT('d',"ll %d bl %d this->base %k",this->lex_level,bl_level,this->base);
      if (this->lex_level == bl_level) {
         if (this->base != TNAME)
            error("%n redefined", this);
         else {
         // is it a CN?
            errorT('w', "%n redefined", this);
         }
      }
      modified_tn = MakeIdList(this, modified_tn);
      this->n_key = HIDDEN;
   }
}

/*
	enter the scope of class tn after seeing "tn.f"
*/
void set_scope(IdP tn) {
   if (tn->base != TNAME) errorT('i', "set_scope: not aTN %d %d", tn, tn->base);
   BaseP b = (BaseP) tn->tp;
   if (b->b_name->tp->base != CLASS) errorT('i', "T of%n not aC (%k)", tn, b->b_name->tp->base);
   ClassP cl = (ClassP) b->b_name->tp;
   for (IdListP l = cl->tn_list; l; l = l->l) {
      IdP n = l->f;
      n->n_key = (n->lex_level) ? 0 : HIDDEN;
//errorT('d',"set_scope%n %d",n,n->n_key);
      modified_tn = MakeIdList(n, modified_tn);
   }
}

void restore(void) {
   for (IdListP l = modified_tn; l; l = l->l) {
      IdP n = l->f;
      n->n_key = (n->lex_level <= bl_level) ? 0 : HIDDEN;
//errorT('d',"restore%n %d",n,n->n_key);
   }
}

BaseP start_cl(Token t, IdP c, IdP b) {
   if (c == 0) c = MakeId(make_name('C'));
//errorT('d',"c: %d %s",c->base,c->string);
   IdP n = tname(c, t); /* t ignored */
   n->where = curloc;
   BaseP bt = (BaseP) n->tp; /* COBJ */
   if (bt->base != COBJ) {
      error("twoDs of%n:%t andC", n, bt);
      errorT('i', "can't recover from previous errors");
   }
   ClassP occl = ccl;
   ccl = (ClassP) bt->b_name->tp; /* CLASS */
   if (ccl->defined) {
      error("C%n defined twice");
      ccl->defined |= IN_ERROR;
   }
   ccl->defined |= DEF_SEEN;
   if (ccl->in_class = occl) occl->tn_list = modified_tn; // save  mod-list
   modified_tn = 0;
   ccl->string = n->string;
   ccl->csu = t;
   if (b) ccl->clbase = tname(b, t);
   return bt;
}

void end_cl(void) {
   ClassP occl = ccl->in_class;
   IdListP ol = occl ? occl->tn_list : 0; // saved modified name list
   ccl->c_body = 2;
   if (modified_tn) { // export nested class names to outer scope:
      IdListP local = 0;
      for (IdListP l = modified_tn, nl = 0; l; l = nl) {
         nl = l->l;
         IdP n = l->f;
         if (look(ktbl, n->string, 0)) {
         // add it to enclosing class's modified name list
            l->l = ol;
            ol = l;
         } else { // retain it in this class's modified name list
            l->l = local;
            local = l;
         }
      }
      if (ccl->tn_list = modified_tn = local) restore();
   }
   modified_tn = ol; // restore mod-list (possibly modified)
   ccl = occl;
}

BaseP end_enum(IdP n, IdP b) {
   if (n == 0) n = MakeId(make_name('E'));
   n = tname(n, ENUM);
   BaseP bt = (BaseP) n->tp;
   if (bt->base != EOBJ) {
      error("twoDs of%n:%t and enum", n, bt);
      errorT('i', "can't recover from previous errors");
   }
   EnumP en = (EnumP) bt->b_name->tp;
   en->e_body = 2;
   en->mem = name_unlist((IdsP) b);
   if (en->defined) {
      error("enum%n defined twice", n);
      en->defined |= IN_ERROR;
   }
   en->defined |= DEF_SEEN;
   return bt;
}

/*
	typedef "this"
*/
IdP tdef(IdP this) {
//errorT('d',"tdef%n",this);
   IdP n = insert(ktbl, this, 0);
   if (this->tp == 0) errorT('i', "typedef%n tp==0", this);
   n->base = this->base = TNAME;
   PERM(n);
   PERM(this->tp);
   modified_tn = MakeIdList(n, modified_tn);
   return n;
}

/*
	"csu" "this" seen, return typedef'd name for "this"
	return	(TNAME,x)
	x:	(COBJ,y)
	y:	(NAME,z)
	z:	(CLASS,ae);
*/
IdP tname(IdP this, Token csu) {
//fprintf(stderr,"tname %s %d ll %d\n",this->string,this,this->lex_level);
   switch (this->base) {
      case TNAME:
         return this;
      case NAME:
      {
         IdP tn = insert(ktbl, this, 0);
//fprintf(stderr,"tname tn %s %d ll %d (mod %d)\n",tn->string,tn,tn->lex_level,modified_tn);
         IdP on = MakeId(0);
         tn->base = TNAME;
         tn->lex_level = this->lex_level;
         modified_tn = MakeIdList(tn, modified_tn);
         tn->n_list = this->n_list = 0;
         this->string = tn->string;
         *on = *this;
         switch (csu) {
            case ENUM:
               tn->tp = (TypeP)MakeBase(EOBJ, on);
               on->tp = (TypeP)MakeEnum(0);
               break;
            default:
               on->tp = (TypeP)MakeClass(csu, 0);
               ToClassP(on->tp)->string = tn->string;
               tn->tp = (TypeP)MakeBase(COBJ, on);
               ToBaseP(tn->tp)->b_table = ToClassP(on->tp)->memtbl;
         }
         PERM(tn);
         PERM(tn->tp);
         PERM(on);
         PERM(on->tp);
/*fprintf(stderr,"tname %s -> n (%d %d) n->tp (%d %d)\n",this->string,tn,tn->base,tn->tp,tn->tp->base); fflush(stderr);*/
         return tn;
      }
      default:
         errorT('i', "tname(%s %d %k)", this->string, this, this->base);
   }
}

/*
	if (bl) : a function definition (check that it really is a type

	if (cast) : no name string

	for each name on the name list
	invert the declarator list(s) and attatch basetype
	watch out for class object initializers

	convert
		struct s { int a; } a;
	into
		struct s { int a; }; struct s a;
*/
IdP normalizeId(IdP this, BaseP b, BlockP bl, bit cast) {
   IdP n;
   IdP nn;
   Token stc = b->b_sto;
   bit tpdf = b->b_typedef;
   bit inli = b->b_inline;
   bit virt = b->b_virtual;
   FunP f;
   IdP nx;

   if (b == 0) errorT('i', "normalizeId(%d, 0)", this);
   if (this == 0) errorT('i', "normalizeId(0, %k)", this->base);

   if (inli && stc == EXTERN) {
      error("both extern and inline");
      inli = 0;
   }
//fprintf(stderr,"normalizeId(%d %s) tp (%d %d)\n",this,this->string,this->tp,this->tp->base);

   if (stc == FRIEND && this->tp == 0) {
   /*      friend x;
      must be handled during syntax analysis to cope with
      class x { friend y; y* p; };
      "y" is not local to "x":
      class x { friend y; ... }; y* p;
      is legal
    */
      if (b->base) error("T specified for friend");
      if (this->n_list) {
         error("L of friends");
         this->n_list = 0;
      }
      IdP nx = tname(this, CLASS);
      modified_tn = modified_tn->l; /* global */
      this->n_sto = FRIEND;
      this->tp = nx->tp;
      return this;
   }

   if (this->tp && this->n_oper == TNAME && this->tp->base == FCT) { /* HORRIBLE FUDGE: fix the bad grammar */
      FunP f = (FunP) this->tp;
      FunP f2 = (FunP) f->returns;
      if (f2 && f2->base == FCT) {
         ExP e = (ExP)f2->argtype;
//errorT('d',"%s: mis-analyzedP toF",this->string);
         if (e->base == ELIST) {
         //      get the real name, fix its type
            if (e->e2 || e->e1->base != DEREF) goto zse1;
            IdP rn = (IdP) e->e1->e1;
            if (rn->base != NAME) goto zse1;
            f->returns = (TypeP)MakePtr(PTR, 0, 0);
            b = MakeBase(TYPE, look(ktbl, this->string, 0));
            this->n_oper = 0;
            this->string = rn->string;
            this->base = NAME;
//errorT('d',"realN %n b==%t",rn,b);
         }
      }
   }
 zse1:
   if (cast) this->string = "";
   b = checkBase(b, this);

   switch (b->base) { //      separate class definitions
      //      from object and function type declarations
      case COBJ:
         nn = b->b_name;
//fprintf(stderr,"COBJ (%d %s) -> (%d %d body=%d)\n",nn,nn->string,nn->tp,nn->tp->base,ToClassP(nn->tp)->c_body);
         if (ToClassP(nn->tp)->c_body == 2) { /* first occurrence */
            if (this->tp && this->tp->base == FCT) {
               errorTL('s', &this->where, "%k%n defined as returnT for%n (did you forget a ';' after '}' ?)", ToClassP(nn->tp)->csu, nn, this);
               nn = this;
               break;
            }
            nn->n_list = this;
            ToClassP(nn->tp)->c_body = 1; /* other occurences */
         } else
            nn = this;
         break;
      case EOBJ:
         nn = b->b_name;
         if (ToEnumP(nn->tp)->e_body == 2) {
            if (this->tp && this->tp->base == FCT) {
               errorT('s', "enum%n defined as returnT for%n (did you forget a ';'?)", nn, this);
               nn = this;
               break;
            }
            nn->n_list = this;
            ToEnumP(nn->tp)->e_body = 1;
         } else
            nn = this;
         break;
      default:
         nn = this;
   }

   for (n = this; n; n = nx) {
      TypeP t = n->tp;
      nx = n->n_list;
      n->n_sto = stc;
/*
		if (t
		&& this->n_oper==TNAME
		&& t->base==FCT) {	// HORRIBLE FUDGE: fix the bad grammar
			FunP f = (FunP)t;
			FunP f2 = (FunP)f->returns;
			if (f2 && f2->base==FCT) {
				ExP e = f2->argtype;
				if  (e->base == ELIST) {
					// get the real name, fix its type
					if (e->e2 || e->e1->base!=DEREF) goto zse;
					IdP rn = (IdP)e->e1->e1;
					if (rn->base!=NAME) goto zse;
					f->returns = MakePtr(PTR,0);
					b = MakeBase(TYPE,look(ktbl, n->string,0));
					n->n_oper = 0;
					n->string = rn->string;
					n->base = NAME;
				}
			}
		}
zse:
*/
      if (n->base == TNAME) errorT('i', "redefinition ofTN%n", n);

      if (t == 0) {
         if (bl == 0)
            n->tp = t = (TypeP)b;
         else {
            error("body of nonF%n", n);
            t = (TypeP)MakeFun((TypeP)defa_type, 0, 0);
         }
      }

      switch (t->base) {
         case PTR:
         case RPTR:
            n->tp = normalizePtr(ToPtrP(t), (TypeP)b);
            break;
         case VEC:
            n->tp = normalizeVec(ToVecP(t), (TypeP)b);
            break;
         case FCT:
            n->tp = normalizeFun(ToFunP(t), (TypeP)b);
            break;
         case FIELD:
            if (n->string == 0) n->string = make_name('F');
            n->tp = t;
            BaseP tb = b;
          flatten:
//errorT('d',"flatten %d %d %d",tb->base,b->b_unsigned,b->b_const);
            switch (tb->base) {
               case TYPE: /* chase typedefs */
                  tb = (BaseP) tb->b_name->tp;
                  goto flatten;
               case INT:
                  ToBaseP(t)->b_fieldtype = (TypeP)((b->b_unsigned) ? uint_type : int_type);
                  goto iii;
               case CHAR:
                  ToBaseP(t)->b_fieldtype = (TypeP)((b->b_unsigned) ? uchar_type : char_type);
                  goto iii;
               case SHORT:
                  ToBaseP(t)->b_fieldtype = (TypeP)((b->b_unsigned) ? ushort_type : short_type);
                  goto iii;
                iii:
                  ToBaseP(t)->b_unsigned = b->b_unsigned;
                  ToBaseP(t)->b_const = b->b_const;
                  break;
               default:
                  error("non-int field");
                  n->tp = (TypeP)defa_type;
            }
            break;
      }

      f = (FunP) n->tp;

      if (f->base != FCT) {
         if (bl) {
            error("body for nonF%n", n);
            n->tp = (TypeP)(f = MakeFun((TypeP)defa_type, 0, 0));
            continue;
         }
         if (inli) error("inline nonF%n", n);
         if (virt) error("virtual nonF%n", n);

         if (tpdf) {
            if (n->n_initializer) {
               error("Ir for typedefN%n", n);
               n->n_initializer = 0;
            }
            tdef(n);
         }
         continue;
      }

      f->f_inline = inli;
      f->f_virtual = virt;

      if (tpdf) {
         if (f->body = bl) error("typedef%n { ... }", n);
         tdef(n);
         continue;
      }

      if (f->body = bl) continue;

   /*
      Check function declarations.
      Look for class object instansiations
      The real ambiguity:          ; class x fo();
      is interpreted as an extern function
      declaration NOT a class object with an
      empty initializer
    */
      {
         IdP cn = is_cl_obj(f->returns);
         bit clob = (cn || cl_obj_vec);
//errorT('d',"%n: fr%t cn%n",n,f->returns,cn);
         if (f->argtype) { /* check argument/initializer list */
            IdP nn;

            for (nn = f->argtype; nn; nn = nn->n_list) {
               if (nn->base != NAME) {
                  if (!clob) {
                     error("ATX for%n", n);
                     goto zzz;
                  }
                  goto is_obj;
               }
/*
					if (nn->string) {
						error("AN%n inD of%n",nn,n);
						nn->string = 0;
					}
*/
               if (nn->tp) goto ok;
            }
            if (!clob) {
               error("FALX");
               goto zzz;
            }
          is_obj:
//fprintf(stderr,"is_obj: %d %s tp = %d %d\n",this,this->string,f->returns,f->returns->base); fflush(stderr);
         /* it was an initializer: expand to constructor */
            n->tp = f->returns;
            if (f->argtype->base != ELIST) f->argtype = (IdP) MakeEx(ELIST, (ExP) f->argtype, 0);
            n->n_initializer = (ExP)MakeTEx(VALUE, cn->tp, (ExP) f->argtype);
            goto ok;
          zzz:
            if (f->argtype) {
               DEL(Id, f->argtype);
               f->argtype = 0;
               f->nargs = 0;
               f->nargs_known = !fct_void;
            }
         } else { /* T a(); => function declaration */
/*
				if (clob) {
					DEL(Type, n->tp);
					n->tp = f->returns;
				}
*/
         }
       ok:
         ;
      }
   }
   return nn;
}

/*
*/
TypeP normalizeVec(VecP this, TypeP vecof) {
   TypeP t = this->typ;
   if (this == 0) errorT('i', "normalizeVec(0)");
   this->typ = vecof;

   if (t == 0) return (TypeP)this;

 xx:
   switch (t->base) {
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xx;
      case PTR:
      case RPTR:
         return normalizePtr(ToPtrP(t), (TypeP)this);
      case VEC:
         return normalizeVec(ToVecP(t), (TypeP)this);
      case FCT:
         return normalizeFun(ToFunP(t), (TypeP)this);
      default:
         errorT('i', "bad vectorT(%d)", t->base);
   }
}

TypeP normalizePtr(PtrP this, TypeP ptrto) {
   TypeP t = this->typ;
   if (this == 0) errorT('i', "normalizePtr(0)");
   this->typ = ptrto;

   if (t == 0) {
      BaseP b = (BaseP) ptrto;
      if (Pfctvec_type && this->rdo == 0 && b->b_unsigned == 0 && b->b_const == 0 && this->base == PTR) {
         switch (b->base) {
            case INT:
               _delete(this);
               return Pint_type;
            case CHAR:
               _delete(this);
               return Pchar_type;
            case VOID:
               _delete(this);
               return Pvoid_type;
            case TYPE:
               break;
         }
      }
      if (this->base == RPTR && b->base == VOID) error("void& is not a validT");
      return (TypeP)this;
   }

 xx:
   switch (t->base) {
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xx;
      case PTR:
      case RPTR:
         return normalizePtr(ToPtrP(t), (TypeP)this);
      case VEC:
         return normalizeVec(ToVecP(t), (TypeP)this);
      case FCT:
         return normalizeFun(ToFunP(t), (TypeP)this);
      default:
         errorT('i', "badPT(%d)", t->base);
   }
}

/*
	normalize return type
*/
TypeP normalizeFun(FunP this, TypeP ret) {
   register TypeP t = this->returns;

   if (this == 0 || ret == 0) errorT('i', "normalizeFun(%d, %d)", this, ret);

   this->returns = ret;
   if (t == 0) return (TypeP)this;

   if (this->argtype) {
      if (this->argtype->base != NAME) {
         errorT('i', "syntax: ANX");
         this->argtype = 0;
         this->nargs = 0;
         this->nargs_known = 0;
      }
/*
		else {
			IdP n;
			for (n=this->argtype; n; n=n->n_list) {
				if (n->string) {
					error("N inATL");
					n->string = 0;
				}
			}
		}
*/

   }

 xx:
   switch (t->base) {
      case PTR:
      case RPTR:
         return normalizePtr(ToPtrP(t), (TypeP)this);
      case VEC:
         return normalizeVec(ToVecP(t), (TypeP)this);
      case FCT:
         return normalizeFun(ToFunP(t), (TypeP)this);
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xx;
      default:
         errorT('i', "badFT:%k", t->base);
   }

}

/*
	sort out the argument types for old syntax:
			f(a,b) int a; char b; { ... }
	beware of
			f(a) struct s { int a; }; struct s a;
*/
void argdcl(FunP this, IdP dcl, IdP fn) {
   IdP n;
/*fprintf(stderr,"%d argtype %d %d dcl %d %d\n",this, this->argtype, this->argtype?this->argtype->base:0, dcl, dcl?dcl->base:0); fflush(stderr);*/
   switch (this->base) {
      case FCT:
         break;
      case ANY:
         return;
      default:
         errorT('i', "argdcl(this, %d)", this->base);
   }

   if (this->argtype) {
      switch (this->argtype->base) {
         case NAME:
            if (dcl) error("badF definition syntax");
            for (n = this->argtype; n; n = n->n_list) {
               if (n->string == 0) n->string = make_name('A');
            }
            return;
         case ELIST: // expression list:     f(a,b,c) int a; ... { ... }
         // scan the Exs and build a NAME list
         {
            ExP e;
            IdP nn;
            IdP tail = 0;
            n = 0;
            if (old_fct_accepted == 0) errorTL('w', &fn->where, "old style definition of%n", fn);
            for (e = ToExP(this->argtype); e; e = e->e2) {
               ExP id = e->e1;
               if (id->base != NAME) {
                  error("NX inAL");
                  this->argtype = 0;
                  dcl = 0;
                  break;
               }
               nn = MakeId(id->string);
               if (n)
                  tail = tail->n_list = nn;
               else
                  tail = n = nn;
            }
            this->argtype = n;
            break;
         }
         default:
            error("ALX(%d)", this->argtype->base);
            this->argtype = 0;
            dcl = 0;
      }
   } else {
      this->nargs_known = !fct_void;
      this->nargs = 0;
      if (dcl) error("ADL forF withoutAs");
      return;
   }

   this->nargs_known = 0;

   if (dcl) {
      IdP d;
      IdP dx;
   /*      for each  argument name see if its type is specified
      in the declaration list otherwise give it the default type
    */

      for (n = this->argtype; n; n = n->n_list) {
         const char *s = n->string;
         if (s == 0) {
            error("AN missing inF definition");
            n->string = s = make_name('A');
         } else if (n->tp) error("twoTs forA %s", n->string);

         for (d = dcl; d; d = d->n_list) {
            if (strcmp(s, d->string) == 0) {
               if (d->tp->base == VOID) {
                  error("voidA%n", d);
                  d->tp = (TypeP)any_type;
               }
               n->tp = d->tp;
               n->n_sto = d->n_sto;
               d->tp = 0; /* now merged into argtype */
               goto xx;
            }
         }
         n->tp = (TypeP)defa_type;
       xx:;
         if (n->tp == 0) errorT('i', "noT for %s", n->string);
      }

   /*      now scan the declaration list for "unused declarations"
      and delete it
    */
      for (d = dcl; d; d = dx) {
         dx = d->n_list;
         if (d->tp) { /* not merged with argtype list */
         /*if (d->base == TNAME)  ??? */
            switch (d->tp->base) {
               case CLASS:
               case ENUM:
               /* WARNING: this will reverse the order of
                  class and enum declarations
                */
                  d->n_list = this->argtype;
                  this->argtype = d;
                  break;
               default:
                  error("%n inADL not inAL", d);
            }
         }
      }
   }

/* add default argument types if necessary */
   for (n = this->argtype; n; n = n->n_list) {
      if (n->tp == 0) n->tp = (TypeP)defa_type;
      this->nargs++;
   }
}

IdP cl_obj_vec; /* set if is_cl_obj() found a vector of class objects */
IdP eobj; /* set if is_cl_obj() found an enum */

/*
	returns this->b_name if this is a class object
	returns 0 and sets cl_obj_vec to this->b_name
		if this is a vector of class objects
	returns 0 and sets eobj to this->b_name
		if this is an enum object
	else returns 0
*/
IdP is_cl_obj(TypeP this) {
   bit v = 0;
   register TypeP t = this;

   eobj = 0;
   cl_obj_vec = 0;
 xx:
   switch (t->base) {
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xx;

      case COBJ:
         if (v) {
            cl_obj_vec = ToBaseP(t)->b_name;
            return 0;
         } else
            return ToBaseP(t)->b_name;

      case VEC:
         t = ToVecP(t)->typ;
         v = 1;
         goto xx;

      case EOBJ:
         eobj = ToBaseP(t)->b_name;
      default:
         return 0;
   }
}
