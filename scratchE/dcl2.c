// 1985 Feb 08 12:48
/* %Z% %M% %I% %H% %T% */
/**************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T Technologies, Inc. All rigths Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T TECHNOLOGIES, INC.

	If you ignore this notice the ghost of Ma Bell will haunt you forever.

dcl2.c:

*************************************************************************/

#include "cfront.h"
#include "size.h"

/*
	does this class have a constructor taking no arguments?
*/
IdP has_ictor(ClassP this) {
   IdP c = has_ctor(this);
   FunP f;
   IdListP l;

   if (c == 0) return 0;

   f = (FunP) c->tp;

   switch (f->base) {
      default:
         errorT('i', "%s: bad constructor (%k)", this->string, c->tp->base);

      case FCT:
         switch (f->nargs) {
            case 0:
               return c;
            default:
               if (f->argtype->n_initializer) return c;
         }
         return 0;

      case OVERLOAD:
         for (l = ((GenP) f)->fct_list; l; l = l->l) {
            IdP n = l->f;
            f = (FunP) n->tp;
            switch (f->nargs) {
               case 0:
                  return n;
               default:
                  if (f->argtype->n_initializer) return n;
            }
         }
         return 0;
   }
}

GenP MakeGen(char *s) {
   GenP this = (GenP)_new(sizeof *this);
   char *p = _new((strlen(s) + 1)*sizeof *p);
   this->base = OVERLOAD;
   strcpy(p, s);
   this->string = p;
   this->fct_list = 0;
   return this;
}

/*
	add "n" to the tail of "fct_list"
	(overloaded names are searched in declaration order)

	detect:	 	multiple identical declarations
			declaration after use
			multiple definitions
*/
IdP addGen(GenP this, IdP n, int sig) {
   FunP f = (FunP) n->tp;
   IdP nx;

   if (f->base != FCT) errorT(0, "%n: overloaded non-F", n);

   if (this->fct_list && (nx = find(this, f))) {
/*
		FunP nf = (FunP)nx->tp;

		if (nf->body) {
			if (f->body) error("two definitions for overloaded%n",n);
		}
		else {
			if (f->body) nf->body = f->body;
		}
*/
      Nold = 1;
   } else {
      char *s = this->string;

      if (this->fct_list || sig) {
         char buf[128];
         char *bb = signature(n->tp, buf);
         int l1 = strlen(s);
         int l2 = bb - buf - 1;
         char *p = _new((l1 + l2 + 1)*sizeof *p);
         strcpy(p, s);
         strcpy(p + l1, buf);
         n->string = p;
      } else
         n->string = s;

      nx = MakeId(0);
      *nx = *n;
      PERM(nx);
      Nold = 0;
      if (this->fct_list) {
         IdListP gl;
         for (gl = this->fct_list; gl->l; gl = gl->l);
         gl->l = MakeIdList(nx, 0);
      } else
         this->fct_list = MakeIdList(nx, 0);
      nx->n_list = 0;
   }
   return nx;
}

IdP find(GenP this, FunP f) {
   IdListP gl;

   for (gl = this->fct_list; gl; gl = gl->l) {
      IdP nx = gl->f;
      FunP fx = (FunP) nx->tp;
      IdP a, ax;
/*fprintf(stderr,"find %s\n",nx->string); fflush(stderr);*/

      if (fx->nargs_known != f->nargs_known) continue;

      for (ax = fx->argtype, a = f->argtype; a && ax; ax = ax->n_list, a = a->n_list) { //(#) Clipped at "a=a->n_lis".
      /*fprintf(stderr,"ax %d %d a %d %d\n",ax->tp,ax->tp->base,a->tp,a->tp->base); fflush(stderr);*///(#) Clipped at "fflush".
         TypeP at = ax->tp;
         if (checkType(at, a->tp, 0) || vrp_equiv) goto xx;
         switch (at->base) {
            case CHAR:
            case SHORT:
            case INT:
            case LONG:
               if (((BaseP) at)->b_unsigned ^ ((BaseP) a->tp)->b_unsigned) errorT('w', "the overloading mechanism cannot tell an unsigned%k from a%k", at->base, at->base); //(#) Clipped at "((BaseP)a->tp)->b_uns".
         }
      }

      if (ax) {
         if (ax->n_initializer)
            error("Ir makes overloaded %s() ambiguous", this->string);
         continue;
      }

      if (a) {
         if (a->n_initializer)
            error("Ir makes overloaded %s() ambiguous", this->string);
         continue;
      }

      if (checkType(fx->returns, f->returns, 0))
         error("two different return valueTs for overloaded %s: %t and %t", this->string, fx->returns, f->returns); //(#) Clipped at "%s: %t a".

      return nx;
    xx:;
   }

   return 0;
}

void dclClass(ClassP this, IdP cname, TableP tbl) {
   int nmem;
   IdP p;
   PtrP cct;
   BaseP bt;
   IdP px;
   TableP btbl;
   int bvirt;
   ClassP bcl;
   int i;

   int byte_old = byte_offset;
   int bit_old = bit_offset;
   int max_old = max_align;
   int boff;

   int in_union;
   int usz;

/* this is the place for paranoia */
   if (this == 0) errorT('i', "0->dclClass(%d)", tbl);
   if (this->base != CLASS) errorT('i', "dclClass(%d)", this->base);
   if (cname == 0) errorT('i', "unNdC");
   if (cname->tp != (TypeP)this) errorT('i', "bad Class");
   if (tbl == 0) errorT('i', "dclClass(%n,0)", cname);
   if (tbl->base != TABLE) errorT('i', "dclClass(%n,tbl=%d)", cname, tbl->base);

   nmem = no_of_names(this->pubmem) + no_of_names(this->privmem) + no_of_names(this->pubdef); //(#) Clipped at "this->pubdef->no_of_names".
   in_union = (this->csu == UNION || this->csu == ANON);

   if (this->clbase) {
      if (this->clbase->base != TNAME) error("BC%nU", this->clbase);
      this->clbase = ((BaseP) this->clbase->tp)->b_name;
      bcl = (ClassP) this->clbase->tp;
      if (bcl->defined == 0) error("BC%nU", this->clbase);
      tbl = bcl->memtbl;
      if (tbl->base != TABLE) errorT('i', "badBC table %d", tbl);
      btbl = tbl;
      bvirt = bcl->virt_count;
      if (bcl->csu == UNION) errorT('s', "C derived from union");
      if (in_union)
         error("derived union");
      else
         this->csu = (this->pubbase) ? bcl->csu : CLASS;
      boff = tsizeof((TypeP)bcl);
      max_align = align((TypeP)bcl);
   } else {
      btbl = 0;
      bvirt = 0;
      boff = 0;
      if (!in_union) this->csu = (this->virt_count) ? CLASS : STRUCT;
      while (tbl != gtbl && tbl->t_name) tbl = tbl->next; /* nested classes *///(#) Clipped at "nested classes".
      max_align = AL_STRUCT;
   }

   set_scopeTable(this->memtbl, tbl);
   set_name(this->memtbl, cname);
   if (nmem) grow(this->memtbl, (nmem <= 2) ? 3 : nmem);

   stack(cc);
   cc->not = cname;
   cc->cot = this;

   byte_offset = usz = boff;
   bit_offset = 0;

   bt = MakeBase(COBJ, cname);
   bt->b_table = this->memtbl;
   this->this_type = cc->tot = (TypeP)(cct = MakePtr(PTR, (TypeP)bt, 0));
   PERM(cct);
   PERM(bt);

   for (p = this->privmem; p; p = px) {
      IdP m;
      px = p->n_list;
      if (p->tp->base == FCT) {
         FunP f = (FunP) p->tp;
         BlockP b = f->body;
         f->body = 0;
         switch (p->n_sto) {
            case AUTO:
            case STATIC:
            case REGISTER:
            case EXTERN:
               error("M%n cannot be%k", p, p->n_sto);
               p->n_sto = 0;
         }
         m = dclId(p, this->memtbl, 0);
         if (b) {
            if (m->tp->defined)
               error("two definitions of%n", m);
            else if (p->where.line != m->where.line)
               errorT('s', "previously declared%n cannot be defined inCD", p); //(#) Clipped at "cannot be ".
            else
               ((FunP) m->tp)->body = b;
         }
      } else {
         m = dclId(p, this->memtbl, 0);
         if (m) {
            if (m->n_stclass == STATIC && m->n_initializer)
               errorT('s', "staticM%n withIr", m);
            if (in_union) {
               if (usz < byte_offset) usz = byte_offset;
               byte_offset = 0;
            }
         }
      }
   }
   if (this->privmem && this->csu == STRUCT) this->csu = CLASS;

   for (p = this->pubmem; p; p = px) {
      IdP m;
      px = p->n_list;
      if (p->tp->base == FCT) {
         FunP f = (FunP) p->tp;
         BlockP b = f->body;
         f->body = 0;
         switch (p->n_sto) {
            case AUTO:
            case STATIC:
            case REGISTER:
            case EXTERN:
               error("M%n cannot be%k", p, p->n_sto);
               p->n_sto = 0;
         }
         m = dclId(p, this->memtbl, PUBLIC);
         if (b) {
            if (m->tp->defined)
               error("two definitions of%n", m);
            else if (p->where.line != m->where.line)
               errorT('s', "previously declared%n cannot be defined inCD", p); //(#) Clipped at "cannot be ".
            else
               ((FunP) m->tp)->body = b;
         }
      } else {
         m = dclId(p, this->memtbl, PUBLIC);
         if (m) {
            if (m->n_stclass == STATIC && m->n_initializer)
               errorT('s', "staticM%n withIr", m);
            if (in_union) {
               if (usz < byte_offset) usz = byte_offset;
               byte_offset = 0;
            }
         }
      }
   /* FreeId(p); */
   }
/* this->pubmem = 0;
*/
   if (in_union) byte_offset = usz;

   if (this->virt_count || bvirt) { /* assign virtual indices */
      IdP vp[100];
      IdP nn;

      nn = has_ctor(this);
      if (nn == 0 || nn->n_table != this->memtbl)
         errorT('s', "C%n with virtual but no constructor", cname);

      { /*      FUDGE vtbl
           so that the name can be used in initializers
         */
         char *s = _new(20*sizeof *s);
         sprintf(s, "%s__vtbl", this->string);
         IdP n = MakeId(s);
         n->tp = Pfctvec_type;
         IdP nn = insert(gtbl, n, 0);
         use(nn);
      }

      if (this->virt_count = bvirt)
         for (i = 0; i < bvirt; i++) vp[i] = bcl->virt_init[i];

      for (nn = get_mem(this->memtbl, i = 1); nn; nn = get_mem(this->memtbl, ++i)) {
         switch (nn->tp->base) {
            case FCT:
            {
               FunP f = (FunP) nn->tp;
               if (bvirt) {
                  IdP vn = look(btbl, nn->string, 0);
                  if (vn) { /* match up with base class */
                     if (vn->n_table == gtbl) goto vvv;
                     FunP vnf;
                     switch (vn->tp->base) {
                        case FCT:
                           vnf = (FunP) vn->tp;
                           if (vnf->f_virtual) {
                              if (checkType((TypeP)vnf, (TypeP)f, 0)) error("virtual%nT mismatch:%t and%t", nn, f, vnf); //(#) Clipped at "virtual".
                              f->f_virtual = vnf->f_virtual;
                              vp[f->f_virtual - 1] = nn;
                           } else
                              goto vvv;
                           break;
                        case OVERLOAD:
                        {
                           GenP g = (GenP) vn->tp;
                           if (f->f_virtual || ((FunP) g->fct_list->f->tp)->f_virtual)
                              errorT('s', "virtual%n overloaded inBC but not in derivedC", nn); //(#) Clipped at "overloaded inB".
                           break;
                        }
                        default:
                           goto vvv;
                     }
                  } else
                     goto vvv;
               } else {
                vvv:
/*errorT('d',"vvv: %n f_virtual %d virt_count %d",nn,f->f_virtual,this->virt_count);*/
                  if (f->f_virtual) {
                     f->f_virtual = ++this->virt_count;
                     switch (f->f_virtual) {
                        case 1:
                        {
                           IdP vpn = MakeId("_vptr");
                           vpn->tp = Pfctvec_type;
                           (void)dclId(vpn, this->memtbl, PUBLIC);
                           FreeId(vpn);
                        }
                        default:
                           vp[f->f_virtual - 1] = nn;
                     }
                  }
               }
               break;
            }

            case OVERLOAD:
            {
               IdListP gl;
               GenP g = (GenP) nn->tp;
/*errorT('d',"overload%n bvirt==%d",nn,bvirt);*/
               if (bvirt) {
                  IdP vn = look(btbl, nn->string, 0);
                  GenP g2;
                  FunP f2;
                  if (vn) {
/*errorT('d',"vn%n tp%k",vn,vn->tp->base);*/
                     if (vn->n_table == gtbl) goto ovvv;
                     switch (vn->tp->base) {
                        default:
                           goto ovvv;
                        case FCT:
                           f2 = (FunP) vn->tp;
                           if (f2->f_virtual || ((FunP) g->fct_list->f->tp)->f_virtual)
                              errorT('s', "virtual%n overloaded in derivedC but not inBC", nn); //(#) Clipped at "overloaded in ".
                           break;
                        case OVERLOAD:
                           g2 = (GenP) vn->tp;

                           for (gl = g->fct_list; gl; gl = gl->l) {
                              IdP fn = gl->f;
                              FunP f = (FunP) fn->tp;
                              IdP vn2 = find(g2, f);

                              if (vn2 == 0) {
                                 if (f->f_virtual) errorT('s', "virtual overloaded%n not found inBC", fn); //(#) Clipped at "errorT('s'".
                              } else {
                                 FunP vn2f = (FunP) vn2->tp;
                                 if (vn2f->f_virtual) {
                                    f->f_virtual = vn2f->f_virtual; //(#) Clipped at "vn2f".
                                    vp[f->f_virtual - 1] = fn; //(#) Clipped at "f_virtual-1] ".
                                 }
                              }
                           }
                           break;
                     }
                  } else
                     goto ovvv;
               } else {
                ovvv:
                  for (gl = g->fct_list; gl; gl = gl->l) {
                     IdP fn = gl->f;
                     FunP f = (FunP) fn->tp;

                  /*fprintf(stderr,"fn %s f %d %d %d count %d\n",fn->string,f,f->base,f->f_virtual,this->virt_count+1);*///(#) Clipped at "f_virtual,vir".
                     if (f->f_virtual) {
                        f->f_virtual = ++this->virt_count;
                        switch (f->f_virtual) {
                           case 1:
                           {
                              IdP vpn = MakeId("_vptr");
                              vpn->tp = Pfctvec_type;
                              (void)dclId(vpn, this->memtbl, 0);
                              FreeId(vpn);
                           }
                           default:
                              vp[f->f_virtual - 1] = fn;
                        }
                     }
                  }
               }
               break;
            }
         }
      }
      this->virt_init = _new(this->virt_count*sizeof *this->virt_init);
      for (i = 0; i < this->virt_count; i++) this->virt_init[i] = vp[i];
   }

   for (p = this->pubdef, this->pubdef = 0; p; p = p->n_list) {
      char *qs = p->n_qualifier->string;
      char *ms = p->string;
      IdP cx;
      TableP ctbl;
      IdP mx;

      if (strcmp(ms, qs) == 0) ms = "_ctor";

      for (cx = this->clbase; cx; cx = ((ClassP) cx->tp)->clbase) {
         if (strcmp(cx->string, qs) == 0) goto ok;
      }
      error("publicQr %s not aBC", qs);
      continue;
    ok:
      ctbl = ((ClassP) cx->tp)->memtbl;
      mx = lookc(ctbl, ms, 0);

      if (Ebase) {
         if (!has_friend(Ebase, cc->nof)) error("QdMN%n is in privateBC", p); //(#) Clipped at "is in privat".
      } else if (Epriv) {
         if (!has_friend(Epriv, cc->nof)) error("QdMN%n is private", p); //(#) Clipped at 'is private",'.
      }
      if (mx == 0) {
         error("C%n does not have aM %s", cx, p->string);
         p->tp = (TypeP)any_type;
      } else {
         if (mx->tp->base == OVERLOAD)
            errorT('s', "public specification of overloaded%n", mx);
         p->base = PUBLIC;
      }

      p->n_qualifier = mx;
      (void)insert(this->memtbl, p, 0);
      if (Nold) error("twoDs of CM%n", p);
   }

   if (bit_offset) byte_offset += SZ_WORD;
   if (byte_offset < SZ_STRUCT) {
      IdP n = MakeId("_dummy");
      switch (SZ_STRUCT - this->obj_size) {
         case 1:
            n->tp = (TypeP)char_type;
            break;
         case 2:
            n->tp = char2_type;
            break;
         case 3:
            n->tp = char3_type;
            break;
         case 4:
            n->tp = char4_type;
            break;
         default:
            n->tp = (TypeP)MakeVec((TypeP)char_type, 0);
            ToVecP(n->tp)->size = SZ_STRUCT - this->obj_size;
      }
      (void)dclId(n, this->memtbl, 0);
      FreeId(n);
/*errorT('d',"dummy bo=%d",byte_offset);*/
   }
   int waste = byte_offset % max_align;
   if (waste) { /* fudge, ensure derived class get right sizeof */
      waste = max_align - waste;
/*errorT('d',"%s: waste %d tbl=%d",this->string,waste,this->memtbl);*/
      IdP n = MakeId("_waste");
      switch (waste) {
         case 1:
            n->tp = (TypeP)char_type;
            break;
         case 2:
            n->tp = char2_type;
            break;
         case 3:
            n->tp = char3_type;
            break;
         case 4:
            n->tp = char4_type;
            break;
         default:
            n->tp = (TypeP)MakeVec((TypeP)char_type, 0);
            ToVecP(n->tp)->size = waste;
      }
      (void)dclId(n, this->memtbl, 0);
      FreeId(n);
      if (byte_offset % max_align) errorT('i', "failed to align %s", this->string);
   }
/*errorT('d',"sz=%d al=%d",byte_offset,max_align);*/
   this->obj_size = byte_offset;
   this->obj_align = max_align;

   if (has_dtor(this) && has_ctor(this) == 0)
      errorT('w', "%s has destructor but no constructor", this->string);

   if (this->itor == 0 && has_oper(this, ASSIGN))
      errorT('w', "%s has assignment defined but not initialization (no %s(...))", this->string, this->string); //(#) Clipped at "(no %s(". Not verified.

   this->defined = 1;

   for (p = get_mem(this->memtbl, i = 1); p; p = get_mem(this->memtbl, ++i)) {
   /* define members defined inline */
      switch (p->tp->base) {
         case FCT:
         {
            FunP f = (FunP) p->tp;
            if (f->body) {
               f->f_inline = 1;
               p->n_sto = STATIC;
               dclFun(f, p);
            }
            break;
         }
         case OVERLOAD:
         {
            GenP g = (GenP) p->tp;
            IdListP gl;
            for (gl = g->fct_list; gl; gl = gl->l) {
               IdP n = gl->f;
               FunP f = (FunP) n->tp;
               if (f->body) {
                  f->f_inline = 1;
                  n->n_sto = STATIC;
                  dclFun(f, n);
               }
            }
         }
      }
   }

   IdListP fl; /* define friends defined inline */
   for (fl = this->friend_list; fl; fl = fl->l) {
      IdP p = fl->f;
      switch (p->tp->base) {
         case FCT:
         {
            FunP f = (FunP) p->tp;
            if (f->body && f->defined == 0) {
               f->f_inline = 1;
               p->n_sto = STATIC;
               dclFun(f, p);
            }
            break;
         }
         case OVERLOAD:
         {
            GenP g = (GenP) p->tp;
            IdListP gl;
            for (gl = g->fct_list; gl; gl = gl->l) {
               IdP n = gl->f;
               FunP f = (FunP) n->tp;
               if (f->body && f->defined == 0) {
                  f->f_inline = 1;
                  n->n_sto = STATIC;
                  dclFun(f, n);
               }
            }
         }
      }
   }

   byte_offset = byte_old;
   bit_offset = bit_old;
   max_align = max_old;

   unstack(cc);
}

void dclEnum(EnumP this, IdP n, TableP tbl) {
#define FIRST_ENUM 0
   int nmem = no_of_names(this->mem);
   IdP p;
   IdP ns = 0;
   IdP nl;
   int enum_old = enum_count;
   this->no_of_enumerators = nmem;

   enum_count = FIRST_ENUM;

   if (this == 0) errorT('i', "0->dclEnum(%d)", tbl);

   for (p = this->mem, this->mem = 0; p; p = p->n_list) {
      IdP nn;
      if (p->n_initializer) {
         ExP i = typ(p->n_initializer, tbl);
         Neval = 0;
         enum_count = eval(i);
         if (Neval) error("%s", Neval);
         DEL(Ex, i);
         p->n_initializer = 0;
      }
      p->n_evaluated = 1;
      p->n_val = enum_count++;
      nn = insert(tbl, p, 0); /* ??? */
      if (Nold) {
         if (nn->n_stclass == ENUM) {
            if (p->n_val != nn->n_val) error("twoDs of enum constant%n", nn); //(#) Clipped at "twoDs of enum con". Not verified.
         } else
            error("incompatibleDs of%n", nn);
      } else {
         nn->n_stclass = ENUM; /* no store will be allocated */
         if (ns)
            nl->n_list = nn;
         else
            ns = nn;
         nl = nn;
      }
      FreeId(p);
   }

   this->mem = ns;

   enum_count = enum_old;
   this->defined = 1;
}

/*

	The argument names are placed in the memtable of the body.
	This makes
		f(int a) { int a; };
	illegal

	The argument names/types remain linked even after they are entered
	into the symbol table,
	but class and enum declarations are unlinked
void dclFun(FunP this, TableP tbl) {
   int nmem = TBLSIZE;
   IdP a;
   IdP ll;
   int bit_old = bit_offset;
   int byte_old = byte_offset;
   int max_old = max_align;
   int stack_old = stack_size;

   if (this->base != FCT) errorT('i', "dclFun(%d)", this->base);
   if (this->body == 0 || this->body->memtbl) errorT('i', "dclFun(%d)", this->body);
   if (tbl->base != TABLE) errorT('i', "dclFun(tbl=%d)", tbl->base);

   this->body->memtbl = MakeTable(nmem, tbl, 0);
   this->body->own_tbl = 1;

   max_align = AL_FRAME;
   stack_size = byte_offset = SZ_BOTTOM;
   bit_offset = 0;

   for (a = this->argtype, ll = 0; a; a = a->n_list) {
      IdP n = dclId(a, this->body->memtbl, ARG);
      n->n_list = 0;
      switch (a->tp->base) {
         case CLASS:
         case ENUM:
            break;
         default:
            if (ll)
               ll->n_list = n;
            else
               this->argtype = n;
            ll = n;
      }
   }

   this->frame_size = stack_size + SZ_TOP;
   this->frame_size = ((this->frame_size - 1) / AL_FRAME) * AL_FRAME + AL_FRAME;
   bit_offset = bit_old;
   byte_offset = byte_old;
   max_align = max_old;
   stack_size = stack_old;
}*/

StP curr_loop;
StP curr_switch;
BlockP curr_block;

void reached(StP this) {
   register StP ss = this->s_list;

   if (ss == 0) return;

   switch (ss->base) {
      case LABEL:
      case CASE:
      case DEFAULT:
         break;
      default:
         errorT('w', "statement not reached");
      /* delete unreacheable code */
         for (; ss; ss = ss->s_list) {
            switch (ss->base) {
               case LABEL:
               case CASE:
               case DEFAULT: /* reachable */
                  this->s_list = ss;
                  return;
               case IF:
               case DO:
               case WHILE:
               case SWITCH:
               case FOR:
               case BLOCK: /* may hide a label */
                  this->s_list = ss;
                  return;
            }
         }
         this->s_list = 0;
   }
}

bit arg_err_suppress;

ExP check_cond(ExP e, Token b, TableP tbl) {
   IdP cn;
   if (cn = is_cl_obj(e->tp)) {
      ClassP cl = (ClassP) cn->tp;
      int i = 0;
      IdP found = 0;
      for (IdP on = cl->conv; on; on = on->n_list) {
         FunP f = (FunP) on->tp;
         TypeP t = f->returns;
       xx:
         switch (t->base) {
            case TYPE:
               t = ((BaseP) t)->b_name->tp;
               goto xx;
            case CHAR:
            case SHORT:
            case INT:
            case LONG:
            case EOBJ:
            case FLOAT:
            case DOUBLE:
            case PTR:
               i++;
               found = on;
         }
      }
      switch (i) {
         case 0:
            error("%nO in%k expression", cn, b);
            return e;
         case 1:
         {
/*errorT('d',"cond%t<-%t",((FunP)found->tp)->returns,e->tp);*/
            ClassP cl = (ClassP) cn->tp;
            RefP r = MakeRef(DOT, e, found);
            ExP c = MakeEx(G_CALL, (ExP)r, 0);
            c->fct_name = found;
            c->tp = ((FunP) found->tp)->returns;
            return c;
         }
         default:
            error("%d possible conversions for%nO in%k expression", i, cn, b); //(#) Clipped at "i,cn".
            return e;
      }

   }
   num_ptr(e->tp, b);
   return e;
}

/*
	typecheck statement "this" in scope "curr_block->tbl"
*/
void dclSt(StP this) {
   StP ss;
   IdP n;
   IdP nn;
   StP ostmt = Cstmt;

   for (ss = this; ss; ss = ss->s_list) {
      StP old_loop, old_switch;
      Cstmt = ss;
      TableP tbl = curr_block->memtbl;
   /*errorT('d',"ss %d%k tbl %d e %d%k s %d%k sl %d%k", ss, ss->base, tbl, ss->e, (ss->e)?ss->e->base:0, ss->s, (ss->s)?ss->s->base:0, ss->s_list, (ss->s_list)?ss->s_list->base:0);*///(#) Clipped at "ss->e, (ss->e".
      switch (ss->base) {
         case BREAK:
            if (curr_loop == 0 && curr_switch == 0)
               error("%k not in loop or switch", BREAK);
            reached(ss);
            break;

         case CONTINUE:
            if (curr_loop == 0) error("%k not in loop", CONTINUE);
            reached(ss);
            break;

         case DEFAULT:
            if (curr_switch == 0) {
               error("default not in switch");
               break;
            }
            if (curr_switch->has_default) error("two defaults in switch"); //(#) Clipped at "two defaults in switch".
            curr_switch->has_default = ss;
            ss->s->s_list = ss->s_list;
            ss->s_list = 0;
            dclSt(ss->s);
            break;

         case SM:
            ss->e = (ss->e != dummy) ? typ(ss->e, tbl) : 0;
            break;

         case DELETE:
         {
            int i;
            ss->e = typ(ss->e, tbl);
            i = num_ptr(ss->e->tp, DELETE);
            if (i != P) error("nonP deleted");
            break;
         }

         case RETURN:
         {
            IdP fn = cc->nof;
            TypeP rt = ((FunP) fn->tp)->returns;
            ExP v = ss->e;
            if (v != dummy) {
               if (rt->base == VOID)
                  errorT('w', "unX return value");
               else {
                  v = typ(v, tbl);
                lx:
                  switch (rt->base) {
                     case TYPE:
                        rt = ((BaseP) rt)->b_name->tp;
                        goto lx;
                     case RPTR:
                        ss->e = ref_init((PtrP) rt, v, tbl);
                        break;
                     case COBJ:
                     {
                        IdP rv = look(tbl, "_result", 0);
                        ss->e = class_init((ExP)rv, rt, v, tbl);
                        break;
                     }
                     case ANY:
                        break;
                     case INT:
                     case CHAR:
                     case LONG:
                     case SHORT:
                        if (((BaseP) rt)->b_unsigned && v->base == UMINUS && v->e2->base == ICON)
                           errorT('w', "negative retured fromF returning unsigned"); //(#) Clipped at "negative retured".
                     default:
                        ss->e = v;
                        if (checkType(rt, v->tp, ASSIGN))
                           error("bad return valueT for%n:%t (%tX)", fn, v->tp, rt); //(#) Clipped at "bad return valueT fo".
                  }
               }
            } else {
               if (rt->base != VOID) errorT('w', "return valueX");
            }
            reached(ss);
            break;
         }

         case DO: /* in DO the stmt is before the test */
            inline_restr |= 8; //(#) Clipped at "before the test */                 ".
            old_loop = curr_loop;
            curr_loop = ss;
            if (ss->s->base == DCL) errorT('s', "D as onlyS in do-loop");
            dclSt(ss->s);
         /*      tbl = curr_block->memtbl; */
            ss->e = typ(ss->e, tbl);
            ss->e = check_cond(ss->e, DO, tbl);
            curr_loop = old_loop;
            break;

         case WHILE:
            inline_restr |= 8;
            old_loop = curr_loop;
            curr_loop = ss;
            ss->e = typ(ss->e, tbl);
         /*num_ptr(ss->e->tp, ss->base); */
            ss->e = check_cond(ss->e, WHILE, tbl);
            if (ss->s->base == DCL) errorT('s', "D as onlyS in while-loop"); //(#) Clipped at "in while-loop".
            dclSt(ss->s);
            curr_loop = old_loop;
            break;

         case SWITCH:
         {
            int ne = 0;
            inline_restr |= 4;
            old_switch = curr_switch;
            curr_switch = ss;
            ss->e = typ(ss->e, tbl);
         /*      num_ptr(ss->e->tp, SWITCH); */
            ss->e = check_cond(ss->e, SWITCH, tbl);
            {
               TypeP tt = ss->e->tp;
             sii:
               switch (tt->base) {
                  case TYPE:
                     tt = ((BaseP) tt)->b_name->tp;
                     goto sii;
                  case EOBJ:
                     ne = ToEnumP(ToBaseP(tt)->b_name->tp)->no_of_enumerators; //(#) Clipped at "no_of_en".
                  case ZTYPE:
                  case ANY:
                  case CHAR:
                  case SHORT:
                  case INT:
                  case LONG:
                     break;
                  default:
                     errorT('s', "%t switch expression", ss->e->tp); //(#) Clipped at "ss->e->tp)".
               }
            }
            dclSt(ss->s);
            if (ne) { /* see if the number of cases is "close to"
                         but not equal to the number of enumerators //(#) Clipped at "enumerato".
                       */
               int i = 0;
               StP cs;
               for (cs = ss->case_list; cs; cs = cs->case_list) i++;
               if (i && i != ne) {
                  if (ne < i) {
                   ee:errorT('w', "switch (%t) with %d cases (%d enumerators)", ss->e->tp, i, ne);
                                                                                                 //(#) Clipped at "%d case".
                  } else {
                     switch (ne - i) {
                        case 1:
                           if (3 < ne) goto ee;
                        case 2:
                           if (7 < ne) goto ee;
                        case 3:
                           if (23 < ne) goto ee;
                        case 4:
                           if (60 < ne) goto ee;
                        case 5:
                           if (99 < ne) goto ee;
                     }
                  }
               }
            }
            curr_switch = old_switch;
            break;
         }
         case CASE:
            if (curr_switch == 0) {
               error("case not in switch");
               break;
            }
            ss->e = typ(ss->e, tbl);
            num_ptr(ss->e->tp, CASE);
            {
               TypeP tt = ss->e->tp;
             iii:
               switch (tt->base) {
                  case TYPE:
                     tt = ((BaseP) tt)->b_name->tp;
                     goto iii;
                  case ZTYPE:
                  case ANY:
                  case CHAR:
                  case SHORT:
                  case INT:
                  case LONG:
                     break;
                  default:
                     errorT('s', "%t case expression", ss->e->tp);
               }
            }
            if (1) {
               Neval = 0;
               int i = eval(ss->e);
               if (Neval == 0) {
                  StP cs;
                  for (cs = curr_switch->case_list; cs; cs = cs->case_list) { //(#) Clipped at "cs; cs->cs->".
                     if (cs->case_value == i) error("case %d used twice in switch", i); //(#) Clipped at "cas".
                  }
                  ss->case_value = i;
                  ss->case_list = curr_switch->case_list;
                  curr_switch->case_list = ss;
               }
            }
            if (ss->s->s_list) errorT('i', "case%k", ss->s->s_list->base);
            ss->s->s_list = ss->s_list;
            ss->s_list = 0;
            dclSt(ss->s);
            break;

         case GOTO:
            inline_restr |= 2;
            reached(ss);
         case LABEL:
         /* Insert label in function mem table;
            labels have function scope.
          */
            n = ss->d;
            nn = insert(cc->ftbl, n, LABEL);

         /* Set a ptr to the mem table corresponding to the scope
            in which the label actually occurred.  This allows the
            processing of goto's in the presence of ctors and dtors
          */
            if (ss->base == LABEL) {
               nn->n_realscope = curr_block->memtbl;
               inline_restr |= 1;
            }

            if (Nold) {
               if (ss->base == LABEL) {
                  if (nn->n_initializer) error("twoDs of label%n", n); //(#) Clipped at "twoDs of labe".
                  nn->n_initializer = (ExP) 1;
               }
               if (n != nn) ss->d = nn;
            } else {
               if (ss->base == LABEL) nn->n_initializer = (ExP) 1;
               nn->where = ss->where;
            }
            if (ss->base == GOTO)
               use(nn);
            else {
               if (ss->s->s_list) errorT('i', "label%k", ss->s->s_list->base); //(#) Clipped at "ss->s->s_lis".
               ss->s->s_list = ss->s_list;
               ss->s_list = 0;
               assign(nn);
            }
            if (ss->s) dclSt(ss->s);
            break;

         case IF:
         {
            ExP ee = typ(ss->e, tbl);
            if (ee->base == ASSIGN) {
               Neval = 0;
               (void)eval(ee->e2);
               if (Neval == 0) errorT('w', "constant assignment in condition"); //(#) Clipped at "assignment in c".
            }
            ss->e = ee = check_cond(ee, IF, tbl);
            switch (ee->tp->base) {
               case INT:
               case ZTYPE:
               {
                  int i;
                  Neval = 0;
                  i = eval(ee);
                  if (Neval == 0) {
/*fprintf(stderr,"if (%d) %d %d\n",i,ss->e,ss->e->base);*/
                     StP sl = ss->s_list;
                     if (i) {
                        DEL(St, ss->else_stmt);
                        dclSt(ss->s);
                        *ss = *ss->s;
                     } else {
                        DEL(St, ss->s);
                        if (ss->else_stmt) {
                           dclSt(ss->else_stmt);
                           *ss = *ss->else_stmt;
                        } else {
                           ss->base = SM;
                           ss->e = dummy;
                           ss->s = 0;
                        }
                     }
                     ss->s_list = sl;
                     continue;
                  }
               }
            }
            dclSt(ss->s);
            if (ss->else_stmt) dclSt(ss->else_stmt);
            break;
         }
         case FOR:
            inline_restr |= 8;
            old_loop = curr_loop;
            curr_loop = ss;
            if (ss->for_init) {
               StP fi = ss->for_init;
               switch (fi->base) {
                  case SM:
                     if (fi->e == dummy) {
                        ss->for_init = 0;
                        break;
                     }
                  default:
                     dclSt(fi);
                     break;
                  case DCL:
                     dclSt(fi);
/*errorT('d',"dcl=>%k",fi->base);*/
                     switch (fi->base) {
                        case BLOCK:
                        {
                        /* { ... for( { a } b ; c) d ; e }
                           =>
                           { ... { a for ( ; b ; c) d ; e }}
                         */
                           StP tmp = MakeSt(SM, curloc, 0);
                           *tmp = *ss; /* tmp = for */
                           tmp->for_init = 0;
                           *ss = *fi; /* ss = { } */
                           if (ss->s)
                              ss->s->s_list = tmp;
                           else
                              ss->s = tmp;
                           curr_block = (BlockP) ss;
                           tbl = curr_block->memtbl;
                           ss = tmp; /* rest of for and s_list *///(#) Clipped at "for and ".
                           break;
                        }
                     }
               }
            }
            if (ss->e == dummy)
               ss->e = 0;
            else {
               ss->e = typ(ss->e, tbl);
               ss->e = check_cond(ss->e, FOR, tbl);
            }
            if (ss->s->base == DCL) errorT('s', "D as onlyS in for-loop"); //(#) Clipped at '"D as onlyS in for-loop")'.
            dclSt(ss->s);
            ss->e2 = (ss->e2 == dummy) ? 0 : typ(ss->e2, tbl);
            curr_loop = old_loop;
            break;

         case DCL: /* declaration after statement */
            if (0) {
               IdP n;
               ExP in;
               if (curr_block->own_tbl == 0) {
                  curr_block->memtbl = tbl = MakeTable(8, tbl, 0);
                  curr_block->own_tbl = 1;
               }
               IdP dd = ss->d;
               if (dd->n_list) errorT('s', "list ofDs not at head of block");
               n = dclId(dd, tbl, FCT);
               in = n->n_initializer;
               ss->base = SM;
               if (n->n_stclass == STATIC && in) {
                  errorT('s', "Id static not at head of block");
                  goto dum;
               }
               IdP cln = is_cl_obj(n->tp);
               if (cln && has_dtor(((ClassP) cln->tp)))
                  errorT('s', "%n ofC%n with destructor not at head of block", n, cln); //(#) Clipped at "not at head of ". Not verified.
               if (in) {
                  n->n_initializer = 0;
                  switch (in->base) {
                     case G_CALL: /* constructor? */
                     {
                        IdP fn = in->fct_name;
                        if (fn == 0 || fn->n_oper != CTOR) goto ass;
                        break;
                     }
                     case ILIST:
                        errorT('s', "Ir list not at head of block");
                        goto dum;
                     case STRING:
                        n->n_initializer = in; /* constant */
                        goto dum;
                     default:
                      ass:
                        in = MakeEx(ASSIGN, (ExP)n, in);
                  }
                  ss->e = in;
               } else {
                dum:
                  ss->e = dummy;
               }
               break;
            }

            {
            /*      collect all the contiguous DCL nodes from the
               head of the s_list. find the next statement
             */
               int non_trivial = 0;
               int count = 0;
               IdP tail = ss->d;
               for (IdP nn = tail; nn; nn = nn->n_list) {
               /*      find tail;
                  detect non-trivial declarations
                */
                  count++;
                  if (nn->n_list) tail = nn->n_list;
                  IdP n = look(tbl, nn->string, 0);
                  if (n && n->n_table == tbl) non_trivial = 2;
                  if (non_trivial) continue;
                  ExP in = nn->n_initializer;
/*errorT('d',"in %d",in);*/
                  if (in == 0) continue;
                  if (non_trivial == 0) non_trivial = 1;
                  if (nn->n_stclass == STATIC) {
                     non_trivial = 2;
                     continue;
                  }
                  switch (in->base) {
                     case ILIST:
                     case STRING:
                        non_trivial = 2;
                        continue;
                  }
                  IdP cln = is_cl_obj(nn->tp);
                  if (cln == 0) continue;
                  if (has_dtor(((ClassP) cln->tp))) non_trivial = 2;
               }
/*errorT('d',"non_trivial %d",non_trivial);*/
               while (ss->s_list && ss->s_list->base == DCL) {
                  StP sx = ss->s_list;
                  tail = tail->n_list = sx->d; /* add to tail */
                  for (nn = sx->d; nn; nn = nn->n_list) {
                  /*      find tail;
                     detect non-trivial declarations
                   */
                     count++;
                     if (nn->n_list) tail = nn->n_list;
                     IdP n = look(tbl, nn->string, 0);
                     if (n && n->n_table == tbl) non_trivial = 2;
                     if (non_trivial) continue;
                     ExP in = nn->n_initializer;
                     if (in == 0) continue;
                     if (non_trivial == 0) non_trivial = 1;
                     if (nn->n_stclass == STATIC) {
                        non_trivial = 2;
                        continue;
                     }
                     switch (in->base) {
                        case ILIST:
                        case STRING:
                           non_trivial = 2;
                           continue;
                     }
                     IdP cln = is_cl_obj(nn->tp);
                     if (cln == 0) continue;
                     if (has_dtor(((ClassP) cln->tp))) non_trivial = 2; //(#) Clipped at "non_triv".
                  }
                  ss->s_list = sx->s_list;
               /* FreeSt(sx);      */
               }
               StP next_st = ss->s_list;
            /*errorT('d',"non_trivial %d curr_block->own_tbl %d inline_restr %d",non_trivial,curr_block->own_tbl,inline_restr);*///(#) Clipped at "non_trivial,curr".
               if (non_trivial == 2 /* must */
                  || (non_trivial == 1 /* might */
                     && (curr_block->own_tbl == 0 /* just as well */
                        || inline_restr & 3 /* label seen */ )
                  )
                  ) {
               /*      Create a MakeBlock,
                  put all the declarations at the head,
                  and the remainder of the slist as the
                  statement list of the Block.
                */
                  ss->base = BLOCK;

               /*      check that there are no redefinitions since the last //(#) Clipped at "redefinitions since ".
                  "real" (user-written, non-generated) block
                */
                  for (nn = ss->d; nn; nn = nn->n_list) {
                     IdP n;
                     if (curr_block->own_tbl && (n = look(curr_block->memtbl, nn->string, 0)) //(#) Clipped at "nn->string,".
                        && n->n_table->real_block == curr_block->memtbl->real_block) //(#) Clipped at "==curr_block->mem".
                        error("twoDs of%n", n);
                  }

               /*      attach the remainder of the s_list
                  as the statement part of the Block.
                */
                  ss->s = next_st;
                  ss->s_list = 0;

               /*      create the table in advance, in order to set the //(#) Clipped at "in order to se".
                  real_block ptr to that of the enclosing table //(#) Clipped at "enclosing tab".
                */
                  ss->memtbl = MakeTable(count + 4, tbl, 0);
                  ss->memtbl->real_block = curr_block->memtbl->real_block; //(#) Clipped at "->real_b".

                  dclBlock(((BlockP) ss), ss->memtbl);
               } else { /*      to reduce the number of symbol tables,
                           do not make a MakeBlock,
                           instead insert names in enclosing block,
                           and make the initializers into expression
                           statements.
                         */
                  StP sss = ss;
                  for (nn = ss->d; nn; nn = nn->n_list) {
                     IdP n = dclId(nn, tbl, FCT);
/*errorT('d',"%n->dclId(%d) -> %d init %d sss=%d",nn,tbl,n,n->n_initializer,sss);*/
                     if (n == 0) continue;
                     ExP in = n->n_initializer;
                     n->n_initializer = 0;
                     if (ss) {
                        sss->base = SM;
                        ss = 0;
                     } else
                        sss = sss->s_list = (StP)MakeESt(SM, sss->where, 0, 0); //(#) Clipped at "estmt(SM,ss".
                     if (in) {
                        switch (in->base) {
                           case G_CALL: /* constructor? */
                           {
                              IdP fn = in->fct_name;
                              if (fn && fn->n_oper == CTOR) break; //(#) Clipped at "==CTOR)".
                           }
                           default:
                              in = MakeEx(ASSIGN, (ExP)n, in);
                        }
                        sss->e = typ(in, tbl);
                     } else
                        sss->e = dummy;
                  }
                  ss = sss;
                  ss->s_list = next_st;
               }
               break;
            }

         case BLOCK:
            dclBlock(((BlockP) ss), tbl);
            break;

         case ASM:
         /* save string */
            break;

         default:
            errorT('i', "badS(%d %d)", ss, ss->base);
      }
   }

   Cstmt = ostmt;
}

/*
	Note: for a block without declarations memtbl denotes the table
	for the enclosing scope.
	A function body has its memtbl created by dclFun().
*/
void dclBlock(BlockP this, TableP tbl) {
   int bit_old = bit_offset;
   int byte_old = byte_offset;
   int max_old = max_align;
   BlockP block_old = curr_block;

   if (this->base != BLOCK) errorT('i', "dclBlock(%d)", this->base);

   curr_block = this;

   if (this->d) {
      IdP n;
      this->own_tbl = 1;
      if (this->memtbl == 0) {
         int nmem = no_of_names(this->d) + 4;
         this->memtbl = MakeTable(nmem, tbl, 0);
         this->memtbl->real_block = (StP)this;
      /*      this is a "real" block from the
         source text, and not one created by DCL's
         inside a Block. */
      } else if (this->memtbl != tbl) errorT('i', "dclBlock(?)");

      IdP nx;
      for (n = this->d; n; n = nx) {
         nx = n->n_list;
         dclId(n, this->memtbl, FCT);
         switch (n->tp->base) {
            case CLASS:
            case ANON:
            case ENUM:
               break;
            default:
               FreeId(n);
         }
      }
   } else
      this->memtbl = tbl;

   if (this->s) {
      IdP odcl = Cdcl;
      IdP m;
      int i;

      dclSt(this->s);

      if (this->own_tbl)
         for (m = get_mem(this->memtbl, i = 1); m; m = get_mem(this->memtbl, ++i)) {
            TypeP t = m->tp;

            if (t == 0) {
               if (m->n_assigned_to == 0) errorT('w', "undefined label %s", m->string); //(#) Clipped at "undefined lab".
               if (m->n_used == 0) errorT('w', "label %s not used", m->string); //(#) Clipped at '"label %s not used", '.
               continue;
            }
          ll:
            switch (t->base) {
               case TYPE:
                  t = ((BaseP) t)->b_name->tp;
                  goto ll;
               case CLASS:
               case ENUM:
               case FCT:
               case VEC:
                  continue;
            }

            if (m->n_addr_taken == 0) {
               if (m->n_used) {
                  if (m->n_assigned_to) {
                  } else {
                     switch (m->n_scope) {
                        case FCT:
                           Cdcl = m;
                           errorT('w', "%n used but not set", m); //(#) Clipped at "%n used but not ".
                     }
                  }
               } else {
                  if (m->n_assigned_to) {
                  } else {
                     switch (m->n_scope) {
                        case ARG:
                           if (m->string[0] == '_' && m->string[1] == 'A') break; /* generated name: cannot be used *///(#) Clipped at "&& m-".
                        case FCT:
                           Cdcl = m;
                           errorT('w', "%n not used", m);
                     }
                  }
               }
            }
         }
      Cdcl = odcl;
   }

   this->d = 0;

   if (bit_offset) byte_offset += SZ_WORD;
   if (stack_size < byte_offset) stack_size = byte_offset;
   bit_offset = bit_old;
   byte_offset = byte_old;
   curr_block = block_old;
}

int no_of_names(IdP this) {
   register int i = 0;
   register IdP n;
   for (n = this; n; n = n->n_list) i++;
   return i;
}

static ExP lvec[20], *lll;
static ExP list_back = 0;
#define list_put_back(x) list_back = x;

void new_list(ExP lx) {
   if (lx->base != ILIST) errorT('i', "IrLX");

   lll = lvec;
   lll++;
   *lll = lx->e1;
}

ExP next_elem(void) {
   ExP e;
   ExP lx;

   if (lll == lvec) return 0;

   lx = *lll;

   if (list_back) {
      e = list_back;
      list_back = 0;
      return e;
   }

   if (lx == 0) { /* end of list */
      lll--;
      return 0;
   }

   switch (lx->base) {
      case ELIST:
         e = lx->e1;
         *lll = lx->e2;
         switch (e->base) {
            case ILIST:
               lll++;
               *lll = e->e1;
               return (ExP) 1; /* start of new ILIST */
            case ELIST:
               error("nestedEL");
               return 0;
            default:
               return e;
         }
      default:
         errorT('i', "IrL");
   }
}

/*
	see if the list lll can be assigned to something of type t
	nn is the name of the variable for which the assignment is taking place.
	il is the last list element returned by next_elem()
*/
void list_check(IdP nn, TypeP t, ExP il) {
   ExP e;
   bit lst = 0;
   int i;
   ClassP cl;
   switch ((int)il) {
      case 0:
         break;
      case 1:
         lst = 1;
         break;
      default:
         list_put_back(il);
   }

 zzz:
   switch (t->base) {
      case TYPE:
         t = ((BaseP) t)->b_name->tp;
         goto zzz;

      case VEC:
      {
         VecP v = (VecP) t;
         TypeP vt = v->typ;

         if (v->size) { /* get at most v->size initializers */
            for (i = 0; i < v->size; i++) { /* check next list element type *///(#) Clipped at "list element type".
             ee:
               e = next_elem();

            /* "too few" initializers are legal */
               if (e == 0) goto xsw;
             vtz:
               switch (vt->base) {
                  case TYPE:
                     vt = ((BaseP) vt)->b_name->tp;
                     goto vtz;
                  case VEC:
                  case COBJ:
                     list_check(nn, vt, e);
                     break;
                  default:
                     if (e == (ExP) 1) {
                        error("unXIrL");
                        goto ee;
                     }
                     if (checkType(vt, e->tp, ASSIGN))
                        error("badIrT for%n:%t (%tX)", nn, e->tp, vt); //(#) Clipped at ",nn,e-".
               }
            }
            if (lst && (e = next_elem()))error("end of IrLX after vector"); //(#) Clipped at "after ve".
          xsw:;
         } else { /* determine v->size */
            i = 0;
          xx:
            while (e = next_elem()) { /* get another initializer *///(#) Clipped at "get another initializer ".
               i++;
             vtzz:
               switch (vt->base) {
                  case TYPE:
                     vt = ((BaseP) vt)->b_name->tp;
                     goto vtzz;
                  case VEC:
                  case COBJ:
                     list_check(nn, vt, e);
                     break;
                  default:
                     if (e == (ExP) 1) {
                        error("unXIrL");
                        goto xx;
                     }
                     if (checkType(vt, e->tp, ASSIGN))
                        error("badIrT for%n:%t (%tX)", nn, e->tp, vt); //(#) Clipped at "nn,e-".
               }
            }
            v->size = i;
         }
         break;
      }

      case CLASS:
         cl = (ClassP) t;
         goto ccc;

      case COBJ: /* initialize members */
         cl = (ClassP) ((BaseP) t)->b_name->tp;
       ccc:
         {
            TableP tbl = cl->memtbl;
            IdP m;

            if (cl->clbase) {
               list_check(nn, cl->clbase->tp, 0);
            }
            for (m = get_mem(tbl, i = 1); m; m = get_mem(tbl, ++i)) {
               TypeP mt = m->tp;
               switch (mt->base) {
                  case FCT:
                  case OVERLOAD:
                  case CLASS:
                  case ENUM:
                     continue;
               }
               if (m->n_stclass == STATIC) continue;
            /* check assignment to next member */
             dd:
               e = next_elem();
               if (e == 0) break;
             mtz:
               switch (mt->base) {
                  case TYPE:
                     mt = ((BaseP) mt)->b_name->tp;
                     goto mtz;
                  case CLASS:
                  case ENUM:
                     break;
                  case VEC:
                  case COBJ:
                     list_check(nn, m->tp, e);
                     break;
                  default:
                     if (e == (ExP) 1) {
                        error("unXIrL");
                        goto dd;
                     }
                     if (checkType(mt, e->tp, ASSIGN))
                        error("badIrT for %s .%n:%t (%tX)", cl->string, m, e->tp, m->tp); //(#) Clipped at ",cl->stri". Not verified.
               }
            }
            if (lst && (e = next_elem()))error("end of IrLX afterO");
            break;
         }
      default:
         e = next_elem();

         if (e == 0) {
            error("noIr forO");
            break;
         }

         if (e == (ExP) 1) {
            error("unXIrL");
            break;
         }
         if (checkType(t, e->tp, ASSIGN))
            error("badIrT for%n:%t (%tX)", nn, e->tp, t);
         if (lst && (e = next_elem()))error("end of IrLX afterO");
         break;
   }
}
