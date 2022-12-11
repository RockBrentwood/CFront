/* @(#) print.c 1.7 1/27/86 17:49:19 */
/*ident	"@(#)cfront:src/print.c	1.7" */
/**************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

print.c:

	print the output of simpl, typ, or syn in a form suitable for cc input

****************************************************************************/

#include "cfront.h"

extern FILE *out_file;

/*
	print the declaration tree
*/

bit print_mode = 0;
extern int ntok;
int ntok = 0;
int forced_sm = 0;
bit Cast = 0;
InLineP curr_icall;

int MAIN = 0; // fudge to get _main() called by main()

/*
	print the output representation of "t"
*/
void puttok(Token t) {
   const char *s;
   if (t <= 0 || MAXTOK <= t) error("illegal token %d", t);
   s = keys[t];
   if (s == 0) error("V representation token %d", t);
   putst(s);
   if (12 < ntok++) {
      forced_sm = 1;
      ntok = 0;
   /*      putch('\n'); */
      putline(&last_line);
   } else if (t == SM) {
      forced_sm = 1;
      ntok = 0;
      putch('\n');
      last_line.line++;
   }
}

#define MX	20
#define NTBUF	10
typedef struct DecBuf *DecBufP;
struct DecBuf {
/*
   buffer for assembling declaration (or cast)
   left contains CONST_PTR      => *CONST
   CONST_RPTR => &CONST
   PTR  => *
   RPTR => &
   LP   => (
   right contains       RP      => )
   VEC  => [ rnode ]
   FCT  => ( rnode )
   FIELD        => : rnode
 */
// private:
   BaseP b;
   IdP n;
   Token left[MX], right[MX];
   NodeP rnode[MX];
   int li, ri;
};
static inline DecBufP MakeDecBuf(void) {
   DecBufP this = _new(sizeof *this);
   return this;
}
static inline void init(DecBufP this, IdP nn) {
   this->b = 0;
   this->n = nn;
   this->li = this->ri = 0;
};
static inline void base(DecBufP this, BaseP bb) {
   this->b = bb;
};
static inline void frontDecBuf(DecBufP this, Token t) {
   this->left[++this->li] = t;
};
static inline void backDecBuf(DecBufP this, Token t, NodeP nod) {
   this->right[++this->ri] = t;
   this->rnode[this->ri] = nod;
};
static inline void paran(DecBufP this) {
   frontDecBuf(this, LP);
   backDecBuf(this, RP, 0);
};
void putDecBuf(DecBufP this);
DecBufP tbufvec[NTBUF] = { 0 }, tbuf = 0;

int freetbuf = 0;

void putDecBuf(DecBufP this) {
   int i;

   if (MX <= this->li || MX <= this->ri) errorT('i', "T buffer overflow");
   if (this->b == 0) errorT('i', "noBT%s", Cast ? " in cast" : "");

   if (this->n && this->n->n_sto) puttok(this->n->n_sto);

   dcl_printBase(this->b);

   for (; this->li; this->li--)
      switch (this->left[this->li]) {
         case LP:
            puttok(LP);
            break;
         case CONST_PTR:
            puttok(MUL);
            if (print_mode != SIMPL) puttok(CONST);
            break;
         case CONST_RPTR:
            if (print_mode == SIMPL)
               puttok(MUL);
            else
               puttok(ADDROF);
            if (print_mode != SIMPL) puttok(CONST);
            break;
         case PTR:
            puttok(MUL);
            break;
         case RPTR:
            if (print_mode == SIMPL)
               puttok(MUL);
            else
               puttok(ADDROF);
      }

   if (this->n) printId(this->n);

   for (i = 1; i <= this->ri; i++)
      switch (this->right[i]) {
         case RP:
            puttok(RP);
            break;
         case VEC:
            puttok(LB);
            {
               VecP v = (VecP) this->rnode[i];
               ExP d = v->dim;
               int s = v->size;
               if (d) printEx(d);
               if (s) fprintf(out_file, "%d", s);
            }
            puttok(RB);
            break;
         case FCT:
         {
            FunP f = (FunP) this->rnode[i];
            dcl_printFun(f);
         }
            break;
         case FIELD:
         {
            BaseP f = (BaseP) this->rnode[i];
            ExP d = (ExP) f->b_name;
            int s = f->b_bits;
            puttok(COLON);
            if (d) printEx(d);
            if (s) fprintf(out_file, "%d", s);
         }
            break;
      }
}

#define eprint(e) if (e) Eprint(e)

void Eprint(ExP e) {
   switch (e->base) {
      case DUMMY:
         break;
      case NAME:
      case ID:
      case ZERO:
      case ICON:
      case CCON:
      case FCON:
      case STRING:
      case IVAL:
      case TEXT:
      case CM:
      case ELIST:
      case COLON:
      case ILIST:
      case DOT:
      case REF:
      case THIS:
      case CALL:
      case G_CALL:
      case ICALL:
      case ANAME:
         printEx(e);
         break;
      default:
         puttok(LP);
         printEx(e);
         puttok(RP);
         break;
   }
}

/*
	Print the declaration for a name (list==0) or a name list (list!=0):
		For each name
		(1) print storage class
		(2) print base type
		(3) print the name with its declarators
	Avoid (illegal) repetition of basetypes which are class or enum declarations
	(A name list may contain names with different base types)
	list == SM :	terminator SM
	list == 0:	single declaration with terminator SM
	list == CM :	separator CM
*/
void dcl_printId(IdP this, Token list) {
   IdP n;

   if (this == 0) error("dcl_printId(0)");

   for (n = this; n; n = n->n_list) {
      TypeP t = n->tp;
      int sm = 0;

      if (t == 0) errorT('i', "dclPrintId(%n)T missing", n);
      if (print_mode == SIMPL && n->n_stclass == ENUM) continue;

      if (n->n_stclass == STATIC) putline(&n->where);

      switch (t->base) {
         case CLASS:
         {
            ClassP cl = (ClassP) t;
            if (n->base == TNAME) break;
            dcl_printClass(cl, n);
            sm = 1;
            break;
         }

         case ENUM:
            dcl_printEnum(ToEnumP(t), n);
            sm = 1;
            break;

         case FCT:
         {
            FunP f = (FunP) t;
            if (n->base == TNAME) puttok(TYPEDEF);
            if (debug == 0 && f->f_inline) {
               if (print_mode == SIMPL) {
                  if (f->f_virtual || n->n_addr_taken) {
                     Token st = n->n_sto;
                     BlockP b = f->body;
                     f->body = 0;
                  /*      n->n_sto = 0;   */
                     dcl_printType(t, n);
                     n->n_sto = st;
                     f->body = b;
                  }
               } else {
                  if (print_mode != SIMPL)
                     puttok(INLINE);
                  else
                     putst("/* inline */");
                  dcl_printType(t, n);
               }
            } else {
               if (n->n_table == gtbl && strcmp(n->string, "main") == 0) {
                  MAIN = 1;
                  use(look(gtbl, "main", 0));
                  dcl_printType(t, n);
                  MAIN = 0;
               } else
                  dcl_printType(t, n);
            }
            break;
         }

         case OVERLOAD:
         {
            GenP g = (GenP) t;
            IdListP gl;
            fprintf(out_file, "\t/* overload %s: */\n", g->string);
            for (gl = g->fct_list; gl; gl = gl->l) {
               IdP nn = gl->f;
               dcl_printId(nn, 0);
               sm = 1;
            }
            break;
         }

         case ASM:
            fprintf(out_file, "asm(\"%s\")\n", (char *)ToBaseP(t)->b_name);
            break;

         case INT:
         case CHAR:
         case LONG:
         case SHORT: // do not allocate space for constants unless necessary
            if (print_mode == SIMPL && ToBaseP(t)->b_const && n->n_sto != EXTERN // extern const one;
            // const one = 1;
            // allocates storage
               && (n->n_scope == EXTERN // FUDGE const one = 1;
               // is treated as static
               // need loader support
                  || n->n_scope == STATIC || n->n_scope == FCT)
               ) {

               if (n->n_evaluated) {
                  sm = 1; /* no ; */
                  break;
               }
            }

         default:
         {
            ExP i = n->n_initializer;
            if (n->base == TNAME) puttok(TYPEDEF);
//errorT('d',"%s: init %d %d tbl %d %d sto %d sc %d scope %d\n",n->string?n->string:"",i,i?i->base:0,n->n_table,gtbl,n->n_sto,n->n_stclass,n->n_scope);
            if (i) {
               if (n->n_sto == EXTERN && n->n_stclass == STATIC) {
                  n->n_initializer = 0;
                  dcl_printType(t, n);
                  puttok(SM);
                  n->n_initializer = i;
                  n->n_sto = 0;
                  dcl_printType(t, n);
                  n->n_sto = EXTERN;
               } else
                  dcl_printType(t, n);
            } else {
               if (fct_void == 0 && n->n_sto == 0 && this->n_stclass == STATIC && n->n_table == gtbl) {
                  switch (t->base) {
                     case CHAR:
                     case SHORT:
                     case INT:
                     case LONG:
                     case FLOAT:
                     case DOUBLE:
                     case EOBJ:
                     case PTR:
                     // "int a;" == "int a = 0;"
                        n->n_initializer = i = zero;
                  }
               }
               dcl_printType(t, n);
            }

            if (n->n_scope != ARG) {
               if (i) {
                  puttok(ASSIGN);
                  if (t != i->tp && i->base != ZERO && i->base != ILIST /*&& i->tp!=Pchar_type */ ) {
                     TypeP t1 = n->tp;
                   cmp:
//errorT('d',"t1%t",t1);
                     switch (t1->base) {
                        default:
                           printEx(i);
                           break;
                        case TYPE:
                           t1 = ToBaseP(t1)->b_name->tp;
                           goto cmp;
                        case VEC:
                           if (ToVecP(t1)->typ->base == CHAR) {
                              printEx(i);
                              break;
                           }
                        case PTR:
                        case RPTR:
                           puttok(LP);
                           {
                              bit oc = Cast;
                              Cast = 1;
                              printType(t);
                              Cast = oc;
                           }
                           puttok(RP);
                           eprint(i);
                     }
                  } else
                     printEx(i);
               } else if (n->n_evaluated) {
                  puttok(ASSIGN);
                  if (n->tp->base != INT) {
                     puttok(LP);
                     puttok(LP);
                     {
                        bit oc = Cast;
                        Cast = 1;
                        printType(n->tp);
                        Cast = oc;
                     }
                     fprintf(out_file, ")%d)", n->n_val);
                  } else
                     fprintf(out_file, "%d", n->n_val);
               }
            }
         }
      }

      switch (list) {
         case SM:
            if (sm == 0) puttok(SM);
            break;
         case 0:
            if (sm == 0) puttok(SM);
            return;
         case CM:
            if (n->n_list) puttok(CM);
            break;
      }
   }
}

/*
	print just the name itself
*/
void printId(IdP this) {
   if (this == 0) errorT('i', "printId(0)");

   if (this->string == 0) {
      if (print_mode == ERROR) putst(" ?");
      return;
   }

   switch (this->base) {
      default:
         errorT('i', "PrintId(%d) base=%d", this, this->base);
      case TNAME:
         putst(this->string);
         return;
      case NAME:
      case ANAME:
         break;
   }

   switch (print_mode) {
      case SIMPL:
      {
         TableP tbl;
         int i = this->n_union;
         if (this->tp) {
            switch (this->tp->base) {
               default:
                  if (tbl = this->n_table) {
                     IdP tn;
//fprintf(stderr,"%s: tbl %d gtbl %d\n",this->string,tbl,gtbl);
                     if (tbl == gtbl) break;
                     if (tn = tbl->t_name) {
                        if (i)
                           fprintf(out_file, "_%s__O%d.__C%d_", tn->string, i, i);
                        else
                           fprintf(out_file, "_%s_", tn->string);
                        break;
                     }
                  }
//fprintf(stderr,"%s: stc %d\n",this->string,this->n_stclass);
                  switch (this->n_stclass) {
                     case STATIC:
                     case EXTERN:
                        if (i)
                           fprintf(out_file, "_O%d.__C%d_", i, i);
                        else if (this->n_sto == STATIC && this->tp->base != FCT)
                           fprintf(out_file, "_static_");
                        break;
                     default:
                        if (i)
                           fprintf(out_file, "_auto__O%d.__C%d_", i, i);
                        else
                           fprintf(out_file, "_auto_");
                  }
                  break;
               case CLASS:
               case ENUM:
                  break;
            }
         }
         break;
      }
      case ERROR:
      {
         TableP tbl;
         const char *cs = 0;
         bit f = 0;
         if (this->tp) {
            switch (this->tp->base) {
               case OVERLOAD:
               case FCT:
                  f = 1;
               default:
                  if (tbl = this->n_table) {
                     if (tbl == gtbl) {
                        if (f == 0) putstring("::");
                     } else {
                        if (tbl->t_name) {
                           cs = tbl->t_name->string;
                           fprintf(out_file, "%s::", cs);
                        }
                     }
                  }
                  if (this->n_sto == REGISTER && this->n_scope == ARG && strcmp(this->string, "this") == 0) {
                     TypeP tt = ToPtrP(this->tp)->typ;
                     IdP cn = ToBaseP(tt)->b_name;
                     fprintf(out_file, "%s::", cn->string);
                  }
                  break;
               case CLASS:
               case ENUM:
               //      case TYPE:
                  break;
            }

            switch (this->n_oper) {
               case TYPE:
                  putstring("operator ");
                  dcl_printType(ToFunP(this->tp)->returns, 0);
                  break;
               case 0:
                  putstring(this->string);
                  break;
               case DTOR:
                  puttok(COMPL);
               case CTOR:
                  if (cs)
                     putstring(cs);
                  else {
                     putstring("constructor");
                     f = 0;
                  }
                  break;

               default:
                  putstring("operator ");
                  putstring(keys[this->n_oper]);
                  break;
            }
            if (f) putstring("()");
         } else if (this->string) putstring(this->string);
         return;
      }
      default:
         if (this->n_qualifier) {
            printId(this->n_qualifier);
            puttok(DOT);
         }
   }

   if (this->string) putst(this->string);
}

void printType(TypeP this) {
/*fprintf(stderr,"type %d %d\n",this,this->base); fflush(stderr);*/
   switch (this->base) {
      case PTR:
      case RPTR:
         dcl_printType((TypeP)ToPtrP(this), 0);
         break;
      case FCT:
         dcl_printFun(ToFunP(this));
         break;
      case VEC:
         dcl_printType((TypeP)ToVecP(this), 0);
         break;
      case CLASS:
      case ENUM:
         if (print_mode == ERROR)
            fprintf(out_file, "%s", this->base == CLASS ? "class" : "enum");
         else
            errorT('i', "printType(%d, %k)", this, this->base);
         break;
      case TYPE:
         if (Cast) {
            printType(ToBaseP(this)->b_name->tp);
            break;
         }
      default:
         dcl_printBase(ToBaseP(this));
   }
}

/*
	take a signature suitable for argument types for overloaded
	function names
*/
char *signature(TypeP this, register char *p) {
#define SDEL	'_'

   TypeP t = this;
   int pp = 0;

 xx:
   switch (t->base) {
      case TYPE:
         t = ToBaseP(t)->b_name->tp;
         goto xx;
      case PTR:
         *p++ = 'P';
         t = ToPtrP(t)->typ;
         pp = 1;
         goto xx;
      case RPTR:
         *p++ = 'R';
         t = ToPtrP(t)->typ;
         pp = 1;
         goto xx;
      case VEC:
         *p++ = 'V';
         t = ToVecP(t)->typ;
         pp = 1;
         goto xx;
      case FCT:
      {
         FunP f = (FunP) this;
         IdP n;
         t = (f->s_returns) ? f->s_returns : f->returns;
         *p++ = 'F';
         for (n = f->argtype; n; n = n->n_list) {
            p = signature(n->tp, p);
            *p++ = SDEL;
         }
         *p++ = SDEL;
         if (f->nargs_known == ELLIPSIS) *p++ = 'E';
         *p = 0;
         return p;
      }
   }

   if (ToBaseP(t)->b_unsigned) *p++ = 'U';

   switch (t->base) {
      case ANY:
         *p++ = 'A';
         break;
      case ZTYPE:
         *p++ = 'Z';
         break;
      case VOID:
         *p++ = 'V';
         break;
      case CHAR:
         *p++ = (pp) ? 'C' : 'I';
         break;
      case SHORT:
         *p++ = (pp) ? 'S' : 'I';
         break;
      case EOBJ:
      case INT:
         *p++ = 'I';
         break;
      case LONG:
         *p++ = 'L';
         break;
      case FLOAT:
         *p++ = 'F';
         break;
      case DOUBLE:
         *p++ = 'D';
         break;
      case COBJ:
         *p++ = 'C';
         strcpy(p, ToBaseP(t)->b_name->string);
         while (*p++);
         *(p - 1) = SDEL;
         break;
      case FIELD:
      default:
         errorT('i', "signature of %k", t->base);
   }

   *p = 0;
   return p;
}

void dcl_printBase(BaseP this) {
   IdP nn;
   ClassP cl;

   if (print_mode != SIMPL) {
      if (this->b_virtual) puttok(VIRTUAL);
      if (this->b_inline) puttok(INLINE);
      if (this->b_const) puttok(CONST);
   }
   if (this->b_unsigned) puttok(UNSIGNED);

   switch (this->base) {
      case ANY:
         putst("any");
         break;

      case ZTYPE:
         putst("zero");
         break;

      case VOID:
         if (print_mode == SIMPL) {
            puttok(INT);
            break;
         }
      case CHAR:
      case SHORT:
      case INT:
      case LONG:
      case FLOAT:
      case DOUBLE:
         puttok(this->base);
         break;

      case EOBJ:
         nn = this->b_name;
       eob:
         if (print_mode == SIMPL)
            puttok(INT);
         else {
            puttok(ENUM);
            printId(nn);
         }
         break;

      case COBJ:
         nn = this->b_name;
       cob:
         cl = (ClassP) nn->tp;
         switch (cl->csu) {
            case UNION:
            case ANON:
               puttok(UNION);
               break;
            default:
               puttok(STRUCT);
         }
         putst(cl->string);
         break;

      case TYPE:
         if (print_mode == SIMPL) {
            switch (this->b_name->tp->base) {
               case COBJ:
                  nn = ToBaseP(this->b_name->tp)->b_name;
                  goto cob;
               case EOBJ:
                  nn = ToBaseP(this->b_name->tp)->b_name;
                  goto eob;
            }
         }
         printId(this->b_name);
         break;

      default:
         if (print_mode == ERROR) {
            if (0 < this->base && this->base < MAXTOK && keys[this->base])
               fprintf(out_file, " %s", keys[this->base]);
            else
               fprintf(out_file, "?");
         } else
            errorT('i', "dcl_printBase(%d, %d)", this, this->base);
   }
}

/*
	"this" type is the type of "n". Print the declaration
*/
void dcl_printType(TypeP this, IdP n) {
   TypeP t = this;
   FunP f;
   VecP v;
   PtrP p;
   Token pre = 0;

   if (t == 0) errorT('i', "dcl_printType(0)");
   if (n && n->tp != t) errorT('i', "not %n'sT (%d)", n, t);

   if (this->base == OVERLOAD) {
      if (print_mode == ERROR) {
         puttok(OVERLOAD);
         return;
      }
      GenP g = (GenP) this;
      IdListP gl;
      fprintf(out_file, "\t/* overload %s: */\n", g->string);
      for (gl = g->fct_list; gl; gl = gl->l) {
         IdP nn = gl->f;
         dcl_printType(nn->tp, nn);
         if (gl->l) puttok(SM);
      }
      return;
   }

   tbuf = tbufvec[freetbuf];
   if (tbuf == 0) {
      if (freetbuf == NTBUF - 1) errorT('i', "AT nesting overflow");
      tbufvec[freetbuf] = tbuf = MakeDecBuf();
   }
   freetbuf++;
   init(tbuf, n);

   while (t) {
      Token k;

      switch (t->base) {
         case PTR:
            p = (PtrP) t;
            k = (p->rdo) ? CONST_PTR : PTR;
            goto ppp;
         case RPTR:
            p = (PtrP) t;
            k = (p->rdo) ? CONST_RPTR : RPTR;
          ppp:
            frontDecBuf(tbuf, k);
            pre = PTR;
            t = p->typ;
            break;
         case VEC:
            v = (VecP) t;
            if (Cast) {
               frontDecBuf(tbuf, PTR);
               pre = PTR;
            } else {
               if (pre == PTR) paran(tbuf);
               backDecBuf(tbuf, VEC, (NodeP)v);
               pre = VEC;
            }
            t = v->typ;
            break;
         case FCT:
            f = (FunP) t;
            if (pre == PTR) paran(tbuf);
            backDecBuf(tbuf, FCT, (NodeP)f);
            pre = FCT;
            t = (f->s_returns) ? f->s_returns : f->returns;
            break;
         case FIELD:
            backDecBuf(tbuf, FIELD, (NodeP)t);
            base(tbuf, ToBaseP(ToBaseP(t)->b_fieldtype));
            t = 0;
            break;
         case CLASS:
         case ENUM:
            errorT('i', "unX%k asBT", t->base);
         case 0:
            errorT('i', "noBT(B=0)%s", Cast ? " in cast" : "");
         case TYPE:
            if (Cast) { // unravel type in case it contains vectors
               t = ToBaseP(t)->b_name->tp;
               break;
            }
         default: // the base has been reached
            base(tbuf, ToBaseP(t));
            t = 0;
            break;
      } // switch
   } // while

   putDecBuf(tbuf);
   freetbuf--;
}

void dcl_printFun(FunP this) {
   IdP nn;

   if (print_mode == ERROR) {
      puttok(LP);
      for (nn = this->argtype; nn;) {
         dcl_printType(nn->tp, 0);
         if (nn = nn->n_list) puttok(CM);
         else break;
      }
      switch (this->nargs_known) {
         case 0: //      putst("?"); break;
         case ELLIPSIS:
            puttok(ELLIPSIS);
            break;
      }
      puttok(RP);
      return;
   }

   IdP at = (this->f_this) ? this->f_this : this->argtype;
//fprintf(stderr,"f_this %d argtype %d body %d\n",this->f_this,this->argtype,this->body); fflush(stderr);
   puttok(LP);
   if (this->body && Cast == 0) {

      for (nn = at; nn;) {
         printId(nn);
         if (nn = nn->n_list) puttok(CM);
         else break;
      }
      puttok(RP);

      if (at) dcl_printId(at, SM);

      if (this->b_init && print_mode != SIMPL) {
         puttok(COLON);
         puttok(LP);
         printEx(this->b_init);
         puttok(RP);
      }

      if (MAIN) {
         putst("{ _main();");
         printBlock(this->body);
         putst("}");
      } else
         printBlock(this->body);
   } else
      puttok(RP);
}

void print_members(ClassP this) {
   int i;
   IdP nn;

   if (this->clbase) {
      ClassP bcl = (ClassP) this->clbase->tp;
      print_members(bcl);
   }
   for (nn = get_mem(this->memtbl, i = 1); nn; nn = get_mem(this->memtbl, ++i)) {
//errorT('d',"mem %n: base(%d %d) union=%d tp=%d stc(%d %k)",nn,nn->base,nn->base,nn->n_union,nn->tp->base,nn->n_stclass,nn->n_stclass);
      if (nn->base == NAME && nn->n_union == 0 && nn->tp->base != FCT && nn->tp->base != OVERLOAD && nn->tp->base != CLASS && nn->tp->base != ENUM && nn->n_stclass != STATIC) {
         ExP i = nn->n_initializer;
         nn->n_initializer = 0;
         dcl_printId(nn, 0);
         nn->n_initializer = i;
      }
   }
}

void dcl_printClass(ClassP this, IdP n) {
   IdListP l;
   Token c = this->csu;
   if (c == CLASS && print_mode == SIMPL) c = STRUCT;

   if (print_mode == SIMPL) { /* cope with nested classes */
      int i;
      IdP nn;

      for (nn = get_mem(this->memtbl, i = 1); nn; nn = get_mem(this->memtbl, ++i)) {
/*fprintf(stderr, "mem %d %s %d union %d tp %d %d\n", nn, nn->string, nn->base, nn->n_union, nn->tp, nn->tp->base);*/
         if (nn->base == NAME && nn->n_union == 0) {
            if (nn->tp->base == CLASS) dcl_printClass(ToClassP(nn->tp), nn);
         }
      }
   }

   puttok(c);
   putst(this->string);

   if (this->c_body == 0) return;
   this->c_body = 0;

   if (print_mode == SIMPL) {
      int i;
      int sm = 0;
      IdP nn;
      int sz = tsizeof((TypeP)this);

      puttok(LC);
      fprintf(out_file, "/* sizeof = %d */\n", sz);
      if (this->real_size)
         print_members(this);
      else
         putst("char _dummy;");
      puttok(RC);
      puttok(SM);

      if (this->virt_count) { /* print initialized jump-table */

         for (nn = get_mem(this->memtbl, i = 1); nn; nn = get_mem(this->memtbl, ++i)) {
            if (nn->base == NAME && nn->n_union == 0) { /* declare function names */
               TypeP t = nn->tp;
               switch (t->base) {
                  case FCT:
                  {
                     FunP f = (FunP) t;
//errorT('d',"%n i==%d v==%d returns (%d %d)%t",nn,f->f_inline,f->f_virtual,f->s_returns,f->returns,f->s_returns?f->s_returns:f->returns);
                     if (f->f_virtual == 0) break;
                     if (f->f_inline) puttok(STATIC);
                     printType(f->returns);
                     printId(nn);
                     putst("()");
                     puttok(SM);
                     break;
                  }
                  case OVERLOAD:
                  {
                     GenP g = (GenP) t;
                     IdListP gl;
                     for (gl = g->fct_list; gl; gl = gl->l) {
                        FunP f = (FunP) gl->f->tp;
                        if (f->f_virtual == 0) break;
                        if (f->f_inline) puttok(STATIC);
                        printType(f->returns);
                        printId(gl->f);
                        putst("()");
                        puttok(SM);
                     }
                  }
               }
            }
         }

         fprintf(out_file, "static int (*%s__vtbl[])() =", this->string);
         puttok(LC);
         for (i = 0; i < this->virt_count; i++) {
            fprintf(out_file, " (int(*)()) ");
            printId(this->virt_init[i]);
            puttok(CM);
         }
         puttok(ZERO);
         puttok(RC);
         puttok(SM);
      }

      for (nn = get_mem(this->memtbl, i = 1); nn; nn = get_mem(this->memtbl, ++i)) {
         if (nn->base == NAME && nn->n_union == 0) {
            TypeP t = nn->tp;
            switch (t->base) {
               case FCT:
               case OVERLOAD:
                  break;
               default:
                  if (nn->n_stclass == STATIC) {
                  //      nn->n_sto = 0;
                     dcl_printId(nn, 0);
                  }
            }
         }
      }

      for (nn = get_mem(this->memtbl, i = 1); nn; nn = get_mem(this->memtbl, ++i)) {
         if (nn->base == NAME && nn->n_union == 0) {
            FunP f = (FunP) nn->tp;
            switch (f->base) {
               case FCT:
               /* suppress duplicate or spurious declaration */
                  if (debug == 0 && f->f_virtual) break;
                  if (debug == 0 && f->f_inline) break;
                  dcl_printId(nn, 0);
                  break;
               case OVERLOAD:
                  dcl_printId(nn, 0);
                  break;
            }
         }
      }

      for (l = this->friend_list; l; l = l->l) {
         IdP nn = l->f;
//errorT('d',"friend%n %d%k",nn,nn->tp,nn->tp->base);
         switch (nn->tp->base) {
            case FCT:
               putst("/* friend */");
               Cast = 1;
               dcl_printId(nn, 0);
               Cast = 0;
               break;
            case OVERLOAD: /* first Fun */
               l->f = nn = ToGenP(nn->tp)->fct_list->f;
               putst("/* friend */");
               dcl_printId(nn, 0);
               break;
         }
      }
      return;
   }

   if (this->clbase) {
      puttok(COLON);
      if (this->pubbase) puttok(PUBLIC);
      printId(this->clbase);
   }
   puttok(LC);

   for (l = this->friend_list; l; l = l->l) {
      IdP fr = l->f;
      puttok(FRIEND);
      switch (fr->tp->base) {
         case FCT:
         default:
            printId(fr);
            puttok(SM);
      }
   }

   if (this->privmem) dcl_printId(this->privmem, SM);
   if (this->memtbl) dcl_printTable(this->memtbl, NE, PUBLIC);
   puttok(PUBLIC);
   puttok(COLON);
   if (this->pubmem) dcl_printId(this->pubmem, SM);
   if (this->memtbl) dcl_printTable(this->memtbl, EQ, PUBLIC);

   if (this->pubdef) {
      puttok(PUBLIC);
      puttok(COLON);
      printId(this->pubdef);
      puttok(SM);
   }

   puttok(RC);
}

void dcl_printEnum(EnumP this, IdP n) {
   if (print_mode == SIMPL) {
      if (this->mem) {
         fprintf(out_file, "/* enum %s */\n", n->string);
         dcl_printId(this->mem, SM);
      }
   } else {
      puttok(ENUM);
      if (n) printId(n);
      puttok(LC);
      if (this->mem) dcl_printId(this->mem, SM);
      puttok(RC);
   }
}

int addrof_cm = 0;

void printEx(ExP this) {
   if (this == 0) errorT('i', "printEx(0)");
   if (this == this->e1 || this == this->e2) errorT('i', "printEx(%d%k, %d %d)", this, this->base, this->e1, this->e2);
/*errorT('d',"Ex %d%k e1=%d e2=%d tp2=%d",this,this->base,this->e1,this->e2,this->tp2);*/
   switch (this->base) {
      case NAME:
      {
         IdP n = (IdP) this;
         if (n->n_evaluated && n->n_scope != ARG) {
            if (n->tp->base != INT) {
               puttok(LP);
               puttok(LP);
               bit oc = Cast;
               Cast = 1;
               printType(n->tp);
               Cast = oc;
               fprintf(out_file, ")%d)", n->n_val);
            } else
               fprintf(out_file, "%d", n->n_val);
         } else
            printId(n);
         break;
      }
      case ANAME:
         if (curr_icall) { /*in expansion: look it up */
            IdP n = (IdP) this;
            int argno = n->n_val;
            InLineP il;
            for (il = curr_icall; il; il = il->i_next)
               if (n->n_table == il->i_table) goto aok;
            goto bok;
          aok:
            if (n = il->local[argno]) {
//errorT('d',"n   %d %s",n,n->string);
               printId(n);
            } else {
               ExP ee = il->arg[argno];
               TypeP t = il->tp[argno];
               if (ee == 0 || ee == this) errorT('i', "printEx(%d, A %d)", this, ee);
//errorT('d',"ee  %d %d",ee,ee->base);
               if (t != ee->tp && is_cl_obj(t) == 0 && eobj == 0) {
                  puttok(LP);
                  puttok(LP);
                  {
                     bit oc = Cast;
                     Cast = 1;
                     printType(t);
                     Cast = oc;
                  }
                  puttok(RP);
                  eprint(ee);
                  puttok(RP);
               } else
                  eprint(ee);
            }
         } else {
          bok: /* in body: print it: */
//errorT('d',"bok %d %d",this,this->base);
            printId(ToIdP(this));
         }
         break;

      case ICALL:
      {
         this->il->i_next = curr_icall;
         curr_icall = this->il;
         if (this->il == 0) errorT('i', "Ex::print: InLine missing");
         ExP a0 = this->il->arg[0];
         int val = QUEST;
         if (this->il->fct_name->n_oper != CTOR) goto dumb;

      /*
         find the value of "this"
         if the argument is a "this" NOT assigned to
         by the programmer, it was initliazed
       */

         switch (a0->base) {
            case ZERO:
               val = 0;
               break;
            case ADDROF:
            case G_ADDROF:
               val = 1;
               break;
            case CAST:
               if (a0->e1->base == ANAME) {
                  IdP a = (IdP) a0->e1;
                  if (a->n_assigned_to == FUDGE111) val = FUDGE111;
               }
         }
         if (val == QUEST) goto dumb;
//errorT('d',"%n's this == %d",this->il->fct_name,val);
      /*
         now find the test:  "(this==0) ? _new(sizeof(X)) : 0"

         this->e1 is a comma expression,
         the test is either the first sub-expression
         or the first sub-expression after the assignments
         initializing temporary variables
       */

         {
            ExP e = this->e1;
          lx:
            switch (e->base) {
               case CM:
               /*      if (val==1 && e->e1->base==ASSIGN) {
                  ExP ass = e->e1;
                  IdP a = e->e1->e1;
                  if (a->base==ANAME && 1) {
                  }
                  }
                */
                  e = (e->e2->base == QUEST || e->e1->base == ASSIGN) ? e->e2 : e->e1;
                  goto lx;

               case QUEST:
               {
                  ExP q = e->cond;
                  if (q->base == EQ && q->e1->base == ANAME && q->e2 == zero) {
                     IdP a = (IdP) q->e1;
                     ExP saved = MakeEx(0, 0, 0);
                     *saved = *e;
                     *e = (val == 0) ? *e->e1 : *e->e2;
                     eprint(this->e1);
                     *e = *saved;
                     FreeEx(saved);
                     curr_icall = this->il->i_next;
                     return;
                  }
               }
            }
         }
       dumb:
//errorT('d',"dumb%n",this->il->fct_name);
         eprint(this->e1);
         if (this->e2) printSt(ToStP(this->e2));
         curr_icall = this->il->i_next;
//errorT('d',"end%n",this->il->fct_name);
         break;
      }
      case REF:
      case DOT:
         eprint(this->e1);
         puttok(this->base);
         printId(this->mem);
         break;

      case VALUE:
         printType(this->tp2);
         puttok(LP);
         if (this->e2) {
            putst("/* &");
            printEx(this->e2);
            putst(", */");
         }
         if (this->e1) printEx(this->e1);
         puttok(RP);
         break;

      case SIZEOF:
         puttok(SIZEOF);
         if (this->e1 && this->e1 != dummy) {
            eprint(this->e1);
         } else if (this->tp2) {
            puttok(LP);
            printType(this->tp2);
            puttok(RP);
         }
         break;

      case NEW:
         puttok(NEW);
         printType(this->tp2);
         if (this->e1) {
            puttok(LP);
            printEx(this->e1);
            puttok(RP);
         }
         break;

      case DELETE:
         puttok(DELETE);
         printEx(this->e1);
         break;

      case CAST:
         puttok(LP);
         if (this->tp2->base != VOID) {
            puttok(LP);
            bit oc = Cast;
            Cast = 1;
            printType(this->tp2);
            Cast = oc;
            puttok(RP);
         }
         puttok(LP);
         printEx(this->e1);
         puttok(RP);
         puttok(RP);
         break;

      case ICON:
      case FCON:
      case CCON:
      case ID:
         if (this->string) putst(this->string);
         break;

      case STRING:
         fprintf(out_file, "\"%s\"", this->string);
         break;

      case THIS:
      case ZERO:
         puttok(this->base);
         break;

      case IVAL:
         fprintf(out_file, "%d", (int)this->e1);
         break;

      case TEXT:
         if (this->string2)
            fprintf(out_file, " %s_%s", this->string, this->string2);
         else
            fprintf(out_file, " %s", this->string);
         break;

      case DUMMY:
         break;

      case G_CALL:
      case CALL:
      {
         IdP fn = this->fct_name;
         IdP at;
         if (fn && print_mode == SIMPL) {
            FunP f = (FunP) fn->tp;
//errorT('d',"call%n: %t",fn,f);
            if (f->base == OVERLOAD) { /* overloaded after call */
               GenP g = (GenP) f;
               this->fct_name = fn = g->fct_list->f;
               f = (FunP) fn->tp;
            }
            printId(fn);
            at = (f->f_this) ? f->f_this : f->argtype;
         } else {
//errorT('d',"e1%k e1->tp %d %d%t",this->e1->base,this->e1->tp,this->e1->tp->base,this->e1->tp);
            eprint(this->e1);
            if (this->e1->tp) { /* pointer to Fun */
               at = ToFunP(this->e1->tp)->argtype;
            } else { /* virtual: argtype encoded */
               at = (IdP) this->e1->e1->tp;
            }
         }
         puttok(LP);
         if (this->e2) {
            if (at && print_mode == SIMPL) {
               ExP e = this->e2;
               while (at) {
                  ExP ex;
                  TypeP t = at->tp;

                  if (e == 0) errorT('i', "A missing for %s()", (fn) ? fn->string : "??");
                  if (e->base == ELIST) {
                     ex = e->e1;
                     e = e->e2;
                  } else
                     ex = e;

                  if (ex == 0) errorT('i', "A ofT%t missing", t);

                  if (t != ex->tp && is_cl_obj(t) == 0 && eobj == 0) {
                     puttok(LP);
                     {
                        bit oc = Cast;
                        Cast = 1;
                        printType(t);
                        Cast = oc;
                     }
                     puttok(RP);
                     if (ex->base == DIV) { // defend against perverse SUN cc bug
                        putst("(0+");
                        eprint(ex);
                        putst(")");
                     } else
                        eprint(ex);
                  } else
                     printEx(ex);
                  at = at->n_list;
                  if (at) puttok(CM);
               }
               if (e) {
                  puttok(CM);
                  printEx(e);
               }
            } else
               printEx(this->e2);
         }
         puttok(RP);
         break;
      }
      case ASSIGN:
         if (this->e1->base == ANAME && ToIdP(this->e1)->n_assigned_to == FUDGE111) {
         /* suppress assignment to "this" that has been optimized away */
            IdP n = (IdP) this->e1;
            int argno = n->n_val;
            InLineP il;
            for (il = curr_icall; il; il = il->i_next)
               if (il->i_table == n->n_table) goto akk;
            goto bkk;
          akk:
            if (il->local[argno] == 0) {
               printEx(this->e2);
               break;
            }
         }
      case EQ:
      case NE:
      case GT:
      case GE:
      case LE:
      case LT:
       bkk:
         eprint(this->e1);
         puttok(this->base);
         if (this->e1->tp != this->e2->tp && this->e2->base != ZERO) { /* cast, but beware of int!=long etc */
            TypeP t1 = this->e1->tp;
          cmp:
            switch (t1->base) {
               default:
                  break;
               case TYPE:
                  t1 = ToBaseP(t1)->b_name->tp;
                  goto cmp;
               case PTR:
               case RPTR:
               case VEC:
                  if (ToPtrP(t1)->typ != ToPtrP(this->e2->tp)->typ) {
                     puttok(LP);
                     bit oc = Cast;
                     Cast = 1;
                     printType(this->e1->tp);
                     Cast = oc;
                     puttok(RP);
                  }
            }
         }
         eprint(this->e2);
         break;

      case DEREF:
         if (this->e2) {
            eprint(this->e1);
            puttok(LB);
            printEx(this->e2);
            puttok(RB);
         } else {
            puttok(MUL);
            eprint(this->e1);
         }
         break;

      case ILIST:
         puttok(LC);
         if (this->e1) printEx(this->e1);
         puttok(RC);
         break;

      case ELIST:
      {
         ExP e = this;
         while (1) {
            if (e->base == ELIST) {
               printEx(e->e1);
               if (e = e->e2)
                  puttok(CM);
               else
                  return;
            } else {
               printEx(e);
               return;
            }
         }
      }
      case QUEST:
         eprint(this->cond);
         puttok(QUEST);
         eprint(this->e1);
         puttok(COLON);
         eprint(this->e2);
         break;

      case CM: /* do &(a,b) => (a,&b) for previously checked inlines */
         switch (this->e1->base) {
            case ZERO:
            case IVAL:
            case ICON:
            case NAME:
            case DOT:
            case REF:
            case FCON:
            case FVAL:
            case STRING:
               puttok(LP);
               goto le2;
            default:
               puttok(LP);
               {
                  int oo = addrof_cm; // &(a,b) does not affect a
                  addrof_cm = 0;
                  eprint(this->e1);
                  addrof_cm = oo;
               }
               puttok(CM);
             le2:
//errorT('d',"le2 %k(%k,%k) addrof_cm %d",this->base,this->e1->base,this->e2->base,addrof_cm);
               if (addrof_cm) {
                  switch (this->e2->base) {
                     case CAST:
                        switch (this->e2->e2->base) {
                           case CM:
                           case ICALL:
                              goto ec;
                        }
                     case NAME:
                     case DOT:
                     case DEREF:
                     case REF:
                     case ANAME:
                        puttok(ADDROF);
                        addrof_cm--;
                        eprint(this->e2);
                        addrof_cm++;
                        break;
                     case ICALL:
                     case CM:
                      ec:
                        eprint(this->e2);
                        break;
                     case G_CALL:
                     /* & ( e, ctor() ) with temporary optimized away */
                        if (this->e2->fct_name && this->e2->fct_name->n_oper == CTOR) {
                           addrof_cm--;
                           eprint(this->e2);
                           addrof_cm++;
                           break;
                        }
                     default:
                        errorT('i', "& inlineF call (%k)", this->e2->base);
                  }
               } else {
                  eprint(this->e2);
               }
               puttok(RP);
         }
         break;

      case UMINUS:
      case NOT:
      case COMPL:
         puttok(this->base);
         eprint(this->e2);
         break;
      case ADDROF:
      case G_ADDROF:
//errorT('d',"&%k",this->e2->base);
         switch (this->e2->base) {
            case DEREF:
               if (this->e2->e2 == 0) {
                  printEx(this->e2->e1);
                  return;
               }
               break;
            case ICALL:
               addrof_cm++; // assumes inline expanded into ,-expression
               eprint(this->e2);
               addrof_cm--;
               return;
         }

      // suppress cc warning on &Fun
         if (this->e2->tp == 0 || this->e2->tp->base != FCT) puttok(ADDROF);

         eprint(this->e2);
         break;

      case PLUS:
      case MINUS:
      case MUL:
      case DIV:
      case MOD:
      case LS:
      case RS:
      case AND:
      case OR:
      case ER:
      case ANDAND:
      case OROR:
      case ASPLUS:
      case ASMINUS:
      case ASMUL:
      case ASMOD:
      case ASDIV:
      case ASLS:
      case ASRS:
      case ASOR:
      case ASER:
      case ASAND:
      case DECR:
      case INCR:
         eprint(this->e1);
         puttok(this->base);
         eprint(this->e2);
         break;

      default:
         errorT('i', "%d->Ex::print%k", this, this->base);
   }
}

ExP aval(IdP a) {
   int argno = a->n_val;
   InLineP il;
   for (il = curr_icall; il; il = il->i_next)
      if (il->i_table == a->n_table) goto aok;
   return 0;
 aok: ;
   ExP aa = il->arg[argno];
/*errorT('d',"aval(%n) -> %k",a,aa->base);*/
 ll:
   switch (aa->base) {
      case CAST:
         aa = aa->e1;
         goto ll;
      case ANAME:
         return aval(ToIdP(aa));
      default:
         return aa;
   }
}

#define putcond(e)	puttok(LP); printEx(e); puttok(RP)

void printSt(StP this) {
   if (forced_sm) {
      forced_sm = 0;
      putline(&this->where);
   }
/*errorTL('d',&this->where,"St::print %d:%k s %d s_list %d",this,this->base,this->s,this->s_list);*/

   if (this->memtbl && this->base != BLOCK) { /* also print declarations of temporaries */
      puttok(LC);
      TableP tbl = this->memtbl;
      this->memtbl = 0;
      IdP n;
      int i;
      int bl = 1;
      for (n = get_mem(tbl, i = 1); n; n = get_mem(tbl, ++i)) {
      /* avoid double declarartion of temporaries from inlines */
         const char *s = n->string;
         if (s[0] != '_' || s[1] != 'X') {
            dcl_printId(n, 0);
            bl = 0;
         }
         IdP cn;
         if (bl && (cn = is_cl_obj(n->tp)) && has_dtor(ToClassP(cn->tp))) bl = 0;
      }
/*errorT('d',"%d (tbl=%d) list %d",this,tbl,this->s_list);*/
      if (bl) {
         StP sl = this->s_list;
         this->s_list = 0;
         printSt(this);
         this->memtbl = tbl;
         puttok(RC);
         if (sl) {
            this->s_list = sl;
            printSt(sl);
         }
      } else {
         printSt(this);
         this->memtbl = tbl;
         puttok(RC);
      }
      return;
   }

   switch (this->base) {
      default:
         errorT('i', "printSt(base=%k)", this->base);
      case ASM:
         fprintf(out_file, "asm(\"%s\");\n", (char *)this->e);
         break;
      case DCL:
         dcl_printId(this->d, SM);
         break;
      case BREAK:
      case CONTINUE:
         puttok(this->base);
         puttok(SM);
         break;
      case DEFAULT:
         puttok(this->base);
         puttok(COLON);
         printSt(this->s);
         break;
      case SM:
/*if (this->e->base==CALL || this->e->base==G_CALL) errorT('d',"%n",(IdP)this->e->e1);*/
         if (this->e) {
            printEx(this->e);
            if (this->e->base == ICALL && this->e->e2) break; /* a block: no SM */
         }
         puttok(SM);
         break;
      case WHILE:
         puttok(WHILE);
         putcond(this->e);
         printSt(this->s);
         break;
      case DO:
         puttok(DO);
         printSt(this->s);
         puttok(WHILE);
         putcond(this->e);
         puttok(SM);
         break;
      case SWITCH:
         puttok(SWITCH);
         putcond(this->e);
         printSt(this->s);
         break;
      case RETURN:
         if (outlined) { // FUDGE, handle return statement of outlined inline
         // it has been simplified as an inline, that is
         // ignoring _return;
            if (this->e && this->e != dummy) {
//errorT('d',"outlined %t %t => %d",outlined,this->e->tp,outlined != this->e->tp);
               if (outlined != this->e->tp) {
                  switch (outlined->base) {
                     case RPTR:
                     case PTR:
                        if (ToPtrP(outlined)->typ == ToPtrP(this->e->tp)->typ) break;
                     default:
                        this->e = (ExP)MakeTEx(CAST, outlined, this->e);
                  }
               }
            }
         }
         puttok(RETURN);
         if (this->e) printEx(this->e);
         puttok(SM);
         while (this->s_list && this->s_list->base == SM) this->s_list = this->s_list->s_list; // FUDGE!!
         break;
      case CASE:
         puttok(CASE);
         eprint(this->e);
         puttok(COLON);
         printSt(this->s);
         break;
      case GOTO:
         puttok(GOTO);
         printId(this->d);
         puttok(SM);
         break;
      case LABEL:
         printId(this->d);
         puttok(COLON);
         printSt(this->s);
         break;
      case IF:
      {
         int val = QUEST;
         if (this->e->base == ANAME) {
            IdP a = (IdP) this->e;
            ExP arg = aval(a);
//errorT('d',"arg %d%k %d (%d)",arg,arg?arg->base:0,arg?arg->base:0,arg?arg->e1:0);
            if (arg)
               switch (arg->base) {
                  case ZERO:
                     val = 0;
                     break;
                  case ADDROF:
                  case G_ADDROF:
                     val = 1;
                     break;
                  case IVAL:
                     val = ToInt(arg->e1) != 0;
               }
         }
//errorT('d',"val %d",val);
         switch (val) {
            case 1:
               printSt(this->s);
               break;
            case 0:
               if (this->else_stmt)
                  printSt(this->else_stmt);
               else
                  puttok(SM); /* null statement */
               break;
            default:
               puttok(IF);
               putcond(this->e);
               if (this->s->s_list) {
                  puttok(LC);
                  printSt(this->s);
                  puttok(RC);
               } else
                  printSt(this->s);
               if (this->else_stmt) {
                  puttok(ELSE);
                  if (this->else_stmt->s_list) {
                     puttok(LC);
                     printSt(this->else_stmt);
                     puttok(RC);
                  } else
                     printSt(this->else_stmt);
               }
         }
         break;
      }
      case FOR:
      {
         int fi = this->for_init && (this->for_init->base != SM || this->for_init->memtbl || this->for_init->s_list);
//errorT('d',"fi %d for_init %d base %d tbl %d Sts %d",fi,this->for_init,this->for_init->base,this->for_init->memtbl,this->for_init->s_list);
         if (fi) {
            puttok(LC);
            printSt(this->for_init);
         }
         puttok(FOR);
         puttok(LP);
         if (fi == 0 && this->for_init) printEx(this->for_init->e);
         putch(';'); /* to avoid newline: not puttok(SM) */
         eprint(this->e);
         putch(';');
         eprint(this->e2);
         puttok(RP);
         printSt(this->s);
      /*      if (this->for_init) {
       *      if (this->s_list) printSt(this->s_list);
       puttok(RC);
       return;*
       puttok(RC);
       }*/
         if (fi) puttok(RC);
         break;
      }
      case PAIR:
         if (this->s && this->s2) {
            puttok(LC);
            printSt(this->s);
            printSt(this->s2);
            puttok(RC);
         } else {
            if (this->s) printSt(this->s);
            if (this->s2) printSt(this->s2);
         }
         break;
      case BLOCK:
         puttok(LC);
         putline(&this->where);
         if (this->d) dcl_printId(this->d, SM);
         if (this->memtbl && this->own_tbl) {
            IdP n;
            int i;
            for (n = get_mem(this->memtbl, i = 1); n; n = get_mem(this->memtbl, ++i)) {
               if (n->tp && n->n_union == 0)
                  switch (n->n_scope) {
                     case ARGT:
                     case ARG:
                        break;
                     default:
                        dcl_printId(n, 0);
                  }
            }
         }
         if (this->s) printSt(this->s);
         puttok(RC);
   }
   if (this->s_list) printSt(this->s_list);
}

/*
	print the declarations of the entries in the order they were inserted
	ignore labels (this->tp==0)
*/
void dcl_printTable(TableP this, Token s, Token pub) {
   register IdP *np;
   register int i;

   if (this == 0) return;

   np = this->entries;
   for (i = 1; i < this->free_slot; i++) {
      register IdP n = np[i];
      switch (s) {
         case 0:
            dcl_printId(n, 0);
            break;
         case EQ:
            if (n->tp && n->n_scope == pub) dcl_printId(n, 0);
            break;
         case NE:
            if (n->tp && n->n_scope != pub) dcl_printId(n, 0);
            break;
      }
   }
}
