/* @(#) simpl.c 1.7 1/27/86 17:49:27 */
/*ident	"@(#)cfront:src/simpl.c	1.7" */
/*******************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

simpl.c:

	simplify the typechecked function
	remove:		classes:
				class Fun-calls
				operators
				value constructors and destructors
			new and delete operators (replace with function calls)
			initializers		(turn them into statements)
			constant expressions		(evaluate them)
			inline functions		(expand the calls)
			enums				(make const ints)
			unreachable code		(delete it)
	make implicit coersions explicit

	in general you cannot simplify something twice

*******************************************************************/

#include "cfront.h"
#include "size.h"
#include <ctype.h>

IdP new_fct;
IdP del_fct;
IdP vec_new_fct;
IdP vec_del_fct;
StP del_list;
StP block_del_list;
IdP ret_var;
bit not_inl; // is the current function an inline?
IdP curr_fct; // current function
ExP init_list;
ExP one;

void simpl_init(void) {
   IdP nw = MakeId(oper_name(NEW));
   IdP dl = MakeId(oper_name(DELETE));
   IdP vn = MakeId("_vec_new");
   IdP vd = MakeId("_vec_delete");

   new_fct = insert(gtbl, nw, 0); /* void* operator new(long); */
   FreeId(nw);
   IdP a = MakeId(0);
   a->tp = (TypeP)long_type;
   new_fct->tp = (TypeP)MakeFun(Pvoid_type, a, 1);
   new_fct->n_scope = EXTERN;
   PERM(new_fct);
   PERM(new_fct->tp);
   use(new_fct);
   dcl_printId(new_fct, 0);

   del_fct = insert(gtbl, dl, 0); /* void operator delete(void*); */
   FreeId(dl);
   a = MakeId(0);
   a->tp = Pvoid_type;
   del_fct->tp = (TypeP)MakeFun((TypeP)void_type, a, 1);
   del_fct->n_scope = EXTERN;
   PERM(del_fct);
   PERM(del_fct->tp);
   use(del_fct);
   dcl_printId(del_fct, 0);

   a = MakeId(0);
   a->tp = Pvoid_type;
   IdP al = a;
   a = MakeId(0);
   a->tp = (TypeP)int_type;
   a->n_list = al;
   al = a;
   a = MakeId(0);
   a->tp = (TypeP)int_type;
   a->n_list = al;
   al = a;
   a = MakeId(0);
   a->tp = Pvoid_type;
   a->n_list = al;
   al = a; /* (Pvoid, int, int, Pvoid) */

   vec_new_fct = insert(gtbl, vn, 0);
   FreeId(vn);
   vec_new_fct->tp = (TypeP)MakeFun(Pvoid_type, al, 1);
   vec_new_fct->n_scope = EXTERN;
   PERM(vec_new_fct);
   PERM(vec_new_fct->tp);
   use(vec_new_fct);
   dcl_printId(vec_new_fct, 0);

   a = MakeId(0);
   a->tp = (TypeP)int_type;
   al = a;
   a = MakeId(0);
   a->tp = Pvoid_type;
   a->n_list = al;
   al = a;
   a = MakeId(0);
   a->tp = (TypeP)int_type;
   a->n_list = al;
   al = a;
   a = MakeId(0);
   a->tp = (TypeP)int_type;
   a->n_list = al;
   al = a;
   a = MakeId(0);
   a->tp = Pvoid_type;
   a->n_list = al;
   al = a; /* (Pvoid, int, int, Pvoid, int) */

   vec_del_fct = insert(gtbl, vd, 0);
   FreeId(vd);
   vec_del_fct->tp = (TypeP)MakeFun((TypeP)void_type, al, 1);
   vec_del_fct->n_scope = EXTERN;
   PERM(vec_del_fct);
   PERM(vec_del_fct->tp);
   use(vec_del_fct);
   dcl_printId(vec_del_fct, 0);

   one = MakeEx(IVAL, (ExP) 1, 0);
   one->tp = (TypeP)int_type;
   PERM(one);
}

TableP scope; /* current scope for simplClass() */
IdP expand_fn; /* name of function being expanded or 0 */
TableP expand_tbl; /* scope for inline function variables */

IdP has_oper(ClassP this, Token op) {
   const char *s = oper_name(op);
   IdP n;
   if (this == 0) errorT('i', "has_oper(0, %s)", s);
   n = lookc(this->memtbl, s, 0);
   if (n == 0) return 0;
   switch (n->n_scope) {
      case 0:
      case PUBLIC:
         return n;
      default:
         return 0;
   }
}

int no_of_returns;

void simplId(IdP this) {
   if (this->base == PUBLIC) return;

   if (this->tp == 0) errorT('i', "simplId(%n, this->tp==0)", this);

   switch (this->tp->base) {
      case 0:
         errorT('i', "%n->simplId(this->tp->base==0)", this);

      case OVERLOAD:
      {
         IdListP gl;
         for (gl = ToGenP(this->tp)->fct_list; gl; gl = gl->l) simplId(gl->f);
         break;
      }

      case FCT:
      {
         FunP f = (FunP) this->tp;
         IdP n;
         IdP th = f->f_this;
//errorT('d',"simpl%n tp=%t def=%d th=%d n_oper%k",this,this->tp,this->tp->defined,th,this->n_oper);
         if (th) {
            th->n_list = f->argtype;
            if (this->n_oper == CTOR) f->s_returns = th->tp;
         }

         if (this->tp->defined & (SIMPLIFIED | ~DEFINED)) return;
      //      this->tp->defined |= SIMPLIFIED;

         for (n = (th) ? th : f->argtype; n; n = n->n_list) simplId(n);

         if (f->body) {
            TableP oscope = scope;
            scope = f->body->memtbl;
            if (scope == 0) errorT('i', "%n memtbl missing", this);
            curr_fct = this;
            simplFun(f);
            if (f->f_inline && debug == 0) {
               if (MIA <= f->nargs) {
                  errorT('w', "too many arguments for inline%n (inline ignored)", this);
                  f->f_inline = 0;
                  scope = oscope;
                  break;
               }
               int i = 0;
               for (n = (th) ? th : f->argtype; n; n = n->n_list) {
                  n->base = ANAME;
                  n->n_val = i++;
                  if (n->n_table != scope) errorT('i', "%s %d %d\n", n->string, n->n_table, scope);
               }
               expand_tbl = (f->returns->base != VOID || this->n_oper == CTOR) ? scope : 0;
               expand_fn = this;
               if (expand_tbl) {
               // value returning: generate expression
                  ExP ee = (ExP) expandBlock(f->body);
               /* the body still holds the memtbl */
                  f->f_expr = (ee->base == CM) ? ee : MakeEx(CM, zero, ee);
               /* print.c assumes expansion into comma expression */
               } else { // not value return: can generate block
                  f->f_expr = 0;
                  f->body = (BlockP) expandBlock(f->body);
               }
               expand_fn = 0;
               expand_tbl = 0;
            }
            scope = oscope;
         }
         break;
      }

      case CLASS:
         simplClass(ToClassP(this->tp));
         break;
/*
	case EOBJ:
		this->tp->base = INT;
		break;
*/
      default:
//errorT('d',"%n tp %t n_init %d",this,this->tp, this->n_initializer);
         break;
   }

   if (this->n_initializer) simplEx(this->n_initializer);
   this->tp->defined |= SIMPLIFIED;
}

/*
	call only for the function definition (this->body != 0)

	simplify argument initializers, and base class initializer, if any
	then simplify the body, if any

	for constructor:call allocator if this==0 and this not assigned to
			(auto and static objects call constructor with this!=0,
			the new operator generates calls with this==0)
			call base & member constructors
	for destructor:	call deallocator (no effect if this==0)
			case base & member destructors

	for arguments  and function return values look for class objects
	that must be passed by constructor "operator X(X&)".

	Allocate temporaries for class object expressions, and see if
	class object return values can be passed as pointers.

	call constructor and destructor for local class variables.
*/
void simplFun(FunP this) {
   ExP th = (ExP)this->f_this;
   TableP tbl = this->body->memtbl;
   StP ss = 0;
   StP tail;
   IdP cln;
   ClassP cl;
   StP dtail = 0;

   not_inl = debug || this->f_inline == 0;
   ret_var = look(tbl, "_result", 0);
   if (ret_var && not_inl == 0) /* "_result" not used in inlines */
      ret_var->n_used = ret_var->n_assigned_to = ret_var->n_addr_taken = 0;
   del_list = 0;
   block_del_list = 0;
   scope = tbl;
   if (scope == 0) errorT('i', "simplFun()");

   if (th) {
      PtrP p = (PtrP) th->tp;
      cln = ToBaseP(p->typ)->b_name;
      cl = (ClassP) cln->tp;
   }

   if (curr_fct->n_oper == DTOR) { /* initialize del_list */
      ExP ee;
      StP es;
      IfStP ifs;
      IdP bcln = cl->clbase;
      ClassP bcl;
      IdP d;

      IdP fa = MakeId("_free"); /* fake argument for dtor */
      fa->tp = (TypeP)int_type;
      IdP free_arg = dclId(fa, this->body->memtbl, ARG);
      FreeId(fa);
      this->f_this->n_list = free_arg;

      TableP tbl = cl->memtbl;
      int i;
      IdP m;

   /* generate calls to destructors for all members of class cl */
      for (m = get_mem(tbl, i = 1); m; m = get_mem(tbl, ++i)) {
         TypeP t = m->tp;
         IdP cn;
         ClassP cl;
         IdP dtor;
         if (m->n_stclass == STATIC) continue;

         if (cn = is_cl_obj(t)) {
            cl = (ClassP) cn->tp;
            if (dtor = has_dtor(cl)) {
            /*      dtor(this,0);   */
               ExP aa = MakeEx(ELIST, zero, 0);
               ee = (ExP)MakeRef(REF, th, m);
               ee = (ExP)MakeRef(DOT, ee, dtor);
               ee = (ExP)MakeCall(ee, aa);
               ee->fct_name = dtor;
               ee->base = G_CALL;
               es = (StP)MakeESt(SM, curloc, ee, 0);
               if (dtail)
                  dtail->s_list = es;
               else
                  del_list = es;
               dtail = es;
            }
         } else if (cl_obj_vec) {
            cl = ToClassP(cl_obj_vec->tp);
            if (dtor = has_dtor(cl)) {
               int esz = tsizeof((TypeP)cl);
               ExP noe = MakeEx(IVAL, ToExP(tsizeof(t) / esz), 0);
               ExP sz = MakeEx(IVAL, (ExP) esz, 0);
               ExP mm = (ExP)MakeRef(REF, th, m);
               ExP arg = MakeEx(ELIST, (ExP)dtor, zero);
               lval((ExP)dtor, ADDROF);
               arg = MakeEx(ELIST, sz, arg);
               arg = MakeEx(ELIST, noe, arg);
               arg = MakeEx(ELIST, mm, arg);
               ee = (ExP)MakeCall((ExP)vec_del_fct, arg);
               ee->base = G_CALL;
               es = (StP)MakeESt(SM, curloc, ee, 0);
               if (dtail)
                  dtail->s_list = es;
               else
                  del_list = es;
               dtail = es;
            }
         }
      }

   // delete base
      if (bcln && (bcl = (ClassP) bcln->tp)
         && (d = has_dtor(bcl))) { // base::dtor(this,_free);
         ExP aa = MakeEx(ELIST, (ExP)free_arg, 0);
         ee = (ExP)MakeRef(REF, th, d);
         ee = (ExP)MakeCall(ee, aa);
      /*ee->fct_name = d; NO would suppress virtual */
         ee->base = G_CALL;
         es = (StP)MakeESt(SM, curloc, ee, 0);
      } else { // if (_free) _delete(this);
         ExP aa = MakeEx(ELIST, th, 0);
         ee = (ExP)MakeCall((ExP)del_fct, aa);
         ee->fct_name = del_fct;
         ee->base = G_CALL;
         es = (StP)MakeESt(SM, curloc, ee, 0);
         es = (StP)MakeIfSt(curloc, (ExP)free_arg, es, 0);
      }
      use(free_arg);
      use(ToIdP(th));
      if (dtail)
         dtail->s_list = es;
      else
         del_list = es;
      ifs = MakeIfSt(curloc, th, del_list, 0);
/*
		ifs = MakeIfSt(curloc,cc,es,0);
		if (dtail)
			dtail->s_list = ifs;
		else
			del_list = ifs;
		dtail = ifs;
*/
      del_list = (StP)ifs;

      if (del_list) simplSt(del_list);
   }

   int ass_count;
   if (curr_fct->n_oper == CTOR) {
      ExP ee;
      TableP tbl = cl->memtbl;
      IdP m;
      int i;

   /*
      generate: this=base::base(args)
      this->b_init == base::base(args) or 0
    */
      if (this->b_init) {
//errorT('d',"b_init %k",this->b_init->base);
         switch (this->b_init->base) {
            case ASSIGN:
            case CM:
               break;
            default:
            {
               CallP cc = (CallP) this->b_init;
               IdP bn = cc->fct_name;
               IdP tt = ToFunP(bn->tp)->f_this;
               ass_count = tt->n_assigned_to;
               simplCall(cc);
               init_list = MakeEx(ASSIGN, th, (ExP)cc);
            }
         }
      } else {
         ass_count = 0;
         init_list = 0;
      }

      if (cl->virt_count) { /* generate: this->_vptr=this->virt_init; */
         IdP vp = look(cl->memtbl, "_vptr", 0);
         ExP vtbl = (ExP)MakeStrEx(cl->string, "_vtbl");
         ee = (ExP)MakeRef(REF, th, vp);
         ee = MakeEx(ASSIGN, ee, vtbl);
         init_list = (init_list) ? MakeEx(CM, init_list, ee) : ee;
      }
      for (IdP nn = this->f_init; nn; nn = nn->n_list) {
         if (nn->n_initializer == 0) continue;
         IdP m = look(tbl, nn->string, 0);
         if (m && m->n_table == tbl) m->n_initializer = nn->n_initializer;
      }
   /* generate cl::new(args) for all members of cl */
      for (m = get_mem(tbl, i = 1); m; m = get_mem(tbl, ++i)) {
         TypeP t = m->tp;
         IdP cn;
         ClassP cl;
         IdP ctor;

         switch (m->n_stclass) {
            case STATIC:
            case ENUM:
               continue;
         }
         switch (t->base) {
            case FCT:
            case OVERLOAD:
            case CLASS:
            case ENUM:
               continue;
         }
         if (m->base == PUBLIC) continue;

         if (cn = is_cl_obj(t)) {
            ExP ee = m->n_initializer;
            m->n_initializer = 0; // from Fun must not persist until next Fun

            if (ee == 0) { // try default
               cl = (ClassP) cn->tp;
               if (ctor = has_ictor(cl)) {
                  ee = (ExP)MakeRef(REF, th, m);
                  ee = (ExP)MakeRef(DOT, ee, ctor);
                  ee = (ExP)MakeCall(ee, 0);
                  ee->fct_name = ctor;
                  ee->base = G_CALL;
                  ee = typ(ee, tbl); // look for default arguments
               } else if (has_ctor(cl)) {
                  error("M%n needsIr (no default constructor forC %s)", m, cl->string);
               }
            }

            if (ee) {
               simplEx(ee);
               if (init_list)
                  init_list = MakeEx(CM, init_list, ee);
               else
                  init_list = ee;
            }
         } else if (cl_obj_vec) {
            cl = (ClassP) cl_obj_vec->tp;
            if (ctor = has_ictor(cl)) { /*  _new_vec(vec,noe,sz,ctor); */
               int esz = tsizeof((TypeP)cl);
               ExP noe = MakeEx(IVAL, (ExP) (tsizeof(t) / esz), 0);
               ExP sz = MakeEx(IVAL, (ExP) esz, 0);
               ExP mm = (ExP)MakeRef(REF, th, m);
               ExP arg = MakeEx(ELIST, (ExP)ctor, 0);
            /*take_addr(ctor); */
               lval((ExP)ctor, ADDROF);
               arg = MakeEx(ELIST, sz, arg);
               arg = MakeEx(ELIST, noe, arg);
               arg = MakeEx(ELIST, mm, arg);
               ee = (ExP)MakeCall((ExP)vec_new_fct, arg);
               ee->fct_name = vec_new_fct;
               ee->base = G_CALL;
            /*      ee = typ(ee, tbl);       look for default arguments */
               simplEx(ee);
               if (init_list)
                  init_list = MakeEx(CM, init_list, ee);
               else
                  init_list = ee;
            } else if (has_ctor(cl)) {
               error("M%n[] needsIr (no default constructor forC %s)", m, cl->string);
            }
         } else if (m->n_initializer) {
         // init of non-class mem
         // set in mem_init()
            if (init_list)
               init_list = MakeEx(CM, init_list, m->n_initializer);
            else
               init_list = m->n_initializer;
            m->n_initializer = 0; // from Fun must not persist until next Fun
         } else if (is_ref(t)) {
            error("referenceM%n needsIr", m);
         } else if (tconst(t) && vec_const == 0) {
            error("constM%n needsIr", m);
         }
      } // for m
   }

   no_of_returns = 0;

   tail = simplBlock(this->body);
   if (this->body->s == 0) this->body->empty = 1; // null function

   if (this->returns->base != VOID) { /* return must have been seen */
      if (no_of_returns) {
         switch (tail->base) {
            case SM:
               switch (tail->e->base) {
                  case ICALL:
                  case G_CALL: /* not good enough */
                     goto dontknow;
               };
            default:
/*fprintf(stderr,"t %d %d\n",tail->base,tail->e->base);*/
               if (strcmp(curr_fct->string, "main"))
                  errorT('w', "maybe no value returned from%n", curr_fct);
               if (del_list) goto zaq;
               break;
            case RETURN:
            case IF:
            case SWITCH:
            case DO:
            case FOR:
            case LABEL:
            case BLOCK:
            case PAIR:
            case GOTO:
             dontknow:
               break;
         }
      } else {
         if (strcmp(curr_fct->string, "main"))
            errorT('w', "no value returned from%n", curr_fct);
         if (del_list) goto zaq;
      }
   } else if (del_list) { /* return may not have been seen */
    zaq:
      if (tail)
         tail->s_list = del_list;
      else
         this->body->s = del_list;
      tail = dtail;
   }

   if (curr_fct->n_oper == CTOR) {

      if (ToIdP(th)->n_assigned_to == 0) {
      /* generate:    if (this==0) this=_new( sizeof(class cl) );
         init_list ;
       */
         ToIdP(th)->n_assigned_to = ass_count ? ass_count : FUDGE111;
         ExP sz = MakeEx(IVAL, ToExP(tsizeof((TypeP)cl)), 0);
         ExP ee = MakeEx(ELIST, sz, 0);
         ee = (ExP)MakeCall((ExP)new_fct, ee);
         ee->fct_name = new_fct;
         ee->base = G_CALL;
         simplEx(ee);
         ee = MakeEx(ASSIGN, th, ee);
         StP es = (StP)MakeESt(SM, curloc, ee, 0);
         ee = MakeEx(EQ, th, zero);
         IfStP ifs = MakeIfSt(curloc, ee, es, 0);
      /*simplSt(ifs);
         do not simplify
         or "this = " will cause an extra call of base::base
       */
         if (init_list) {
            es = (StP)MakeESt(SM, curloc, init_list, 0);
            es->s_list = this->body->s;
            this->body->s = es;
            if (tail == 0) tail = es;
         }
         ifs->s_list = this->body->s;
         this->body->s = (StP)ifs;
         if (tail == 0) tail = (StP)ifs;
      }

      StP st = (StP)MakeESt(RETURN, curloc, th, 0);
      if (tail)
         tail->s_list = st;
      else
         this->body->s = st;
      tail = st;
   }
}

StP simplBlock(BlockP this) {
   int i;
   IdP n;
   StP ss = 0, sst;
   StP dd = 0, ddt;
   StP stail;
   TableP old_scope = scope;

   if (this->own_tbl == 0) {
      StP obd = block_del_list;
      block_del_list = 0;
      ss = (this->s) ? simplSt(this->s) : 0;
      block_del_list = obd;
      return ss;
   }

   scope = this->memtbl;
   if (scope->init_stat == 0) scope->init_stat = 1; /* table is simplified. */

   for (n = get_mem(scope, i = 1); n; n = get_mem(scope, ++i)) {
      StP st = 0;
      IdP cln;
      ExP in = n->n_initializer;
//errorT('d',"auto %n",n);
      if (in) scope->init_stat = 2; /* initializer in this scope */

      switch (n->n_scope) {
         case ARG:
         case 0:
         case PUBLIC:
            continue;
      }

      if (n->n_stclass == STATIC) continue;

      if (in && in->base == ILIST)
         errorT('s', "initialization of automatic aggregates");

      if (n->tp == 0) continue; /* label */

      if (n->n_evaluated) continue;

   /* construction and destruction of temporaries is handled locally */
      {
         const char *s = n->string;
         register char c3 = s[3];
         if (s[0] == '_' && s[1] == 'D' && isdigit(c3)) continue;
      }
//errorT('d',"cln %d",is_cl_obj(n->tp));
      if (cln = is_cl_obj(n->tp)) {
         ClassP cl = (ClassP) cln->tp;
         IdP d = has_dtor(cl);

         if (d) { // dtor(&n->cl, 0);
            RefP r = MakeRef(DOT, (ExP)n, d);
            ExP ee = MakeEx(ELIST, zero, 0);
            CallP dl = MakeCall((ExP)r, ee);
            StP dls = (StP)MakeESt(SM, n->where, (ExP)dl, 0);
            dl->base = G_CALL;
            dl->fct_name = d;
            if (dd)
               ddt->s_list = dls;
            else
               dd = dls;
            ddt = dls;
         }
//errorT('d',"in %d %k %n",in,in?in->base:0,cln);
         if (in) {
            switch (in->base) {
               case DEREF: // *constructor?
                  if (in->e1->base == G_CALL) {
                     IdP fn = in->e1->fct_name;
                     if (fn == 0 || fn->n_oper != CTOR) goto ddd;
                     st = (StP)MakeESt(SM, n->where, in->e1, 0);
                     n->n_initializer = 0;
                     break;
                  }
                  goto ddd;
               case ASSIGN: // assignment to "n"?
                  if (in->e1 == (ExP)n) {
                     st = (StP)MakeESt(SM, n->where, in, 0);
                     n->n_initializer = 0;
                     break;
                  }
               default:
                  goto ddd;
            }
         }
      } else if (cl_obj_vec) {
         ClassP cl = (ClassP) cl_obj_vec->tp;
         IdP d = has_dtor(cl);
         IdP c = has_ictor(cl);

         if (in) {
            if (c) { /*  _vec_new(vec,noe,sz,ctor); */
               FunP f = ToFunP(c->tp); // null constructor?
               BlockP b = f->body;
               if (f->f_inline && f->body->empty) {
                  n->n_initializer = 0;
                  goto skip;
               }
               int esz = tsizeof((TypeP)cl);
               ExP noe = MakeEx(IVAL, ToExP(tsizeof(n->tp) / esz), 0);
               ExP sz = MakeEx(IVAL, (ExP) esz, 0);
               ExP arg = MakeEx(ELIST, (ExP)c, 0);
            /*take_addr(c); */
               lval((ExP)c, ADDROF);
               arg = MakeEx(ELIST, sz, arg);
               arg = MakeEx(ELIST, noe, arg);
               arg = MakeEx(ELIST, (ExP)n, arg);
               arg = (ExP)MakeCall((ExP)vec_new_fct, arg);
               arg->base = G_CALL;
               arg->fct_name = vec_new_fct;
               st = (StP)MakeESt(SM, n->where, arg, 0);
               n->n_initializer = 0;
            } else
               goto ddd;
          skip:;
         }
         if (d) { /*  _vec_delete(vec,noe,sz,dtor,0); */
            StP dls;
            int esz = tsizeof((TypeP)cl);
            ExP noe = MakeEx(IVAL, ToExP(tsizeof(n->tp) / esz), 0);
            ExP sz = MakeEx(IVAL, (ExP) esz, 0);
            ExP arg = MakeEx(ELIST, (ExP)d, zero);
         /*take_addr(c); */
            lval((ExP)d, ADDROF);
            arg = MakeEx(ELIST, sz, arg);
            arg = MakeEx(ELIST, noe, arg);
            arg = MakeEx(ELIST, (ExP)n, arg);
            arg = (ExP)MakeCall((ExP)vec_del_fct, arg);
            arg->base = G_CALL;
            arg->fct_name = vec_del_fct;
            dls = (StP)MakeESt(SM, n->where, arg, 0);
            if (dd)
               ddt->s_list = dls;
            else
               dd = dls;
            ddt = dls;
         }
      } else if (in /*&& n->n_scope==FCT */ ) {
         switch (in->base) {
            case ILIST:
               switch (n->n_scope) {
                  case FCT:
                  case ARG:
                     errorT('s', "Ir list for localV%n", n);
               }
               break;
            case STRING:
               if (n->tp->base == VEC) break; /* BUG char vec only */
            default:
             ddd:
            {
               ExP ee = MakeEx(ASSIGN, (ExP)n, in);
               st = (StP)MakeESt(SM, n->where, ee, 0);
               n->n_initializer = 0;
            }
         }
      }

      if (st) {
         if (ss)
            sst->s_list = st;
         else
            ss = st;
         sst = st;
      }
   }

   if (dd) {
      StP od = del_list;
      StP obd = block_del_list;

      simplSt(dd);
   /*PERM(dd);
    */
      if (od)
         del_list = (StP)MakePair(curloc, dd, od);
      else
         del_list = dd;
      block_del_list = dd;

      stail = (this->s) ? simplSt(this->s) : 0;

      FunP f = (FunP) curr_fct->tp;
      if (this != f->body || f->returns->base == VOID || strcmp(curr_fct->string, "main") == 0) {
      // not dropping through the bottom of a value returning function
         if (stail)
            stail->s_list = dd;
         else
            this->s = dd;
         stail = ddt;
      }

      del_list = od;
      block_del_list = obd;
   } else
      stail = (this->s) ? simplSt(this->s) : 0;

   if (ss) { /* place constructor calls */
      simplSt(ss);
      sst->s_list = this->s;
      this->s = ss;
      if (stail == 0) stail = sst;
   }

   scope = old_scope;

   return stail;
}

void simplClass(ClassP this) {
   int i;
   IdP m;
   ClassP oc = this->in_class;

   this->in_class = this;
   for (m = get_mem(this->memtbl, i = 1); m; m = get_mem(this->memtbl, ++i)) {
      ExP i = m->n_initializer;
      m->n_initializer = 0;
      simplId(m);
      m->n_initializer = i;
   }
   this->in_class = oc;

   IdListP fl; /* simplify friends */
   for (fl = this->friend_list; fl; fl = fl->l) {
      IdP p = fl->f;
      switch (p->tp->base) {
         case FCT:
         case OVERLOAD:
            simplId(p);
      }
   }
}

void simplEx(ExP this) {
//errorT('d',"Ex::simpl (%d) %d%k e1=%d e2=%d tp2=%d cf%n",this->permanent,this,this->base,this->e1,this->e2,this->tp2,curr_fct);
   if (this == 0 || this->permanent == 2) return;

   switch (this->base) {
      case BLOCK:
      case SM:
      case IF:
      case FOR:
      case WHILE:
      case SWITCH:
         errorT('i', "%k inE", this->base);

      case VALUE:
         errorT('i', "simplEx(value)");

      case DELETE:
      /*      delete p => _delete(p);
         or  cl::~cl(p,1);
         delete[this->s]p => _delete(p);
         or vec_del_fct(p,vec_sz,elem_sz,~cl,1);
       */
      {
         IdP cln;
         ClassP cl;
         IdP n;
         TypeP tt = this->e1->tp;
       ttloop:
         switch (tt->base) {
            case TYPE:
               tt = ToBaseP(tt)->b_name->tp;
               goto ttloop;
            case VEC:
            case PTR:
               tt = ToPtrP(tt)->typ;
               break;
         }

         cln = is_cl_obj(tt);
         if (cln) cl = (ClassP) cln->tp;
         if (cln && (n = has_dtor(cl))) { // ~cl() might be virtual
//errorT('d',"%n %d",n,ToFunP(n->tp)->f_virtual);
            if (this->e2 == 0) { // this->e1->cl::~cl(1)
               this->base = G_CALL;
               this->e1 = (ExP)MakeRef(REF, this->e1, n);
               this->e2 = MakeEx(ELIST, one, 0);
               this->fct_name = n;
            } else { // del_cl_vec(this->e1,this->e2,elem_size,~cl,1);
               int esz = tsizeof((TypeP)cl);
               ExP sz = MakeEx(IVAL, ToExP(esz), 0);
               ExP arg = one;
               if (ToFunP(n->tp)->f_virtual) {
               // beware of sideeffects in expression this->e1
                  if (this->e1->base != NAME) errorT('s', "PE too complicated for delete[]");
                  ExP a = (ExP)MakeRef(REF, this->e1, n);
                  a = address(a);
                  arg = MakeEx(ELIST, a, arg);
               } else {
                  arg = MakeEx(ELIST, (ExP)n, arg);
                  lval((ExP)n, ADDROF); // take_addr(n);
               }
               arg = MakeEx(ELIST, sz, arg);
               arg = MakeEx(ELIST, this->e2, arg);
               arg = MakeEx(ELIST, this->e1, arg);
               this->base = G_CALL;
               this->e1 = (ExP)vec_del_fct;
               this->e2 = arg;
               this->fct_name = vec_del_fct;
            }
         } else if (cl_obj_vec) {
            errorT('i', "Ex::simpl: delete vector");
         } else { // _delete(this->e1)
            this->base = G_CALL;
            this->e2 = MakeEx(ELIST, this->e1, 0);
            this->e1 = (ExP)(this->fct_name = del_fct);
         }
      //      *this = *typ(this, gtbl);
         simplCall(ToCallP(this));
         break;
      }

      case G_ADDROF:
      case ADDROF:
         simplEx(this->e2);
         switch (this->e2->base) {
            case DOT:
            case REF:
            {
               RefP r = (RefP) this->e2;
               IdP m = r->mem;
               if (m->n_stclass == STATIC) { /* & static member */
                  ExP x;
                delp:
                  x = this->e2;
                  this->e2 = (ExP)m;
                  r->mem = 0;
                  DEL(Ex, x);
               } else if (m->tp->base == FCT) { /* & member Fun */
                  FunP f = (FunP) m->tp;
                  if (f->f_virtual) {
                  /* &p->f ==> p->vtbl[fi] */
                     int index = f->f_virtual;
                     ExP ie = (1 < index) ? MakeEx(IVAL, (ExP) (index - 1), 0) : 0;
                     IdP vp = look(m->n_table, "_vptr", 0);
                     r->mem = vp;
                     this->base = DEREF;
                     this->e1 = this->e2;
                     this->e2 = ie;
                  } else {
                     goto delp;
                  }
               }
            }
         }
         break;

      default:
         if (this->e1) simplEx(this->e1);
         if (this->e2) simplEx(this->e2);
         break;

      case NAME:
      case DUMMY:
      case ICON:
      case FCON:
      case CCON:
      case IVAL:
      case FVAL:
      case LVAL:
      case STRING:
      case ZERO:
      case ILIST:
         return;

      case SIZEOF:
         this->base = IVAL;
         this->e1 = (ExP) tsizeof(this->tp2);
         DEL(Type, this->tp2);
         this->tp2 = 0;
         break;

      case G_CALL:
      case CALL:
         simplCall(ToCallP(this));
         break;

      case QUEST:
         simplEx(this->cond);
         simplEx(this->e1);
         simplEx(this->e2);
         break;

      case NEW: /* change NEW node to CALL node */
      {
         IdP cln;
         IdP ctor;
         int sz = 1;
         int esz;
         ExP var_expr = 0;
         ExP const_expr;
         TypeP tt = this->tp2;
         ExP arg;

         if (cln = is_cl_obj(tt)) {
            ClassP cl = (ClassP) cln->tp;
            if (ctor = has_ctor(cl)) { /* cl_ctor(0, args) */
               ExP p = zero;
               if (ctor->n_table != cl->memtbl) {
               /*      no derived constructor: pre-allocate */
                  int dsz = tsizeof((TypeP)cl);
                  ExP ce = MakeEx(IVAL, (ExP) dsz, 0);
                  ce = MakeEx(ELIST, ce, 0);
                  p = MakeEx(G_CALL, (ExP)new_fct, ce);
                  p->fct_name = new_fct;
               }
               CallP c = (CallP) this->e1;
               c->e1 = (ExP)MakeRef(REF, p, (IdP) c->e1);
            /*      set_fct_name(c, ctor); */
               simplCall(c);
               *this = *ToExP(c);
               return;
            }
         } else if (cl_obj_vec) {
            ClassP cl = (ClassP) cl_obj_vec->tp;
            ctor = has_ictor(cl);
            if (ctor == 0) {
               if (has_ctor(cl)) error("new %s[], no default constructor", cl->string);
               cl_obj_vec = 0;
            }
         }

       xxx:
         switch (tt->base) {
            case TYPE:
               tt = ToBaseP(tt)->b_name->tp;
               goto xxx;
            default:
               esz = tsizeof(tt);
               break;
            case VEC:
            {
               VecP v = (VecP) tt;
               if (v->size)
                  sz *= v->size;
               else if (v->dim)
                  var_expr = (var_expr) ? MakeEx(MUL, var_expr, v->dim) : v->dim;
               else {
                  sz = SZ_WPTR;
                  break;
               }
               tt = v->typ;
               goto xxx;
            }
         }

         if (cl_obj_vec) { // _vec_new(0,no_of_elements,element_size,ctor)
            FunP f = ToFunP(cl_obj_vec->tp); // null constructor?
            BlockP b = f->body;
            if (f->f_inline && b->empty) goto skip2;
            const_expr = MakeEx(IVAL, (ExP) sz, 0);
            ExP noe = (var_expr) ? (sz != 1) ? MakeEx(MUL, const_expr, var_expr) : var_expr : const_expr;
            const_expr = MakeEx(IVAL, (ExP) esz, 0);
            this->base = CALL;
            arg = MakeEx(ELIST, (ExP)ctor, 0);
         /*take_addr(ctor); */
            lval((ExP)ctor, ADDROF);
            arg = MakeEx(ELIST, const_expr, arg);
            arg = MakeEx(ELIST, noe, arg);
            this->e2 = MakeEx(ELIST, zero, arg);
            this->e1 = (ExP)vec_new_fct;
            this->fct_name = vec_new_fct;
            break;
         }
       skip2:;
      /* call _new(element_size*no_of_elements) */
         sz *= esz;
         const_expr = MakeEx(IVAL, (ExP) sz, 0);
         arg = (var_expr) ? (sz != 1) ? MakeEx(MUL, const_expr, var_expr) : var_expr : const_expr;
//errorT('d',"new: (%t)_new(...)",this->tp);
         this->base = CAST;
         this->tp2 = this->tp;
         this->e1 = MakeEx(G_CALL, (ExP)new_fct, MakeEx(ELIST, arg, 0));
         this->e1->fct_name = new_fct;
         simplEx(this);
         break;
      }
      case CAST:
         simplEx(this->e1);
         break;

      case REF:
         simplEx(this->e1);
         break;
      case DOT:
         simplEx(this->e1);
         if (this->e1->base == CM) { /* &( , name). => ( ... , &name)-> */
            ExP ex = this->e1;
          cfr:
            switch (ex->e2->base) {
               case NAME:
                  this->base = REF;
                  ex->e2 = address(ex->e2);
                  break;
               case CM:
                  ex = ex->e2;
                  goto cfr;
            }
         }
         break;

      case ASSIGN:
      {
         FunP f = (FunP) curr_fct->tp;
         ExP th = (ExP)f->f_this;

         if (this->e1) simplEx(this->e1);
         if (this->e2) simplEx(this->e2);

         if (th && th == this->e1) {
            if (curr_fct->n_oper == CTOR) {
               if (init_list) {
               /* this=this->e2 => (this=this->e2,init_list) */
                  this->base = CM;
                  this->e1 = MakeEx(ASSIGN, this->e1, this->e2);
                  this->e2 = init_list;
               }
            }
         }
         break;
      }
   }

   if (this->tp && this->tp->base == INT) {
      Neval = 0;
      int i = eval(this);
      if (Neval == 0) {
         this->base = IVAL;
         this->e1 = (ExP) i;
      }
   }
}

/*
	fix member function calls:
		p->f(x) becomes f(p,x)
		o.f(x)  becomes f(&o,x)
	or if f is virtual:
		p->f(x) becomes ( *p->_vptr[ type_of(p).index(f)-1 ] )(p,x)
	replace calls to inline functions by the expanded code
*/
void simplCall(CallP this) {
   IdP fn = this->fct_name;
   FunP f = (fn) ? (FunP) fn->tp : 0;

   if (fn == 0) simplEx(this->e1);

   if (f) {
      switch (f->base) {
         case ANY:
            return;
         case FCT:
            break;
         case OVERLOAD:
         {
            GenP g = (GenP) f;
            this->fct_name = fn = g->fct_list->f;
            f = (FunP) fn->tp;
         }
      }
   }

   if (f && curr_expr == (ExP)this) { /* check for class object returning Fun */
      IdP cln = is_cl_obj(f->returns);
      if (cln && has_dtor(ToClassP(cln->tp))) errorT('s', "%n returned by%n is not used (%n has destructor)", cln, fn, cln);
   }
//errorT('d',"simpl call%n e1: %d%k",fn,this->e1,this->e1->base);
   switch (this->e1->base) {
      case DOT:
      case REF:
      {
         RefP r = (RefP) this->e1;
         ExP a1 = r->e1;
//errorT('d',"simpl fn %s f %d fv %d",fn?fn->string:"?",f,f?f->f_virtual:0);
         if (f && f->f_virtual) {
            ExP a11 = 0;

            switch (a1->base) { // see if temporary might be needed
               case NAME:
                  a11 = a1;
                  break;
               case ADDROF:
               case G_ADDROF:
                  if (a1->e2->base == NAME) a11 = a1;
                  break;
            }

            if (this->e1->base == DOT) {
               if (a11) a11 = address(a11);
               a1 = address(a1);
            }

            if (a11 == 0) {
            /* temporary (maybe) needed
               e->f() => (t=e,t->f(t))
             */
               char *s = make_name('K');
               IdP n = MakeId(s);
               n->tp = a1->tp;
               n = dclId(n, scope, ARG); /* no init! */
               n->n_scope = FCT;
               assign(n);
               a11 = (ExP)n;
               a1 = MakeEx(ASSIGN, (ExP)n, a1);
               a1->tp = n->tp;
               simplEx(a1);
               CallP cc = MakeCall(0, 0);
               *cc = *this;
               this->base = CM;
               this->e1 = a1;
               this->e2 = (ExP)cc;
               this = cc;
            }
            this->e2 = MakeEx(ELIST, a11, this->e2);
            int index = f->f_virtual;
            ExP ie = (1 < index) ? MakeEx(IVAL, (ExP) (index - 1), 0) : 0;
            IdP vp = look(fn->n_table, "_vptr", 0);
            ExP vptr = (ExP)MakeRef(REF, a11, vp); /* p->vptr */
            ExP ee = MakeEx(DEREF, vptr, ie); /* p->vptr[i] */
            TypeP pft = (TypeP)MakePtr(PTR, (TypeP)f, 0);
            ee = (ExP)MakeTEx(CAST, pft, ee); /* (T)p->vptr[i] */
            ee->tp = (TypeP) f->f_this; /* encode argtype */
            this->e1 = MakeEx(DEREF, ee, 0); /* *(T)p->vptr[i] */
         /* this->e1->tp must be 0, means "argtype encoded" */
            this->fct_name = 0;
            fn = 0;
            simplEx(this->e2);
            return; /* (*(T)p->vptr[i])(this->e2) */
         } else {
//errorT('d',"a1 %k%k%n",a1->base,this->e1->base,r->mem);
            if (this->e1->base == DOT) a1 = address(a1);
            this->e2 = MakeEx(ELIST, a1, this->e2);
            this->e1 = (ExP)r->mem;
         }
      }
   }

//errorT('d',"ex1 %d %d %d",fn,f->f_inline,debug);
   simplEx(this->e2);
//errorT('d',"ex2 %d %d %d",fn,f->f_inline,debug);
   if (this->e1->base == NAME && this->e1->tp->base == FCT) {
   /* reconstitute fn destroyed to suppress "virtual" */
      this->fct_name = fn = (IdP) this->e1;
      f = (FunP) fn->tp;
   }
//errorT('d',"ex3 %d %d %d",fn,f->f_inline,debug);
   if (fn && f->f_inline && debug == 0) {
//errorT('d',"expand%n",fn);
      ExP ee = expandFun(f, fn, scope, this->e2);
//errorT('d',"expanded %d %d",fn,ee);
      if (ee) *ToExP(this) = *ee;
   }
}

ExP curr_expr; /* to protect against an inline being expanded twice
                    in a simple expression keep track of expressions
                    being simplified
                  */

/*
	return a pointer to the last statement in the list, or 0
*/
StP simplSt(StP this) {
   if (this == 0) errorT('i', "simplSt(0)");
/*errorT('d',"St::simpl %d%k e %d%k s %d%k sl %d%k\n",this,this->base,this->e,this->e?this->e->base:0,this->s,this->s?this->s->base:0,this->s_list,this->s_list?this->s_list->base:0); fflush(stderr);*/

   curr_expr = this->e;

   switch (this->base) {
      default:
         errorT('i', "simplSt(%k)", this->base);

      case ASM:
         break;

      case BREAK:
      case CONTINUE:
         if (block_del_list) {
         /*      break           =>      { _dtor()s; break; }
            continue     =>      { _dtor()s; continue; }
          */
            StP bs = MakeSt(this->base, this->where, 0);
            StP dl = copy(block_del_list);
            this->base = BLOCK;
            this->s = (StP)MakePair(this->where, dl, bs);
            break;
         }
         break;

      case DEFAULT:
         simplSt(this->s);
         break;

      case SM:
         if (this->e) simplEx(this->e);
         break;

      case RETURN:
      { /*      return x;       =>
           { _ret_var = x; _dtor()s;  return _ret_var; }
           return ctor(x);      =>
           { ctor(&_result,x); _dtor()s;  return _ret_var; }
           return;              =>
           { _dtor()s; return; } OR (in constructors)
           { _dtor()s; return _this; }
         */

         no_of_returns++;

         if (not_inl) {
            StP as;
            if (this->e && this->e != dummy) {
               ExP ee;
               if (this->e->base == G_CALL && this->e->fct_name && this->e->fct_name->n_oper == CTOR && this->e->e1->base == DOT) {
                  RefP r = (RefP) this->e->e1;
                  r->e1 = (ExP)ret_var;
                  ee = this->e;
               } else {
                  ee = MakeEx(ASSIGN, (ExP)ret_var, this->e);
               }
               simplEx(ee);
               as = (StP)MakeESt(SM, this->where, ee, 0);
            } else
               as = 0;

            this->base = BLOCK;
            this->s = 0;
            this->d = 0;
            this->own_tbl = (this->memtbl) ? 1 : 0;
            simplBlock(ToBlockP(this));

            StP dl = (del_list) ? copy(del_list) : 0;
            if (this->s) dl = (dl) ? (StP)MakePair(this->where, this->s, dl) : this->s;

            StP rs = (StP)MakeESt(RETURN, this->where, (ret_var) ? (ExP) ret_var : 0, 0);
            if (as) {
               if (dl) as = (StP)MakePair(this->where, as, dl);
               this->s = (StP)MakePair(this->where, as, rs);
            } else {
               if (curr_fct->n_oper == CTOR) {
                  rs->e = (ExP)ToFunP(curr_fct->tp)->f_this;
               }
               this->s = (dl) ? (StP)MakePair(this->where, dl, rs) : rs;
            }
         } else {
            if (this->e->base == VALUE) errorT('s', "inlineF returns constructor");
            simplEx(this->e);
         }
         break;
      }

      case WHILE:
      case DO:
         simplEx(this->e);
         simplSt(this->s);
         break;
      case SWITCH:
         simplEx(this->e);
         simplSt(this->s);
         switch (this->s->base) {
            case DEFAULT:
            case LABEL:
            case CASE:
               break;
            case BLOCK:
               if (this->s->s)
                  switch (this->s->s->base) {
                     case BREAK: /* to cope with the "break; case" macro */
                     case CASE:
                     case LABEL:
                     case DEFAULT:
                        break;
                     default:
                        goto df;
                  }
               break;
            default:
             df:
               errorTL('w', &this->s->where, "statement not reached: case label missing");
         }
         break;
      case CASE:
         simplEx(this->e);
         simplSt(this->s);
         break;
      case LABEL:
         if (del_list) errorT('s', "label in block with destructors");
         simplSt(this->s);
         break;
      case GOTO:
      /* If the goto is going to a different (effective) scope,
       * then it is necessary to activate all relevant destructors
       * on the way out of nested scopes, and issue errors if there
       * are any constructors on the way into the target. */

      /* Only bother if the goto and label have different effective
       * scopes. (If mem table of goto == mem table of label, then
       * they're in the same scope for all practical purposes. */

      {
         IdP n = look(scope, this->d->string, LABEL);
         if (n == 0) errorTL('i', &this->where, "label%n missing", this->d);
         if (n->n_realscope != scope) {

         /* Find the root of the smallest subtree containing
          * the path of the goto.  This algorithm is quadratic
          * only if the goto is to an inner or unrelated scope. */

            TableP r = 0;

            for (TableP q = n->n_realscope; q != gtbl; q = q->next) {
               for (TableP p = scope; p != gtbl; p = p->next) {
                  if (p == q) {
                     r = p; /* found root of subtree! */
                     goto xyzzy;
                  }
               }
            }

          xyzzy:if (r == 0) errorTL('i', &this->where, "finding root of subtree");

         /* At this point, r = root of subtree, n->n_realscope
          * = mem table of label, and scope = mem table of goto. */

         /* Climb the tree from the label mem table to the table
          * preceding the root of the subtree, looking for
          * initializers and ctors.  If the mem table "belongs"
          * to an unsimplified block(s), the n_initializer field
          * indicates presence of initializer, otherwise initializer
          * information is recorded in the init_stat field of
          * mem table. */

            for (TableP p = n->n_realscope; p != r; p = p->next)
               if (p->init_stat == 2) {
                  errorL(&this->where, "goto%n pastD withIr", this->d);
                  goto plugh; /* avoid multiple error msgs */
               } else if (p->init_stat == 0) {
                  int i;
                  for (IdP nn = get_mem(p, i = 1); nn; nn = get_mem(p, ++i))
                     if (nn->n_initializer || nn->n_evaluated) {
                        errorL(&nn->where, "goto%n pastId%n", this->d, nn);
                        goto plugh;
                     }
               }
          plugh:

         /* Proceed in a similar manner from the point of the goto,
          * generating the code to activate dtors before the goto. */
         /* There is a bug in this code.  If there are class objects
          * of the same name and type in (of course) different mem
          * tables on the path to the root of the subtree from the
          * goto, then the innermost object's dtor will be activated
          * more than once. */

            {
               StP dd = 0, ddt;

               for (TableP p = scope; p != r; p = p->next) {
                  int i;
                  for (IdP n = get_mem(p, i = 1); n; n = get_mem(p, ++i)) {
                     IdP cln;
                     if (n->tp == 0) continue; /* label */

                     if (cln = is_cl_obj(n->tp)) {
                        ClassP cl = (ClassP) cln->tp;
                        IdP d = has_dtor(cl);

                        if (d) { /* n->cl::delete(0); */
                           RefP r = MakeRef(DOT, (ExP)n, d);
                           ExP ee = MakeEx(ELIST, (ExP)zero, 0);
                           CallP dl = MakeCall((ExP)r, ee);
                           StP dls = (StP)MakeESt(SM, n->where, (ExP)dl, 0);
                           dl->base = G_CALL;
                           dl->fct_name = d;
                           if (dd)
                              ddt->s_list = dls;
                           else
                              dd = dls;
                           ddt = dls;
                        }

                     } else if (cl_obj_vec) { /* never "new x" is a pointer */
                        ClassP cl = (ClassP) cl_obj_vec->tp;
                        IdP c = has_ictor(cl);
                        IdP d = has_dtor(cl);

                        if (d) { /*  _vec_delete(vec,noe,sz,dtor,0); */
                           StP dls;
                           int esz = tsizeof((TypeP)cl);
                           ExP noe = MakeEx(IVAL, (ExP) (tsizeof(n->tp) / esz), 0);
                           ExP sz = MakeEx(IVAL, (ExP) esz, 0);
                           ExP arg = MakeEx(ELIST, (ExP)d, zero);
                           lval((ExP)d, ADDROF);
                           arg = MakeEx(ELIST, sz, arg);
                           arg = MakeEx(ELIST, noe, arg);
                           arg = MakeEx(ELIST, (ExP)n, arg);
                           arg = (ExP)MakeCall((ExP)vec_del_fct, arg);
                           arg->base = G_CALL;
                           arg->fct_name = vec_del_fct;
                           dls = (StP)MakeESt(SM, n->where, arg, 0);
                           if (dd)
                              ddt->s_list = dls;
                           else
                              dd = dls;
                           ddt = dls;
                        }
                     }
                  } /* end mem table scan */
               } /* end dtor loop */

            /* "activate" the list of dtors obtained. */

               if (dd) {
                  simplSt(dd);
                  StP bs = MakeSt(this->base, this->where, 0);
                  *bs = *this;
                  this->base = PAIR;
                  this->s = dd;
                  this->s2 = bs;
               }
            }
         } /* end special case for non-local goto */
      }
         break;

      case IF:
         simplEx(this->e);
         simplSt(this->s);
         if (this->else_stmt) simplSt(this->else_stmt);
         break;
      case FOR:
      /* "for (this->s;this->e;this->e2) this->s2; => this->s; while(this->e) {this->s2;e3}" */
         if (this->for_init) {
            simplSt(this->for_init);
            if (this->for_init->base == SM && this->for_init->e->tp == (TypeP)void_type)
               errorT('s', "call of inline voidF in for-expression");
         }
         if (this->e) simplEx(this->e);
         if (this->e2) {
            curr_expr = this->e2;
            simplEx(this->e2);
            if (this->e2->base == ICALL && this->e2->tp == (TypeP)void_type)
               errorT('s', "call of inline voidF in for-expression");
         }
         simplSt(this->s);
         break;
      case BLOCK:
         simplBlock(ToBlockP(this));
         break;
      case PAIR:
         break;
   }

/*if (this->s) simplSt(this->s); */
   if (this->base != BLOCK && this->memtbl) {
      int i;
      StP t1 = (this->s_list) ? simplSt(this->s_list) : 0;
      StP ss = 0;
      IdP cln;
      for (IdP tn = get_mem(this->memtbl, i = 1); tn; tn = get_mem(this->memtbl, ++i)) {
/*fprintf(stderr,"tmp %s tbl %d\n",tn->string,this->memtbl);*/
         if (cln = is_cl_obj(tn->tp)) {
            ClassP cl = (ClassP) cln->tp;
            IdP d = has_dtor(cl);
            if (d) { /* n->cl::delete(0); */
               RefP r = MakeRef(DOT, (ExP)tn, d);
               ExP ee = MakeEx(ELIST, zero, 0);
               CallP dl = MakeCall((ExP)r, ee);
               StP dls = (StP)MakeESt(SM, tn->where, (ExP)dl, 0);
               dl->base = G_CALL;
               dl->fct_name = d;
               dls->s_list = ss;
               ss = dls;
/*errorT('d',"%d (tbl=%d): %n.%n %d->%d",this,this->memtbl,tn,d,ss,ss->s_list);*/
            }
         }
      }
      if (ss) {
         StP t2 = simplSt(ss);
         switch (this->base) {
            case IF: {
               StP es = copy(ss);
               if (this->else_stmt) {
                  StP t = es;
                  for (; t->s_list; t = t->s_list);
                  t->s_list = this->else_stmt;
               }
               this->else_stmt = es;
               t2->s_list = this->s;
               this->s = ss;
               break;
            }
            case RETURN:
            case WHILE:
            case FOR:
            case DO:
            case SWITCH:
               errorT('s', "E in%kS needs temporary ofC%n with destructor", this->base, cln);
               break;
            default:
               if (t1) {
                  t2->s_list = this->s_list;
                  this->s_list = ss;
                  return t1;
               }
               this->s_list = ss;
               return t2;
         }
      }
      return (t1) ? t1 : this;
   }

   return (this->s_list) ? simplSt(this->s_list) : this;
}

// now handles dtors in the expression of an IF St
// not general!
StP copy(StP this) {
   StP ns = MakeSt(0, curloc, 0);

   *ns = *this;
   if (this->s) ns->s = copy(this->s);
   if (this->s_list) ns->s_list = copy(this->s_list);

   switch (this->base) {
      case PAIR:
         ns->s2 = copy(this->s2);
         break;
   }

   return ns;
}
