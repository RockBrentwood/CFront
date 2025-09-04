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

Pname classdef.has_ictor()
/*
	does this class have a constructor taking no arguments?
*/
{
	Pname c = has_ctor();
	Pfct f;
	Plist l;

	if (c == 0) return 0;

	f = (Pfct)c->tp;

	switch (f->base) {
	default:
		error('i',"%s: bad constructor (%k)",string,c->tp->base);

	case FCT:
		switch (f->nargs) {
		case 0:		return c;
		default:	if (f->argtype->n_initializer) return c;
		}
		return 0;

	case OVERLOAD:
		for (l=((Pgen)f)->fct_list; l; l=l->l) {
			Pname n = l->f;
			f = (Pfct)n->tp;
			switch (f->nargs) {
			case 0:		return n;
			default:	if (f->argtype->n_initializer) return n;
			}
		}
		return 0;
	}
}

gen.gen(char* s)
{
	char * p = new char[ strlen(s)+1 ];
	base = OVERLOAD;
	strcpy(p,s);
	string = p;
	fct_list = 0;
}

Pname gen.add(Pname n,int sig)
/*
	add "n" to the tail of "fct_list"
	(overloaded names are searched in declaration order)

	detect:	 	multiple identical declarations
			declaration after use
			multiple definitions
*/
{
	Pfct f = (Pfct)n->tp;
	Pname nx;

	if (f->base != FCT) error(0,"%n: overloaded non-F",n);

	if ( fct_list && (nx=find(f)) ) {
/*
		Pfct nf = (Pfct)nx->tp;

		if (nf->body) {
			if (f->body) error("two definitions for overloaded%n",n);
		}
		else {
			if (f->body) nf->body = f->body;
		}
*/
		Nold = 1;
	}
	else {
		char* s = string;

		if (fct_list || sig) {
			char buf[128];
			char* bb = n->tp->signature(buf);
			int l1 = strlen(s);
			int l2 = bb-buf-1;
			char* p = new char[l1+l2+1];
			strcpy(p,s);
			strcpy(p+l1,buf);
			n->string = p;
		}
		else 
			n->string = s;

		nx = new name;
		*nx = *n;
		PERM(nx);
		Nold = 0;
		if (fct_list) {
			Plist gl;
			for (gl=fct_list; gl->l; gl=gl->l) ;
			gl->l = new name_list(nx,0); 
		}
		else
			fct_list = new name_list(nx,0);
		nx->n_list = 0;
	}
	return nx;
}

Pname gen.find(Pfct f)
{
	Plist gl;
	
	for (gl=fct_list; gl; gl=gl->l) {
		Pname nx = gl->f;
		Pfct fx = (Pfct)nx->tp;
		Pname a, ax;
/*fprintf(stderr,"find %s\n",nx->string); fflush(stderr);*/

		if (fx->nargs_known != f->nargs_known) continue;

		for (ax=fx->argtype, a=f->argtype; a&&ax; ax=ax->n_list, a=a->n_list) { //(#) Clipped at "a=a->n_lis".
/*fprintf(stderr,"ax %d %d a %d %d\n",ax->tp,ax->tp->base,a->tp,a->tp->base); fflush(stderr);*/ //(#) Clipped at "fflush".
			Ptype at = ax->tp;
			if ( at->check(a->tp,0) || vrp_equiv ) goto xx;
			switch (at->base) {
			case CHAR:
			case SHORT:
			case INT:
			case LONG:
				if (((Pbase)at)->b_unsigned ^ ((Pbase)a->tp)->b_unsigned) error('w',"the overloading mechanism cannot tell an unsigned%k from a%k",at->base,at->base); //(#) Clipped at "((Pbase)a->tp)->b_uns".
			}
		}

		if (ax) {
			if (ax->n_initializer)
				error("Ir makes overloaded %s() ambiguous",string);
			continue;
		}

		if (a) {
			if (a->n_initializer)
				error("Ir makes overloaded %s() ambiguous",string);
			continue;
		}

		if ( fx->returns->check(f->returns,0) )
			error("two different return valueTs for overloaded %s: %t and %t", string, fx->returns, f->returns); //(#) Clipped at "%s: %t a".

		return nx;
	xx:;
	}

	return 0;
}

void classdef.dcl(Pname cname, Ptable tbl)
{
	int nmem;
	Pname p;
	Pptr cct;
	Pbase bt;
	Pname px;
	Ptable btbl;
	int bvirt;
	Pclass bcl;
	int i;

	int byte_old = byte_offset;
	int bit_old = bit_offset;
	int max_old = max_align;
	int boff;

	int in_union;
	int usz;

	/* this is the place for paranoia */
	if (this == 0) error('i',"0->Cdef.dcl(%d)",tbl);
	if (base != CLASS) error('i',"Cdef.dcl(%d)",base);
	if (cname == 0) error('i',"unNdC");
	if (cname->tp != this) error('i',"badCdef");
	if (tbl == 0) error('i',"Cdef.dcl(%n,0)",cname);
	if (tbl->base != TABLE) error('i',"Cdef.dcl(%n,tbl=%d)",cname,tbl->base);

	nmem = pubmem->no_of_names() + privmem->no_of_names() + pubdef->no_of_names(); //(#) Clipped at "pubdef->no_of_names".
	in_union = (csu==UNION || csu==ANON);

	if (clbase) {
		if (clbase->base != TNAME) error("BC%nU",clbase);
		clbase = ((Pbase)clbase->tp)->b_name;
		bcl = (Pclass)clbase->tp;
		if (bcl->defined == 0) error("BC%nU",clbase);
		tbl = bcl->memtbl;
		if (tbl->base != TABLE) error('i',"badBC table %d",tbl);
		btbl = tbl;
		bvirt = bcl->virt_count;
		if (bcl->csu == UNION) error('s',"C derived from union");
		if (in_union) 
			error("derived union");
		else
			csu = (pubbase) ? bcl->csu : CLASS;
		boff = bcl->tsizeof();
		max_align = bcl->align();
	}
	else {
		btbl = 0;
		bvirt = 0;
		boff = 0;
		if (!in_union) csu = (virt_count) ? CLASS : STRUCT;
		while (tbl!=gtbl && tbl->t_name) tbl = tbl->next; /* nested classes */ //(#) Clipped at "nested classes".
		max_align = AL_STRUCT;
	}

	memtbl->set_scope(tbl);
	memtbl->set_name(cname);
	if (nmem) memtbl->grow((nmem<=2)?3:nmem);

	cc->stack();
	cc->not = cname;
	cc->cot = this;

	byte_offset = usz = boff;
	bit_offset = 0;

	bt = new basetype(COBJ,cname);
	bt->b_table = memtbl;
	this_type = cc->tot = cct = new ptr(PTR,bt,0);
	PERM(cct);
	PERM(bt);

	for (p=privmem; p; p=px) {
		Pname m;
		px = p->n_list;
		if (p->tp->base==FCT) {
			Pfct f = (Pfct)p->tp;
			Pblock b = f->body;
			f->body = 0;
			switch( p->n_sto ) {
			case AUTO:
			case STATIC:
			case REGISTER:
			case EXTERN:
				error("M%n cannot be%k",p,p->n_sto);
				p->n_sto = 0;
			}
			m =  p->dcl(memtbl,0);
			if (b) {
				if (m->tp->defined)
					error("two definitions of%n",m);
				else if (p->where.line!=m->where.line)
					error('s',"previously declared%n cannot be defined inCD",p); //(#) Clipped at "cannot be ".
				else
					((Pfct)m->tp)->body = b;
			}
		}
		else {
			m = p->dcl(memtbl,0);
			if (m) {
				if (m->n_stclass==STATIC
				&& m->n_initializer)
					error('s',"staticM%n withIr",m);
				if (in_union) {
					if (usz < byte_offset) usz = byte_offset;
					byte_offset = 0;
				}
			}
		}
	}
	if (privmem && csu==STRUCT) csu = CLASS;

	for (p=pubmem; p; p=px) {
		Pname m;
		px = p->n_list;
		if (p->tp->base == FCT) {
			Pfct f = (Pfct)p->tp;
			Pblock b = f->body;
			f->body = 0;
			switch(p->n_sto) {
			case AUTO:
			case STATIC:
			case REGISTER:
			case EXTERN:
				error("M%n cannot be%k",p,p->n_sto);
				p->n_sto = 0;
			}
			m = p->dcl(memtbl,PUBLIC);
			if (b) {
				if (m->tp->defined)
					error("two definitions of%n",m);
				else if (p->where.line!=m->where.line)
					error('s',"previously declared%n cannot be defined inCD",p); //(#) Clipped at "cannot be ".
				else
					((Pfct)m->tp)->body = b;
			}
		}
		else {
			m = p->dcl(memtbl,PUBLIC);
			if (m) {
				if (m->n_stclass==STATIC
				&& m->n_initializer)
					error('s',"staticM%n withIr",m);
				if (in_union) {
					if (usz < byte_offset) usz = byte_offset;
					byte_offset = 0;
				}
			}
		}
		/*delete p;*/
	}
/*	pubmem = 0;
*/
	if (in_union) byte_offset = usz;

	if (virt_count || bvirt) {	/* assign virtual indices */
		Pname vp[100];
		Pname nn;

		nn = has_ctor();
		if (nn==0 || nn->n_table!=memtbl)
			error('s',"C%n with virtual but no constructor",cname);

		{	/*	FUDGE vtbl
				so that the name can be used in initializers
			*/
			char* s = new char[20];
			sprintf(s,"%s__vtbl",string);
			Pname n = new name(s);
			n->tp = Pfctvec_type;
			Pname nn = gtbl->insert(n,0);
			nn->use();
		}

		if (virt_count = bvirt)
			for (i=0; i<bvirt; i++) vp[i] = bcl->virt_init[i];

for ( nn=memtbl->get_mem(i=1); nn; nn=memtbl->get_mem(++i) ) {
	switch (nn->tp->base) {
	case FCT:
	{	Pfct f = (Pfct)nn->tp;
		if (bvirt) {
			Pname vn = btbl->look(nn->string,0);
			if (vn) {	/* match up with base class */
				if (vn->n_table==gtbl) goto vvv;
				Pfct vnf;
				switch (vn->tp->base) {
				case FCT:
					vnf = (Pfct)vn->tp;
					if (vnf->f_virtual) {
						if (vnf->check(f,0)) error("virtual%nT mismatch:%t and%t",nn,f,vnf); //(#) Clipped at "virtual".
						f->f_virtual = vnf->f_virtual;
						vp[f->f_virtual-1] = nn;
					}
					else
						goto vvv;
					break;
				case OVERLOAD:
				{	Pgen g = (Pgen)vn->tp;
					if (f->f_virtual
					|| ((Pfct)g->fct_list->f->tp)->f_virtual)
						error('s',"virtual%n overloaded inBC but not in derivedC",nn); //(#) Clipped at "overloaded inB".
					break;
				}
				default:
					goto vvv;
				}
			}
			else
				goto vvv;
		}
		else {
		vvv:
/*error('d',"vvv: %n f_virtual %d virt_count %d",nn,f->f_virtual,virt_count);*/
			if (f->f_virtual)  {
				f->f_virtual = ++virt_count;
				switch (f->f_virtual) {
				case 1:
				{	Pname vpn = new name("_vptr");
					vpn->tp = Pfctvec_type;
					(void) vpn->dcl(memtbl,PUBLIC);
					delete vpn;
				}
				default:
					vp[f->f_virtual-1] = nn;
				}
			}
		}
		break;
	}

	case OVERLOAD:
	{	Plist gl;
		Pgen g = (Pgen)nn->tp;
/*error('d',"overload%n bvirt==%d",nn,bvirt);*/
		if (bvirt) {
			Pname vn = btbl->look(nn->string,0);
			Pgen g2;
			Pfct f2;
			if (vn) {
/*error('d',"vn%n tp%k",vn,vn->tp->base);*/
				if (vn->n_table == gtbl) goto ovvv;
				switch (vn->tp->base) {
				default:
					goto ovvv;
				case FCT:
					f2 = (Pfct)vn->tp;
					if (f2->f_virtual
					|| ((Pfct)g->fct_list->f->tp)->f_virtual)
						error('s',"virtual%n overloaded in derivedC but not inBC",nn); //(#) Clipped at "overloaded in ".
					break;
				case OVERLOAD:
					g2 = (Pgen)vn->tp;
						
					for (gl=g->fct_list; gl; gl=gl->l) {
						Pname fn = gl->f;
						Pfct f = (Pfct)fn->tp;
						Pname vn2 = g2->find(f);

						if (vn2 == 0) {
							if (f->f_virtual) error('s',"virtual overloaded%n not found inBC",fn); //(#) Clipped at "error('s'".
						}
						else {
							Pfct vn2f = (Pfct)vn2->tp;
							if (vn2f->f_virtual) {
								f->f_virtual = vn2f->f_virtual; //(#) Clipped at "vn2f".
								vp[f->f_virtual-1] = fn; //(#) Clipped at "f_virtual-1] ".
							}
						}
					}
					break;
				}
			}
			else
				goto ovvv;
		}
		else {
		ovvv:
			for (gl=g->fct_list; gl; gl=gl->l) {
				Pname fn = gl->f;
				Pfct f = (Pfct)fn->tp;

/*fprintf(stderr,"fn %s f %d %d %d count %d\n",fn->string,f,f->base,f->f_virtual,virt_count+1);*/ //(#) Clipped at "f_virtual,vir".
				if (f->f_virtual) {
					f->f_virtual = ++virt_count;
					switch (f->f_virtual) {
					case 1:
					{	Pname vpn = new name("_vptr");
						vpn->tp = Pfctvec_type;
						(void) vpn->dcl(memtbl,0);
						delete vpn;
					}
					default:
						vp[f->f_virtual-1] = fn;
					}
				}
			}
		}
		break;
	}
}
		}
		virt_init = new Pname[virt_count];
		for (i=0; i<virt_count; i++) virt_init[i] = vp[i];
	}

	for (p=pubdef, pubdef=0; p; p=p->n_list) {
		char* qs = p->n_qualifier->string;
		char* ms = p->string;
		Pname cx;
		Ptable ctbl;
		Pname mx;

		if (strcmp(ms,qs)==0) ms = "_ctor";

		for (cx = clbase; cx; cx = ((Pclass)cx->tp)->clbase) {
			if (strcmp(cx->string,qs) == 0) goto ok;
		}
		error("publicQr %s not aBC",qs);
		continue;
	ok:
		ctbl = ((Pclass)cx->tp)->memtbl;
		mx = ctbl->lookc(ms,0);

		if (Ebase) {
			if (!Ebase->has_friend(cc->nof)) error("QdMN%n is in privateBC",p); //(#) Clipped at "is in privat".
		}
		else if (Epriv) {
			if (!Epriv->has_friend(cc->nof)) error("QdMN%n is private",p); //(#) Clipped at 'is private",'.
		}
		if (mx == 0) {
			error("C%n does not have aM %s",cx,p->string);
			p->tp = any_type;
		}
		else {
			if (mx->tp->base==OVERLOAD)
				error('s',"public specification of overloaded%n",mx);
			p->base = PUBLIC;
		}
		
		p->n_qualifier = mx;
		(void) memtbl->insert(p,0);
		if (Nold) error("twoDs of CM%n",p);
	}

	if (bit_offset) byte_offset += SZ_WORD;
	if (byte_offset < SZ_STRUCT) {
		Pname n = new name("_dummy");
		switch (SZ_STRUCT-obj_size) {
		case 1:		n->tp = char_type; break;
		case 2:		n->tp = char2_type; break;
		case 3:		n->tp = char3_type; break;
		case 4:		n->tp = char4_type; break;
		default:	n->tp = new vec(char_type,0);
				Pvec(n->tp)->size = SZ_STRUCT-obj_size;
		}
		(void) n->dcl(memtbl,0);
		delete n;
/*error('d',"dummy bo=%d",byte_offset);*/
	}
	int waste = byte_offset%max_align;
	if (waste) {	/* fudge, ensure derived class get right sizeof */
		waste = max_align-waste;
/*error('d',"%s: waste %d tbl=%d",string,waste,memtbl);*/
		Pname n = new name("_waste");
		switch (waste) {
		case 1:		n->tp = char_type; break;
		case 2:		n->tp = char2_type; break;
		case 3:		n->tp = char3_type; break;
		case 4:		n->tp = char4_type; break;
		default:	n->tp = new vec(char_type,0);
				Pvec(n->tp)->size = waste;
		}
		(void) n->dcl(memtbl,0);
		delete n;
		if (byte_offset%max_align) error('i',"failed to align %s",string);
	}
/*error('d',"sz=%d al=%d",byte_offset,max_align);*/
	obj_size = byte_offset;
	obj_align = max_align;
	
	if ( has_dtor() && has_ctor()==0)
		error('w',"%s has destructor but no constructor",string);

	if ( itor==0 && has_oper(ASSIGN) )
		error('w',"%s has assignment defined but not initialization (no %s(...))",string,string); //(#) Clipped at "(no %s(". Not verified.
	
	defined = 1;

	for (p=memtbl->get_mem(i=1); p; p=memtbl->get_mem(++i)) {
	/* define members defined inline */
		switch (p->tp->base) {
		case FCT:
		{	Pfct f = (Pfct)p->tp;
			if (f->body) {
				f->f_inline = 1;
				p->n_sto = STATIC;
				f->dcl(p);
			}
			break;
		}
		case OVERLOAD:
		{	Pgen g = (Pgen)p->tp;
			Plist gl;
			for (gl=g->fct_list; gl; gl=gl->l) {
				Pname n = gl->f;
				Pfct f = (Pfct)n->tp;
				if (f->body) {
					f->f_inline = 1;
					n->n_sto = STATIC;
					f->dcl(n);
				}
			}
		}
		}
	}

	Plist fl;				/* define friends defined inline */
	for (fl=friend_list; fl; fl=fl->l) {
		Pname p = fl->f;
		switch (p->tp->base) {
		case FCT:
		{	Pfct f = (Pfct)p->tp;
			if (f->body && f->defined==0) {
				f->f_inline = 1;
				p->n_sto = STATIC;
				f->dcl(p);
			}
			break;
		}
		case OVERLOAD:
		{	Pgen g = (Pgen)p->tp;
			Plist gl;
			for (gl=g->fct_list; gl; gl=gl->l) {
				Pname n = gl->f;
				Pfct f = (Pfct)n->tp;
				if (f->body && f->defined==0) {
					f->f_inline = 1;
					n->n_sto = STATIC;
					f->dcl(n);
				}
			}
		}
		}
	}

	byte_offset = byte_old;
	bit_offset = bit_old;
	max_align = max_old;

	cc->unstack();
}

void enumdef.dcl(Pname, Ptable tbl)
{
#define FIRST_ENUM 0
	int nmem = mem->no_of_names();
	Pname p;
	Pname ns = 0;
	Pname nl;
	int enum_old = enum_count;
	no_of_enumerators = nmem;

	enum_count = FIRST_ENUM;

	if (this == 0) error('i',"0->enumdef.dcl(%d)",tbl);

	for(p=mem, mem=0; p; p=p->n_list) {
		Pname nn;
		if (p->n_initializer) {
			Pexpr i = p->n_initializer->typ(tbl);
			Neval = 0;
			enum_count = i->eval();
			if (Neval) error("%s",Neval);
			DEL(i);
			p->n_initializer = 0;
		}
		p->n_evaluated = 1;
		p->n_val = enum_count++; 
		nn = tbl->insert(p,0); /* ??? */
		if (Nold) {
			if (nn->n_stclass == ENUM) {
				if (p->n_val != nn->n_val) error("twoDs of enum constant%n",nn); //(#) Clipped at "twoDs of enum con". Not verified.
			}
			else
				error("incompatibleDs of%n",nn);
		}
		else {
			nn->n_stclass = ENUM; /* no store will be allocated */
			if (ns)
				nl->n_list = nn;
			else
				ns = nn;
			nl = nn;
		}
		delete p;
	}

	mem = ns;

	enum_count = enum_old;
	defined = 1;
}
/*
void fct.dcl(Ptable tbl)

	The argument names are placed in the memtable of the body.
	This makes
		f(int a) { int a; };
	illegal

	The argument names/types remain linked even after they are entered
	into the symbol table,
	but class and enum declarations are unlinked
{
	int nmem = TBLSIZE;
	Pname a;
	Pname ll;
	int bit_old = bit_offset;
	int byte_old = byte_offset;
	int max_old = max_align;
	int stack_old = stack_size;

	if (base != FCT) error('i',"fct.dcl(%d)",base);
	if (body==0 || body->memtbl) error('i',"fct.dcl(%d)",body);
	if (tbl->base != TABLE) error('i',"fct.dcl(tbl=%d)",tbl->base);

	body->memtbl = new table(nmem,tbl,0);
	body->own_tbl = 1;

	max_align = AL_FRAME;
	stack_size = byte_offset = SZ_BOTTOM;
	bit_offset = 0;

	for (a=argtype, ll=0; a; a=a->n_list) {
		Pname n = a->dcl(body->memtbl,ARG);
		n->n_list = 0;
		switch (a->tp->base) {
		case CLASS:
		case ENUM:
			break;
		default:
			if (ll)
				ll->n_list = n;
			else
				argtype = n;
			ll = n;
		}
	}

	frame_size = stack_size + SZ_TOP;
	frame_size = ((frame_size-1)/AL_FRAME)*AL_FRAME+AL_FRAME;
	bit_offset = bit_old;
	byte_offset = byte_old;
	max_align = max_old;
	stack_size = stack_old;
}*/

Pstmt curr_loop;
Pstmt curr_switch;
Pblock curr_block;

void stmt.reached()
{
	register Pstmt ss = s_list;

	if (ss == 0) return;

	switch (ss->base) {
	case LABEL:
	case CASE:
	case DEFAULT:
		break;
	default:
		error('w',"statement not reached");
		/* delete unreacheable code */
		for (; ss; ss=ss->s_list) {
			switch (ss->base) {
			case LABEL:
			case CASE:
			case DEFAULT:	/* reachable */
				s_list = ss;
				return;
			case IF:
			case DO:
			case WHILE:
			case SWITCH:
			case FOR:
			case BLOCK:	/* may hide a label */
				s_list = ss;
				return;
			}
		}
		s_list = 0;
	}
}

bit arg_err_suppress;

Pexpr check_cond(Pexpr e, TOK b, Ptable)
{
	Pname cn;
	if (cn = e->tp->is_cl_obj()) {	
		Pclass cl = (Pclass)cn->tp;
		int i = 0;
		Pname found = 0;
		for (Pname on = cl->conv; on; on=on->n_list) {
			Pfct f = (Pfct)on->tp;
			Ptype t = f->returns;
		xx:
			switch (t->base) {
			case TYPE:
				t = ((Pbase)t)->b_name->tp;
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
			error("%nO in%k expression",cn,b);
			return e;
		case 1:
		{
/*error('d',"cond%t<-%t",((Pfct)found->tp)->returns,e->tp);*/
			Pclass cl = (Pclass)cn->tp;
			Pref r = new ref(DOT,e,found);
			Pexpr c = new expr(G_CALL,r,0);
			c->fct_name = found;
			c->tp = ((Pfct)found->tp)->returns;
			return c;
		}
		default:
			error("%d possible conversions for%nO in%k expression",i,cn,b); //(#) Clipped at "i,cn".
			return e;
		}
		
	}
	e->tp->num_ptr(b);
	return e;
}

void stmt.dcl()
/*
	typecheck statement "this" in scope "curr_block->tbl"
*/
{
	Pstmt ss;
	Pname n;
	Pname nn;
	Pstmt ostmt = Cstmt;

	for (ss=this; ss; ss=ss->s_list) {
		Pstmt old_loop, old_switch;
		Cstmt = ss;
		Ptable tbl = curr_block->memtbl;
/*error('d',"ss %d%k tbl %d e %d%k s %d%k sl %d%k", ss, ss->base, tbl, ss->e, (ss->e)?ss->e->base:0, ss->s, (ss->s)?ss->s->base:0, ss->s_list, (ss->s_list)?ss->s_list->base:0);*/ //(#) Clipped at "ss->e, (ss->e".
		switch (ss->base) {
		case BREAK:
			if (curr_loop==0 && curr_switch==0)
				error("%k not in loop or switch",BREAK);
			ss->reached();
			break;

		case CONTINUE:
			if (curr_loop == 0) error("%k not in loop",CONTINUE);
			ss->reached();
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
			ss->s->dcl();
			break;

		case SM:
			ss->e = (ss->e != dummy) ? ss->e->typ(tbl) : 0;
			break;

		case DELETE:
		{	int i;
			ss->e = ss->e->typ(tbl);
			i = ss->e->tp->num_ptr(DELETE);
			if (i != P) error("nonP deleted");
			break;
		}

		case RETURN:
		{	Pname fn = cc->nof;
			Ptype rt = ((Pfct)fn->tp)->returns;
			Pexpr v = ss->e;
			if (v != dummy) {
				if (rt->base == VOID)
					error('w',"unX return value");
				else {
					v = v->typ(tbl);
				lx:
					switch (rt->base) {
					case TYPE:
						rt = ((Pbase)rt)->b_name->tp;
						goto lx;
					case RPTR:
						ss->e = ref_init((Pptr)rt,v,tbl);
						break;
					case COBJ:
					{	Pname rv = tbl->look("_result",0);
						ss->e = class_init(rv,rt,v,tbl);
						break;
					}
					case ANY:
						break;
					case INT:
					case CHAR:
					case LONG:
					case SHORT:
						if (((Pbase)rt)->b_unsigned
						&& v->base==UMINUS
						&& v->e2->base==ICON)
							error('w',"negative retured fromF returning unsigned"); //(#) Clipped at "negative retured".
					default:
						ss->e = v;
						if (rt->check(v->tp,ASSIGN))
							error("bad return valueT for%n:%t (%tX)",fn,v->tp,rt); //(#) Clipped at "bad return valueT fo".
					}
				}
			}
			else {
				if (rt->base != VOID) error('w',"return valueX");
			}
			ss->reached();
			break;
		}

		case DO:	/* in DO the stmt is before the test */					inline_restr |= 8; //(#) Clipped at "before the test */			".
			old_loop = curr_loop;
			curr_loop = ss;
			if (ss->s->base == DCL) error('s',"D as onlyS in do-loop");
			ss->s->dcl();
		/*	tbl = curr_block->memtbl;*/
			ss->e = ss->e->typ(tbl);
			ss->e = check_cond(ss->e,DO,tbl);
			curr_loop = old_loop;
			break;

		case WHILE:
			inline_restr |= 8;
			old_loop = curr_loop;
			curr_loop = ss;
			ss->e = ss->e->typ(tbl);
			/*ss->e->tp->num_ptr(ss->base);*/
			ss->e = check_cond(ss->e,WHILE,tbl);
			if (ss->s->base == DCL) error('s',"D as onlyS in while-loop"); //(#) Clipped at "in while-loop".
			ss->s->dcl();
			curr_loop = old_loop;
			break;

		case SWITCH:
		{	int ne = 0;
			inline_restr |= 4;
			old_switch = curr_switch;
			curr_switch = ss;
			ss->e = ss->e->typ(tbl);
		/*	ss->e->tp->num_ptr(SWITCH);*/
			ss->e = check_cond(ss->e,SWITCH,tbl);
			{	Ptype tt = ss->e->tp;
			sii:
				switch (tt->base) {
				case TYPE:
					tt = ((Pbase)tt)->b_name->tp; goto sii;
				case EOBJ:
					ne = Penum(Pbase(tt)->b_name->tp)->no_of_enumerators; //(#) Clipped at "no_of_en".
				case ZTYPE:
				case ANY:
				case CHAR:
				case SHORT:
				case INT:
				case LONG:
					break;
				default:
					error('s',"%t switch expression",ss->e->tp); //(#) Clipped at "ss->e->tp)".
				}
			}
			ss->s->dcl();
			if (ne) {	/* see if the number of cases is "close to"
					   but not equal to the number of enumerators //(#) Clipped at "enumerato".
					*/
				int i = 0;
				Pstmt cs;
				for (cs=ss->case_list; cs; cs=cs->case_list) i++;
				if (i && i!=ne) {
					if (ne < i) {
				ee:		error('w',"switch (%t) with %d cases (%d enumerators)",ss->e->tp,i,ne); //(#) Clipped at "%d case".
					}
					else {
						switch (ne-i) {
						case 1: if (3<ne) goto ee;
						case 2: if (7<ne) goto ee;
						case 3: if (23<ne) goto ee;
						case 4: if (60<ne) goto ee;
						case 5: if (99<ne) goto ee;
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
			ss->e = ss->e->typ(tbl);
			ss->e->tp->num_ptr(CASE);
			{	Ptype tt = ss->e->tp;
			iii:
				switch (tt->base) {
				case TYPE:
					tt = ((Pbase)tt)->b_name->tp; goto iii;
				case ZTYPE:
				case ANY:
				case CHAR:
				case SHORT:
				case INT:
				case LONG:
					break;
				default:
					error('s',"%t case expression",ss->e->tp);
				}
			}
			if (1) {
				Neval = 0;
				int i = ss->e->eval();
				if (Neval == 0) {
					Pstmt cs;
					for (cs=curr_switch->case_list; cs; cs=cs->case_list) { //(#) Clipped at "cs; cs->cs->".
						if (cs->case_value == i) error("case %d used twice in switch",i); //(#) Clipped at "cas".
					}
					ss->case_value = i;
					ss->case_list = curr_switch->case_list;
					curr_switch->case_list = ss;
				}
			}
			if (ss->s->s_list) error('i',"case%k",ss->s->s_list->base);
			ss->s->s_list = ss->s_list;
			ss->s_list = 0;
			ss->s->dcl();
			break;

		case GOTO:
			inline_restr |= 2;
			ss->reached();
		case LABEL:
			/* Insert label in function mem table;
			   labels have function scope.
			*/
			n = ss->d;
			nn = cc->ftbl->insert(n,LABEL);

			/* Set a ptr to the mem table corresponding to the scope
			   in which the label actually occurred.  This allows the
			   processing of goto's in the presence of ctors and dtors
			*/
			if(ss->base == LABEL) {
				nn->n_realscope = curr_block->memtbl;
				inline_restr |= 1;
			}

			if (Nold) {
				if (ss->base == LABEL) {
					if (nn->n_initializer) error("twoDs of label%n",n); //(#) Clipped at "twoDs of labe".
					nn->n_initializer = (Pexpr)1;
				}
				if (n != nn) ss->d = nn;
			}
			else {
				if (ss->base == LABEL) nn->n_initializer = (Pexpr)1;
				nn->where = ss->where;
			}
			if (ss->base == GOTO)
				nn->use();
			else {
				if (ss->s->s_list) error('i',"label%k",ss->s->s_list->base); //(#) Clipped at "ss->s->s_lis".
				ss->s->s_list = ss->s_list;
				ss->s_list = 0;
				nn->assign();
			}
			if (ss->s) ss->s->dcl();
			break;

		case IF:
		{	Pexpr ee = ss->e->typ(tbl);
			if (ee->base == ASSIGN) {
				Neval = 0;
				(void)ee->e2->eval();
				if (Neval == 0) error('w',"constant assignment in condition"); //(#) Clipped at "assignment in c".
			}
			ss->e = ee = check_cond(ee,IF,tbl);
			switch (ee->tp->base) {
			case INT:
			case ZTYPE:
			{	int i;
				Neval = 0;
				i = ee->eval();
				if (Neval == 0) {
/*fprintf(stderr,"if (%d) %d %d\n",i,ss->e,ss->e->base);*/
					Pstmt sl = ss->s_list;
					if (i) {
						DEL(ss->else_stmt);
						ss->s->dcl();
						*ss = *ss->s;
					}
					else {
						DEL(ss->s);
						if (ss->else_stmt) {
							ss->else_stmt->dcl();
							*ss = *ss->else_stmt;
						}
						else {
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
			ss->s->dcl();
			if (ss->else_stmt) ss->else_stmt->dcl();
			break;
		}
		case FOR:
			inline_restr |= 8;
			old_loop = curr_loop;
			curr_loop = ss;
			if (ss->for_init) {
				Pstmt fi = ss->for_init;
				switch (fi->base) {
				case SM:
					if (fi->e == dummy) {
						ss->for_init = 0;
						break;
					}
				default:
					fi->dcl();
					break;
				case DCL:
					fi->dcl();
/*error('d',"dcl=>%k",fi->base);*/
					switch (fi->base) {
					case BLOCK:
					{
					/* { ... for( { a } b ; c) d ; e }
						=>
					   { ... { a for ( ; b ; c) d ; e }}
					*/
						Pstmt tmp = new stmt (SM,curloc,0);
						*tmp = *ss;	/* tmp = for */
						tmp->for_init = 0;
						*ss = *fi;	/* ss = { } */
						if (ss->s)
							ss->s->s_list = tmp;
						else
							ss->s = tmp;
						curr_block = (Pblock)ss;
						tbl = curr_block->memtbl;
						ss = tmp;	/* rest of for and s_list */ //(#) Clipped at "for and ".
						break;
					}
					}
				}
			}
			if (ss->e == dummy)
				ss->e = 0;
			else {
				ss->e = ss->e->typ(tbl);
				ss->e = check_cond(ss->e,FOR,tbl);
			}
			if (ss->s->base == DCL) error('s',"D as onlyS in for-loop"); //(#) Clipped at '"D as onlyS in for-loop")'.
			ss->s->dcl();
			ss->e2 = (ss->e2 == dummy) ? 0 : ss->e2->typ(tbl);
			curr_loop = old_loop;
			break;

		case DCL:	/* declaration after statement */
if (0)
		{	Pname n;
			Pexpr in;
			if (curr_block->own_tbl==0) {
				curr_block->memtbl = tbl = new table(8,tbl,0);
				curr_block->own_tbl = 1;
			}
			Pname dd = ss->d;
			if (dd->n_list) error('s',"list ofDs not at head of block");
			n = dd->dcl(tbl,FCT);
			in = n->n_initializer;
			ss->base = SM;
			if (n->n_stclass == STATIC && in) {
				error('s',"Id static not at head of block");
				goto dum;
			}
			Pname cln = n->tp->is_cl_obj();
			if (cln && ((Pclass)cln->tp)->has_dtor())
				error('s',"%n ofC%n with destructor not at head of block",n,cln); //(#) Clipped at "not at head of ". Not verified.
			if (in) {
				n->n_initializer = 0;
				switch (in->base) {
				case G_CALL:	/* constructor? */
				 {
					Pname fn = in->fct_name;
					if (fn==0 || fn->n_oper!=CTOR) goto ass;
					break;
				}
				case ILIST:
					error('s',"Ir list not at head of block");
					goto dum;
				case STRING:
					n->n_initializer = in;	/* constant */
					goto dum;
				default:
				ass:
					in = new expr(ASSIGN,n,in);
				}
				ss->e = in;
			}
			else {
			dum:
				ss->e = dummy;
			}
			break;
		}

		{
			/*	collect all the contiguous DCL nodes from the
				head of the s_list. find the next statement
			*/
			int non_trivial = 0;
			int count = 0;
			Pname tail = ss->d;
			for (Pname nn=tail; nn; nn=nn->n_list) {
				/*	find tail;
					detect non-trivial declarations
				*/
				count++;
				if (nn->n_list) tail = nn->n_list;
				Pname n = tbl->look(nn->string,0);
				if (n && n->n_table==tbl) non_trivial = 2;
				if (non_trivial) continue;
				Pexpr in = nn->n_initializer;
/*error('d',"in %d",in);*/
				if (in == 0) continue;
				if (non_trivial == 0) non_trivial = 1;
				if (nn->n_stclass==STATIC) {
					non_trivial = 2;
					continue;
				}
				switch (in->base) {
				case ILIST:
				case STRING:
					non_trivial = 2;
					continue;
				}
				Pname cln = nn->tp->is_cl_obj();
				if (cln == 0) continue;
				if (((Pclass)cln->tp)->has_dtor()) non_trivial = 2;
			}
/*error('d',"non_trivial %d",non_trivial);*/
			while( ss->s_list && ss->s_list->base==DCL ) {
				Pstmt sx = ss->s_list;
				tail = tail->n_list = sx->d;	/* add to tail */
				for (nn=sx->d; nn; nn=nn->n_list) {
					/*	find tail;
						detect non-trivial declarations
					*/
					count++;
					if (nn->n_list) tail = nn->n_list;
					Pname n = tbl->look(nn->string,0);
					if (n && n->n_table==tbl) non_trivial = 2;
					if (non_trivial) continue;
					Pexpr in = nn->n_initializer;
					if (in == 0) continue;
					if (non_trivial == 0) non_trivial = 1;
					if (nn->n_stclass==STATIC) {
						non_trivial = 2;
						continue;
					}
					switch (in->base) {
					case ILIST:
					case STRING:
						non_trivial = 2;
						continue;
					}
					Pname cln = nn->tp->is_cl_obj();
					if (cln == 0) continue;
					if (((Pclass)cln->tp)->has_dtor()) non_trivial = 2; //(#) Clipped at "non_triv".
				}
				ss->s_list = sx->s_list;
			/*	delete sx;	*/
			}
			Pstmt next_st = ss->s_list;
/*error('d',"non_trivial %d curr_block->own_tbl %d inline_restr %d",non_trivial,curr_block->own_tbl,inline_restr);*/ //(#) Clipped at "non_trivial,curr".
			if (non_trivial==2	/* must */
			|| (non_trivial==1	/* might */
				&& ( curr_block->own_tbl==0	/* just as well */
				|| inline_restr&3		/* label seen */)
			  	)
			) {
				/*	Create a new block,
					put all the declarations at the head,
					and the remainder of the slist as the
					statement list of the block.
				*/
				ss->base = BLOCK;

				/*	check that there are no redefinitions since the last //(#) Clipped at "redefinitions since ".
					"real" (user-written, non-generated) block
				*/
				for( nn=ss->d; nn; nn=nn->n_list ) {
					Pname n;
					if( curr_block->own_tbl
					&&  (n=curr_block->memtbl->look(nn->string,0)) //(#) Clipped at "look(nn->string,".
					&&  n->n_table->real_block==curr_block->memtbl->real_block) //(#) Clipped at "==curr_block->mem".
						error("twoDs of%n",n);
				}

				/*	attach the remainder of the s_list
					as the statement part of the block.
				*/
				ss->s = next_st;
				ss->s_list = 0;

				/*	create the table in advance, in order to set the //(#) Clipped at "in order to se".
					real_block ptr to that of the enclosing table //(#) Clipped at "enclosing tab".
				*/
				ss->memtbl = new table(count+4,tbl,0);
				ss->memtbl->real_block = curr_block->memtbl->real_block; //(#) Clipped at "->real_b".

				((Pblock)ss)->dcl(ss->memtbl);
			}
			else {	/*	to reduce the number of symbol tables,
					do not make a new block,
					instead insert names in enclosing block,
					and make the initializers into expression
					statements.
				*/
				Pstmt sss = ss;
				for( nn=ss->d; nn; nn=nn->n_list ) {
					Pname n = nn->dcl(tbl,FCT);
/*error('d',"%n->dcl(%d) -> %d init %d sss=%d",nn,tbl,n,n->n_initializer,sss);*/
					if (n == 0) continue;
					Pexpr in = n->n_initializer;
					n->n_initializer = 0;
					if (ss) {
						sss->base = SM;
						ss = 0;
					}
					else
						sss = sss->s_list = new estmt(SM,sss->where,0,0); //(#) Clipped at "estmt(SM,ss".
					if (in) {
						switch (in->base) {
						case G_CALL:	/* constructor? */
						{
							Pname fn = in->fct_name;
							if (fn && fn->n_oper==CTOR) break; //(#) Clipped at "==CTOR)".
						}
						default:
							in = new expr(ASSIGN,n,in);
						}
						sss->e = in->typ(tbl);
					}
					else
						sss->e = dummy;
				}
				ss = sss;
				ss->s_list = next_st;
			}
			break;
		}

		case BLOCK:
			((Pblock)ss)->dcl(tbl);
			break;

		case ASM:
			/* save string */
			break;

		default:
			error('i',"badS(%d %d)",ss,ss->base);
		}
	}

	Cstmt = ostmt;
}

void block.dcl(Ptable tbl)
/*
	Note: for a block without declarations memtbl denotes the table
	for the enclosing scope.
	A function body has its memtbl created by fct.dcl().
*/
{
	int bit_old = bit_offset;
	int byte_old = byte_offset;
	int max_old = max_align;
	Pblock block_old = curr_block;

	if (base != BLOCK) error('i',"block.dcl(%d)",base);

	curr_block = this;

	if (d) {
		Pname n;
		own_tbl = 1;
		if (memtbl == 0) {
			int nmem = d->no_of_names()+4;
			memtbl = new table(nmem,tbl,0);
			memtbl->real_block = this;
			/*	this is a "real" block from the
				source text, and not one created by DCL's
				inside a block. */
		}
		else
			if (memtbl != tbl) error('i',"block.dcl(?)");

		Pname nx;
		for (n=d; n; n=nx) {
			nx = n->n_list;
			n->dcl(memtbl,FCT);
			switch (n->tp->base) {
			case CLASS:
			case ANON:
			case ENUM:
				break;
			default:
				delete n;
			}
		}
	}
	else
		memtbl = tbl;

	if (s) {
		Pname odcl = Cdcl;
		Pname m;
		int i;

		s->dcl();

		if (own_tbl)
		for (m=memtbl->get_mem(i=1); m; m=memtbl->get_mem(++i)) {
			Ptype t = m->tp;

			if (t == 0) {
				if (m->n_assigned_to == 0) error('w',"undefined label %s",m->string); //(#) Clipped at "undefined lab".
				if (m->n_used == 0) error('w',"label %s not used", m->string); //(#) Clipped at '"label %s not used", '.
				continue;
			}
		ll:
			switch (t->base) {
			case TYPE:	t=((Pbase)t)->b_name->tp; goto ll;
			case CLASS:
			case ENUM:
			case FCT:
			case VEC:	continue;
			}

			if (m->n_addr_taken == 0) {
				if (m->n_used) {
					if (m->n_assigned_to) {
					}
					else {
						switch (m->n_scope) {
						case FCT:
							Cdcl = m;
							error('w',"%n used but not set",m); //(#) Clipped at "%n used but not ".
						}
					}
				}
				else {
					if (m->n_assigned_to) {
					}
					else {
						switch (m->n_scope) {
						case ARG:
							if (m->string[0]=='_' && m->string[1]=='A') break; /* generated name: cannot be used */ //(#) Clipped at "&& m-".
						case FCT:
							Cdcl = m;
							error('w',"%n not used",m);
						}
					}
				}
			}
		}
		Cdcl = odcl;
	}

	d = 0;

	if (bit_offset) byte_offset += SZ_WORD;
	if (stack_size < byte_offset) stack_size = byte_offset;
	bit_offset = bit_old;
	byte_offset = byte_old;
	curr_block = block_old;
}

int name.no_of_names()
{
	register int i = 0;
	register Pname n;
	for (n=this; n; n=n->n_list) i++;
	return i;
}

static Pexpr lvec[20], *lll;
static Pexpr list_back = 0;
#define list_put_back(x) list_back = x;

void new_list(Pexpr lx)
{
	if (lx->base != ILIST) error('i',"IrLX");

	lll = lvec;
	lll++;
	*lll = lx->e1;
}

Pexpr next_elem()
{
	Pexpr e;
	Pexpr lx;

	if (lll == lvec) return 0;

 	lx = *lll;

	if (list_back) {
		e = list_back;
		list_back = 0;
		return e;
	}

	if (lx == 0) {				/* end of list */
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
			return (Pexpr)1;	/* start of new ILIST */
		case ELIST:
			error("nestedEL");
			return 0;
		default:
			return e;
		}
	default:
		error('i',"IrL");
	}
}

void list_check(Pname nn, Ptype t, Pexpr il)
/*
	see if the list lll can be assigned to something of type t
	nn is the name of the variable for which the assignment is taking place.
	il is the last list element returned by next_elem()
*/
{
	Pexpr e;
	bit lst = 0;
	int i;
	Pclass cl;
	switch ( (int)il ) {
	case 0:		break;
	case 1:		lst = 1; break;
	default:	list_put_back(il);
	}

zzz:
	switch (t->base) {
	case TYPE:
		t = ((Pbase)t)->b_name->tp;
		goto zzz;

	case VEC:
	{	Pvec v = (Pvec)t;
		Ptype vt = v->typ;

		if (v->size) {	/* get at most v->size initializers */
			for (i=0; i<v->size; i++) { /* check next list element type */ //(#) Clipped at "list element type".
			ee:
				e = next_elem();

				 /* "too few" initializers are legal */
				if (e == 0) goto xsw;
			vtz:
				switch (vt->base) {
				case TYPE:
					vt = ((Pbase)vt)->b_name->tp;
					goto vtz;
				case VEC:
				case COBJ:
					list_check(nn,vt,e);
					break;
				default:
					if (e == (Pexpr)1) {
						error("unXIrL");
						goto ee;
					}
					if (vt->check(e->tp,ASSIGN))
						error("badIrT for%n:%t (%tX)",nn,e->tp,vt); //(#) Clipped at ",nn,e-".
				}
			}
			if ( lst && (e = next_elem()) ) error("end of IrLX after vector"); //(#) Clipped at "after ve".
		xsw:;
		}
		else {		/* determine v->size */
			i = 0;
		xx:
			while ( e=next_elem() ) {	/* get another initializer */ //(#) Clipped at "get another initializer ".
				i++;
			vtzz:
				switch (vt->base) {
				case TYPE:
					vt = ((Pbase)vt)->b_name->tp;
					goto vtzz;
				case VEC:
				case COBJ:
					list_check(nn,vt,e);
					break;
				default:
					if (e == (Pexpr)1) {
						error("unXIrL");
						goto xx;
					}
					if (vt->check(e->tp,ASSIGN))
						error("badIrT for%n:%t (%tX)",nn,e->tp,vt); //(#) Clipped at "nn,e-".
				}
			}
			v->size = i;
		}
		break;
	}

	case CLASS:
		cl = (Pclass)t;
		goto ccc;

	case COBJ:	/* initialize members */
		cl = (Pclass)((Pbase)t)->b_name->tp;
	ccc:
	{	Ptable tbl = cl->memtbl;
		Pname m;

		if (cl->clbase) {
			list_check(nn,cl->clbase->tp,0);
		}
		for (m=tbl->get_mem(i=1); m; m=tbl->get_mem(++i)) {
			Ptype mt = m->tp;
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
				mt = ((Pbase)mt)->b_name->tp;
				goto mtz;
			case CLASS:
			case ENUM:
				break;
			case VEC:
			case COBJ:
				list_check(nn,m->tp,e);
				break;
			default:
				if (e == (Pexpr)1) {
					error("unXIrL");
					goto dd;
				}
				if (mt->check(e->tp,ASSIGN))
					error("badIrT for %s .%n:%t (%tX)",cl->string,m,e->tp,m->tp); //(#) Clipped at ",cl->stri". Not verified.
			}
		}
		if (lst && (e = next_elem()) ) error("end of IrLX afterO");
		break;
	}
	default:
		e = next_elem();

		if (e == 0) {
			error("noIr forO");
			break;
		}
		
		if (e == (Pexpr)1) {
			error("unXIrL");
			break;
		}
		if (t->check(e->tp,ASSIGN))
			error("badIrT for%n:%t (%tX)",nn,e->tp,t);
		if (lst && (e = next_elem()) ) error("end of IrLX afterO");
		break;
	}
}

