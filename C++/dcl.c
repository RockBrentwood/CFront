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

class dcl_context ccvec[MAXCONT], * cc = ccvec;
int byte_offset;
int bit_offset;
int max_align;
int stack_size;
int enum_count;
int friend_in_class;

void name.check_oper(Pname cn)
{
	switch (n_oper) {
	case CALL:
		if (cn == 0) error("operator() must be aM");
		break;
	case DEREF:
		if (cn == 0) error("operator[] must be aM");
		break;
	case 0:
	case TNAME:	/* may be a constructor */
		if (cn && strcmp(cn->string,string)==0) {
			if (tp->base == FCT) {
				Pfct f = (Pfct)tp;
				if (f->returns!=defa_type && fct_void==0)
					error("%s::%s() with returnT",string,string); //(#) Clipped at "string,string".
				f->returns = void_type;
				string = "_ctor";
				n_oper = CTOR;
			}
			else
				error('s',"struct%cnM%n",cn,cn);
		}
		else
			n_oper = 0;
		break;
	case DTOR:	/* must be a destructor */
		if (cn == 0) {
			n_oper = 0;
			error("destructor ~%s() not inC",string);
		}
		else if (strcmp(cn->string,string) == 0) {
			Pfct f = (Pfct)tp;
			string = "_dtor";
			if (tp->base != FCT) {
				error("%s::~%s notF",cn->string,cn->string);
				tp = new fct(void_type,0,1);
			}
			else if (f->returns!=defa_type && fct_void==0)
				error("%s::~%s() with returnT",cn->string,cn->string); //(#) Clipped at "cn->string,cn->strin".
			if (f->argtype) {
				if (fct_void==0) error("%s::~%s() withAs",cn->string,cn->string); //(#) Clipped at 'withAs",cn->strin'.
				f->nargs = 0;
				f->nargs_known = 1;
				f->argtype = 0;
			}
			f->returns = void_type;
		}
		else {
			error("~%s in%s",string,cn->string);
			n_oper = 0;
		}
		break;
	case TYPE:
		if (cn == 0) {
			error("operator%t() not aM",(Ptype)n_initializer);
			n_oper = 0;
			n_initializer = 0;
		}
		else {
			Pfct f = (Pfct)tp;
			Ptype tx = (Ptype)n_initializer;
/*error('d',"operator%t()",tx);*/
			n_initializer = 0;
			if (f->base != FCT) error("badT for%n::operator%t()",cn,tx);
			if (f->returns != defa_type) {
				if (f->returns->check(tx,0)) error("bad resultT for%n::operator%t()",cn,tx); //(#) Clipped at "returnT for".
				DEL(f->returns);
			}
			if (f->argtype) {
				error("%n::operator%t() withAs",cn,tx);
				f->argtype = 0;
			}
			f->returns = tx;
			Pname nx = tx->is_cl_obj();
			if (nx && can_coerce(tx,cn->tp)) error("both %n::%n(%n) and %n::operator%t()",cn,cn,nx,tx); //(#) Clipped at "%n(%n) and".
			char buf[128];
			char* bb = tx->signature(buf);
			int l2 = bb-buf-1;
			char* p = new char[l2+1];
			strcpy(p,buf);
			string = p;
		}
		break;
	}
}

Pname name.dcl(Ptable tbl, TOK scope)
/*
	enter a copy of this name into symbol table "tbl";
		- create local symbol tables as needed
	
	"scope" gives the scope in which the declaration was found
		- EXTERN, FCT, ARG, PUBLIC, or 0
	Compare "scope" with the specified storage class "n_sto"
		- AUTO, STATIC, REGISTER, EXTERN, OVERLOAD, FRIEND, or 0

	After name.dcl()
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

	names of typenames are marked with n_oper==TNAME

	WARNING: The handling of scope and storage class is cursed!
*/
{
	Pname nn;
	Ptype nnt = 0;
	Pname odcl = Cdcl;

	if (this == 0) error('i',"0->name.dcl()");
	if (tbl == 0) error('i',"%n->name.dcl(tbl=0,%k)",this,scope);
	if (tbl->base != TABLE) error('i',"%n->name.dcl(tbl=%d,%k)",this,tbl->base,scope); //(#) Clipped at "this,tbl->base,".
	if (tp == 0) error('i',"name.dcl(%n,%k)T missing",this,scope);
/*fprintf(stderr,"(%d %s)->dcl(tbl=%d,scope=%d) tp = (%d %d)\n",this,string,tbl,scope,tp,tp->base); fflush(stderr);*/ //(#) Clipped at "tbl,scop".
	Cdcl = this;
	switch (base) {
	case TNAME:
		tp->dcl(tbl);
		PERM(tp);
		nn = new name(string);
		nn->base = TNAME;
		nn->tp = tp;
		tbl->insert(nn,0);
		delete nn;
		Cdcl = odcl;
		return this;
	case NAME:
		switch (n_oper) {
		case TNAME:
			if (tp->base != FCT) n_oper = 0;
			break;
		case COMPL:
			if (tp->base != FCT) {
				error("~%s notF",string);
				n_oper = 0;
			}
			break;
		}
		break;
	default:
		error('i',"NX in name.dcl()");
	}

	if (n_qualifier) {	/*	class function: c.f(); */
		if (tp->base != FCT) {
			error("QdN%n inD of nonF",this);
			Cdcl = odcl;
			return 0;
		}

		Pname cn = n_qualifier;
		switch (cn->base) {
		case TNAME:
			break;
		case NAME:
			cn = gtbl->look(cn->string,0);
			if (cn && cn->base==TNAME) break;
		default:
			error("badQr%n for%n",n_qualifier,this);
			Cdcl = odcl;
			return 0;
		}
		cn = ((Pbase)cn->tp)->b_name;
		if (n_oper) check_oper(cn);

		Pclass cl = (Pclass)cn->tp;
		if (cl == cc->cot) {
			n_qualifier = 0;
			goto xdr;
		}
		else if (cl->defined == 0) {
			error("C%nU",cn);
			Cdcl = odcl;
			return 0;
		}

		Ptable etbl = cl->memtbl;
		Pname x = etbl->look(string,0);
		if(x==0 || x->n_table!=etbl) {
			error("%n is not aM of%n",this,cn);
			Cdcl = odcl;
			return 0;
		}
	}
xdr:
	if (n_oper && tp->base!=FCT && n_sto!=OVERLOAD)
		error("operator%k not aF",n_oper);


	/*	if a storage class was specified
			check that it is legal in the scope 
		else
			provide default storage class
		some details must be left until the type of the object is known
	*/

	n_stclass = n_sto;
	n_scope = scope;	/* default scope & storage class */

	if (n_sto==0 && scope==EXTERN) {
		if (scope_default==STATIC) {
			switch (tp->base) {
			case FCT:
			{
				Pfct f = (Pfct)tp;
				if ( strcmp(string,"main") )
					n_scope = (f->body) ? STATIC : EXTERN;
				break;
			}
			case CLASS:
			case ENUM:
			default:
				n_scope = STATIC;
			}
		}
	}

	switch (n_sto) {
	default:
		error('i',"unX %k",n_sto);
	case FRIEND:
	{
		Pclass cl = cc->cot;

		switch (scope) {
		case 0:
		case PUBLIC:
			break;
		default:
			error("friend%n not in classD(%k)",this,scope);
			base = 0;
			Cdcl = odcl;
			return 0;
		}

		switch (n_oper) {
		case 0:
		case NEW:
		case DELETE:
		case CTOR:
		case DTOR:
			n_sto = 0;
			break;
		default:
			n_sto = OVERLOAD;
		}

		switch (tp->base) {
	/*	case INT:	 undefined: implicitly define as class
			nn = tname(CLASS);
			nn->tp->dcl(gtbl);
			break;
	*/
		case COBJ:
			nn = ((Pbase)tp)->b_name;
			break;
		case CLASS:
			nn = this;
			break;
		case FCT:
			cc->stack();
			cc->not = 0;
			cc->tot = 0;
			cc->cot = 0;
			friend_in_class++;
			nn = dcl(gtbl,EXTERN);
			friend_in_class--;
/*fprintf(stderr,"ff %s %d\n",nn->string,nn->tp->base);*/
			cc->unstack();
			if (nn->tp->base == OVERLOAD) {
				Pgen g = (Pgen)nn->tp;
				nn = g->find( (Pfct)tp );
			}
			break;
		default:
			error("badT%t of friend%n",tp,this);
		}
		PERM(nn);
		cl->friend_list = new name_list(nn,cl->friend_list);
		Cdcl = odcl;
		return nn;
	}
	case OVERLOAD:
		n_sto = 0;
		switch (scope) {
		case 0:
		case PUBLIC:
			error('w',"overload inCD (ignored)");
			switch (tp->base) {
			case INT:
				base = 0;
				Cdcl = odcl;
				return this;
			case FCT:
				return dcl(tbl,scope);
			}
		}
		if (n_oper && tp->base==FCT) break;
		nn = tbl->insert(this,0);

		if (Nold) {
			if (nn->tp->base != OVERLOAD) {
				error("%n redefined as overloaded",this);
				nn->tp = new gen(string);
			}
		}
		else {
			nn->tp = new gen(string);
		}

		switch (tp->base) {
		case INT:
			base = 0;
			Cdcl = odcl;
			return nn;
		case FCT:
			break;
		default:
			error("N%n ofT%k cannot be overloaded",this,tp->base);
			Cdcl = odcl;
			return nn;
		}
		break;
	case REGISTER:
		if (tp->base == FCT) {
			error('w',"%n: register (ignored)",this);
			goto ddd;
		}
	case AUTO:
		switch (scope) {
		case 0:
		case PUBLIC:
		case EXTERN:
			error("%k not inF",n_sto);
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
			/* extern is provided as a default for functions without body */ //(#) Clipped at "without bo".
			if (tp->base != FCT) error("externM%n",this);
			goto ddd;
		}
		n_stclass = STATIC;
		n_scope = EXTERN;	/* avoid FCT scoped externs to allow better checking */ //(#) Clipped at "to allow better".
		break;
	case STATIC:
		switch (scope) {
		case ARG:
			error("static used forA%n",this);
			goto ddd;
		case 0:
		case PUBLIC:
			n_stclass = STATIC;
			n_scope = scope;
			break;
		default:
			n_scope = STATIC;
		}
		break;
	case 0:
	ddd:
		switch (scope) {	/* default storage classes */
		case EXTERN:
			switch (tp->base) {
			case FCT:	/* anomaly:	f(int); => extern f(int); */ //(#) Clipped at "f(int); *".
				break;
			default:
				n_scope = scope_default;
			}
			n_stclass = STATIC;
			break;
		case FCT:
			if (tp->base == FCT) {
				n_stclass = STATIC;
				n_scope = EXTERN;
			}
			else
				n_stclass = AUTO;
			break;
		case ARG:
			if (tp->base == FCT) error("%n asA",this);
			n_stclass = AUTO;
			break;
		case 0:
		case PUBLIC:
			n_stclass = 0;
			break;
		}
	}

	
	/*
		now insert the name into the appropriate symbol table,
		and compare types with previous declarations of that name

		do type dependent adjustments of the scope
	*/

	switch (tp->base) {
	case ASM:
	{	Pbase b = (Pbase)tp;
		Pname n = tbl->insert(this,0);
		n->assign();
		n->use();
		return this;
	}

	case CLASS:
	{	Pclass cl;
		Pbase bt;
		Pname bn;
		Pclass nest;
		Pname nx = ktbl->look(string,0);		/* TNAME */
/*fprintf(stderr,"%s: nx %d\n",string,nx);*/
		if (nx == 0) {
			/*	search for hidden name for
					(1) nested class declaration
					(2) local class declaration
			*/
			for (nx=ktbl->look(string,HIDDEN); nx; nx=nx->n_tbl_list) {
				if (nx->n_key != HIDDEN) continue;
				if (nx->tp->base != COBJ) continue;
				bt = (Pbase)nx->tp;
				bn = bt->b_name;
				cl = (Pclass)bn->tp;
				if (cl == 0) continue;
				if ((nest=cl->in_class) && nest==cc->cot) 
					goto bbb;
				else if (cc->nof	/* fudge */
					&& cc->nof->where.line<nx->where.line)
					goto bbb;
			}
			error('i',"%n is not aTN",this);
		}
		else {
			bt = (Pbase)nx->tp;			/* COBJ */
			bn = bt->b_name;
			nest = 0;
		}
bbb:
/*fprintf(stderr,"bbb: bt %d %d\n",bt,bt->base); fflush(stderr);*/
		bn->where = nx->where;
		Pname bnn = tbl->insert(bn,CLASS);	/*copy for member lookup */
		cl = (Pclass)bn->tp;
								/* CLASS */
/*fprintf(stderr,"cl %d %d\n",cl,cl->base); fflush(stderr);*/
		if (cl->defined)
			error("C%n defined twice",this);
		else {
			if (bn->n_scope == ARG) bn->n_scope = ARGT;
			cl->dcl(bn,tbl);
			if (nest) {
				int l1 = strlen(cl->string);
				int l2 = strlen(nest->string);
				char* s = new char[l1+l2+2];
				strcpy(s,nest->string);
				s[l2] = '_';
				strcpy(s+l2+1,cl->string);
				cl->string = s;
			/*	cl->memtbl->t_name->string = s;*/
			}
		}
		tp = cl;
		Cdcl = odcl;
		return bnn;
	}

	case ENUM:
	{	Pname nx = ktbl->look(string,0);		/* TNAME */
		if (nx == 0) {
			nx = ktbl->look(string,HIDDEN);		/* hidden TNAME */
		}
		Pbase bt = (Pbase)nx->tp;			/* EOBJ */
		Pname bn = bt->b_name;
		Pname bnn = tbl->insert(bn,CLASS);
		Penum en = (Penum)bn->tp;			/* ENUM */
		if (en->defined)
			error("enum%n defined twice",this);
		else {
			if (bn->n_scope == ARG) bn->n_scope = ARGT;
			en->dcl(bn,tbl);
		}
		tp = en;
		Cdcl = odcl;
		return bnn;
	}

	case FCT:
	{	Pfct f = (Pfct)tp;
		Pname class_name;
		Ptable etbl;
		int can_overload;
		int in_class_dcl = (int)cc->not;
		int just_made = 0;

		if (f->f_inline) n_sto = STATIC;

		if (f->argtype) {
			Pname a;
			int oo = const_save;
			const_save = 1;
			for (a=f->argtype; a; a=a->n_list) {
				Pexpr init;
				if (init = a->n_initializer) {
					int i = 0;
					init = init->typ(tbl);
					if (a->tp->check(init->tp,ARG)==0
					|| (i=can_coerce(a->tp,init->tp))) {
						if (1 < i) error("%d possible conversions for defaultA",i); //(#) Clipped at "possible conve".
						if (Ncoerce) {
							Pname cn = init->tp->is_cl_obj(); //(#) Clipped at "init->tp->is_cl_".
							Pclass cl = (Pclass)cn->tp;
							Pref r = new ref(DOT,init,Ncoerce); //(#) Clipped at "(DOT,init,N".
							init = new expr(G_CALL,r,0); //(#) Clipped at "(G_CALL,r,0)".
							init->fct_name = Ncoerce;
							init->tp = a->tp;
						}
						init->simpl();
						init->permanent = 2;
						a->n_initializer = init;
					}
					else {
						error("badIrT%t forA%n",init->tp,a);
						DEL(init);
						a->n_initializer = 0;
					}
				}

			flatten1:
				switch (a->tp->base) {
				case TYPE:
					a->tp = ((Pbase)a->tp)->b_name->tp;
					goto flatten1;
				case CHAR:
				case SHORT:
				/*	error('w',"A ofT%k (becomes int)",a->tp->base);	*/ //(#) Clipped at "a->tp->ba".
					a->tp = int_type;
					break;
				case FLOAT:
				/*	error('w',"A ofT float (becomes double)");	*/ //(#) Clipped at 'double)");	'.
					a->tp = double_type;
					break;
				}
			}
			const_save = oo;
		}

		tp->dcl(tbl); /* must be done before the type check */

		if (n_qualifier) {	/* qualified name: c.f() checked above */
			if (in_class_dcl) {
				error("unXQN%n",this);
				Cdcl = odcl;
				return 0;
			}
			class_name = ((Pbase)n_qualifier->tp)->b_name;
			etbl = ((Pclass)class_name->tp)->memtbl;
		}
		else {
			class_name = cc->not;
			/* beware of local function declarations in member functions */ //(#) Clipped at "member function".
			if (class_name && tbl!=cc->cot->memtbl) {
				class_name = 0;
				in_class_dcl = 0;
			}
			if (n_oper) check_oper(class_name);
			etbl = tbl;
		}

		if (etbl==0 || etbl->base!=TABLE) error('i',"N.dcl: etbl=%d",etbl);

		switch (n_oper) {
		case NEW:
		case DELETE:
			switch (scope) {
			case 0:
			case PUBLIC:
				error("%nMF",this);
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
			if (fct_void) n_scope = PUBLIC;
			can_overload = in_class_dcl;
			break;
		default:
			can_overload = 1;	/* all operators are overloaded */
		}

		switch (scope) {
		case FCT:
		case ARG:
		{	Pname nx = gtbl->insert(this,0);
			n_table = 0;
			n_tbl_list = 0;
			/* no break */
		}
		default:
			nn = etbl->insert(this,0);
			nn->assign();
			n_table = etbl;
			break;
		}

			
		if (Nold) {
			Pfct nf = (Pfct)nn->tp;
/*error('d',"%n: tp%t nf%t",nn,tp,nf);*/
			if (nf->base==ANY || f->base==ANY)
				;
			else if (nf->base == OVERLOAD) {
				Pgen g = (Pgen) nf;
				nn = g->add(this,0);
				string = nn->string;
				if (Nold == 0) {
					if (f->body) {
						if (n_qualifier) {
							error(0,"badAL for overloaded %n::%s()",n_qualifier,g->string); //(#) Clipped at "for overload".
							Cdcl = odcl;
							return 0;
						}
						else if (f->f_inline==0 && n_oper==0) //(#) Clipped at "n_oper==".
							error('w',"overloaded %n defined without being previously declared",nn); //(#) Clipped at "%n de".
					}
					goto thth;
				}
				else {
					if (f->body==0 && friend_in_class==0) error('w',"overloaded%n redeclared",nn); //(#) Clipped at "error".
				}
				
				nf = (Pfct)nn->tp;

				if (f->body && nf->body) {
					error("two definitions of overloaded%n",nn);
					Cdcl = odcl;
					return 0;
				}

				if (f->body) goto bdbd;
				
				goto stst;
			}
			else if (nf->base != FCT) {
				error("%n declared both as%t and asF",this,nf);
				f->body = 0;
			}
			else if (can_overload) {
				if (nf->check(f,OVERLOAD) || vrp_equiv) {
					if (f->body && n_qualifier) {
						error("badAT for%n",nn);
						Cdcl = odcl;
						return 0;
					}
					Pgen g = new gen(string);
					Pname n1 = g->add(nn,in_class_dcl);
					Pname n2 = g->add(this,0);
/*error('d',"n1%n n2%n\n",n1,n2);*/
					nn->tp = (Ptype)g;
					nn->string = g->string;
					nn = n2;
					goto thth;
				}
				
				if (in_class_dcl) {
					error("two declarations of%n",this);
					f->body = 0;
					Cdcl = odcl;
					return 0;
				}
				
				if (nf->body && f->body) {
					error("two definitions of%n",this);
					f->body = 0;
					Cdcl = odcl;
					return 0;
				}
				
				if (f->body) goto bdbd;

				goto stst;
			}
			else if (nf->check(f,0)) {
				switch (n_oper) {
				case CTOR:
				case DTOR:
					f->s_returns = nf->s_returns;
				}
				error("%nT mismatch:%t and%t",this,nf,f);
				f->body = 0;
			}
			else if (nf->body && f->body) {
				error("two definitions of%n",this);
				f->body = 0;
			}
			else if (f->body) {
				Pname a1, a2;
			bdbd: 
				if (f->nargs_known && nf->nargs_known)
				for (a1=f->argtype, a2=nf->argtype; a1; a1=a1->n_list, a2=a2->n_list) { //(#) Clipped at "a1=a1->n_li".
					int i1 = a1->n_initializer || a1->n_evaluated; //(#) Clipped at "a1->n_evaluat".
					int i2 = a2->n_initializer || a2->n_evaluated; //(#) Clipped at "a2->n_evaluat".
					if (i1) {
						if (i2
						&& (	a1->n_evaluated==0
							||a2->n_evaluated==0
							|| a1->n_val!=a2->n_val)
						)
							error("twoIrs for%nA%n",nn,a1); //(#) Clipped at ",nn,".
					}
					else if (i2) {
						a1->n_initializer = a2->n_initializer; //(#) Clipped at "n_initializ".
						a1->n_evaluated = a2->n_evaluated;
						a1->n_val = a2->n_val;
					}
				}
				f->f_virtual = nf->f_virtual;
				f->f_this = nf->f_this;
/*fprintf(stderr,"bdbd %s: f %d inl %d nf %d inl %d\n",string,f,f->f_inline,nf,nf->f_inline);*/ //(#) Clipped at "nf,nf->f".
				nn->tp = f;
				if (f->f_inline) {
					if (nf->f_inline==0 && nn->n_used) error("%n called before defined as inline",nn); //(#) Clipped at 'error("%'.
					nf->f_inline = 1;
					nn->n_sto = STATIC;
				}
				else if (nf->f_inline) {
					/*error("%n defined as inline but not declared as inline",this);*/ //(#) Clipped at "but not decla".
					f->f_inline = 1;
				}
				goto stst2;
			}
			else {	/* two declarations */
				Pname a1, a2;
				f->f_this = nf->f_this;
			stst:
				if (f->nargs_known && nf->nargs_known)
				for (a1=f->argtype, a2=nf->argtype; a1; a1=a1->n_list, a2=a2->n_list) { //(#) Clipped at "a1=a1->n_li".
					int i1 = a1->n_initializer || a1->n_evaluated; //(#) Clipped at "a1->n_evaluat".
					int i2 = a2->n_initializer || a2->n_evaluated; //(#) Clipped at "a2->n_evaluat".
					if (i1) {
						if (i2) {
							if (a1->n_evaluated==0
							|| a2->n_evaluated==0
							|| a1->n_val!=a2->n_val)
								error("twoIrs for%nA%n",nn,a1); //(#) Clipped at "twoIrs for%n".
						}
						else if (class_name)
							error("defaultA for%n",nn);
					}
					else if (i2) {
						a1->n_initializer = a2->n_initializer; //(#) Clipped at "a2->n_initializ".
						a1->n_evaluated = a2->n_evaluated;
						a1->n_val = a2->n_val;
					}
				}
			stst2:
				if (f->f_inline) n_sto = STATIC;
				if (n_sto) {
					if (nn->n_scope!=n_sto && f->f_inline==0)
						error("%n both%k and%k",this,n_sto,nn->n_scope); //(#) Clipped at "this,n_sto,".
				}
				else {
					if (nn->n_scope==STATIC && n_scope==EXTERN) error("%n both static and extern",this); //(#) Clipped at "==EXTERN)". Not verified.
				}
				n_scope = nn->n_scope; /* first specifier wins */
			/*	n_sto = nn->n_sto;*/
			
			}
		/*	((Pfct)nn->tp)->nargs_known = nf->nargs_known;	*/
		}
		else {	/* new function: make f_this for member functions */
		thth:
			just_made = 1;
			if (f->f_inline) nn->n_sto = STATIC;
/*fprintf(stderr,"thth %s: f %d nn->tp %d inl %d\n",string,f,nn->tp,f->f_inline);*/
			if (class_name && etbl!=gtbl) {	/* beware of implicit declaration */ //(#) Clipped at "implicit decla".
				Pname cn = nn->n_table->t_name;
				Pname tt = new name("this");
				tt->n_scope = ARG;
				tt->n_sto = REGISTER;
				tt->tp = ((Pclass)class_name->tp)->this_type;
				PERM(tt);
				((Pfct)nn->tp)->f_this = f->f_this = tt;
				tt->n_list = f->argtype;
			}

			if (f->f_virtual) {
				switch (nn->n_scope) {
				default:
					error("nonC virtual%n",this);
					break;
				case 0:
				case PUBLIC:
					cc->cot->virt_count = 1;
					((Pfct)nn->tp)->f_virtual = 1;
					break;
				}
			}
		}

		/*	an operator must take at least one class object or
			reference to class object argument
		*/
		switch (n_oper) {
		case CTOR:
			if (f->nargs == 1) {	/* check for X(X) and X(X&) */
				Ptype t = f->argtype->tp;
			clll:
				switch (t->base) {
				case TYPE:
					t = ((Pbase)t)->b_name->tp;
					goto clll;
				case RPTR:			/* X(X&) ? */
					t = ((Pptr)t)->typ;
				cxll:
					switch (t->base) {
					case TYPE:
						t = ((Pbase)t)->b_name->tp;
						goto cxll;
					case COBJ:
						if (class_name == ((Pbase)t)->b_name) //(#) Clipped at "->b_nam".
							((Pclass)class_name->tp)->itor = nn; //(#) Clipped at "->i".
					}
					break;
				case COBJ:			/* X(X) ? */
					if (class_name == ((Pbase)t)->b_name)
						error("impossible constructor: %s(%s)",class_name->string,class_name->string); //(#) Clipped at "%s(%".
				}
			}
			break;
		case TYPE:
/*error('d',"just_made %d %n",just_made,this);*/
			if (just_made) {
				nn->n_list = ((Pclass)class_name->tp)->conv;
				((Pclass)class_name->tp)->conv = nn;
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
				error("ATs must be fully specified for%n",nn);
			}
			else if (class_name == 0) {
				Pname a;
				switch (f->nargs) {
				case 1:
				case 2:
					for (a=f->argtype; a; a=a->n_list) {
						Ptype tx = a->tp;
						if (tx->base == RPTR) tx = ((Pptr)tx)->typ; //(#) Clipped at "((Pptr)t".
						if (tx->is_cl_obj()) goto cok;
					}
					error("%n must take at least oneCTA",nn);
					break;
				default:
					error("%n must take 1 or 2As",nn);
				}
			}
			else {
				switch (f->nargs) {
				case 0:
				case 1:
					break;
				default:
					error("%n must take 0 or 1As",nn);
				}
			}
		cok:;
		}

		/*
			the body cannot be checked until the name
			has been checked and entered into its table
		*/
		if (f->body) f->dcl(nn);
		break;
	}

	case FIELD:
	{	Pbase fld = (Pbase)tp;
		char x;

		if (cc->not==0 || cc->cot->csu==UNION) {
			if (cc->not)
				error("field in union");
			else
				error("field not inC");
			PERM(tp);
			Cdcl = odcl;
			return this;
		}

		if (string) {
			nn = tbl->insert(this,0);
			n_table = nn->n_table;
			if (Nold) error("twoDs of field%n",this);
		}

		tp->dcl(tbl);
		if (fld->b_bits == 0) {	/* force word alignment */
			int b;
			if (bit_offset)
				fld->b_bits = BI_IN_WORD - bit_offset;
			else if (b = byte_offset%SZ_WORD)
				fld->b_bits = b * BI_IN_BYTE;
		}
		x = bit_offset += fld->b_bits;
		if (BI_IN_WORD < x) {
			fld->b_offset = 0;
			byte_offset += SZ_WORD;
			bit_offset = fld->b_bits;
		}
		else {
			fld->b_offset = bit_offset;
			if (BI_IN_WORD == x) {
				bit_offset = 0;
				byte_offset += SZ_WORD;
			}
			else
				bit_offset = x;
		}
		n_offset = byte_offset;
		break;
	}

	case COBJ:
	{	Pclass cl = (Pclass) ((Pbase)tp)->b_name->tp;
/*fprintf(stderr,"COBJ %d %s -> (%d %d)\n",tp,((Pbase)tp)->b_name->string,cl,cl->base); fflush(stderr);*/ //(#) Clipped at "cl->bas".
		if (cl->csu == ANON) {	/* export member names to enclosing scope */ //(#) Clipped at "scope *".
			Pname nn;
			int i;
			int uindex;
			Ptable mtbl = cl->memtbl;
			char* p = cl->string;

			if (tbl == gtbl) error('s',"global anonymous union");
			while (*p++ != 'C');	/* UGH!!! */
			uindex = str_to_int(p);
			for ( nn=mtbl->get_mem(i=1); nn; nn=mtbl->get_mem(++i) ) {
				Ptable tb = nn->n_table;
				nn->n_table = 0;
				Pname n = tbl->insert(nn,0);
				n->n_union = uindex;
				nn->n_table = tb;
			}
		}
		goto cde;
	}

	case VEC:
	case PTR:
	case RPTR:
		tp->dcl(tbl);

	default:
	cde:
		nn = tbl->insert(this,0);

		n_table = nn->n_table;
/*error('d',"Nold %d tbl %d nn %d%n tp%t",Nold,tbl,nn,nn,nn->tp);*/
		if (Nold) {
			if (nn->tp->base == ANY) goto zzz;
			if (tp->check(nn->tp,0)) {
				error("twoDs of%n;Ts:%t and%t",this,nn->tp,tp);
				Cdcl = odcl;
				return 0;
			}

			if (n_sto && n_sto!=nn->n_scope) 
				error("%n both%k and%k",this,n_sto,nn->n_scope);
			else if (nn->n_scope==STATIC && n_scope==EXTERN)
				error("%n both%k and%k",this,n_sto,nn->n_scope);
			else if (nn->n_scope == STATIC) 
				error("static%n declared twice",this);

		/*	n_sto = nn->n_sto;	 first scope specifier wins */
			n_scope = nn->n_scope;

			switch (scope) {
			case FCT:
				if (nn->n_stclass==STATIC && n_stclass==STATIC) break; //(#) Clipped at "bre". Not verified.
				error("twoDs of%n",this);
				Cdcl = odcl;
				return 0;
			case ARG:
				error("two arguments%n",this);
				Cdcl = odcl;
				return 0;
			case 0:
			case PUBLIC:
				error("twoDs ofM%n",this);
				Cdcl = odcl;
				return 0;
			}
/* n_val */
			if (n_initializer) {
				if (nn->n_initializer) error("twoIrs for%n",this);
				nn->n_initializer = n_initializer;
			}
		}
	
	zzz:
		if (base != TNAME) {
			Ptype t = nn->tp;
/*fprintf(stderr,"tp %d %d nn->tp %d %d\n",tp,tp->base,nn->tp,nn->tp?nn->tp->base:0); fflush(stderr);*/ //(#) Clipped at "nn->tp->base;0)".
			switch (nn->n_stclass) {
			default:
				switch (t->base) {
				case FCT:
				case OVERLOAD:
					break;
				default:
				{	int x = t->align();
					int y = t->tsizeof();

					if (max_align < x) max_align = x;

					while (0 < bit_offset) {
						byte_offset++;
						bit_offset -= BI_IN_BYTE;
					}
					bit_offset = 0;

					if (byte_offset && 1<x) byte_offset = ((byte_offset-1)/x)*x+x; //(#) Clipped at "= ((byt".
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
					t->tsizeof();	/* check that size is known */ //(#) Clipped at "is known".
				}
				break;
			}
		}

	{	Ptype t = nn->tp;
		int const_old = const_save;
		bit vec_seen = 0;
		Pexpr init = n_initializer;

		if (init) {
			switch (n_scope) {
			case 0:
			case PUBLIC:
				if (n_stclass!=STATIC) error("Ir forM%n",this);
				break;
			}
		}

	/*	if (n_scope == EXTERN) break;		*/

	lll:
		switch (t->base) {
		case RPTR:
/*fprintf(stderr,"RPTR init=%d\n",init);*/
			if (init) {
				init = init->typ(tbl);
				nn->n_initializer = n_initializer = ref_init((Pptr)t,init,tbl); //(#) Clipped at "ref_init((Pptr)".
				nn->assign();
			}
			else {
				switch (nn->n_scope) {
				default:
					error("unId reference%n",this);
					break;
				case ARG:
				case PUBLIC:
				case 0:
					break;
				}
			}
			break;
		case COBJ:
/*fprintf(stderr,"COBJ %s init=%d scope %d n_scope %d\n",string,init,scope,nn->n_scope);*/ //(#) Clipped at "nn->n_sco". Not verified.
					/*	TEMPORARY fudge
						to allow initialization of
						global objects
					*/
			if (init && st_init==0)
				switch (nn->n_scope) {
				case EXTERN:
				case STATIC:
					if (init->base == ILIST) goto str;
				}
		{	Pname cn = ((Pbase)t)->b_name;
			Pclass cl = (Pclass)cn->tp;
			Pname ctor = cl->has_ctor();
			Pname dtor = cl->has_dtor();
			if (dtor) {
				Pstmt dls;
				switch ( nn->n_scope ) {
				case EXTERN:
					if (n_sto==EXTERN) break;
				case STATIC:
					if (st_init==0) {
						if (ctor==0) error('s',"staticO %n",this); //(#) Clipped at "staticO %n". Not verified.
						break;
					}
					if (vec_seen) {	/*  _vec_delete(vec,noe,sz,dtor,0); */ //(#) Clipped at "noe,sz,".
						int esz = cl->tsizeof();
						Pexpr noe = new expr(IVAL, (Pexpr)(nn->tp->tsizeof()/esz),0); //(#) Clipped at "IVAL, (Pexpr)(".
						Pexpr sz = new expr(IVAL,(Pexpr)esz,0); //(#) Clipped at "(Pexpr)esz".
						Pexpr arg = new expr(ELIST,dtor,0);
						dtor->lval(ADDROF);
						arg = new expr(ELIST,sz,arg);
						arg = new expr(ELIST,noe,arg);
						arg = new expr(ELIST,nn,arg);
						arg = new call(vec_del_fct,arg);
						arg->base = G_CALL;
						arg->fct_name = vec_del_fct;
						dls = new estmt(SM,nn->where,arg,0);
					}
					else {	/* nn->cl.~cl(0); */
						Pref r = new ref(DOT,nn,dtor);
						Pexpr ee = new expr(ELIST,zero,0);
						Pcall dl = new call(r,ee);
						dls = new estmt(SM,nn->where,dl,0);
						dl->base = G_CALL;
						dl->fct_name = dtor;
					}
					if (st_dlist) dls->s_list = st_dlist;
					st_dlist = dls;
				}
			}
			if (ctor)	{
				Pexpr oo = (vec_seen) ? nn->contents() : nn;
/*error('d',"ctor init=%d n_scope=%d",init,nn->n_scope);*/
				switch (nn->n_scope) {
				case EXTERN:
					if (init==0 && n_sto==EXTERN) goto ggg;
				case STATIC:
					if (st_init==0) {
						error('s',"staticO%n ofC%n that has a constructor",this,nn); //(#) Clipped at "that has". Not verified.
						nn->n_initializer = n_initializer = 0; //(#) Clipped at "= n_initializer =". Not verified.
						goto ggg;
					}
				default:
					if (vec_seen && init) error("Ir forCO%n\[\]",this); //(#) Clipped at "%n\[\]".
					break;
				case ARG:
					if (init == 0) goto ggg;
				case PUBLIC:
				case 0:
					init = new texpr(VALUE,cl,0);
					init->e2 = oo;
					nn->n_initializer = n_initializer = init = 0; //(#) Clipped at "n_initializer = inint = ". Not verified.
					goto ggg;
				}
				const_save = 1;
				nn->assign();
				if (init) {
					if (init->base==VALUE && init->tp2==cl) {
						init->e2 = oo;
						init = init->typ(tbl);
					}
					else {
						init = init->typ(tbl);
						init = class_init(nn,nn->tp,init,tbl); //(#) Clipped at "nn->tp,init,tb".
					}
				}
				else {
					init = new texpr(VALUE,cl,0);
					init->e2 = oo;
					init = init->typ(tbl);
				}
				if (init && st_init) {
					switch (nn->n_scope) {
					case EXTERN:
					case STATIC:
						if (vec_seen) {	/*  _vec_new(vec,noe,sz,ctor); */ //(#) Clipped at "(vec,no".
							Pname c = cl->has_ictor();
							if (c == 0) error("vector ofC%n that do not have a constructor taking noAs",cn); //(#) Clipped at "vector o". Not verified.
							int esz = cl->tsizeof();
							Pexpr noe = new expr(IVAL,(Pexpr)(nn->tp->tsizeof()/esz),0); //(#) Clipped at "(IVAL,(".
							Pexpr sz = new expr(IVAL,(Pexpr)esz,0); //(#) Clipped at "(IVAL,(P".
							Pexpr arg = new expr(ELIST,c,0); //(#) Clipped at "(ELIST,".
							c->lval(ADDROF);
							arg = new expr(ELIST,sz,arg); //(#) Clipped at "sz,arg".
							arg = new expr(ELIST,noe,arg); //(#) Clipped at "noe,ar".
							arg = new expr(ELIST,nn,arg); //(#) Clipped at "nn,arg".
							init = new call(vec_new_fct,arg); //(#) Clipped at "vec_new_fct".
							init->base = G_CALL;
							init->fct_name = vec_new_fct; //(#) Clipped at "vec_new_fc".
						}
					{	Pstmt ist = new estmt(SM,nn->where,init,0); //(#) Clipped at "nn->where,".
						static Pstmt itail = 0;
						if (st_ilist == 0)
							st_ilist = ist;
						else
							itail->s_list = ist;
						itail = ist;
						init = 0;
					}
					}
				}
				nn->n_initializer = n_initializer = init;
				const_save = const_old;
			}
			else if (init == 0)		/* no initializer */
				goto str;
			else if (cl->is_simple())	/* struct */
				goto str;
			else {				/* bitwise copy ok? */
				init = init->typ(tbl);
				if ( nn->tp->check(init->tp,ASSIGN)==0 )
					goto str;
				else
					error("cannotI%n:C %s has privateMs but no constructor",nn,cl->string); //(#) Clipped at "but no ".
			}
			break;
		}
		case VEC:	
			t = ((Pvec)t)->typ;
			vec_seen = 1;
			goto lll;
		case TYPE:
			t = ((Pbase)t)->b_name->tp;
			goto lll;
		default:
		str:
			if (init == 0) {
				switch (n_scope) {
				case ARG:
				case 0:
				case PUBLIC:
					break;
				default:
					if (n_sto!=EXTERN && t->tconst())
						error('w',"unId const%n",this);
				}

				break;
			}

			const_save = const_save || n_scope==ARG || (t->tconst() && vec_const==0); //(#) Clipped at "(t->tconst() && ".
			nn->n_initializer = n_initializer = init = init->typ(tbl);
			if (const_save) PERM(init);
			nn->assign();
			const_save = const_old;

			switch (init->base) {
			case ILIST:
				new_list(init);
				list_check(nn,nn->tp,0);
				if (next_elem()) error("IrL too long");
				break;
			case STRING:
				if (nn->tp->base == VEC) {
					Pvec v = (Pvec)nn->tp;
					if (v->typ->base == CHAR) {
					/*	error('w',"\"char[] = string\"");*/
						v->size = Pvec(init->tp)->size;
						break;
					}
				}
			default:
			{	Ptype nt = nn->tp;

				if (vec_seen) {
					error("badIr for vector%n",nn);
					break;
				}
			tlx:
				switch (nt->base) {
				case TYPE:
					nt = ((Pbase)nt)->b_name->tp;
					goto tlx;
				case INT:
				case CHAR:
				case SHORT:
					if (init->base==ICON && init->tp==long_type) //(#) Clipped at "==long_type".
						error('w',"longIr constant for%k%n",nn->tp->base,nn); //(#) Clipped at '"longIr constant for%k%n"'.
				case LONG:
					if (((Pbase)nt)->b_unsigned
					&& init->base==UMINUS
					&& init->e2->base==ICON)
						error('w',"negativeIr for unsigned%n",nn); //(#) Clipped at "unsigned%".
					if ( ((Pbase)nt)->b_const ) {
						int i;
						Neval = 0;
						i = init->eval();
						if (Neval == 0) {
							DEL(init);
							nn->n_evaluated = n_evaluated = 1; //(#) Clipped at "= n_evaluat".
							nn->n_val = n_val = i;
							nn->n_initializer = n_initializer = 0; //(#) Clipped at "= n_initi".
						/*	if (i) {
								nn->n_initializer = 0; //(#) Clipped at "n_initializer =". Not verified.
								nn->n_val = i;
								n_initializer = 0;
								n_val = i;
							}
							else {
								nn->n_initializer = 0; //(#) Clipped at "n_initializer =". Not verified.
								n_initializer = zero; //(#) Clipped at "= zer". Not verified.
							}
						*/
						}
					}
					goto cvcv;
		case PTR:
		{	Pfct ef = (Pfct)((Pptr)nt)->typ;
			if (ef->base == FCT) {
				Pfct f;
				Pname n = 0;
				switch (init->base) {
				case NAME:
					f = (Pfct)init->tp;
					n = Pname(init);
					switch (f->base) {
					case FCT:
					case OVERLOAD:
						init = new expr(G_ADDROF,0,init);
						init->tp = f;
					}
					goto ad;
				case DOT:
				case REF:
					f = (Pfct) init->mem->tp;
					switch (f->base) {
					case FCT:
					case OVERLOAD:
						n = Pname(init->mem);
						init = new expr(G_ADDROF,0,init);
						init = init->typ(tbl);
					}
					goto ad;
				case ADDROF:
				case G_ADDROF:
					f = (Pfct)init->e2->tp;
				ad:
					if (f->base == OVERLOAD) {
						Pgen g = (Pgen)f;
						n = g->find(ef);
						if (n == 0) {
							error("cannot deduceT for &overloaded %s()",g->string); //(#) Clipped at ""deduceT for &".
						}
						init->e2 = n;
						n_initializer = init;
						n->lval(ADDROF);
						goto stgg;
					}
					if (n) n->lval(ADDROF);
				}
			}
		}
				} 
	cvcv:
		{	Pname cn;
			int i;
			if ((cn=init->tp->is_cl_obj())
			&& (i=can_coerce(nt,init->tp))
			&& Ncoerce) {
				if (1 < i) error("%d possible conversions forIr");
/*error('d',"dcl %t<-%t",nt,init->tp);*/
				Pclass cl = (Pclass)cn->tp;
				Pref r = new ref(DOT,init,Ncoerce);
				Pexpr c = new expr(G_CALL,r,0);
				c->fct_name = Ncoerce;
				c->tp = nt;
				n_initializer = c;
				goto stgg;
			}
		}
				if (nt->check(init->tp,ASSIGN))
					error("badIrT%t for%n (%tX)",init->tp,this,nn->tp); //(#) Clipped at "init->tp,this,".
			else {
			stgg:
				if (init && n_stclass== STATIC) {
					/* check if non-static variables are used */ //(#) Clipped at "are used *".
					/* INCOMPLETE */
					switch (init->base) {
					case NAME:
						if (init->tp->tconst()==0) error("v%n used inIr for%n",init,nn); //(#) Clipped at 'error("v'. Not verified.
						break;
					case DEREF:
					case DOT:
					case REF:
					case CALL:
					case G_CALL:
						error("%k inIr of static%n",init->base,nn); //(#) Clipped at ",init->b".
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
	switch (n_scope) {
	case FCT:
		nn->n_initializer = n_initializer;
		break;
	default:
	{/*	Pexpr ii = nn->n_initializer;*/
		Ptype t = nn->tp;
	/*	if (ii) PERM(ii);*/
	px:
		PERM(t);
		switch (t->base) {
		case PTR:
		case RPTR:	t = ((Pptr)t)->typ; goto px;
		case VEC:	t = ((Pvec)t)->typ; goto px;
		case TYPE:	t = ((Pbase)t)->b_name->tp; goto px;
		case FCT:	t = ((Pfct)t)->returns; goto px; /* args? */
		} 
	}
	}
	
	Cdcl = odcl;
	return nn;
}
int inline_restr;	/* report use of constructs that the inline expanded cannot
			   handle here
			*/

void fct.dcl(Pname n)
{
	int nmem = TBLSIZE;
	Pname a;
	Pname ll;
	Ptable ftbl;

	Pptr cct = 0;
	int const_old = const_save;

	int bit_old = bit_offset;
	int byte_old = byte_offset;
	int max_old = max_align;
	int stack_old = stack_size;

	if (base != FCT) error('i',"fct.dcl(%d)",base);
	if (body==0 || body->memtbl) error('i',"fct.dcl(body=%d)",body);
	if (n==0 || n->base!=NAME) error('i',"fct.dcl(name=%d %d)",n,(n)?n->base:0);

	body->memtbl = ftbl = new table(nmem+3,n->n_table,0);
	body->own_tbl = 1;

	max_align = AL_FRAME;
	stack_size = byte_offset = SZ_BOTTOM;
	bit_offset = 0;

	cc->stack();
	cc->nof = n;
	cc->ftbl = ftbl;

	switch (n->n_scope) {
	case 0:
	case PUBLIC:
		cc->not = n->n_table->t_name;
		cc->cot = (Pclass)cc->not->tp;
		cc->tot = cc->cot->this_type;
		if (f_this==0 || cc->tot==0) error('i',"fct.dcl(%n): f_this=%d cc->tot=%d",n,f_this,cc->tot); //(#) Clipped at "cc->".
		f_this->n_table = ftbl;		/* fake for inline printout */
		cc->c_this = f_this;
	
	}

	Pname ax;
	for (a=argtype, ll=0; a; a=ax) {
		ax = a->n_list;
		Pname nn = a->dcl(ftbl,ARG);
		nn->n_assigned_to = nn->n_used = nn->n_addr_taken = 0;
		nn->n_list = 0;
		switch (a->tp->base) {
		case CLASS:
		case ENUM:	/* unlink types declared in arg list */
			a->n_list = dcl_list;
			dcl_list = a;
			break;
		default:
			if (ll)
				ll->n_list = nn;
			else {
				argtype = nn;
				if (f_this) f_this->n_list = argtype;
	 		}
			ll = nn;
			delete a;
		}
	}


	/* handle initializer for base class constructor */
	if (n->n_oper == CTOR) {
		Pname bn = cc->cot->clbase;

		if (bn) {
			Pclass bcl = (Pclass)bn->tp;
			Pname bnw = bcl->has_ctor();

			if (bnw) {
				Ptype bnwt = bnw->tp;
				Pfct bnwf = (Pfct) ((bnwt->base==FCT) ? bnwt : ((Pgen)bnwt)->fct_list->f->tp); //(#) Clipped at "? bnwt : ((Pg". Not verified.
				Ptype ty = bnwf->f_this->tp;
				Pexpr v = new texpr(VALUE,bcl,f_init);
				Pexpr th = new texpr(CAST,ty,f_this);
				v->e2 = new expr(DEREF,th,0);
				const_save = 1;
				f_init = v->typ(ftbl);
				const_save = const_old;
			}
			else if (f_init)
				error(0,"unXAL: noBC constructor");
		}
		else if (f_init)
			error( "unXAL: noBC" );
	}
	else if (f_init)
		error(0,"unXAL: not a constructor");

	PERM(returns);
	if (returns->base != VOID) {
		Pname rv = new name("_result");
		rv->tp  = returns;
		ftbl->insert(rv,0);
		delete rv;
	}	

	const_save = f_inline?1:0;
	inline_restr = 0;
	body->dcl(ftbl);
	if( f_inline && inline_restr ) {
		f_inline = 0;
		error( 'w', "\"inline\" ignored, %n contains%s%s%s%s",n,
			(inline_restr & 8) ? " loop" : "",
			(inline_restr & 4) ? " switch" : "",
			(inline_restr & 2) ? " goto" : "",
			(inline_restr & 1) ? " label" : "" );
	}
	const_save = const_old;

	if (f_inline) {
		isf_list = new name_list(n,isf_list);
	}

	defined = 1;

	frame_size = stack_size + SZ_TOP;
	frame_size = ((frame_size-1)/AL_FRAME)*AL_FRAME+AL_FRAME;
	bit_offset = bit_old;
	byte_offset = byte_old;
	max_align = max_old;
	stack_size = stack_old;

	cc->unstack();
}
