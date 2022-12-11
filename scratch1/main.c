/* @(#) main.c 1.8 1/27/86 17:49:11 */
/*ident	"@(#)cfront:src/main.c	1.8" */
/***********************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

main.c:

	Initialize global environment
	Read argument line
	Start compilation
	Clean up end exit

**************************************************************************/

#include <time.h> /* For ctime() and time() */
#include <ctype.h>
#include <stdlib.h>

long start_time, stop_time;

#include "cfront.h"

const char *prog_name = "<<cfront 10/10/85>>";

extern char *src_file_name;
char *src_file_name = 0;

bit Styp = 1;
bit Ssimpl = 1;

bit old_fct_accepted;
bit fct_void;

const char *line_format = "\n# %d \"%s\"\n";

IdListP isf_list;
StP st_ilist;
StP st_dlist;
TableP sti_tbl;
TableP std_tbl;

int Nspy;
int Nfile = 1, Nline, Ntoken;
int Nfree_store, Nalloc, Nfree;
int Nname;
int Nn, Nbt, Nt, Ne, Ns, Nc, Nstr, Nl;
extern int NFn, NFtn, NFbt, NFpv, NFf, NFe, NFs, NFc, NFl;

extern void simpl_init(void);
extern void typ_init(void);
extern void syn_init(void);
extern void lex_init(void);
extern void error_init(void);
extern void print_free(void);
extern int read_align(const char *);
extern int print_align(const char *);

void spy(char *s) {
   if (s) fprintf(stderr, "%s:\n", s);
   fprintf(stderr, "files=%d lines=%d tokens=%d\n", Nfile, Nline, Ntoken);
   fprintf(stderr, "Names: distinct=%d global=%d type=%d\n", Nname, max(gtbl), max(ktbl));
   fflush(stderr);
   if (start_time && stop_time) {
      fprintf(stderr, "start time: %s", ctime(&start_time));
      fprintf(stderr, "stop time:  %s", ctime(&stop_time));
      fprintf(stderr, "real time delay %ld: %d lines per second\n", stop_time - start_time, Nline / (stop_time - start_time));
      fflush(stderr);
   }
   fprintf(stderr, "free store=%dbytes alloc()=%d free()=%d ", Nfree_store, Nalloc, Nfree);
   print_free();
   fflush(stderr);
   fprintf(stderr, "sizeof: n=%d bt=%d f=%d p=%d v=%d e=%d c=%d l=%d\n", sizeof(struct Id), sizeof(struct Base), sizeof(struct Fun), sizeof(struct Ptr), sizeof(struct Vec), sizeof(struct Ex), sizeof(struct ObjEx), sizeof(struct Exs)/* 16 */);
   fprintf(stderr, "alloc(): n=%d bt=%d t=%d e=%d s=%d c=%d str=%d l=%d\n", Nn, Nbt, Nt, Ne, Ns, Nc, Nstr, Nl);
   fprintf(stderr, "free(): n=%d bt=%d t=%d e=%d s=%d c=%d str=? l=%d\n", NFn, NFbt, NFpv + NFf, NFe, NFs, NFc, NFl);
   fflush(stderr);
   fprintf(stderr, "%d errors\n", error_count);
   fflush(stderr);
}

IdP dcl_list; /* declarations generated while declaring something else */

char *st_name(char *); /* generates names of static ctor, dtor callers */

/*
	run the appropriate stages
*/
void run(void) {
   IdP n;
   int i = 1;

   while (n = syn()) {
      IdP nn;
      IdP nx;

      if (n == (IdP) 1) continue;

      if (Styp == 0) {
         dcl_printId(n, SM);
         lex_clear();
         continue;
      }

      for (nn = n; nn; nn = nx) {
         nx = nn->n_list;
         nn->n_list = 0;

         if (dclId(nn, gtbl, EXTERN) == 0) continue;

         if (error_count) continue;

         if (Ssimpl) simplId(nn);

      /* handle generated declarations */
         for (IdP dx, d = dcl_list; d; d = dx) {
            dx = d->n_list;
            dcl_printId(d, 0);
            FreeId(d);
         }
         dcl_list = 0;

         if (nn->base) dcl_printId(nn, 0);

         switch (nn->tp->base) { /* clean up */
            default:
            {
               ExP i = nn->n_initializer;
               if (i && i != (ExP) 1) DEL(Ex, i);
               break;
            }

            case FCT:
            {
               FunP f = (FunP) nn->tp;
               if (f->body && (debug || f->f_inline == 0)) {
                  DEL(Block, f->body);
               /* f->body = 0;  leave to detect re-definition, but do not use it */
               }
               break;
            }

            case CLASS:
            {
               ClassP cl = (ClassP) nn->tp;
               register IdP p;
               for (p = cl->pubmem; p; p = p->n_list) {
                  switch (p->tp->base) {
                     case FCT:
                     {
                        FunP f = (FunP) p->tp;
                        if (f->body && (debug || f->f_inline == 0)) {
                           DEL(Block, f->body);
                           f->body = 0;
                        }
                     }
                     case CLASS:
                     case ENUM:
                        break;
                     case COBJ:
                     case EOBJ:
                        DEL(Id, p);
                        break;
                     default:
                        FreeId(p);
                  }
               }
               cl->pubmem = 0;

               for (p = cl->privmem; p; p = p->n_list) {
                  switch (p->tp->base) {
                     case FCT:
                     {
                        FunP f = (FunP) p->tp;
                        if (f->body && (debug || f->f_inline == 0)) {
                           DEL(Block, f->body);
                           f->body = 0;
                        }
                     }
                     case CLASS:
                     case ENUM:
                        break;
                     case COBJ:
                     case EOBJ:
                        DEL(Id, p);
                        break;
                     default:
                        FreeId(p);
                  }
               }
               cl->privmem = 0;
               cl->permanent = 3;
               break;
            }
         }
         DEL(Id, nn);
      }

      lex_clear();
   }

   switch (no_of_undcl) {
      case 0:
         break;
      case 1:
         errorT('w', "undeclaredF%n called", undcl);
         break;
      default:
         errorT('w', "%d undeclaredFs called; for example%n", no_of_undcl, undcl);
   }

   switch (no_of_badcall) {
      case 0:
         break;
      case 1:
         errorT('w', "%n declared withoutAs called withAs", badcall);
         break;
      default:
         errorT('w', "%d Fs declared withoutAs called withAs; for example%n", no_of_badcall, badcall);
   }

   IdP m;
   if (fct_void == 0)
      for (m = get_mem(gtbl, i = 1); m; m = get_mem(gtbl, ++i)) {
/*errorT('d',"global:%k n_key%k perm %d %n", m->base, m->n_key, m->permanent, m );*/
         if (m->base == TNAME || m->n_sto == EXTERN || m->n_stclass == ENUM) continue;

         TypeP t = m->tp;
         if (t == 0) continue;
       ll:
         switch (t->base) {
            case TYPE:
               t = ToBaseP(t)->b_name->tp;
               goto ll;
            case CLASS:
            case ENUM:
            case COBJ:
            case OVERLOAD:
            case VEC:
               continue;
            case FCT:
               if (ToFunP(t)->f_inline || ToFunP(t)->body == 0) continue;
         }

         if (m->n_addr_taken == 0 && m->n_used == 0 && tconst(m->tp) == 0)
            if (m->n_sto == STATIC)
               errorTL('w', &m->where, "%n defined but not used", m);
      //      else
      //              errorTL('w',&m->where,"%n defined but not used",m);
      }

   const char *ctor_name = 0, *dtor_name = 0;

   if (st_ilist) { /*      make an "init" function;
                      it calls all constructors for static objects
                    */
      IdP n = MakeId(st_name("_STI"));
      FunP f = MakeFun((TypeP)void_type, 0, 1);
      n->tp = (TypeP)f;
      f->body = MakeBlock(st_ilist->where, 0, 0);
      f->body->s = st_ilist;
      f->body->memtbl = sti_tbl;
      n->n_sto = EXTERN;
      (void)dclId(n, gtbl, EXTERN);
      simplId(n);
      dcl_printId(n, 0);
      ctor_name = n->string;
   }

   if (st_dlist) { /*      make a "done" function;
                      it calls all destructors for static objects
                    */
      IdP n = MakeId(st_name("_STD"));
      FunP f = MakeFun((TypeP)void_type, 0, 1);
      n->tp = (TypeP)f;
      f->body = MakeBlock(st_dlist->where, 0, 0);
      f->body->s = st_dlist;
      f->body->memtbl = std_tbl;
      n->n_sto = EXTERN;
      (void)dclId(n, gtbl, EXTERN);
      simplId(n);
      dcl_printId(n, 0);
      dtor_name = n->string;
   }

#ifdef PATCH
/*For fast load: make a static "__link" */
   if (ctor_name || dtor_name) {
      printf("static struct __link { struct __link * next;\n");
      printf("int (*ctor)(); int (*dtor)(); } __LINK = \n");
      printf("{ (struct __link *)0, %s, %s };\n", ctor_name ? ctor_name : "0", dtor_name ? dtor_name : "0");
   }
#endif

   if (debug == 0) { /* print inline function definitions */
      IdListP l;
      for (l = isf_list; l; l = l->l) {
         IdP n = l->f;
         FunP f = (FunP) n->tp;

         switch (f->base) {
            default:
               errorT('i', "inline list corrupted\n");
            case FCT:
               break;
            case OVERLOAD:
               n = ToGenP(f)->fct_list->f; /* first Fun */
               f = (FunP) n->tp;
         }

/*fprintf(stderr,"%s() tp (%d %d) %d %d\n", n->string, n->tp, n->tp?n->tp->base:0, n->n_addr_taken, f->f_virtual); fflush(stderr);*/
         if (n->n_addr_taken || f->f_virtual) {
            outlined = ToFunP(n->tp)->s_returns ? ToFunP(n->tp)->s_returns : ToFunP(n->tp)->returns;
            dcl_printType(n->tp, n);
            outlined = 0;
         }
      }
   }

   fprintf(out_file, "\n/* the end */\n");

}

TypeP outlined; // fudge to get outlined inlines printed correctly
bit warn = 1; /* printout warning messages */
bit debug; /* code generation for debugger */
const char *afile = "default";

int no_of_undcl, no_of_badcall;
IdP undcl, badcall;

/*
	read options, initialize, and run
*/
int main(int argc, char *argv[]) {
   Enter();
   extern char *mktemp();
   register char *cp;
   short i;

   out_file = stdout; /* Moved from lex.c */
   in_file = stdin; /* Moved from lex.c */
/*(void) signal(SIGINT,&sig_exit);
   (void) signal(SIGTERM,sig_exit);
   (void) signal(SIGQUIT,sig_exit);
 */

   error_init();

   for (i = 1; i < argc; ++i) {
      switch (*(cp = argv[i])) {
         case '+':
            while (*++cp) {
               switch (*cp) {
                  case 't':
                     fprintf(stderr, "type check only\n");
                     Ssimpl = 0;
                     break;
                  case 's':
                     fprintf(stderr, "syntax check only\n");
                     Styp = Ssimpl = 0;
                     break;
                  case 'w':
                     warn = 0;
                     break;
                  case 'd':
                     debug = 1;
                     break;
                  case 'f':
                     src_file_name = cp + 1;
                     goto xx;
                  case 'x': /* read in table for cross compilation */
                     if (read_align(afile = cp + 1)) {
                        fprintf(stderr, "bad size-table (option +x)");
                        Exit(11);
                     }
                     goto xx;
                  case 'C': /* preserve comments */
                     errorT('s', "cannot preserve comments");
                     break;
                  case 'V': // C compatability
                     fct_void = old_fct_accepted = 1;
                     break;
                  case 'S':
                     Nspy++;
                     break;
                  case 'L':
                     line_format = "\n#line %d \"%s\"\n";
                     break;
                  default:
                     fprintf(stderr, "%s: unexpected option: +%c ignored\n", prog_name, *cp);

                     break;
               }
            }
          xx:
            break;
         default:
            fprintf(stderr, "%s: bad argument \"%s\"\n", prog_name, cp);
            Exit(11);
      }
   }

   fprintf(out_file, line_format + 1, 1, src_file_name); // strips leading \n
   fprintf(out_file, "\n/* %s */\n", prog_name);
   if (src_file_name) fprintf(out_file, "/* < %s */\n", src_file_name);

   if (Nspy) {
      start_time = time(0);
      print_align(afile);
   }
   fflush(stderr);
   if (Ssimpl) print_mode = SIMPL;
   otbl_init();
   lex_init();
   syn_init();
   typ_init();
   simpl_init();
   scan_started = 1;
   putline(&curloc);
   run();
   if (Nspy) {
      stop_time = time(0);
      spy(src_file_name);
   }

   Exit((0 <= error_count && error_count < 127) ? error_count : 127);
}

/*
	make name "pref|source_file_name|_" or "pref|source_file_name|_"
	where non alphanumeric characters are replaced with '_'
*/
char *st_name(char *pref) {
   int prefl = strlen(pref);
   int strl = prefl + 2;
   if (src_file_name) strl += strlen(src_file_name);
   char *name = _new(strl*sizeof *name);
   strcpy(name, pref);
   if (src_file_name) strcpy(name + prefl, src_file_name);
   name[strl - 2] = '_';
   name[strl - 1] = 0;
   char *p = name;
      while (*++p) if (!isalpha(*p) && !isdigit(*p)) *p = '_';
   return name;
}
