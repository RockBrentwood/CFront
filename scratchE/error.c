// 1985 Feb 08 12:48
/* %Z% %M% %I% %H% %T% */
/**************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T Technologies, Inc. All rigths Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T TECHNOLOGIES, INC.

	If you ignore this notice the ghost of Ma Bell will haunt you forever.

error.c :

	write error messages

	Until scan_started != 0 no context can be assumed

***************************************************************************/

#include "cfront.h"
#include "size.h"
#include <stdarg.h>
#include <stdlib.h> /* For exit() */

int error_count;
static int no_of_warnings;
char scan_started;

#define ERRTRACE    20

static char *abbrev_tbl['Z' + 1];

extern void error_init(void);
void error_init(void) {
   static char errbuf[BUFSIZ];
   setbuf(stderr, errbuf);

   abbrev_tbl['A'] = " argument";
   abbrev_tbl['B'] = " base";
   abbrev_tbl['C'] = " class";
   abbrev_tbl['D'] = " declaration";
   abbrev_tbl['E'] = " expression";
   abbrev_tbl['F'] = " function";
   abbrev_tbl['I'] = " initialize";
   abbrev_tbl['J'] = " J";
   abbrev_tbl['K'] = " K";
   abbrev_tbl['L'] = " list";
   abbrev_tbl['M'] = " member";
   abbrev_tbl['N'] = " name";
   abbrev_tbl['O'] = " object";
   abbrev_tbl['P'] = " pointer";
   abbrev_tbl['Q'] = " qualifie";
   abbrev_tbl['R'] = " R";
   abbrev_tbl['S'] = " statement";
   abbrev_tbl['T'] = " type";
   abbrev_tbl['U'] = " undefined";
   abbrev_tbl['V'] = " variable";
   abbrev_tbl['W'] = " W";
   abbrev_tbl['X'] = " expected";
   abbrev_tbl['Y'] = " Y";
   abbrev_tbl['Z'] = " Z";

}

#define INTERNAL 127

/*
	remove temp_file and exit
*/
void ext(int n) {
/*	if (n==INTERNAL) abort();*/
   exit(n);
}

static void print_loc(void) {
   LocP sl = (Cstmt) ? &Cstmt->where : 0;
   LocP dl = (Cdcl) ? &Cdcl->where : 0;

   if (sl && dl && sl->file == dl->file) {
      if (sl->line <= dl->line)
         putLoc(dl, out_file);
      else
         putLoc(sl, out_file);
      return;
   }

   if (sl) {
      if (sl->file == curloc.file) {
         putLoc(sl, out_file);
         return;
      }
   }

   if (dl) {
      if (dl->file == curloc.file) {
         putLoc(dl, out_file);
         return;
      }
   }

   putLoc(&curloc, out_file);
}

static void print_context(void) {
   putc('\n', out_file);
}

static char in_error = 0;
static struct Loc dummy_loc;

static int verror(int t, LocP lc, char *s, va_list a);

void yyerror(char *s) {
   errorTL(0, &dummy_loc, s);
}

void error(char *s, ...) {
   va_list a; va_start(a, s);
   verror(0, &dummy_loc, s, a); //(#) Clipped at "a[8".
   va_end(a);
}

void errorT(int t, char *s, ...) {
   va_list a; va_start(a, s);
   verror(t, &dummy_loc, s, a); //(#) Clipped at "a[8".
   va_end(a);
}

void errorL(LocP l, char *s, ...) {
   va_list a; va_start(a, s);
   verror(0, l, s, a);
   va_end(a);
}

void errorTL(int t, LocP lc, char *s, ...) {
   va_list a; va_start(a, s);
   verror(t, lc, s, a);
   va_end(a);
}

/*
	"int" not "void" because of "pch" in lex.c
	subsequent arguments fill in %mumble fields

	legal error types are:
		'w'		warning	 (not counted in error count)
		'd'		debug
		's'		"not implemented" message
    		0		error
    		'i'		internal error (causes abort)
		't'		error while printing error message
*/
static int verror(int t, LocP lc, char *s, va_list a) {
   FILE *of = out_file;
   int c;
   char format[3]; /* used for "% mumble" sequences */

   if (t == 'w' && warn == 0) return 0;

   if (in_error++)
      if (t != 't' || 4 < in_error) {
         fprintf(stderr, "\nUPS!, error while handling error\n");
         ext(13);
      } else if (t == 't')
         t = 'i';

   out_file = stderr;
   if (!scan_started)
   /*fprintf(out_file,"error during %s initializing: ",prog_name); */
      putch('\n');
   else if (t == 't')
      putch('\n');
   else if (lc != &dummy_loc)
      putLoc(lc, out_file);
   else
      print_loc();

   switch (t) {
      case 0:
         fprintf(out_file, "error: ");
         break;
      case 'w':
         no_of_warnings++;
         fprintf(out_file, "warning: ");
         break;
      case 's':
         fprintf(out_file, "sorry, not implemented: ");
         break;
      case 'i':
         if (error_count) {
            fprintf(out_file, "sorry, %s cannot recover from earlier errors\n", prog_name); //(#) Clipped at "from earlier err".
            ext(INTERNAL);
         } else
            fprintf(out_file, "internal %s error: ", prog_name);
         break;
   }

   while (c = *s++) {
      if ('A' <= c && c <= 'Z' && abbrev_tbl['A'])
         putstring(abbrev_tbl[c]);
      else if (c == '%')
         switch (c = *s++) {
            case 'k':
            {
               Token x = (Token)va_arg(a, int);
               if (0 < x && x < MAXTOK && keys[x])
                  fprintf(out_file, " %s", keys[x]);
               else
                  fprintf(out_file, " token(%d)", x);
               break;
            }
            case 't': /* TypeP */
            {
               TypeP tt = va_arg(a, TypeP);
               if (tt) {
                  Token pm = print_mode;
                  extern int ntok;
                  int nt = ntok;
                  print_mode = ERROR;
                  fprintf(out_file, " ");
                  dcl_printType(tt, 0);
                  print_mode = pm;
                  ntok = nt;
               }
               break;
            }
            case 'n': /* IdP */
            {
               IdP nn = va_arg(a, IdP);
               if (nn) {
                  Token pm = print_mode;
                  print_mode = ERROR;
                  fprintf(out_file, " ");
                  printId(nn);
                  print_mode = pm;
               } else
                  fprintf(out_file, " ?");
               break;
            }
            default:
               fprintf(out_file, "%c", va_arg(a, int));
               break;
      } else
         putch(c);
   }

   if (!scan_started) ext(4);

   switch (t) {
      case 'd':
      case 't':
      case 'w':
         putch('\n');
         break;
      default:
         print_context();
   }
   fflush(stderr);
/* now we may want to carry on */

   out_file = of;

   switch (t) {
      case 't':
         if (--in_error) return 0;
      case 'i':
         ext(INTERNAL);
      case 0:
      case 's':
         if (MAXERR < ++error_count) {
            fprintf(stderr, "Sorry, too many errors\n");
            ext(7);
         }
   }

   in_error = 0;
   return 0;
}
