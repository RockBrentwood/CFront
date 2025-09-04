/* @(#) error.c 1.3 1/27/86 17:48:46 */
/*ident	"@(#)cfront:src/error.c	1.3" */
/**************************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T, Inc. All Rights Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T, INC.

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

static const char *abbrev_tbl['Z' + 1];

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
   if (error_count == 0) error_count = 1;
   exit(error_count);
}

static void print_loc(void) {
   LocP sl = (Cstmt) ? &Cstmt->where : 0;
   LocP dl = (Cdcl) ? &Cdcl->where : 0;

   if (sl && dl && sl->file == dl->file) // Cstmt and Cdcl in same file
      if (sl->line <= dl->line)
         putLoc(dl, out_file);
      else
         putLoc(sl, out_file);
   else if (sl && sl->file == curr_file) // Cstmt in current file
      putLoc(sl, out_file);
   else if (dl && dl->file == curr_file) // Cdcl in current file
      putLoc(dl, out_file);
   else
      putLoc(&curloc, out_file);
}

static void print_context(void) {
   putc('\n', out_file);
}

static char in_error = 0;

void yyerror(const char *s) {
   error(s);
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
static int verror(int t, LocP lc, const char *s, va_list a) {
   FILE *of = out_file;
   int c;

   if (t == 'w' && warn == 0) return 0;

   if (in_error++)
      if (t != 't' || 4 < in_error) {
//       fprintf(stderr, "\nUPS!, error while handling error\n"); //(@) Changed from this
         printf("\n// UPS!, error while handling error\n"); //(@) Changed to this
         ext(13);
      } else if (t == 't')
         t = 'i';

// out_file = stderr; //(@) Changed
   if (!scan_started)
   /*fprintf(out_file,"error during %s initializing: ",prog_name); */
//    putch('\n'); //(@) Changed from this.
      fprintf(out_file, "\n// "); //(@) Changed to this.
   else if (t == 't')
//    putch('\n'); //(@) Changed from this
      fprintf(out_file, "\n// "); //(@) Changed to this.
   else if (lc != NULL)
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
         if (error_count++) {
            fprintf(out_file, "sorry, %s cannot recover from earlier errors\n", prog_name);
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
               fprintf(out_file, "%c", (char)va_arg(a, int));
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

// out_file = of; //(@) Removed.

   switch (t) {
      case 't':
         if (--in_error) return 0;
      case 'i':
         ext(INTERNAL);
      case 0:
      case 's':
         if (MAXERR < ++error_count) {
         // fprintf(stderr, "Sorry, too many errors\n"); //(@) Changed from this.
            printf("// Sorry, too many errors\n"); //(@) Changed to this.
            ext(7);
         }
   }

   in_error = 0;
   return 0;
}

int errorTL(int t, LocP lc, const char *s, ...) {
   va_list a; va_start(a, s); verror(t, lc, s, a); va_end(a);
}

int errorT(int t, const char *s, ...) {
   va_list a; va_start(a, s); verror(t, NULL, s, a); va_end(a);
}

int errorL(LocP lc, const char *s, ...) {
   va_list a; va_start(a, s); verror(0, lc, s, a); va_end(a);
}

int error(const char *s, ...) {
   va_list a; va_start(a, s); verror(0, NULL, s, a); va_end(a);
}
