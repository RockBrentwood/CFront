// 1985 Feb 08 12:48
// @(#) size.f 1.2 2/8/85 14:33:46 */
/*********************************************************************

	C++ source for cfront, the C++ compiler front-end
	written in the computer science research center of Bell Labs

	Copyright (c) 1984 AT&T Technologies, Inc.
		All rigths Reserved
	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF
		AT&T TECHNOLOGIES, INC.

	If you ignore the above notice the gost of Ma Bell will haunt you forever.

size.c:

	initialize alignment and sizeof "constants"

**********************************************************************/

#include <stdio.h>
#include "lib.h"
#include "size.h"
extern int strcmp(char *, char *);
extern int strcpy(char *, char *);
extern int strlen(char *);

int BI_IN_WORD = 32;
int BI_IN_BYTE = 8;

int SZ_CHAR = 1;
int AL_CHAR = 1;

int SZ_SHORT = 2;
int AL_SHORT = 2;

int SZ_INT = 4;
int AL_INT = 4;

int SZ_LONG = 4;
int AL_LONG = 4;

int SZ_FLOAT = 4;
int AL_FLOAT = 4;

int SZ_DOUBLE = 8;
int AL_DOUBLE = 4;

int SZ_STRUCT = 4; /* minimum struct sze */
int AL_STRUCT = 4;

int SZ_FRAME = 4;
int AL_FRAME = 4;

int SZ_WORD = 4;

int SZ_WPTR = 4;
int AL_WPTR = 4;

int SZ_BPTR = 4;
int AL_BPTR = 4;

                                /*      space at top and bottom of stack frame
                                   (for registers, return ptr, etc.)
                                 */
int SZ_TOP = 0;
int SZ_BOTTOM = 0;

char *LARGEST_INT = "2147483647"; /* 2**31 - 1 */

int arg1;
int arg2;

int get_line(FILE * fp) {
   char s[32];

   if (fscanf(fp, " %s %d %d", s, &arg1, &arg2) == EOF) return 0;

   if (strcmp("char", s) == 0) {
      SZ_CHAR = arg1;
      AL_CHAR = arg2;
      return 1;
   }
   if (strcmp("short", s) == 0) {
      SZ_SHORT = arg1;
      AL_SHORT = arg2;
      return 1;
   }
   if (strcmp("int", s) == 0) {
      SZ_INT = arg1;
      AL_INT = arg2;
      if (fscanf(fp, " %s", s) == EOF) return 0;
      int ll = strlen(s);
      LARGEST_INT = _new((ll + 1)*sizeof *LARGEST_INT);
      strcpy(LARGEST_INT, s);
      return 1;
   }
   if (strcmp("long", s) == 0) {
      SZ_LONG = arg1;
      AL_LONG = arg2;
      return 1;
   }
   if (strcmp("float", s) == 0) {
      SZ_FLOAT = arg1;
      AL_FLOAT = arg2;
      return 1;
   }
   if (strcmp("double", s) == 0) {
      SZ_DOUBLE = arg1;
      AL_DOUBLE = arg2;
      return 1;
   }
   if (strcmp("bit", s) == 0) {
      BI_IN_BYTE = arg1;
      BI_IN_WORD = arg2;
      return 1;
   }
   if (strcmp("struct", s) == 0) {
      SZ_STRUCT = arg1;
      AL_STRUCT = arg2;
      return 1;
   }
   if (strcmp("frame", s) == 0) {
      SZ_FRAME = arg1;
      AL_FRAME = arg2;
      return 1;
   }
   if (strcmp("word", s) == 0) {
      SZ_WORD = arg1;
      return 1;
   }
   if (strcmp("wptr", s) == 0) {
      SZ_WPTR = arg1;
      AL_WPTR = arg2;
      return 1;
   }
   if (strcmp("bptr", s) == 0) {
      SZ_BPTR = arg1;
      AL_BPTR = arg2;
      return 1;
   }
   if (strcmp("top", s) == 0) {
      SZ_TOP = arg1;
      SZ_BOTTOM = arg2;
      return 1;
   }
   return 0;
}

extern int read_align(char *f) {
   FILE *fp = fopen(f, "r");
   if (fp == 0) return 1;
   while (get_line(fp));
   return 0;
}

extern int print_align(char *s) {
   fprintf(stderr, "%s sizes and alignments\n\n", s);

   fprintf(stderr, "	size	align\n");
   fprintf(stderr, "char	%d	%d\n", SZ_CHAR, AL_CHAR);
   fprintf(stderr, "short	%d	%d\n", SZ_SHORT, AL_SHORT);
   fprintf(stderr, "int	%d	%d\n", SZ_INT, AL_INT);
   fprintf(stderr, "long	%d	%d\n", SZ_LONG, AL_LONG);
   fprintf(stderr, "float	%d	%d\n", SZ_FLOAT, AL_FLOAT);
   fprintf(stderr, "double	%d	%d\n", SZ_DOUBLE, AL_DOUBLE);
   fprintf(stderr, "bptr	%d	%d\n", SZ_BPTR, AL_BPTR);
   fprintf(stderr, "wptr	%d	%d\n", SZ_WPTR, AL_WPTR);
   fprintf(stderr, "struct	%d	%d\n", SZ_STRUCT, AL_STRUCT);
   fprintf(stderr, "frame	%d	%d\n", SZ_FRAME, AL_FRAME);
   fprintf(stderr, "large   %s\n\n", LARGEST_INT);

   fprintf(stderr, "%d bits in a byte, %d bits in a word, %d bytes in a word\n", //(#) Clipped at 'word\n"'.
      BI_IN_BYTE, BI_IN_WORD, SZ_WORD);
   return 1;
}
