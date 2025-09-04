## 1985 Feb 08 12:47
# %Z% %M% %I% %H% %T%
#
# cfront makefile
#
CC=CC
CFLAGS=-c
YACC=yacc
YFLAGS=
OSUF=.o
HDRS=cfront.h  \
	size.h		\
	token.h		\
	typedef.h	\
	yystype.h
OBJS=alloc$(OSUF)		\
	dcl$(OSUF)		\
	dcl2$(OSUF)		\
	del$(OSUF)		\
	error$(OSUF)		\
	expand$(OSUF)	\
	expr$(OSUF)		\
	expr2$(OSUF)		\
	lex$(OSUF)		\
	main$(OSUF)		\
	norm$(OSUF)		\
	norm2$(OSUF)		\
	print$(OSUF)		\
	repr$(OSUF)		\
	simpl$(OSUF)		\
	size$(OSUF)		\
	table$(OSUF)		\
	typ$(OSUF)		\
	typ2$(OSUF)		\

all	:	cfront

cfront	:	$(OBJS) y.tab$(OSUF)
		$(CC) $(OBJS) y.tab$(OSUF) -o cfront

y.tab.c	:	gram.y
		$(YACC) $(YFLAGS) gram.y

y.tab$(OSUF)	:	y.tab.c $(HDRS)
		$(CC) $(CFLAGS) +E y.tab.c 

alloc$(OSUF)	:	alloc.c
		$(CC) $(CFLAGS) alloc.c 

dcl$(OSUF)	:	dcl.c
		$(CC) $(CFLAGS) dcl.c 

dcl2$(OSUF)	:	dcl2.c
		$(CC) $(CFLAGS) dcl2.c 

del$(OSUF)	:	del.c
		$(CC) $(CFLAGS) del.c 

error$(OSUF)	:	error.c
		$(CC) $(CFLAGS) error.c 

expand$(OSUF)	:	expand.c
		$(CC) $(CFLAGS) expand.c 

expr$(OSUF)	:	expr.c
		$(CC) $(CFLAGS) expr.c 

expr2$(OSUF)	:	expr2.c
		$(CC) $(CFLAGS) expr2.c 

lex$(OSUF)	:	lex.c
		$(CC) $(CFLAGS) lex.c 

main$(OSUF)	:	main.c
		$(CC) $(CFLAGS) main.c 

norm$(OSUF)	:	norm.c
		$(CC) $(CFLAGS) norm.c 

norm2$(OSUF)	:	norm2.c
		$(CC) $(CFLAGS) norm2.c 

print$(OSUF)	:	print.c
		$(CC) $(CFLAGS) print.c 

repr$(OSUF)	:	repr.c
		$(CC) $(CFLAGS) repr.c 

simpl$(OSUF)	:	simpl.c
		$(CC) $(CFLAGS) simpl.c 

size$(OSUF)	:	size.c
		$(CC) $(CFLAGS) size.c 

table$(OSUF)	:	table.c
		$(CC) $(CFLAGS) table.c 

typ$(OSUF)	:	typ.c
		$(CC) $(CFLAGS) typ.c 

typ2$(OSUF)	:	typ2.c
		$(CC) $(CFLAGS) typ2.c 

$(OBJS)	:	$(HDRS)

clean	:
		rm -f $(OBJS) y.tab.c y.tab$(OSUF) *.i *..c

clobber	:	clean
		rm -f cfront
