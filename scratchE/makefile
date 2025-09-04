## 1985 Feb 08 12:47
# %Z% %M% %I% %H% %T%
#
# cfront makefile
#
CC=gcc
CFLAGS=-c -std=gnu99
YACC=yacc -d
YFLAGS=
O=.o
OBJS=alloc$O dcl$O dcl2$O del$O error$O expand$O expr$O expr2$O lex$O main$O norm$O norm2$O print$O repr$O simpl$O size$O table$O typ$O typ2$O y.tab$O
LIBS=lib$O

all: cfront
cfront: $(OBJS) $(LIBS)
	$(CC) $(OBJS) $(LIBS) -o cfront

## Dependencies.
cfront.h: token.h typedef.h
lex.c: y.tab.h
dcl2.c dcl.c del.c expr2.c expr.c gram.y lex.c norm2.c norm.c simpl.c typ2.c typ.c: cfront.h size.h
alloc.c error.c expand.c main.c print.c repr.c table.c: cfront.h
size.c: size.h
y.tab.h y.tab.c: gram.y
	$(YACC) $(YFLAGS) gram.y
size.c lib.c cfront.h:  lib.h

y.tab$O: y.tab.c
#	$(CC) $(CFLAGS) +E y.tab.c ## The +E was a CC option, telling it to use C's namespace.
	$(CC) $(CFLAGS) y.tab.c

lib$O: lib.c
	$(CC) $(CFLAGS) lib.c

alloc$O: alloc.c
	$(CC) $(CFLAGS) alloc.c
dcl$O: dcl.c
	$(CC) $(CFLAGS) dcl.c
dcl2$O: dcl2.c
	$(CC) $(CFLAGS) dcl2.c
del$O: del.c
	$(CC) $(CFLAGS) del.c
error$O: error.c
	$(CC) $(CFLAGS) error.c
expand$O: expand.c
	$(CC) $(CFLAGS) expand.c
expr$O: expr.c
	$(CC) $(CFLAGS) expr.c
expr2$O: expr2.c
	$(CC) $(CFLAGS) expr2.c
lex$O: lex.c
	$(CC) $(CFLAGS) lex.c
main$O: main.c
	$(CC) $(CFLAGS) main.c
norm$O: norm.c
	$(CC) $(CFLAGS) norm.c
norm2$O: norm2.c
	$(CC) $(CFLAGS) norm2.c
print$O: print.c
	$(CC) $(CFLAGS) print.c
repr$O: repr.c
	$(CC) $(CFLAGS) repr.c
simpl$O: simpl.c
	$(CC) $(CFLAGS) simpl.c
size$O: size.c
	$(CC) $(CFLAGS) size.c
table$O: table.c
	$(CC) $(CFLAGS) table.c
typ$O: typ.c
	$(CC) $(CFLAGS) typ.c
typ2$O: typ2.c
	$(CC) $(CFLAGS) typ2.c

clean:
	rm -f $(OBJS) $(LIBS) y.tab.* *.i *..c
clobber: clean
	rm -f cfront
