#ident	"@(#)cfront:src/makefile	1.6"
CC=gcc
## Set $(PATCH) to -DPATCH if using the patch version:
#PATCH=-DPATCH
## Set $(PATCH) to nothing if using nm/munch:
PATCH=
CFLAGS=-std=gnu99 -O
OBJS=alloc.o dcl.o dcl2.o del.o error.o expand.o expr.o expr2.o lalex.o lex.o main.o norm.o norm2.o print.o repr.o simpl.o size.o table.o typ.o typ2.o y.tab.o
LIBS=lib.o
cfront: $(OBJS) $(LIBS)
	$(CC) -o cfront $(OBJS) $(LIBS)

## Dependencies
cfront.h: token.h typedef.h
alloc.c del.c expand.c lalex.c main.c print.c repr.c table.c: cfront.h
dcl.c dcl2.c error.c expr.c expr2.c lex.c norm.c norm2.c simpl.c size.c typ.c typ2.c y.tab.c: cfront.h size.h
lalex.c lex.c: tqueue.h yystype.h
lib.c cfront.h: lib.h

## Compilation
yystype.h y.tab.c: gram.y
	yacc -d gram.y; mv y.tab.h yystype.h

lib.o: lib.c
	$(CC) $(CFLAGS) -c lib.c
alloc.o: alloc.c
	$(CC) $(CFLAGS) -c alloc.c
dcl.o: dcl.c
	$(CC) $(CFLAGS) -c dcl.c
dcl2.o: dcl2.c
	$(CC) $(CFLAGS) -c dcl2.c
del.o: del.c
	$(CC) $(CFLAGS) -c del.c
error.o: error.c
	$(CC) $(CFLAGS) -c error.c
expand.o: expand.c
	$(CC) $(CFLAGS) -c expand.c
expr.o: expr.c
	$(CC) $(CFLAGS) -c expr.c
expr2.o: expr2.c
	$(CC) $(CFLAGS) -c expr2.c
lalex.o: lalex.c
	$(CC) $(CFLAGS) -c lalex.c
lex.o: lex.c
	$(CC) $(CFLAGS) -c lex.c
main.o: main.c
	$(CC) $(PATCH) $(CFLAGS) -c main.c
norm.o: norm.c
	$(CC) $(CFLAGS) -c norm.c
norm2.o: norm2.c
	$(CC) $(CFLAGS) -c norm2.c
print.o: print.c
	$(CC) $(CFLAGS) -c print.c
repr.o: repr.c
	$(CC) $(CFLAGS) -c repr.c
simpl.o: simpl.c
	$(CC) $(CFLAGS) -c simpl.c
size.o: size.c
	$(CC) $(CFLAGS) -c size.c
table.o: table.c
	$(CC) $(CFLAGS) -c table.c
typ.o: typ.c
	$(CC) $(CFLAGS) -c typ.c
typ2.o: typ2.c
	$(CC) $(CFLAGS) -c typ2.c
y.tab.o: y.tab.c
	$(CC) $(CFLAGS) -c y.tab.c

install:
	mv cfront ../cf
test:
	cd scr; ./demo
clean:
	rm -f *.o
clobber: clean
	rm -f cfront
