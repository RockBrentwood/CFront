## 1985 Feb 08 14:05
# %Z% %M% %I% %H% %T%
CC=CC
CFLAGS=+f

all: sscan CCcrt0.o CCmcrt0.o

sscan: sscan.c ldfcn.h
	$(CC) $(CFLAGS) -o sscan sscan.o -lld

CCmcrt0.o: CCmcrt0.s
	cc -c CCmcrt0.s

CCcrt0.o: CCcrt0.s
	cc -c CCcrt0.s

clean:
	rm -f *.i *..c

clobber:	clean
	rm -f sscan *.o
