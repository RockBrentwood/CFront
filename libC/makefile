## 1985 Feb 08 13:31
# %Z% %M% %I% %H% %T%
#
#	Lib master makefile.
#
CC=CC
OSUF=.o

all:
	cd complex ; make CC=$(CC) OSUF=$(OSUF)
	cd stream ; make CC=$(CC) OSUF=$(OSUF)
	cd task ; make CC=$(CC) OSUF=$(OSUF)
	cd new ; make CC=$(CC) OSUF=$(OSUF)
	ar cr libC.a */*$(OSUF)

clean:
	cd complex ; make clean
	cd stream ; make clean
	cd task ; make clean
	cd new ; make clean

clobber:	clean
	rm -f libC.a
