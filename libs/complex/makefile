## 1985 Feb 08 13:37
# %Z% %M% %I% %H% %T%
CC=CC
CFLAGS=-c
OSUF=.o
OBJS=abs$(OSUF) arg$(OSUF) cos$(OSUF) error$(OSUF) exp$(OSUF) io$(OSUF) \
	log$(OSUF) oper$(OSUF) polar$(OSUF) pow$(OSUF) sin$(OSUF) sqrt$(OSUF)
HDRS=../../incl/complex.h const.h
all	:	$(OBJS)

abs$(OSUF)	:	$(HDRS) abs.c
		$(CC) $(CFLAGS) abs.c

arg$(OSUF)	:	$(HDRS) arg.c
		$(CC) $(CFLAGS) arg.c

cos$(OSUF)	:	$(HDRS) cos.c
		$(CC) $(CFLAGS) cos.c

error$(OSUF)	:	$(HDRS) error.c
		$(CC) $(CFLAGS) error.c

exp$(OSUF)	:	$(HDRS) exp.c
		$(CC) $(CFLAGS) exp.c

io$(OSUF)	:	$(HDRS) io.c
		$(CC) $(CFLAGS) io.c

log$(OSUF)	:	$(HDRS) log.c
		$(CC) $(CFLAGS) log.c

oper$(OSUF)	:	$(HDRS) oper.c
		$(CC) $(CFLAGS) oper.c

polar$(OSUF)	:	$(HDRS) polar.c
		$(CC) $(CFLAGS) polar.c

pow$(OSUF)	:	$(HDRS) pow.c
		$(CC) $(CFLAGS) pow.c

sin$(OSUF)	:	$(HDRS) sin.c
		$(CC) $(CFLAGS) sin.c

sqrt$(OSUF)	:	$(HDRS) sqrt.c
		$(CC) $(CFLAGS) sqrt.c

clean	:
		rm -f $(OBJS) *.i *..c

clobber	:	clean
