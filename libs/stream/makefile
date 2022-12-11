## 1985 Feb 08 13:49
# %Z% %M% %I% %H% %T%
CC=CC
CFLAGS=-c
OSUF=.o
OBJS=circbuf$(OSUF) filebuf$(OSUF) in$(OSUF) out$(OSUF) streambuf$(OSUF)
HDRS=../../incl/stream.h
all	:	$(OBJS)

circbuf$(OSUF)	:	$(HDRS) circbuf.c
		$(CC) $(CFLAGS) circbuf.c

filebuf$(OSUF)	:	$(HDRS) filebuf.c
		$(CC) $(CFLAGS) filebuf.c

in$(OSUF)	:	$(HDRS) in.c
		$(CC) $(CFLAGS) in.c

out$(OSUF)	:	$(HDRS) out.c
		$(CC) $(CFLAGS) out.c

streambuf$(OSUF)	:	$(HDRS) streambuf.c
		$(CC) $(CFLAGS) streambuf.c

clean	:
		rm -f $(OBJS) *.i *..c

clobber	:	clean
