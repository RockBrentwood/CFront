## 1985 Feb 08 13:46
# %Z% %M% %I% %H% %T%
CC=CC
CFLAGS=-c
OSUF=.o
OBJS=_delete$(OSUF) _handler$(OSUF) _new$(OSUF) _vec$(OSUF)
all	:	$(OBJS)

_delete$(OSUF)	:	_delete.c
		$(CC) $(CFLAGS) _delete.c

_handler$(OSUF)	:	_handler.c
		$(CC) $(CFLAGS) _handler.c

_new$(OSUF)	:	_new.c
		$(CC) $(CFLAGS) _new.c

_vec$(OSUF)	:	_vec.c
		$(CC) $(CFLAGS) _vec.c

clean	:
		rm -f $(OBJS) *.i *..c

clobber	:	clean
