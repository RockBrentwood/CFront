## 1985 Feb 08 13:54
# %Z% %M% %I% %H% %T%
CC=CC
CFLAGS=-c
OSUF=.o
OBSJ= obj$(OSUF) qhead$(OSUF) qtail$(OSUF) sched$(OSUF) \
	sim$(OSUF) task$(OSUF) timer$(OSUF) swap$(OSUF)
HDRS=../../incl/task.h

all	:	$(OBJS)

obj$(OSUF)	:	$(HDRS) obj.c
		$(CC) $(CFLAGS) obj.c

qhead$(OSUF)	:	$(HDRS) qhead.c
		$(CC) $(CFLAGS) qhead.c

qtail$(OSUF)	:	$(HDRS) qtail.c
		$(CC) $(CFLAGS) qtail.c

sched$(OSUF)	:	$(HDRS) sched.c
		$(CC) $(CFLAGS) sched.c

sim$(OSUF)	:	$(HDRS) sim.c
		$(CC) $(CFLAGS) sim.c

task$(OSUF)	:	$(HDRS) task.c
		$(CC) $(CFLAGS) task.c

timer$(OSUF)	:	$(HDRS) timer.c
		$(CC) $(CFLAGS) timer.c

swap$(OSUF)	:
		if vax; \
		then $(CC) $(CFLAGS) vax_swap.c ; mv vax_swap.o swap$(OSUF) ; \
		else >swap$(OSUF) ; \
		fi

clean	:
		rm -f $(OBJS) *.i *..c

clobber	:	clean
