# 1985 Feb 08 14:11
# %Z% %M% %I% %H% %T%
# C++ master makefile
#
ROOT=
CC=CC
CCLOC=/usr/lbin/CC
CFRONT=/usr/lbin/cfront
LIBC=/lib/libC.a
SSCAN=/usr/lbin/sscan
INCL=/usr/include/CC
CRTLIB=/lib
LSRC=/usr/lsrc/cmd/CC

all	:
		if u3b; then \
			cd C++ ; make CC=$(CC) OSUF=.o ; \
			cd .. ;
			cd libC ; make CC=$(CC) OSUF=.o ; \
		else \
			cd C++ ; make CC=$(CC) OSUF=..o ; \
			cd .. ;
			cd libC ; make CC=$(CC) OSUF=..o ; \
		fi
		if u3b ; \
			then cd sscan ; make CC=$(CC) ; \
		fi

install	:
		if u3b; then \
			cd C++ ; make CC=$(CC) OSUF=.o ; \
		else \
			cd C++ ; make CC=$(CC) OSUF=..o ; \
		fi
		if u3b ; \
			then cd scan ; make CC=$(CC) ; \
		fi
		rm -f $(ROOT)$(CCLOC)
		if test $(ROOT) ; then \
			cp localCC $(ROOT)$(CCLOC) ; \
		else \
			cp CC $(CCLOC) ; \
		fi
		chmod +x $(ROOT)$(CCLOC)
		rm -f $(ROOT)$(CFRONT)
		cp C++/cfront $(ROOT)$(CFRONT)
		if u3b; then \
			rm -f $(ROOT)$(CRTLIB)/CCcrt0.o ; \
			cp sscan/CCcrt0.o $(ROOT)$(CRTLIB)/CCcrt0.o ; \
			rm -f $(ROOT)$(CRTLIB)/CCmcrt0.o ; \
			cp sscan/CCmcrt0.o $(ROOT)$(CRTLIB)/CCmcrt0.o ; \
			rm -f $(ROOT)$(SSCAN) ; \
			cp sscan/sscan $(ROOT)$(SSCAN) ; \
		fi
		if u3b; then \
			cd libC ; make CC=$(CC) OSUF=.o ; \
		else \
			cd libC ; make CC=$(CC) OSUF=..o ; \
		fi
		rm -f $(ROOT)$(LIBC)
		cp libC$(CRTLIB)C.a $(ROOT)$(LIBC)
		rm -rf $(ROOT)$(INCL)/*
		cp incl/sys/* $(ROOT)$(INCL)/sys
		cp incl/*.h $(ROOT)$(INCL)

clean	:
		cd C++ ; make clean
		cd sscan ; make clean
		cd libC ; make clean
		cd c ; rm -f *..o

clobber	:
		cd C++ ; make clobber
		cd sscan ; make clobber
		cd libC ; make clobber

uplist	:
		uplist -f up.C++.3bs
		rm -f $(LSRC)/C++/*
		cp C++/*.[chy] C++/makefile $(LSRC)/C++
		rm -f $(LSRC)/sscan/*
		cp sscan/*.[cs] sscan/makefile $(LSRC)/sscan
		rm -f $(LSRC)/incl/*.h $(LSRC)/incl/sys/*
		cp incl/*.h $(LSRC)/incl
		cp incl/sys/* $(LSRC)/incl/sys
		rm -f $(LSRC)/libC/makefile
		cp libC/makefile $(LSRC)/libC
		rm -f $(LSRC)/libC/complex/*
		cp libC/complex/*.[ch] libC/complex/makefile $(LSRC)/libC/complex
		rm -f $(LSRC)/libC/new/*
		cp libC/new/*.[ch] libC/new/makefile $(LSRC)/libC/new
		rm -f $(LSRC)/ibC/stream/*
		cp libC/stream/*.[ch] libC/stream/makefile $(LSRC)/libC/stream
		rm -f $(LSRC)/libC/task/*
		cp libC/task/*.[chs] libC/task/makefile $(LSRC)/libC/task
		rm -f $(LSRC)/CC $(LSRC)/makefile
		cp CC makefile $(LSRC)
		uplist -f up.C++.vaxes

fromscratch	:
		cd $(ROOT)/c ; cc *..c -o cfront
		mv $(ROOT)/c/cfront $(ROOT)/usr/lib/cfront
		cp $(ROOT)/localCC $(ROOT)/usr/lbin/CC
		chmod +x $(ROOT)/usr/lbin/CC
		if u3b; then \
			cd libC/new ; make CC=$(ROOT)/usr/lbin/CC OSUF=.o ; \
			ar cr libC.a $(ROOT)/libC/new/*.o ; \
			mv libC.a $(ROOT)/lib ; \
			cd ../.. ; \
			make CC=$(ROOT)/usr/lbin/CC ROOT=$(ROOT) OSUF=.o install ;
		else \
			cd libC/new ; make CC=$(ROOT)/usr/lbin/CC OSUF=..o ; \
			ar cr libC.a $(ROOT)/libC/new/*..o ; \
			mv libC.a $(ROOT)/lib ; \
			cd ../.. ; \
			make CC=$(ROOT)/usr/lbin/CC ROOT=$(ROOT) OSUF=..o install ;
		fi
		cp $(ROOT)/localCC $(ROOT)/usr/lbin/CC
		chmod +x $(ROOT)/usr/lbin/CC
