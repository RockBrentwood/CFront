// 1985 Feb 08 13:49
/* %Z% %M% %I% %H% %T% */
/*
	C++ stream i/o source

	in.c
*/
#include <ctype.h>
/*#include <stdio.h>*/
#include "../../incl/stream.h"
#include "../../incl/common.h"

/* the predefined streams */
filebuf cin_file = {
	/*base stuff*/0, 0, 0, 0, 0, filebuf__vtbl, /*fd*/0, /*opened*/1
}
istream cin = { /*bp*/(streambuf*)&cin_file, /* tied_to */&cout, /*skipws*/1, /*state*/0 }; //(#) Clipped at "/*stat".

/* predefined whitespace */
whitespace WS;

/*inline */void eatwhite (istream& is)
{
	if (is.tied_to) is.tied_to->flush();
	register streambuf *nbp = is.bp;
	register char c = nbp->sgetc();
	while (isspace(c)) c = nbp->snextc();
}

istream& istream.operator>>(whitespace& w)
{
	register char c;
	register streambuf *nbp = bp;

	&w;
	if (state) return *this;
	if (tied_to) tied_to->flush();
	c = nbp->sgetc();
	while (isspace(c)) c = nbp->snextc();

	if (c == EOF) state |= _eof;

	return *this;
}

istream& istream.operator>>(register char& s)
/*
	reads characters NOT very small integers
 */
{
	if (skipws) eatwhite(*this);

	if (state) {
		state |= _fail;
		return *this;
	}

	s = bp->sgetc();
	if (s == EOF) {
		state |= _fail|_eof;
		return *this;
	}

	if (bp->snextc() == EOF) state |= _eof;
	return *this;
}

istream& istream.operator>>(register char* s)
{
	register c;
	register streambuf *nbp = bp;

	if (skipws) eatwhite(*this);

	if (state) {
		state |= _fail;
		return *this;
	}

	/* get string */
	c = nbp->sgetc();
	if (c == EOF)
		state |= _fail;
	while (!isspace(c) && c != EOF) {
		*s++ = c;
		c = nbp->snextc();
	}
	*s = '\0';

	if (c == EOF) state |= _eof;

	return *this;
}

istream&
istream.operator>>(long& i)
{
	register c;
	register ii = 0;
	register streambuf *nbp = bp;
	int neg = 0;

	if (skipws) eatwhite(*this);

	if (state) {
		state |= _fail;
		return *this;
	}

	switch (c = nbp->sgetc()) {
	case '-':
	case '+':
		neg = c;
		c = nbp->snextc();
		break;
	case EOF:
		state |= _fail;
	}

	if (isdigit(c)) {
		do {
			ii = ii*10+c-'0';
		} while (isdigit(c=nbp->snextc()));
		i = (neg=='-') ? -ii : ii;
	} else
		state |= _fail;

	if (c == EOF) state |= _eof;
	return *this;
}

istream&
istream.operator>>(int& i)
{
	long l;

	if (skipws) eatwhite(*this);

	if (state) {
		state |= _fail;
		return *this;
	}

	if ( *this>>l ) {
		i = l;
	}
	return *this;
}

istream&
istream.operator>>(short& i)
{
	long l;

	if (skipws) eatwhite(*this);

	if (state) {
		state |= _fail;
		return *this;
	}

	if ( *this>>l ) {
		i = l;
	}
	return *this;
}

istream&
istream.operator>>(double& d)
/*
	{+|-} d* {.} d* { e|E {+|-} d+ } 
	except that
		- a dot must be pre- or succeded by at least one digit
		- an exponent must be preseded by at least one digit
*/
{
	register c = 0;
	char buf[256];
	register char* p = buf;
	register streambuf* nbp = bp;
	extern double atof(char*);

	if (skipws) eatwhite(*this);

	if (state) {
		state |= _fail;
		return *this;
	}

	/* get the sign */
	switch (c = nbp->sgetc()) {
	case EOF:
		state = _eof|_fail;
		return *this;
	case '-':
	case '+':
		*p++ = c;
		c = bp->snextc();
	}

	/* get integral part */
	while (isdigit(c)) {
		*p++ = c;
		c = bp->snextc();
	}

	/* get fraction */
	if (c == '.') {
		do {
			*p++ = c;
			c = bp->snextc();
		} while (isdigit(c));
	}

	/* get exponent */
	if (c == 'e' || c == 'E') {
		*p++ = c;
		switch (c = nbp->snextc()) {
		case EOF:
			state = _eof|_fail;
			return *this;
		case '-':
		case '+':
			*p++ = c;
			c = bp->snextc();
		}
		while (isdigit(c)) {
			*p++ = c;
			c = bp->snextc();
		}
	}

	*p = 0;
	d = atof(buf);

	if (c == EOF) state |= _eof;
	return *this;
}

istream&
istream.operator>>(float& f)
{
	double d;

	if (skipws) eatwhite(*this);

	if (state) {
		state |= _fail;
		return *this;
	}

	if ( *this>>d ) {
		f = d;
	}
	return *this;
}

istream&
istream.get(
	register char* s,	/* character array to read into */
	register int len,	/* size of character array */
	register char term	/* character that terminates input */
) {
	register c;
	register streambuf *nbp = bp;

	if (state) {
		state |= _fail;
		return *this;
	}

	if ((c = bp->sgetc()) == EOF) {
		state |= _fail | _eof;
		return *this;
	}

	while (c != term && c != EOF && len > 1) {
		*s++ = c;
		c = nbp->snextc();
		len--;
	}
	*s = '\0';
	if (c == EOF)
		state |= _eof;
	return *this;
}

istream&
istream.putback(	register char c /* character to put back */) {
	bp->sputbackc(c);
	return *this;
}

istream&
istream.get(
	register streambuf &s,	/* streambuf to input to */
	register char term	/* termination character */
){
	register c;
	register streambuf *nbp = bp;

	if (state) {
		state |= _fail;
		return *this;
	}

	if ((c = bp->sgetc()) == EOF) {
		state |= _fail | _eof;
		return *this;
	}

	while (c != term && c != EOF) {
		if (s.sputc(c) == EOF)
			break;
		c = nbp->snextc();
	}
	if (c == EOF)
		state |= _eof;
	return *this;
}

istream&
istream.operator>>(register streambuf &s) {
	register c;
	register streambuf *nbp = bp;

	if (state) {
		state |= _fail;
		return *this;
	}

	if ((c = bp->sgetc()) == EOF) {
		state |= _fail | _eof;
		return *this;
	}

	while (c != EOF) {
		if (s.sputc(c) == EOF)
			break;
		c = nbp->snextc();
	}
	if (c == EOF)
		state |= _eof;
	return *this;
}

istream& istream.operator>>(common& p)
{
	return p.read(*this);
}
