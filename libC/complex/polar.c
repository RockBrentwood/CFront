// 1985 Feb 09 13:38
/* %Z% %M% %I% %H% %T% */

#include	"../../incl/complex.h"

complex polar(double r, double theta)
{
	return complex(r * cos(theta), r * sin(theta) );
}
