// 1985 Feb 09 13:36
/* %Z% %M% %I% %H% %T% */

#include "../../incl/complex.h"

double	arg(complex z)
{
	return atan2(z.im,z.re);
}
