// 1985 Feb 09 13:37
/* %Z% %M% %I% %H% %T% */
#include "../../incl/complex.h"

void complex_error(int err, double)
{
	errno = err;
}
