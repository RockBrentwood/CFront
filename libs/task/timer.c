// 1985 Feb 08 13:55
/* %Z% %M% %I% %H% %T% */
#include "../../incl/task.h"

timer.timer(int d) : (TIMER)
{
	s_state = IDLE;
	insert(d,this);
}

timer.~timer()
{
	if (s_state != TERMINATED) task_error(E_TIMERDEL,this);
}

void timer.reset(int d)
{
	remove();
	insert(d,this);
}

void timer.print(int n)
{ n;/*avoid warning*/
	long tt = s_time;
	printf("timer %ld == clock+%ld\n",tt,tt-clock);
}
