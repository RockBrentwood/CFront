// 1985 Feb 08 13:46
/* %Z% %M% %I% %H% %T% */

typedef void (*PFVV)();

extern PFVV _new_handler;

extern void* operator new(/* long !!!!!! */ unsigned size)
{
	extern char* malloc(unsigned);
	char* p;

	while ( (p=malloc(size))==0 ) {
		if(_new_handler)
			(*_new_handler)();
		else
			return 0;
	}
	return (void*)p;
}
