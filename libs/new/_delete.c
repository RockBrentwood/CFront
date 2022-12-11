// 1985 Feb 08 13:45
/* %Z% %M% %I% %H% %T% */
free(char*);
extern void operator delete(void* p)
{
	if (p) free( (char*)p );
}
