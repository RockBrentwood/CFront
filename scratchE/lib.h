#include <stdlib.h>

typedef void (*PF)(char *);
typedef void (*PFVV)(void);

extern void *_new(unsigned long Size);
extern void _delete(void *p);

// Allocate a vector of Size elements of size dN and initialize each by a call of Op().
extern void *_vec_new(void *X, unsigned Size, unsigned dN, PF Op);
extern void _vec_delete(void *X, unsigned Size, unsigned dN, PF Op, int _free);

extern PFVV set_new_handler(PFVV Fn);
extern PFVV _ctors[1], _dtors[1];
extern void Exit(int Status);
extern void Enter(void);
