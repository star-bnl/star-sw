/* WHG 07apr98 Written for use in win32 DSL */
#ifndef _RPC_TYPES_H
#define	_RPC_TYPES_H

#include <winsock2.h>

typedef int bool_t;
typedef char * caddr_t;
typedef int enum_t;
typedef int longlong_t[2];
typedef unsigned int u_longlong_t[2];

#define mem_alloc(bsize) malloc(bsize)
#define mem_free(ptr, bsize) free(ptr)
#define WHG_MALLOC_ERR(msg)\
	fprintf(stderr, "out of memory %s, %s(%d)\n", msg, __FILE__, __LINE__)

#ifdef _M_IX86
#define i386
#endif

typedef int mutex_t;
#define mutex_lock(x)
#define mutex_unlock(x)

#endif	/* !_RPC_TYPES_H */
