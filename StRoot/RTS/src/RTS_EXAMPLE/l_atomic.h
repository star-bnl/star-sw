#ifndef _L_ATOMIC_H_
#define _L_ATOMIC_H_

#ifdef __i386__
	#include <I386/atomic.h>
#else
	#ifdef __x86_64__
		#include <I386/atomic.h>
	#else

#warning "There seems to be no atomic operation supported"

typedef unsigned int atomic_t ;

inline void atomic_set(atomic_t *d, unsigned int val)
{
	*d = (atomic_t) val ;
}

inline unsigned int atomic_read(atomic_t *d)
{
	return (unsigned int) *d ;
}

inline void atomic_inc(atomic_t *d)
{
	(*d)++ ;
}

inline void atomic_dec(atomic_t *d)
{
	(*d)-- ;
}



	#endif
#endif



#endif
