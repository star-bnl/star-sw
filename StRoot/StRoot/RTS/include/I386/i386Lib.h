#ifndef _I386_LIB_H
#define _I386_LIB_H


#define rdtsc(low,high) \
     __asm__ __volatile__("rdtsc" : "=a" (low), "=d" (high))
#define rdtscl(low) \
     __asm__ __volatile__("rdtsc" : "=a" (low) : : "edx")
#define rdtscll(val) \
     __asm__ __volatile__("rdtsc" : "=A" (val))

extern inline unsigned long long getfast_l(void)
{
	register unsigned int l, h ;

	__asm__ __volatile__("rdtsc" : "=a" (l), "=d" (h)) ;

	return ((unsigned long long)h<<32) | l ;

}

extern inline unsigned int getfast(void)
{
	register unsigned int l ;

	__asm__ __volatile__("rdtsc" : "=a" (l) : : "edx" ) ;

	return l ;

}

#endif
