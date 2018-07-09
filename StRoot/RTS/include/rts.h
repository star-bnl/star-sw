/*
	This is the main, steering include file that _MUST_
	be used before any other STAR/RTS include.

	It is currently working only for GNU C/C++ on the following
	platforms:

	Host:	Solaris SPARC
		Linux Intel
		Linux Alpha
		OSF Alpha

	Target:	Solaris SPARC
		Linux Intel/AMD
		Linux PowerPC
		Linux Alpha
		vxWorks PowerPC
		vxWorks I960

*/

/* History

	Oct 2004: made ANSI compliant, added OSF; Tonko
	first try, June 2003, tonko
*/
#include <string.h>

#ifndef _RTS_H_
#define _RTS_H_

// jml 1/8/13
//    This ifndef __CINT__ made its way into the code
//    when the APPLE changes were made.   It causes the endianness to
//    remain unset when CINT is processing.   I've no idea what it was
//    for, so if it is needed please figure out a way to implement it
//    without breaking the CINT endiness...
//#ifndef __CINT__
#ifndef __GNUC__
#warning "This may NOT work on non-GNUC compilers!"
#endif


/* From this point on GNU C is assumed */

/* ********************* Let's get the endianess, unless already defined ****************/

#if defined(RTS_LITTLE_ENDIAN)
#elif defined(RTS_BIG_ENDIAN)
#else

/* Let's get the target CPU */
#if defined(__i386__)	/* assume linux, GCC 3.X (new!) */

#define RTS_LITTLE_ENDIAN

#elif defined(__x86_64)

#define RTS_LITTLE_ENDIAN

#elif defined(__i960__)	/* assume vxworks, GCC 2.7 */

#define RTS_LITTLE_ENDIAN

#elif defined(_ARCH_PPC)	/* assume vxworks, really old GCC 2.7 */

#define RTS_BIG_ENDIAN

#elif defined(__ppc__)		/* assume GCC 2.95 at least? */

#define RTS_BIG_ENDIAN

#elif defined(__sparc__)	/* assume Solaris ultrasparc, GCC 2.8 */

#define RTS_BIG_ENDIAN

#elif  defined(__alpha__)

	#if defined(__osf__)
		#define RTS_BIG_ENDIAN	/* OSF is big endian */
	#else
		#define RTS_LITTLE_ENDIAN		
	#endif

#else

#error "Unknown CPU type - can't proceed!"

#endif	/* CPU types */

#endif /* RTS_XXX_ENDIAN */

/*********************** Find the TARGET_SYSTEM unless already defined *******************************/
#ifndef TARGET_SYSTEM

#if defined(__linux__)
#define TARGET_SYSTEM "LINUX"
#elif defined(__APPLE__)
#define TARGET_SYSTEM "LINUX"
#elif defined(__sun__)
#define TARGET_SYSTEM "SUN"
#elif defined(__osf__)
#define TARGET_SYSTEM "OSF1"
#elif defined(__vxworks__)

#if defined(__i960__)
#define TARGET_SYSTEM "I960"
#else
#define TARGET_SYSTEM "MVME"
#endif

#else
#error "Unknown OS type - can't proceed!"
#endif

#endif	/* TARGET_SYSTEM */
//#endif  /* !__CINT__ */

/******************** if any of RTS_PROJECT_XXX variables are not defined, we'll define it here ******/
#ifdef RTS_PROJECT_STAR

	#define RTS_PROJECT "STAR"
	#ifndef PROJDIR
		#define PROJDIR "/RTS"
	#endif

#else
	#ifdef RTS_PROJECT_PP

		#define RTS_PROJECT "PP"
		#ifndef PROJDIR
			#define PROJDIR "/PP"
		#endif
	#else	/* no variable defined in the Makefile - assume STAR/unknown... */

		#define RTS_PROJECT_STAR
		#define RTS_PROJECT "STAR"
		#define PROJDIR "/tmp"	/* unknown */
	#endif
#endif

#ifndef RTS_BINDIR
#define RTS_BINDIR PROJDIR "/bin/" TARGET_SYSTEM  
#endif

/* ********************** BYTESWAPPING STUFF ***********************/

#if defined(__linux__) && ! defined(__APPLE__)
/* linux has its own (fast) swaps */
#include <byteswap.h>

#define swap16(x) bswap_16(x)
#define swap32(x) bswap_32(x)

#else	/* non-linux stuff */

extern inline unsigned short swap16(unsigned short x)
{
	return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8)) ;
}

extern inline unsigned int swap32(unsigned int x) 
{
     return ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |               \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24)) ;
}

extern inline unsigned long long swap64(unsigned long long x) {
    unsigned long long ret = x;
    unsigned int *low = (unsigned int *)&ret;
    unsigned int *high = low + 1;
    *low = swap32(*low);
    *high = swap32(*high);
    return ret;
}


#endif /* BYTESWAP */

/* Don't let floats get casts to ints before swapping.... */
extern inline float swapf(float f)
{
  unsigned int x;
  memcpy((char *)&x, (char *)&f, 4);
  x = swap32(x);
  memcpy((char *)&f, (char *)&x, 4);
  return f;
}

extern inline void swapBuff32(unsigned int *buff, int n)
{
  for(int i=0;i<n;i++) {
    *buff = swap32(*buff);
    buff++;
  }
}

#if defined(RTS_LITTLE_ENDIAN)

#define RTS_ENDIAN	0

#define l2hfloat(x)     (x)
#define l2h64(x)        (x)
#define l2h32(x)	(x)
#define l2h16(x)	(x)
#define b2hfloat(x)     swapf(x)
#define b2h64(x)        swap64(x)
#define b2h32(x)	swap32(x)
#define b2h16(x)	swap16(x)

#elif defined(RTS_BIG_ENDIAN)

#define RTS_ENDIAN	1

#define l2hfloat(x)     swapf(x)
#define l2h64(x)        swap64(x)
#define l2h32(x)	swap32(x)
#define l2h16(x)	swap16(x)
#define b2hfloat(x)     (x)
#define b2h64(x)        (x)
#define b2h32(x)	(x)
#define b2h16(x)	(x)

#else

#error "ENDIANESS NOT DEFINED!"

#endif

#define qswap16(test,x) ((test)?swap16(x):(x))
#define qswap32(test,x) ((test)?swap32(x):(x))
#define qswap64(test,x) ((test)?swap64(x):(x))


/* *** COMPILER TRICKS ***********************************************************/

#if __GNUC__ == 2 && __GNUC_MINOR__ < 96
#define __builtin_expect(x, expected_value) (x)
#endif
 
#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)


/********** UGLINESS for CINT which does not seem to be able to parse "sys/types.h" ********/

#ifdef __CINT__
#define u_int unsigned int
typedef unsigned char u_char ;
typedef unsigned short u_short ;
#endif


// 64 bit int 

typedef unsigned long long int UINT64;

#define hi64(x) ((UINT32)(x >> 32))
#define lo64(x) ((UINT32)(x & 0xffffffff))
#define make64(lo, hi)  ((UINT64)(((UINT64)hi << 32) | lo))

#ifdef __APPLE__
#define open64 open
#define lseek64 lseek
#define mmap64 mmap
#endif /* __APPLE__ */

#endif /* _RTS_H_ */
