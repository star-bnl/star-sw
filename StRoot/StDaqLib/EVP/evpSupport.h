#ifndef _EVP_SUPPORT_H
#define _EVP_SUPPORT_H

#include <sys/types.h>

#define EVP_NO_DET	0
#define EVP_NO_DATA	(-1)
#define EVP_DATA_ERR	(-2) 

extern int checkBank(char *m, char *what) ;

inline uint swap32(uint d)
{
	register uint x;
	x = d ;

        return (x&0xff000000) >> 24 | \
        (x&0x00ff0000) >> 8  | \
        (x&0x0000ff00) << 8  | \
        (x&0x000000ff) << 24;
}

inline ushort swap16(ushort in)
{
        register ushort x ;
        x = in ;

        return (x&0xFF00) >> 8 | (x&0xFF) << 8 ;
}


#if defined (__i386) || defined (i386)		/* will assume little endian */

#ifndef UNIX_LITTLE_ENDIAN
#define UNIX_LITTLE_ENDIAN
#endif

#define l2h32(x) (x)
#define b2h32(x) (swap32(x))
#define l2h16(x) (x)
#define b2h16(x) (swap16(x))

#elif defined (__sparc) || defined (sparc)	/* will assume big endian */

#define l2h32(x) swap32(x)
#define b2h32(x) ((x))
#define l2h16(x) swap16(x)
#define b2h16(x) ((x))

#else
#error "Don't understand this machine architecture!"	/* will scream */
#endif
	

#endif
