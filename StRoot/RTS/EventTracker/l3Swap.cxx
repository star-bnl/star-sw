#include "l3Swap.h"

#include <netinet/in.h>

float l3Fswap(float swapped)
{
    unsigned int* uintptr = (unsigned int*) &swapped;
    unsigned int uintvar = ntohl(*uintptr);
    float* floatvar = (float*)&uintvar;
    return *floatvar;
}

uint32 l3Swap32(uint32 in) 
{
	register uint32 x ;
	x = in ;

	return (x&0xff000000) >> 24 | \
        (x&0x00ff0000) >> 8  | \
        (x&0x0000ff00) << 8  | \
        (x&0x000000ff) << 24;
}


uint16 l3Swap16(uint16 in)
{
	register uint16 x ;

	x = in ;

	return (x&0xFF00) >> 8 | (x&0xFF) << 8 ;
}

void l3SwapBuffer(void *dest, void *src, unsigned int nDWords)
{
    for (unsigned int i = 0; i<nDWords; i++) {
	((uint32 *)dest)[i] = swap32(((uint32 *)src)[i]);
    }
}

bool checkByteOrder(uint32 byte_order) 
{
    uint32 raw_format_order = 0x04030201;
    return ( byte_order == raw_format_order );
}
