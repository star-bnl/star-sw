#include "L3swap.h"

#include <netinet/in.h>

float fswap(float swapped)
{
    unsigned int* uintptr = (unsigned int*) &swapped;
    unsigned int uintvar = ntohl(*uintptr);
    float* floatvar = (float*)&uintvar;
    return *floatvar;
}

UINT32 swap32(UINT32 in) 
{
	register UINT32 x ;
	x = in ;

	return (x&0xff000000) >> 24 | \
        (x&0x00ff0000) >> 8  | \
        (x&0x0000ff00) << 8  | \
        (x&0x000000ff) << 24;
}


UINT16 swap16(UINT16 in)
{
	register UINT16 x ;

	x = in ;

	return (x&0xFF00) >> 8 | (x&0xFF) << 8 ;
}

bool checkByteOrder(UINT32 byte_order) 
{
    return ( byte_order == DAQ_RAW_FORMAT_ORDER );
}
