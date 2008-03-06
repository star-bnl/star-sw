#ifndef L3SWAP_H
#define L3SWAP_H

typedef unsigned int   uint32 ;
typedef unsigned short uint16 ;
typedef unsigned char  uint8 ;


float l3Fswap(float swapped);

uint32 l3Swap32(uint32 in);
uint16 l3Swap16(uint16 in);

void l3SwapBuffer(void *dest, void *src, unsigned int nDWords);

bool checkByteOrder(uint32 byte_order);

#ifndef swap32
#define swap32(x) l3Swap32(x)
#endif

#ifndef swap16
#define swap16(x) l3Swap16(x)
#endif

#ifndef fswap
#define fswap(x) l3FSwap(x)
#endif

#endif
