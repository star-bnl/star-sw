#ifndef L3SWAP_H
#define L3SWAP_H

#include "daqFormats.h"


float fswap(float swapped);

UINT32 swap32(UINT32 in);
UINT16 swap16(UINT16 in);

bool checkByteOrder(UINT32 byte_order);


#endif
