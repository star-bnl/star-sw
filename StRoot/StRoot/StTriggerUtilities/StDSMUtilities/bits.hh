//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 14 Jan 2011
//

#ifndef BITS_HH
#define BITS_HH
#include<stdio.h>
// returns value of bit from x at position pos
inline int btest(int x, int pos) { return x >> pos & 1; }

// returns n bits from x starting at position pos
inline int getbits(int x, int pos, int n) { return x >> pos & ~(~0 << n); }

// OR x with value starting at position pos
inline void setbits(int& x, int pos, int value) { x |= value << pos; }

#endif	// BITS_HH
