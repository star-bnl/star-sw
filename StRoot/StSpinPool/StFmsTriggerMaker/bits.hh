//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 14 Jan 2011
//

#ifndef BITS_HH
#define BITS_HH

// returns value of bit from x at position pos
inline int btest(int x, int pos) { return x >> pos & 1; }

// returns n bits from x starting at position pos
inline int getbits(int x, int pos, int n) { return x >> pos & ~(~0 << n); }

// OR x with value starting at position pos
inline void setbits(int& x, int pos, int value) { x |= value << pos; }

// Reverse bits
inline int reversebit(int x, int max=31) {
  int y=0;
  for(int i=0; i<max; i++) y += ((x >> i) & 1) << (max-i-1);
  return y;
}
#endif	// BITS_HH
