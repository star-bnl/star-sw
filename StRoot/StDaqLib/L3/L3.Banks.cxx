#include "L3.Banks.hh"








// our own swap, which swaps shorts (and not words of shorts)
int l3Swap_short (short* data, short size)
{
  char *curr = (char *)data;
  for (int i=0; i<size; i++) {
        char temp = curr[0];
	curr[0] = curr[1];
	curr[1] = temp;
	curr += 2;
  }
  return 1;
};
