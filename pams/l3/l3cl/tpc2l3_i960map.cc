/*:>-------------------------------------------------------------------
**: FILE:        tpc2l3_i960map.cc (StAF version)
**:
**: AUTHOR:      cs - Christof Struck, struck@star.physics.yale.edu
**:
**: ROUTINE:     rough mapping i960 number <-> TPC padrows (and pads)
**:
**: HISTORY:     last change - 09/16/98  cs 
**:              protect for i960no out of bounds - 10/27/98  py 
**:  
**:<------------------------------------------------------------------*/

#include "tpc2l3_inc.h"
#include <stdlib.h>

#if defined(__cplusplus)
extern "C" {
#endif

void i960map ( int i960no, int *StartRow, int *EndRow )
{
  // tables contain rows in reverse order
  static int End[18] = 
  {
       0,  2,  5,  8, 10, 11,
      13, 16, 18, 21, 24, 26,
      29, 32, 34, 37, 40, 42
  };
  static int Start[18] =
  {
       1,  4,  7,  9, 10, 12,
      15, 17, 20, 23, 25, 28,
      31, 33, 36, 39, 41, 44
  };

  if ( i960no > -1 && i960no < 18 ) {
     *StartRow = Start[i960no];
     *EndRow   = End[i960no];
  }
  else {
     *StartRow = 0 ;
     *EndRow   = 0 ;
  }

}
#if defined(__cplusplus)
}
#endif
