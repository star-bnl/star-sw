//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 20 Jan 2011
//

#include "bits.hh"
#include "Board.hh"
#include "qt32b_fpe_2009_a.hh"

void qt32b_fpe_2009_a(Board& qt)
{
  qt.output = 0;
  for (int ch = 0; ch < 32; ++ch) if (!btest(qt.bitmask,ch)) qt.output += qt.channels[ch];
}
