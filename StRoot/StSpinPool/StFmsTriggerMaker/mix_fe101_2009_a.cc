//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 20 Jan 2011
//

#include "bits.hh"
#include "Board.hh"
#include "mix_fe101_2009_a.hh"

void mix_fe101_2009_a(Board& fe101, int t){
  int* fe101channels = (int*)fe101.channels[t];
  int qt1sum = getbits(fe101channels[0],0,17);
  int qt2sum = getbits(fe101channels[1],0,17);
  int qt3sum = getbits(fe101channels[2],0,17);
  int qt4sum = getbits(fe101channels[3],0,17);
  int sum1 = qt1sum+qt2sum;
  int sum2 = qt3sum+qt4sum;
  const int R0 = fe101.registers[0];
  const int R1 = fe101.registers[1];
  const int fpeThr = R1 << 12 | R0;
  int fpe1 = sum1 > fpeThr;
  int fpe2 = sum2 > fpeThr;
  fe101.output[t] = fpe1 | fpe2 << 1;
}
