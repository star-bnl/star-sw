//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 14 Jan 2011
//

//#include <cstdio>
#include "qt32b_fms_2009_a.hh"
#include "fms_fm005_2011_a.hh"

void fms_fm005_2011_a(Board& fm005,int t){
  int E[4], F[4], G[4], H[4], htadc, htid;

  getQtSumAndHighTower((int*)fm005.channels[t],E,F,G,H,htadc,htid);

  // High tower thresholds
  const int R0 = fm005.registers[0];
  const int R1 = fm005.registers[1];

  // Compare the HT inputs from QT boards E, F, G, and H to two HT
  // thresholds. OR the results together, then output the HT0 and HT1
  // results to the Layer-1 DSM (2 bits)
  int HT0 = htadc > R0;
  int HT1 = htadc > R1;

  // Form the following 6 7-bit sums:
  int SumE  = E[0]+E[1]+E[2]+E[3];
  int SumEF = E[2]+E[3]+F[0]+F[1];
  int SumF  = F[0]+F[1]+F[2]+F[3];
  int SumG  = G[0]+G[1]+G[2]+G[3];
  int SumGH = G[2]+G[3]+H[0]+H[1];
  int SumH  = H[0]+H[1]+H[2]+H[3];

  // Extract the low-order 5 bits from each of the 7-bit sums. If
  // either of the two high-order bits is 1, set the result to 11111.
  if (SumE  > 31) SumE  = 31;
  if (SumEF > 31) SumEF = 31;
  if (SumF  > 31) SumF  = 31;
  if (SumG  > 31) SumG  = 31;
  if (SumGH > 31) SumGH = 31;
  if (SumH  > 31) SumH  = 31;

  // Output the resulting 6 5-bit sums to the Layer-1 DSM (30 bits)
  fm005.output[t] = SumH | SumGH << 5 | SumG << 10 | SumF << 15 | SumEF << 20 | SumE << 25 | HT0 << 30 | HT1 << 31;

#if 0
  if (strcmp(fm005.name,"FM007") == 0) {
    int* channels = (int*)fm005.channels;
    printf("%08x %08x %08x %08x\n",channels[0],channels[1],channels[2],channels[3]);
  }
#endif
}
