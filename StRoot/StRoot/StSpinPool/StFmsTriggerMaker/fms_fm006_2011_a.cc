//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 14 Jan 2011
//

#include "qt32b_fms_2009_a.hh"
#include "fms_fm006_2011_a.hh"

void fms_fm006_2011_a(Board& fm006, int t){
  int I[4], J[4], htadc, htid;

  getQtSumAndHighTower((int*)fm006.channels[t],I,J,htadc,htid);

  // High tower thresholds
  const int R0 = fm006.registers[0];
  const int R1 = fm006.registers[1];

  // Compare the HT inputs from QT boards I and J to two HT
  // thresholds. OR the results together, then output the HT0 and HT1
  // results to the Layer-1 DSM (2 bits)
  int HT0 = htadc > R0;
  int HT1 = htadc > R1;

  // Form the following 3 7-bit sums:
  int SumI  = I[0]+I[1]+I[2]+I[3];
  int SumIJ = I[2]+I[3]+J[0]+J[1];
  int SumJ  = J[0]+J[1]+J[2]+J[3];

  // Extract the low-order 5 bits from each of the 7-bit sums. If
  // either of the two high-order bits is 1, set the result to 11111.
  if (SumI  > 31) SumI  = 31;
  if (SumIJ > 31) SumIJ = 31;
  if (SumJ  > 31) SumJ  = 31;

  // Output the resulting 3 5-bit sums to the Layer-1 DSM (30 bits)
  fm006.output[t] = SumJ << 15 | SumIJ << 20 | SumI << 25 | HT0 << 30 | HT1 << 31;
}
