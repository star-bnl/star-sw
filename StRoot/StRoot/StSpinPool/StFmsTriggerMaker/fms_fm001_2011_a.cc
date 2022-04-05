//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 14 Jan 2011
//

#include "qt32b_fms_2009_a.hh"
#include "fms_fm001_2011_a.hh"

void fms_fm001_2011_a(Board& fm001, int t){
  int A[4], B[4], C[4], D[4], htadc, htid;

  getQtSumAndHighTower((int*)fm001.channels[t],A,B,C,D,htadc,htid);

  // High tower thresholds
  const int R0 = fm001.registers[0];
  const int R1 = fm001.registers[1];

  // Compare the HT inputs from QT boards A, B, C, AND D to two HT
  // thresholds. OR the results together, then output the HT0 and HT1
  // results to the Layer-1 DSM (2 bits)
  int HT0 = htadc > R0;
  int HT1 = htadc > R1;

  // Form the following 6 7-bit sums:
  int SumA  = A[0]+A[1]+A[2]+A[3];
  int SumAB = A[2]+A[3]+B[0]+B[1];
  int SumB  = B[0]+B[1]+B[2]+B[3];
  int SumBC = B[2]+B[3]+C[0]+C[1];
  int SumC  = C[0]+C[1]+C[2]+C[3];
  int SumD  = D[0]+D[1]+D[2]+D[3];

  // Extract the low-order 5 bits from each of the 7-bit sums.
  // If either of the two high-order bits is 1,
  // set the result to 11111.
  if (SumA  > 31) SumA  = 31;
  if (SumAB > 31) SumAB = 31;
  if (SumB  > 31) SumB  = 31;
  if (SumBC > 31) SumBC = 31;
  if (SumC  > 31) SumC  = 31;
  if (SumD  > 31) SumD  = 31;

  // Output the resulting 6 5-bit sums to the Layer-1 DSM (30 bits)
  fm001.output[t] = SumD | SumC << 5 | SumBC << 10 | SumB << 15 | SumAB << 20 | SumA << 25 | HT0 << 30 | HT1 << 31;
}
