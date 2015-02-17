//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 14 Jan 2011
//

#include "bits.hh"
#include "fms_fm101_2011_a.hh"

void fms_fm101_2011_a(Board& fm101, int t)
{
  int* channels = (int*)fm101.channels[t];

  // Board sum thresholds
  const int R0 = fm101.registers[0];
  const int R1 = fm101.registers[1];
  const int R2 = fm101.registers[2];

  // High tower bits
  int HT0 = 0;
  int HT1 = 0;

  // Board sum bits
  int BS0 = 0;
  int BS1 = 0;
  int BS2 = 0;

  // Quadrant sums
  int Sum[4];

  for (int i = 0; i < 4; ++i) {
    int fm001out = channels[i];

    // OR the HT0 bits from the four quadrants together, and likewise
    // for the HT1 bits. Output the results to the Layer-2 DSM (2 bits)
    HT0 |= btest(fm001out,30);
    HT1 |= btest(fm001out,31);

    // Compare the 24 5-bit Sum values to three thresholds (BSum0,
    // BSum1, BSum2). OR the results together, and output them to the
    // Layer-2 DSM (3 bits, not muxed)
    int SumD  = getbits(fm001out,0 ,5);
    int SumC  = getbits(fm001out,5 ,5);
    int SumBC = getbits(fm001out,10,5);
    int SumB  = getbits(fm001out,15,5);
    int SumAB = getbits(fm001out,20,5);
    int SumA  = getbits(fm001out,25,5);

    BS0 |= SumA > R0 || SumAB > R0 || SumB > R0 || SumBC > R0 || SumC > R0 || SumD > R0;
    BS1 |= SumA > R1 || SumAB > R1 || SumB > R1 || SumBC > R1 || SumC > R1 || SumD > R1;
    BS2 |= SumA > R2 || SumAB > R2 || SumB > R2 || SumBC > R2 || SumC > R2 || SumD > R2;

    // Compute the 7-bit total SumA+SumB+SumC+SumD for each of the four quadrants
    Sum[i] = SumA+SumB+SumC+SumD;

    // Extract the low-order 6 bits from each of the 7-bit sums. If the
    // high-order bit is 1, set the result to 111111.
    if (Sum[i] > 63) Sum[i] = 63;
  }

  // Output the resulting 4 6-bit sums to the Layer-2 DSM (24 bits)
  fm101.output[t] = Sum[0] | Sum[1] << 6 | Sum[2] << 12 | Sum[3] << 18 | BS0 << 24 | BS1 << 25 | BS2 << 26 | HT0 << 27 | HT1 << 28;
}
