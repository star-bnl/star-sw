//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 9 Feb 2012
//

#include "bits.hh"
#include "fms_fm101_2012_a.hh"

void fms_fm101_2012_a(Board& fm101, int t){
  int* channels = (int*)fm101.channels[t];

  const int R0 = fm101.registers[0]; // FMSsmall-cluster-th0
  const int R1 = fm101.registers[1]; // FMSsmall-cluster-th1
  const int R2 = fm101.registers[2]; // FMSsmall-layer1-mode: 0=data taking, 1=debug

  // High tower bits
  int HT0 = 0;
  int HT1 = 0;

  // Board sum bits
  int BS0 = 0;
  int BS1 = 0;

  // Quadrant sums
  int Sum[4];
  int PairSum[4];

  for (int i = 0; i < 4; ++i) {
    int fm001out = channels[i];

    // Combine (OR) the HT threshold bits from all 4 quadrants
    HT0 |= btest(fm001out,30);
    HT1 |= btest(fm001out,31);

    // Compare the 24 5-bit Sum values to three thresholds (BSum0,
    // BSum1, BSum2). OR the results together, and output them to the
    // Layer-2 DSM (3 bits, not muxed)
    int SumD  = getbits(fm001out,0 ,5);
    int SumC  = getbits(fm001out,5 ,5);
    int SumBC = getbits(fm001out,10,5);
    int SumB  = getbits(fm001out,15,5);
    int SumCD = getbits(fm001out,20,5);
    int SumA  = getbits(fm001out,25,5);

    BS0 |= SumA > R0 || SumCD > R0 || SumB > R0 || SumBC > R0 || SumC > R0 || SumD > R0;
    BS1 |= SumA > R1 || SumCD > R1 || SumB > R1 || SumBC > R1 || SumC > R1 || SumD > R1;

    // Compute the 5-bit total SumA+SumB+SumC+SumD for each of the four quadrants
    Sum[i] = SumA+SumB+SumC+SumD;
    PairSum[i] = SumC+SumD;
  }

  // Extract the 5 least significant bits from each quadrant sum.
  // Set the result to 31 (i.e. binary 11111) if the most significant bit
  // is set.

  int SumST = Sum[0];
  int SumSB = Sum[1];
  int SumNT = Sum[2];
  int SumNB = Sum[3];

  int SumS = PairSum[0]+PairSum[1];
  int SumN = PairSum[2]+PairSum[3];

  if (SumST > 31) SumST = 31;
  if (SumSB > 31) SumSB = 31;
  if (SumNT > 31) SumNT = 31;
  if (SumNB > 31) SumNB = 31;

  if (SumS > 31) SumS = 31;
  if (SumN > 31) SumN = 31;

  fm101.output[t] = 0;

  switch (R2) {
  case 0: // data taking mode
    fm101.output[t] |= SumST << 0;
    fm101.output[t] |= SumS  << 5;
    fm101.output[t] |= SumSB << 10;
    fm101.output[t] |= SumNT << 15;
    fm101.output[t] |= SumN  << 20;
    fm101.output[t] |= SumNB << 25;
    fm101.output[t] |= BS0   << 30;
    fm101.output[t] |= BS1   << 31;
    break;
  case 1: // debug mode
    fm101.output[t] |= SumST << 0;
    fm101.output[t] |= SumS  << 5;
    fm101.output[t] |= SumSB << 10;
    fm101.output[t] |= SumNT << 15;
    fm101.output[t] |= HT0   << 20;
    fm101.output[t] |= HT1   << 21;
    fm101.output[t] |= SumNB << 25;
    fm101.output[t] |= BS0   << 30;
    fm101.output[t] |= BS1   << 31;
    break;
  }
}
