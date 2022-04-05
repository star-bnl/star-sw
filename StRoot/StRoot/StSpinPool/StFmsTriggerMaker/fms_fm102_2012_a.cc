//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 9 Feb 2012
//

#include "bits.hh"
#include "fms_fm102_2012_a.hh"

void fms_fm102_2012_a(Board& fm102, int t){
  int* channels = (int*)fm102.channels[t];

  // Board sum thresholds
  const int R0 = fm102.registers[0]; // FMSlarge-cluster-th0
  const int R1 = fm102.registers[1]; // FMSlarge-cluster-th1
  const int R2 = fm102.registers[2]; // FMSlarge-cluster-th2

  // High tower bits
  int HT0 = 0;
  int HT1 = 0;

  // Board sum bits
  int BS0 = 0;
  int BS1 = 0;
  int BS2 = 0;

  // Quadrant sums
  int Sum[4];

  for (int i = 0; i < 4; i += 2) {
    int fm005out = channels[i];
    int fm006out = channels[i+1];

    // OR the HT0 bits from the four Layer-0 DSM boards together, and
    // likewise for the HT1 bits. Output the results to the Layer-2 DSM (2 bits)
    HT0 |= btest(fm005out,30);
    HT0 |= btest(fm006out,30);

    HT1 |= btest(fm005out,31);
    HT1 |= btest(fm006out,31);

    // Compare the 18 5-bit Sum values to three thresholds (BSum0,
    // BSum1, BSum2). OR the results together, and output them to the
    // Layer-2 DSM (3 bits, not muxed)
    int SumH  = getbits(fm005out,0 ,5);
    int SumGH = getbits(fm005out,5 ,5);
    int SumG  = getbits(fm005out,10,5);
    int SumF  = getbits(fm005out,15,5);
    int SumEF = getbits(fm005out,20,5);
    int SumE  = getbits(fm005out,25,5);

    int SumJ  = getbits(fm006out,15,5);
    int SumIJ = getbits(fm006out,20,5);
    int SumI  = getbits(fm006out,25,5);

    BS0 |= SumE > R0 || SumEF > R0 || SumF > R0 || SumG > R0 || SumGH > R0 || SumH > R0;
    BS1 |= SumE > R1 || SumEF > R1 || SumF > R1 || SumG > R1 || SumGH > R1 || SumH > R1;
    BS2 |= SumE > R2 || SumEF > R2 || SumF > R2 || SumG > R2 || SumGH > R2 || SumH > R2;
    BS0 |= SumI > R0 || SumIJ > R0 || SumJ > R0;
    BS1 |= SumI > R1 || SumIJ > R1 || SumJ > R1;
    BS2 |= SumI > R2 || SumIJ > R2 || SumJ > R2;

    // Compute the 8-bit total SumE+SumF+SumG+SumH+SumI+SumJ for each of
    // the two quadrants
    Sum[i  ] = SumE+SumF+SumG+SumH+SumI+SumJ;
    Sum[i+1] = SumH+SumI+SumJ;
  }

  // Extract the 5 least significant bits from each quadrant sum. Set the result to 31
  // (i.e. binary 11111) if the most significant bit is set.
  int SumT = Sum[0];
  int SumB = Sum[2];
  int SumS = Sum[1]+Sum[3];

  if (SumT > 31) SumT = 31;
  if (SumB > 31) SumB = 31;
  if (SumS > 31) SumS = 31;

  fm102.output[t] = 0;

  fm102.output[t] |= SumT << 0;
  fm102.output[t] |= SumS << 5;
  fm102.output[t] |= SumB << 10;
  fm102.output[t] |= BS0  << 24;
  fm102.output[t] |= BS1  << 25;
  fm102.output[t] |= BS2  << 26;
  fm102.output[t] |= HT0  << 27;
  fm102.output[t] |= HT1  << 28;
}
