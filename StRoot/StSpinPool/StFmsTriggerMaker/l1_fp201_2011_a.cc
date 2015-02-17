//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 14 Jan 2011
//

#include "bits.hh"
#include "l1_fp201_2011_a.hh"

void l1_fp201_2011_a(Board& fp201, int t){
  int* channels = (int*)fp201.channels[t];

  int fm101out = channels[0];   // small cells
  int fm102out = channels[1];   // large cells south
  int fm103out = channels[2];   // large cells north
  int fe101out = channels[7];   // FE101, FPE

  // Combine (OR) the two FPE bits
  int fpe0 = btest(fe101out,0);
  int fpe1 = btest(fe101out,1);
  int fpe  = fpe0|fpe1;

  // OR the HT0 bits from the three Layer-1 DSM boards together, and
  // likewise for the HT1 bits. Note that this combines the small and
  // large cell HT results together. Output the results to the TCU (2
  // bits: HT0, HT1)
  int HT0 = btest(fm101out,27) | btest(fm102out,27) | btest(fm103out,27);
  int HT1 = btest(fm101out,28) | btest(fm102out,28) | btest(fm103out,28);

  // Pass the small cell BSum0, BSum1, BSum2 bits to the TCU (3 bits:
  // SBsum0, SBsum1, SBsum2)
  int SBS0 = btest(fm101out,24);
  int SBS1 = btest(fm101out,25);
  int SBS2 = btest(fm101out,26);

  // OR the large cell BSum0 bits from the two large-cell Layer-1 DSM
  // boards together, and likewise for the BSum1 and BSum2 bits. Output
  // the results to the TCU (3 bits: LBsum0, LBsum1, LBsum2)
  int LBS0 = btest(fm102out,24) | btest(fm103out,24);
  int LBS1 = btest(fm102out,25) | btest(fm103out,25);
  int LBS2 = btest(fm102out,26) | btest(fm103out,26);

  // For each quadrant:
  // -- Addd the 6-bit quadrant sum from the small cells
  // (SumA+SumB+SumC+SumD) to the analogous 6-bit quadrant sum from the
  // large cells (SumE+SumF+SumG+SumH+SumI+SumJ) to produce a 7-bit number
  // -- Compare the 7-bit result to three thresholds JP0, JP1, JP2
  const int R0 = fp201.registers[0];
  const int R1 = fp201.registers[1];
  const int R2 = fp201.registers[2];

  int SumST, SumSB, SumNT, SumNB;
  computeJetPatchSums(fp201,SumST,SumSB,SumNT,SumNB);

  // OR the JP0 bits from the four quadrants together, and likewise
  // for the JP1 and JP2 bits. Output the results to the TCU (3 bits:
  // JP0, JP1, JP2)
  int JP0 = SumST > R0 || SumSB > R0 || SumNT > R0 || SumNB > R0;
  int JP1 = SumST > R1 || SumSB > R1 || SumNT > R1 || SumNB > R1;
  int JP2 = SumST > R2 || SumSB > R2 || SumNT > R2 || SumNB > R2;

  // If two or more quadrants satisfy the JP0 threshold, set the
  // "di-jet" bit. Output it to the TCU (1 bit: Di-jet)
  int dijet = (((SumST > R0) && ((SumSB > R0) || (SumNT > R0) || (SumNB > R0))) ||
               ((SumSB > R0) && ((SumNT > R0) || (SumNB > R0))) ||
               ((SumNT > R0) &&  (SumNB > R0)));

  fp201.output[t] = HT0 | HT1 << 1 | SBS0 << 2 | SBS1 << 3 | SBS2 << 4 | LBS0 << 5 | LBS1 << 6 | LBS2 << 7 | JP0 << 8 | JP1 << 9 | JP2 << 10 | dijet << 11 | fpe << 14;
}

void computeJetPatchSums(const Board& fp201, int& SumST, int& SumSB, int& SumNT, int& SumNB, int t){
  int* channels = (int*)fp201.channels[t];

  int fm101out = channels[0];   // small cells
  int fm102out = channels[1];   // large cells south
  int fm103out = channels[2];   // large cells north

  // small cells
  int SumSmST = getbits(fm101out,0 ,6); // south-top
  int SumSmSB = getbits(fm101out,6 ,6); // south-bottom
  int SumSmNT = getbits(fm101out,12,6); // north-top
  int SumSmNB = getbits(fm101out,18,6); // north bottom

  // large cells
  int SumLgST = getbits(fm102out,0,6); // south-top
  int SumLgSB = getbits(fm102out,6,6); // south-bottom
  int SumLgNT = getbits(fm103out,0,6); // north-top
  int SumLgNB = getbits(fm103out,6,6); // north-bottom

  // jet patch sums
  SumST = SumSmST + SumLgST; // south-top
  SumSB = SumSmSB + SumLgSB; // south-bottom
  SumNT = SumSmNT + SumLgNT; // north-top
  SumNB = SumSmNB + SumLgNB; // north-bottom
}
