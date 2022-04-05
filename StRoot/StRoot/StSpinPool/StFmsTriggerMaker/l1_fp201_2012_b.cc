//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 13 Feb 2012
//

#include "bits.hh"
#include "l1_fp201_2012_b.hh"
#include <stdio.h>

void l1_fp201_2012_b(Board& fp201, int t){
  int* channels = (int*)fp201.channels[t];

  const int R0 = fp201.registers[0]; // FMS-JP0
  const int R1 = fp201.registers[1]; // FMS-JP1
  const int R2 = fp201.registers[2]; // FMS-JP2
  const int R3 = fp201.registers[3]; // FMS-layer2-mode: 0=data taking, 1=debug
  const int R4 = fp201.registers[4]; // FMS-combo1-enable
  const int R5 = fp201.registers[5]; // FMS-combo2-enable

  int fm101out = channels[0];   // small cells
  int fm102out = channels[1];   // large cells south
  int fm103out = channels[2];   // large cells north
  int fe101out = channels[7];   // FE101, FPE

  // Combine (OR) the two FPE bits
  int fpe0 = btest(fe101out,0);
  int fpe1 = btest(fe101out,1);
  int fpe  = fpe0|fpe1;

  // If the algorithm is in debug mode (R3=1) then delay the HT bits from the small
  // cell array to the 3rd step. Otherwise (R3=0), just zero them out.
  int HT0 = btest(fm102out,27) | btest(fm103out,27);
  int HT1 = btest(fm102out,28) | btest(fm103out,28);

  if (R3 == 1) {
    HT0 |= btest(fm101out,20);
    HT1 |= btest(fm101out,21);
  }

  // Pass the small cell BSum0 and BSum1 bits to the TCU (3 bits:
  // SBsum0 and SBsum1)
  int SBS0 = btest(fm101out,30);
  int SBS1 = btest(fm101out,31);

  // OR the large cell BSum0 bits from the two large-cell Layer-1 DSM
  // boards together, and likewise for the BSum1 and BSum2 bits. Output
  // the results to the TCU (3 bits: LBsum0, LBsum1, LBsum2)
  int LBS0 = btest(fm102out,24) | btest(fm103out,24);
  int LBS1 = btest(fm102out,25) | btest(fm103out,25);
  int LBS2 = btest(fm102out,26) | btest(fm103out,26);

  // Make the six overlapping jet patches by adding together the small
  // and large cell sums for each quadrant
  int SumST, SumSB, SumNT, SumNB, SumS, SumN;
  computeJetPatchSums(fp201,SumST,SumSB,SumNT,SumNB,SumS,SumN);

  // OR the JP0 bits from the four quadrants together, and likewise
  // for the JP1 and JP2 bits. Output the results to the TCU (3 bits:
  // JP0, JP1, JP2)
  int JP0 = SumST > R0 || SumSB > R0 || SumNT > R0 || SumNB > R0 || SumS > R0;
  int JP1 = SumST > R1 || SumSB > R1 || SumNT > R1 || SumNB > R1 || SumS > R1;
  int JP2 = SumST > R2 || SumSB > R2 || SumNT > R2 || SumNB > R2 || SumS > R2;

  if (R3 == 0) {
    JP0 |= SumN > R0;
    JP1 |= SumN > R1;
    JP2 |= SumN > R2;
  }

  // If two or more quadrants satisfy the JP0 threshold, set the
  // "di-jet" bit. Output it to the TCU (1 bit: Di-jet)
  int dijet = (((SumST > R0) && ((SumSB > R0)  || (SumNT > R0)   || (SumNB > R0))) ||
               ((SumSB > R0) && ((SumNT > R0)  || (SumNB > R0))) ||
               ((SumNT > R0) &&  (SumNB > R0)) ||
	       ((SumN  > R0) &&  (SumST > R0)) ||
	       ((SumN  > R0) &&  (SumS  > R0)) ||
	       ((SumN  > R0) &&  (SumSB > R0)) ||
	       ((SumS  > R0) &&  (SumNT > R0)) ||
	       ((SumS  > R0) &&  (SumNB > R0)));

  int combo1 = ((SBS0  && btest(R4,0)) ||
		(SBS1  && btest(R4,1)) ||
		(LBS0  && btest(R4,2)) ||
		(LBS1  && btest(R4,3)) ||
		(LBS2  && btest(R4,4)) ||
		(JP0   && btest(R4,5)) ||
		(JP1   && btest(R4,6)) ||
		(JP2   && btest(R4,7)) ||
		(dijet && btest(R4,8)) ||
		(fpe   && btest(R4,9)));

  int combo2 = ((SBS0  && btest(R5,0)) ||
		(SBS1  && btest(R5,1)) ||
		(LBS0  && btest(R5,2)) ||
		(LBS1  && btest(R5,3)) ||
		(LBS2  && btest(R5,4)) ||
		(JP0   && btest(R5,5)) ||
		(JP1   && btest(R5,6)) ||
		(JP2   && btest(R5,7)) ||
		(dijet && btest(R5,8)) ||
		(fpe   && btest(R5,9)));

  fp201.output[t] = 0;

  fp201.output[t] |= HT0    << 0;
  fp201.output[t] |= HT1    << 1;
  fp201.output[t] |= SBS0   << 2;
  fp201.output[t] |= SBS1   << 3;
  fp201.output[t] |= LBS0   << 5;
  fp201.output[t] |= LBS1   << 6;
  fp201.output[t] |= LBS2   << 7;
  fp201.output[t] |= JP0    << 8;
  fp201.output[t] |= JP1    << 9;
  fp201.output[t] |= JP2    << 10;
  fp201.output[t] |= dijet  << 11;
  fp201.output[t] |= combo1 << 12;
  fp201.output[t] |= combo2 << 13;
  fp201.output[t] |= fpe    << 14;
}

void computeJetPatchSums(const Board& fp201, int& SumST, int& SumSB, int& SumNT, int& SumNB, int& SumS, int& SumN, int t)
{
  int* channels = (int*)fp201.channels[t];

  int fm101out = channels[0];   // small cells
  int fm102out = channels[1];   // large cells south
  int fm103out = channels[2];   // large cells north

  // small cells
  int SumSmST = getbits(fm101out,0 ,5); // south-top
  int SumSmS  = getbits(fm101out,5 ,5); // south
  int SumSmSB = getbits(fm101out,10,5); // south-bottom
  int SumSmNT = getbits(fm101out,15,5); // north-top
  int SumSmN  = getbits(fm101out,20,5); // north
  int SumSmNB = getbits(fm101out,25,5); // north-bottom

  // large cells
  int SumLgST = getbits(fm102out,0 ,5); // south-top
  int SumLgS  = getbits(fm102out,5 ,5); // south
  int SumLgSB = getbits(fm102out,10,5); // south-bottom
  int SumLgNT = getbits(fm103out,0 ,5); // north-top
  int SumLgN  = getbits(fm103out,5 ,5); // north
  int SumLgNB = getbits(fm103out,10,5); // north-bottom

  // jet patch sums
  SumST = SumSmST + SumLgST; // south-top
  SumSB = SumSmSB + SumLgSB; // south-bottom
  SumNT = SumSmNT + SumLgNT; // north-top
  SumNB = SumSmNB + SumLgNB; // north-bottom
  SumS  = SumSmS+SumLgS; // south
  SumN  = SumSmN+SumLgN; // north
}
