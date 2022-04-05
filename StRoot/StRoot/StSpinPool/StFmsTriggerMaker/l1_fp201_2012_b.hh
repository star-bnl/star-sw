//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 13 Feb 2012
//

#ifndef L1_FP201_2012_B_HH
#define L1_FP201_2012_B_HH

#include "Board.hh"

void l1_fp201_2012_b(Board& fp201, int t=MAXPP);
void computeJetPatchSums(const Board& fp201, int& SumST, int& SumSB, int& SumNT, int& SumNB, int& SumS, int& SumN, int t=MAXPP);

#endif	// L1_FP201_2012_B_HH
