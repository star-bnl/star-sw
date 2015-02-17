//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 14 Jan 2011
//

#ifndef L1_FP201_2011_A_HH
#define L1_FP201_2011_A_HH

#include "Board.hh"

void l1_fp201_2011_a(Board& fp201, int t=MAXPP);

void computeJetPatchSums(const Board& fp201, 
			 int& SumST, int& SumSB, int& SumNT, int& SumNB,
			 int t=MAXPP);

#endif	// L1_FP201_2011_A_HH
