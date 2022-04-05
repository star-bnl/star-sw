#ifndef FMS_FM101_2015_B_HH
#define FMS_FM101_2015_B_HH

#include "Board.hh"

void fms_fm101_2015_b(Board& fm101, int t=MAXPP, int simdat=0);

int getFM101_2015b_BS3(int out);
int getFM101_2015b_BS2(int out);
int getFM101_2015b_BS1T(int out);
int getFM101_2015b_BS1M(int out);
int getFM101_2015b_BS1B(int out);
int getFM101_2015b_JpT(int out);
int getFM101_2015b_JpM(int out);
int getFM101_2015b_JpB(int out);

#endif	// FMS_FM101_2015_B_HH
