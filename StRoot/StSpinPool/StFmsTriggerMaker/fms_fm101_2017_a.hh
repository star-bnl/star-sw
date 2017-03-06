#ifndef FMS_FM101_2017_A_HH
#define FMS_FM101_2017_A_HH

#include "Board.hh"

void fms_fm101_2017_a(Board& fm101, int t=MAXPP, int simdat=0);

int getFM101_2017a_BS3(int out);
int getFM101_2017a_BS2(int out);
int getFM101_2017a_BS1T(int out);
int getFM101_2017a_BS1M(int out);
int getFM101_2017a_BS1B(int out);
int getFM101_2017a_JpT(int out);
int getFM101_2017a_JpM(int out);
int getFM101_2017a_JpB(int out);

#endif	// FMS_FM101_2017_A_HH
