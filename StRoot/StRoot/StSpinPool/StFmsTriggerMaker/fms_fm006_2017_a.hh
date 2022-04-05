#ifndef FMS_FM006_2017_A_HH
#define FMS_FM006_2017_A_HH

#include "Board.hh"

void fms_fm006_2017_a(Board& fm006, int t=MAXPP, int simdat=0);

int getFM006_2017a_BS3(int out); 
int getFM006_2017a_BS2(int out); 
int getFM006_2017a_BS1EF(int out);
int getFM006_2017a_JpEF(int out); 

#endif
