#ifndef FMS_FM006_2015_A_HH
#define FMS_FM006_2015_A_HH

#include "Board.hh"

void fms_fm006_2015_a(Board& fm006, int t=MAXPP, int simdat=0);

int getFM006_BS3(int out); 
int getFM006_BS2(int out); 
int getFM006_BS1E(int out);
int getFM006_BS1F(int out);
int getFM006_JpEF(int out); 

#endif
