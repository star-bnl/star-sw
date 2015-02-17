#ifndef FMS_FM005_2015_A_HH
#define FMS_FM005_2015_A_HH

#include "Board.hh"

void fms_fm005_2015_a(Board& fm005, int t=MAXPP, int simdat=0);

int getFM005_BS3(int out);    
int getFM005_BS2(int out);    
int getFM005_BS1GHIJ(int out);   
int getFM005_J23(int out);
int getFM005_JpGH(int out);    
int getFM005_JpIJ(int out);    

#endif
