#ifndef FMS_FM005_2017_A_HH
#define FMS_FM005_2017_A_HH

#include "Board.hh"

void fms_fm005_2017_a(Board& fm005, int t=MAXPP, int simdat=0);

int getFM005_2017a_BS3(int out);    
int getFM005_2017a_BS2(int out);    
int getFM005_2017a_BS1IJ(int out);   
int getFM005_2017a_BS1GH(int out);   
int getFM005_2017a_J23(int out);
int getFM005_2017a_JpGH(int out);    
int getFM005_2017a_JpIJ(int out);    

#endif
