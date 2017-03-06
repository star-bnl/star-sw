#ifndef FMS_FM001_2017_A_HH
#define FMS_FM001_2017_A_HH

#include "Board.hh"

void fms_fm001_2017_a(Board& fm001, int t=MAXPP, int simdat=0);

int getFM001_2017a_BS3(int out);   
int getFM001_2017a_BS2(int out);  
int getFM001_2017a_BS1ABC(int out);  
int getFM001_2017a_BS1CD(int out); 
int getFM001_2017a_D23(int out);   
int getFM001_2017a_JpAB(int out);   
int getFM001_2017a_JpCD(int out);   

#endif	// FMS_FM001_2015_A_HH
