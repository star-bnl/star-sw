#ifndef FMS_FM001_2015_A_HH
#define FMS_FM001_2015_A_HH

struct Board;

void fms_fm001_2015_a(Board& fm001);

int getFM001_BS3(int out);   
int getFM001_BS2(int out);  
int getFM001_BS1A(int out);  
int getFM001_BS1BCD(int out); 
int getFM001_D23(int out);   
int getFM001_JpAB(int out);   
int getFM001_JpCD(int out);   

#endif	// FMS_FM001_2015_A_HH
