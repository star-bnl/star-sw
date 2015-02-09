#ifndef FMS_FM103_2015_A_HH
#define FMS_FM103_2015_A_HH

struct Board;

void fms_fm103_2015_a(Board& fm103);

int getFM103_BS3(int out);
int getFM103_BS2(int out);
int getFM103_BS1T(int out);
int getFM103_BS1M(int out);
int getFM103_BS1B(int out);
int getFM103_JpT(int out);
int getFM103_JpM(int out);
int getFM103_JpB(int out);

#endif	// FMS_FM103_2015_A_HH
