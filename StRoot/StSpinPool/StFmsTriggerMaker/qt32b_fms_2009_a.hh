//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 14 Jan 2011
//

#ifndef QT32B_FMS_2009_HH
#define QT32B_FMS_2009_HH

struct Board;

void qt32b_fms_2009_a(Board& qt);

void getQtDaughterSum(int qtout, int* sum);
int getQtHighTowerAdc(int qtout);
int getQtHighTowerId(int qtout);
void getQtSumAndHighTower(int* channels, int* A, int* B, int* C, int* D, int& htadc, int& htid);
void getQtSumAndHighTower(int* channels, int* I, int* J, int& htadc, int& htid);

#endif	// QT32B_FMS_2009_HH
