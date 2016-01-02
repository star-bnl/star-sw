
#include "run14_gRefMultVPD30VPD5_Weight.h"

void convert()
{
  ofstream fout("weight_grefmult_vpd30_vpd5_Run14.txt");
  for(Int_t j=0; j<680; j++){
    for(Int_t i=0; i<6; i++){
      fout << Form("%20.6f", mgRefMultTriggerCorrDiffVzScaleRatio[i][j]);
    }
    fout << endl;
  }
}


