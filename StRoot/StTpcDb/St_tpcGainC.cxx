#include "St_tpcGainC.h"
ClassImp(St_tpcGainC);
Float_t St_tpcGainC::Gain(Int_t sector, Int_t row, Int_t pad) {
  tpcGain_st *t = GetTable(sector-1);
  return t ? t->Gain[row-1][pad-1] : 0;
}
