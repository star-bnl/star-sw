#include "St_tpcGainC.h"
ClassImp(St_tpcGainC);
Float_t St_tpcGainC::Gain(Int_t sector, Int_t row, Int_t pad) {
  tpcGain_st *t = ((St_tpcGain *) Table())->GetTable() + sector-1;
  Float_t gain =  t ? t->Gain[row-1][pad-1] : 0;
  return gain;
}
St_tpcGainC::~St_tpcGainC() {assert(0);}
