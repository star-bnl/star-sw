#ifndef St_tpcGainC_h
#define St_tpcGainC_h
#include "TChair.h"
#include "tables/St_tpcGain_Table.h"

class St_tpcGainC : public TChair {
 public:
  St_tpcGainC (St_tpcGain *table=0) : TChair(table) {}
  Float_t Gain(Int_t sector, Int_t row, Int_t pad);
  ClassDefChair(St_tpcGain, tpcGain_st )
  ClassDef(St_tpcGainC,1) //C++ TChair for tpcGain table class
};
#endif
