#ifndef St_tpcGainC_h
#define St_tpcGainC_h
#include "TChair.h"
#include "tables/St_tpcGain_Table.h"

class St_tpcGainC : public TChair {
 public:
  static St_tpcGainC* 	instance();
  tpcGain_st  *Struct(Int_t i = 0) {return ((St_tpcGain*) Table())->GetTable()+i;}
  Float_t   Gain(Int_t sector, Int_t row, Int_t pad) {return Struct(sector-1)->Gain[row-1][pad-1];}
 protected:
  St_tpcGainC(St_tpcGain *table=0) : TChair(table) {}
  virtual ~St_tpcGainC() {fgInstance = 0;}
 private:
  static St_tpcGainC* fgInstance;
  ClassDefChair(St_tpcGain, tpcGain_st )
  ClassDef(St_tpcGainC,1) //C++ TChair for tpcGain table class
};

#endif
