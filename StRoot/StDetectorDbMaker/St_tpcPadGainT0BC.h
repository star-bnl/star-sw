#ifndef St_tpcPadGainT0BC_h
#define St_tpcPadGainT0BC_h

#include "TChair.h"
#include "tables/St_tpcPadGainT0B_Table.h"
#include "St_tpcPadConfigC.h"
class St_tpcPadGainT0BC : public TChair {
 public:
  static St_tpcPadGainT0BC* 	instance();
  tpcPadGainT0B_st 	*Struct(Int_t i=0) 	const {return ((St_tpcPadGainT0B *) Table())->GetTable()+i;}
  Int_t 	run()           	const {return Struct()->run;}
  Float_t 	Gain(Int_t sector, Int_t row, Int_t pad) const {
    return ((sector > 0 && sector <= 24) && (row > 0 && row <= St_tpcPadConfigC::instance()->padRows(sector)) && (pad > 0 && pad <= 182)) ?
      Struct(sector-1)->Gain[row-1][pad-1] : 0;
  }
  Float_t 	  T0(Int_t sector, Int_t row, Int_t pad) const {
    return ((sector > 0 && sector <= 24) && (row > 0 && row <= St_tpcPadConfigC::instance()->padRows(sector)) && (pad > 0 && pad <= 182)) ?
      Struct(sector-1)->T0[row-1][pad-1] : 0;
  }
  Bool_t    livePadrow(Int_t sector, Int_t row) {
    for (Int_t pad=1; pad<=182; pad++) if (Gain(sector,row,pad)>0) return kTRUE;
    return kFALSE;
  }
 protected:
  St_tpcPadGainT0BC(St_tpcPadGainT0B *table=0) : TChair(table) {}
  virtual ~St_tpcPadGainT0BC() {fgInstance = 0;}
 private:
  static St_tpcPadGainT0BC* fgInstance;
  ClassDefChair(St_tpcPadGainT0B, tpcPadGainT0B_st )
  ClassDef(St_tpcPadGainT0BC,1) //C++ TChair for tpcPadGainT0B table class
};
#endif
