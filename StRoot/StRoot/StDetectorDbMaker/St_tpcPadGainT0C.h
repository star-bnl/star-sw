#ifndef St_tpcPadGainT0C_h
#define St_tpcPadGainT0C_h

#include "TChair.h"
#include "tables/St_tpcPadGainT0_Table.h"
#include "St_tpcPadConfigC.h"
class St_tpcPadGainT0C : public TChair {
 public:
  static St_tpcPadGainT0C* 	instance();
  tpcPadGainT0_st 	*Struct() 	const {return ((St_tpcPadGainT0 *) Table())->GetTable();}
  Int_t 	run()           	const {return Struct()->run;}
  Float_t 	Gain(Int_t sector, Int_t row, Int_t pad) const {
    Float_t gain = 0;
    if ((sector > 0 && sector <= 24) && (row > 0 && row <= St_tpcPadConfigC::instance()->padRows(sector)) && (pad > 0 && pad <= 182)) { 
      gain = Struct()->Gain[sector-1][row-1][pad-1];
    }
    return gain;
  }
  Float_t 	  T0(Int_t sector, Int_t row, Int_t pad) const {
    Float_t t0 = 0;
    if ((sector > 0 && sector <= 24) && (row > 0 && row <= St_tpcPadConfigC::instance()->padRows(sector)) && (pad > 0 && pad <= 182)) {
      t0 = Struct()->T0[sector-1][row-1][pad-1];
    }
    return t0;
  }
  Bool_t    livePadrow(Int_t sector, Int_t row) {
    for (Int_t pad=1; pad<=182; pad++) if (Gain(sector,row,pad)>0) return kTRUE;
    return kFALSE;
  }
 protected:
  St_tpcPadGainT0C(St_tpcPadGainT0 *table=0) : TChair(table) {}
  virtual ~St_tpcPadGainT0C() {fgInstance = 0;}
 private:
  static St_tpcPadGainT0C* fgInstance;
  ClassDefChair(St_tpcPadGainT0, tpcPadGainT0_st )
  ClassDef(St_tpcPadGainT0C,1) //C++ TChair for tpcPadGainT0 table class
};
#endif
