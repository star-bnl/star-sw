#ifndef St_itpcPadGainT0C_h
#define St_itpcPadGainT0C_h

#include "TChair.h"
#include "tables/St_itpcPadGainT0_Table.h"

class St_itpcPadGainT0C : public TChair {
 public:
  static St_itpcPadGainT0C* 	instance();
  itpcPadGainT0_st 	*Struct(Int_t i = 0) 	const {return ((St_itpcPadGainT0*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	run(Int_t i = 0) 	const {return Struct(i)->run;}
  Float_t 	Gain(Int_t sector, Int_t row, Int_t pad) const {
    return ((sector > 0 && sector <= 24) && (row > 0 && row <= 40) && (pad > 0 && pad <= 120)) ?  
      Struct()->Gain[sector-1][row-1][pad-1] : 0;
  }
  Float_t 	  T0(Int_t sector, Int_t row, Int_t pad) const {
    return ((sector > 0 && sector <= 24) && (row > 0 && row <= 40) && (pad > 0 && pad <= 120)) ?  
      Struct()->T0[sector-1][row-1][pad-1] : 0;
  }
  Bool_t    livePadrow(Int_t sector, Int_t row) {
    for (Int_t pad=1; pad<=120; pad++) if (Gain(sector,row,pad)>0) return kTRUE;
    return kFALSE;
  }
 protected:
  St_itpcPadGainT0C(St_itpcPadGainT0 *table=0) : TChair(table) {}
  virtual ~St_itpcPadGainT0C() {fgInstance = 0;}
 private:
  static St_itpcPadGainT0C* fgInstance;
  ClassDefChair(St_itpcPadGainT0, itpcPadGainT0_st )
  ClassDef(St_itpcPadGainT0C,1) //C++ TChair for itpcPadGainT0 table class
};
#endif
