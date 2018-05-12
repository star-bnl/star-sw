#ifndef St_tpcPadGainT0BC_h
#define St_tpcPadGainT0BC_h
#include "TObject.h"
#include "St_tpcPadGainT0C.h"
#include "St_itpcPadGainT0C.h"
#include "St_tpcPadConfigC.h"
class St_tpcPadGainT0BC : public TObject {
 public:
  static St_tpcPadGainT0BC* 	instance();
  Float_t 	Gain(Int_t sector, Int_t row, Int_t pad) const {
    if (St_tpcPadConfigC::instance()->iTPC(sector)) {
      if (row <= 40) return St_itpcPadGainT0C::instance()->Gain(sector,row,pad);
      St_tpcPadGainT0C::instance()->Gain(sector,row-40,pad);
    }
    return St_tpcPadGainT0C::instance()->Gain(sector,row,pad);
  }
  Float_t 	  T0(Int_t sector, Int_t row, Int_t pad) const {
    if (St_tpcPadConfigC::instance()->iTPC(sector)) {
      if (row <= 40) return St_itpcPadGainT0C::instance()->T0(sector,row,pad);
      St_tpcPadGainT0C::instance()->T0(sector,row-40,pad);
    }
    return St_tpcPadGainT0C::instance()->T0(sector,row,pad);
  }
  Bool_t    livePadrow(Int_t sector, Int_t row) {
    if (St_tpcPadConfigC::instance()->iTPC(sector)) {
      if (row <= 40) return St_itpcPadGainT0C::instance()->livePadrow(sector,row);
      St_tpcPadGainT0C::instance()->livePadrow(sector,row-40);
    }
    return St_tpcPadGainT0C::instance()->livePadrow(sector,row);
  }
 protected:
  St_tpcPadGainT0BC() {}
  virtual ~St_tpcPadGainT0BC() {fgInstance = 0;}
 private:
  static St_tpcPadGainT0BC* fgInstance;
  ClassDef(St_tpcPadGainT0BC,0) //C++ TChair for tpcPadGainT0B table class
};
#endif
