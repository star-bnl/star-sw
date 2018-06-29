#ifndef St_tpcPadGainT0BC_h
#define St_tpcPadGainT0BC_h
#include "TObject.h"
#include "St_tpcPadGainT0C.h"
#include "St_itpcPadGainT0C.h"
#include "St_tpcPadConfigC.h"
class St_tpcPadGainT0BC : public TObject {
 public:
  static St_tpcPadGainT0BC* 	instance();
  Float_t 	Gain(Int_t sector, Int_t row, Int_t pad) const;
  Float_t 	  T0(Int_t sector, Int_t row, Int_t pad) const;
  Bool_t    livePadrow(Int_t sector, Int_t row) const;
 protected:
  St_tpcPadGainT0BC() {}
  virtual ~St_tpcPadGainT0BC() {fgInstance = 0;}
 private:
  static St_tpcPadGainT0BC* fgInstance;
  ClassDef(St_tpcPadGainT0BC,0) //C++ TChair for tpcPadGainT0B table class
};
#endif
