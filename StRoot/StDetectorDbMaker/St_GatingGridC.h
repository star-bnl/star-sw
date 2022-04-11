#ifndef St_GatingGridC_h
#define St_GatingGridC_h

#include "St_tpcCorrectionC.h"

class St_GatingGridC : public St_tpcCorrectionC {
 public:
  static St_GatingGridC* 	instance();
  Float_t 	t0(Int_t i = 0) 	        const {return Struct(i)->a[0];}
  Float_t 	settingTime(Int_t i = 0) 	const {return Struct(i)->a[1];}
  Double_t      CalcCorrection(Int_t i, Double_t x);
 protected:
  St_GatingGridC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_GatingGridC() {fgInstance = 0;}
  
 private:
  static St_GatingGridC* fgInstance;
  ClassDef(St_GatingGridC,1) //C++ St_tpcCorrectionC for GatingGrid table class
};
#endif
