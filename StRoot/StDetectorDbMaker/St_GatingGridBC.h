#ifndef St_GatingGridBC_h
#define St_GatingGridBC_h

#include "St_tpcCorrectionC.h"

class St_GatingGridBC : public St_tpcCorrectionC {
 public:
  static St_GatingGridBC* 	instance();
  Float_t 	t0(Int_t i = 0) 	        const {return Struct(i)->a[0];}
  Float_t 	settingTime(Int_t i = 0) 	const {return Struct(i)->a[1];}
  Double_t      CalcCorrection(Int_t i, Double_t x);
 protected:
  St_GatingGridBC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_GatingGridBC() {fgInstance = 0;}
  
 private:
  static St_GatingGridBC* fgInstance;
  ClassDef(St_GatingGridBC,1) //C++ St_tpcCorrectionC for GatingGridB table class
};
#endif
