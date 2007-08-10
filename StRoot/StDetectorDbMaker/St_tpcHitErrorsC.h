#ifndef St_tpcHitErrorsC_h
#define St_tpcHitErrorsC_h

#include "TChair.h"
#include "tables/St_tpcHitErrors_Table.h"

class St_tpcHitErrorsC : public TChair {
 public:
  static St_tpcHitErrorsC* 	instance();
  tpcHitErrors_st 	*Struct(Int_t i = 0) 	    {return ((St_tpcHitErrors*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	    {return GetNRows();}
  Float_t 	sig2_intrinsic_outer_x(Int_t i = 0) {return Struct(i)->sig2_intrinsic_outer_x;}
  Float_t 	sig2_drift_outer_x(Int_t i = 0)     {return Struct(i)->sig2_drift_outer_x;}
  Float_t 	sig2_tan_outer_x(Int_t i = 0) 	    {return Struct(i)->sig2_tan_outer_x;}
  Float_t 	sig2_intrinsic_outer_z(Int_t i = 0) {return Struct(i)->sig2_intrinsic_outer_z;}
  Float_t 	sig2_drift_outer_z(Int_t i = 0)     {return Struct(i)->sig2_drift_outer_z;}
  Float_t 	sig2_tan_outer_z(Int_t i = 0) 	    {return Struct(i)->sig2_tan_outer_z;}
  Float_t 	sig2_intrinsic_inner_x(Int_t i = 0) {return Struct(i)->sig2_intrinsic_inner_x;}
  Float_t 	sig2_drift_inner_x(Int_t i = 0)     {return Struct(i)->sig2_drift_inner_x;}
  Float_t 	sig2_tan_inner_x(Int_t i = 0) 	    {return Struct(i)->sig2_tan_inner_x;}
  Float_t 	sig2_intrinsic_inner_z(Int_t i = 0) {return Struct(i)->sig2_intrinsic_inner_z;}
  Float_t 	sig2_drift_inner_z(Int_t i = 0)     {return Struct(i)->sig2_drift_inner_z;}
  Float_t 	sig2_tan_inner_z(Int_t i = 0) 	    {return Struct(i)->sig2_tan_inner_z;}
 protected:
  St_tpcHitErrorsC(St_tpcHitErrors *table=0) : TChair(table) {}
  virtual ~St_tpcHitErrorsC() {fgInstance = 0;}
 private:
  static St_tpcHitErrorsC* fgInstance;
  ClassDefChair(St_tpcHitErrors, tpcHitErrors_st )
  ClassDef(St_tpcHitErrorsC,1) //C++ TChair for tpcHitErrors table class
};
#endif
