#ifndef St_GatingGridC_h
#define St_GatingGridC_h

#include "TChair.h"
#include "tables/St_GatingGrid_Table.h"

class St_GatingGridC : public TChair {
 public:
  static St_GatingGridC* 	instance();
  GatingGrid_st 	*Struct(Int_t i = 0) 	const {return ((St_GatingGrid*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t 	t0(Int_t i = 0) 	const {return Struct(i)->t0;}
  Float_t 	settingTime(Int_t i = 0) 	const {return Struct(i)->settingTime;}
  Double_t      CalcCorrection(Int_t i, Double_t x);
 protected:
  St_GatingGridC(St_GatingGrid *table=0) : TChair(table) {}
  virtual ~St_GatingGridC() {fgInstance = 0;}
  
 private:
  static St_GatingGridC* fgInstance;
  ClassDefChair(St_GatingGrid, GatingGrid_st )
  ClassDef(St_GatingGridC,1) //C++ TChair for GatingGrid table class
};
#endif
