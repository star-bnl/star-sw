#ifndef St_tpcAvCurrentC_h
#define St_tpcAvCurrentC_h

#include "TChair.h"
#include "tables/St_tpcAvCurrent_Table.h"

class St_tpcAvCurrentC : public TChair {
 public:
  static St_tpcAvCurrentC* 	instance();
  tpcAvCurrent_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcAvCurrent*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	run(Int_t i = 0) 	const {return Struct(i)->run;}
  Float_t 	currentI(Int_t i = 0) 	const {return Struct(i)->currentI;}
  Float_t 	currentO(Int_t i = 0) 	const {return Struct(i)->currentO;}
  Float_t 	chargeI(Int_t i = 0) 	const {return Struct(i)->chargeI;}
  Float_t 	chargeO(Int_t i = 0) 	const {return Struct(i)->chargeO;}
 protected:
  St_tpcAvCurrentC(St_tpcAvCurrent *table=0) : TChair(table) {}
  virtual ~St_tpcAvCurrentC() {fgInstance = 0;}
 private:
  static St_tpcAvCurrentC* fgInstance;
  ClassDefChair(St_tpcAvCurrent, tpcAvCurrent_st )
  ClassDef(St_tpcAvCurrentC,1) //C++ TChair for tpcAvCurrent table class
};
#endif
