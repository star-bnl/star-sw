#ifndef St_tpcAcChargeC_h
#define St_tpcAcChargeC_h

#include "TChair.h"
#include "tables/St_tpcAcCharge_Table.h"

class St_tpcAcChargeC : public TChair {
 public:
  static St_tpcAcChargeC* 	instance();
  tpcAcCharge_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcAcCharge*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t 	chargeI(Int_t i = 0) 	const {return Struct(i)->chargeI;}
  Float_t 	chargeO(Int_t i = 0) 	const {return Struct(i)->chargeO;}
 protected:
  St_tpcAcChargeC(St_tpcAcCharge *table=0) : TChair(table) {}
  virtual ~St_tpcAcChargeC() {fgInstance = 0;}
 private:
  static St_tpcAcChargeC* fgInstance;
  ClassDefChair(St_tpcAcCharge, tpcAcCharge_st )
  ClassDef(St_tpcAcChargeC,1) //C++ TChair for tpcAcCharge table class
};
#endif
