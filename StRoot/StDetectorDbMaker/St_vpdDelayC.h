#ifndef St_vpdDelayC_h
#define St_vpdDelayC_h

#include "TChair.h"
#include "tables/St_vpdDelay_Table.h"

class St_vpdDelayC : public TChair {
 public:
  static St_vpdDelayC* 	instance();
  vpdDelay_st 	*Struct(Int_t i = 0) 	const {return ((St_vpdDelay*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t* 	delay(Int_t i = 0) 	const {return Struct(i)->delay;}
 protected:
  St_vpdDelayC(St_vpdDelay *table=0) : TChair(table) {}
  virtual ~St_vpdDelayC() {fgInstance = 0;}
 private:
  static St_vpdDelayC* fgInstance;
  ClassDefChair(St_vpdDelay, vpdDelay_st )
  ClassDef(St_vpdDelayC,1) //C++ TChair for vpdDelay table class
};
#endif
