#ifndef St_tofTzeroC_h
#define St_tofTzeroC_h

#include "tables/St_tofTzero_Table.h"
#include "TChair.h"

class St_tofTzeroC : public TChair {
 public:
  static St_tofTzeroC* 	instance();
  tofTzero_st 	*Struct(Int_t i = 0) 	const {return ((St_tofTzero*) Table())->GetTable()+i;}
  Int_t 	entries(Int_t i = 0) 	const {return Struct(i)->entries;}
  short* 	daqChannel(Int_t i = 0) const {return Struct(i)->daqChannel;}
  short* 	tdcChan(Int_t i = 0) 	const {return Struct(i)->tdcChan;}
  Float_t* 	Tzero(Int_t i = 0) 	const {return Struct(i)->Tzero;}
 protected:
  St_tofTzeroC(St_tofTzero *table=0) : TChair(table) {}
  virtual ~St_tofTzeroC() {fgInstance = 0;}
 private:
  static St_tofTzeroC* fgInstance;
  ClassDefChair(St_tofTzero, tofTzero_st )
  ClassDef(St_tofTzeroC,1) //C++ TChair for tofTzero table class
};
#endif
