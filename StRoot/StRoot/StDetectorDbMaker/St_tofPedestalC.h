#ifndef St_tofPedestalC_h
#define St_tofPedestalC_h

#include "TChair.h"
#include "tables/St_tofPedestal_Table.h"

class St_tofPedestalC : public TChair {
 public:
  static St_tofPedestalC* 	instance();
  tofPedestal_st 	*Struct(Int_t i = 0) 	const {return ((St_tofPedestal*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	entries(Int_t i = 0) 	const {return Struct(i)->entries;}
  short* 	daqChannel(Int_t i = 0) 	const {return Struct(i)->daqChannel;}
  short* 	adcChan(Int_t i = 0) 	const {return Struct(i)->adcChan;}
  short* 	adcPedestal(Int_t i = 0) 	const {return Struct(i)->adcPedestal;}
 protected:
  St_tofPedestalC(St_tofPedestal *table=0) : TChair(table) {}
  virtual ~St_tofPedestalC() {fgInstance = 0;}
 private:
  static St_tofPedestalC* fgInstance;
  ClassDefChair(St_tofPedestal, tofPedestal_st )
  ClassDef(St_tofPedestalC,1) //C++ TChair for tofPedestal table class
};
#endif
