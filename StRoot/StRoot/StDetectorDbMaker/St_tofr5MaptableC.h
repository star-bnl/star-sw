#ifndef St_tofr5MaptableC_h
#define St_tofr5MaptableC_h

#include "TChair.h"
#include "tables/St_tofr5Maptable_Table.h"

class St_tofr5MaptableC : public TChair {
 public:
  static St_tofr5MaptableC* 	instance();
  tofr5Maptable_st 	*Struct(Int_t i = 0) 	const {return ((St_tofr5Maptable*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  short* 	tdigchan(Int_t i = 0) 	const {return Struct(i)->tdigchan;}
  short* 	modulechan(Int_t i = 0) 	const {return Struct(i)->modulechan;}
 protected:
  St_tofr5MaptableC(St_tofr5Maptable *table=0) : TChair(table) {}
  virtual ~St_tofr5MaptableC() {fgInstance = 0;}
 private:
  static St_tofr5MaptableC* fgInstance;
  ClassDefChair(St_tofr5Maptable, tofr5Maptable_st )
  ClassDef(St_tofr5MaptableC,1) //C++ TChair for tofr5Maptable table class
};
#endif
