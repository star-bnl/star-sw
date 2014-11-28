#ifndef St_tpcSectorT0offsetC_h
#define St_tpcSectorT0offsetC_h

#include "TChair.h"
#include "tables/St_tpcSectorT0offset_Table.h"

class St_tpcSectorT0offsetC : public TChair {
 public:
  static St_tpcSectorT0offsetC* 	instance();
  tpcSectorT0offset_st 	*Struct(Int_t i = 0) 	{return ((St_tpcSectorT0offset*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Float_t* 	t0(Int_t i = 0) 	        {return Struct(i)->t0;}
  Float_t       t0offset(Int_t sector=1)        {return t0()[sector-1];}
 protected:
  St_tpcSectorT0offsetC(St_tpcSectorT0offset *table=0) : TChair(table) {}
  virtual ~St_tpcSectorT0offsetC() {fgInstance = 0;}
 private:
  static St_tpcSectorT0offsetC* fgInstance;
  ClassDefChair(St_tpcSectorT0offset, tpcSectorT0offset_st )
  ClassDef(St_tpcSectorT0offsetC,1) //C++ TChair for tpcSectorT0offset table class
};
#endif
