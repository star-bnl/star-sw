#ifndef St_tpcFieldCageShortC_h
#define St_tpcFieldCageShortC_h

#include "TChair.h"
#include "tables/St_tpcFieldCageShort_Table.h"

class St_tpcFieldCageShortC : public TChair {
 public:
  static St_tpcFieldCageShortC* 	instance();
  tpcFieldCageShort_st 	*Struct(Int_t i = 0) 	{return ((St_tpcFieldCageShort*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Float_t 	side(Int_t i = 0) 	        {return Struct(i)->side;}
  Float_t 	cage(Int_t i = 0) 	        {return Struct(i)->cage;}
  Float_t 	ring(Int_t i = 0) 	        {return Struct(i)->ring;}
  Float_t 	resistor(Int_t i = 0) 	        {return Struct(i)->resistor;}
  Float_t 	MissingResistance(Int_t i = 0) 	{return Struct(i)->MissingResistance;}
 protected:
  St_tpcFieldCageShortC(St_tpcFieldCageShort *table=0) : TChair(table) {}
  virtual ~St_tpcFieldCageShortC() {fgInstance = 0;}
 private:
  static St_tpcFieldCageShortC* fgInstance;
  ClassDefChair(St_tpcFieldCageShort, tpcFieldCageShort_st )
  ClassDef(St_tpcFieldCageShortC,1) //C++ TChair for tpcFieldCageShort table class
};
#endif
