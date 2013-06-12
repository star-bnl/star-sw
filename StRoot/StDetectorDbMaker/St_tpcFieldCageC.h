#ifndef St_tpcFieldCageC_h
#define St_tpcFieldCageC_h

#include "TChair.h"
#include "tables/St_tpcFieldCage_Table.h"

class St_tpcFieldCageC : public TChair {
 public:
  static St_tpcFieldCageC* 	instance();
  tpcFieldCage_st 	*Struct(Int_t i = 0) 	{return ((St_tpcFieldCage*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Float_t 	innerFieldCageShift(Int_t i = 0){return Struct(i)->innerFieldCageShift;}
  Float_t 	InnerFieldCageShift(Int_t i = 0){return innerFieldCageShift(i);}
  Float_t 	eastClockError(Int_t i = 0) 	{return Struct(i)->eastClockError;}
  Float_t 	EastClockError(Int_t i = 0) 	{return eastClockError(i);}
  Float_t 	westClockError(Int_t i = 0) 	{return Struct(i)->westClockError;}
  Float_t 	WestClockError(Int_t i = 0) 	{return westClockError(i);}
 protected:
  St_tpcFieldCageC(St_tpcFieldCage *table=0) : TChair(table) {}
  virtual ~St_tpcFieldCageC() {fgInstance = 0;}
 private:
  static St_tpcFieldCageC* fgInstance;
  ClassDefChair(St_tpcFieldCage, tpcFieldCage_st )
  ClassDef(St_tpcFieldCageC,1) //C++ TChair for tpcFieldCage table class
};
#endif
