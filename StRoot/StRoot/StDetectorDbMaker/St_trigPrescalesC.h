#ifndef St_trigPrescalesC_h
#define St_trigPrescalesC_h

#include "TChair.h"
#include "tables/St_trigPrescales_Table.h"

class St_trigPrescalesC : public TChair {
 public:
  static St_trigPrescalesC* 	instance();
  trigPrescales_st *Struct(Int_t i = 0) {return ((St_trigPrescales*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()            {return GetNRows();}
  Int_t 	runNumber(Int_t i = 0) 	{return Struct(i)->runNumber;}
  Int_t 	idxTrigger(Int_t i = 0) {return Struct(i)->idxTrigger;}
  Int_t 	idxLevel(Int_t i = 0) 	{return Struct(i)->idxLevel;}
  Int_t 	id(Int_t i = 0) 	{return Struct(i)->id;}
  Float_t 	ps(Int_t i = 0) 	{return Struct(i)->ps;}
 protected:
  St_trigPrescalesC(St_trigPrescales *table=0) : TChair(table) {}
  virtual ~St_trigPrescalesC() {if (Table()->IsMarked()) delete GetThisTable(); fgInstance = 0;}
 private:
  static St_trigPrescalesC* fgInstance;
  ClassDefChair(St_trigPrescales, trigPrescales_st )
  ClassDef(St_trigPrescalesC,1) //C++ TChair for trigPrescales table class
};
#endif
