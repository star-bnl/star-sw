#ifndef St_dsmPrescalesC_h
#define St_dsmPrescalesC_h

#include "TChair.h"
#include "tables/St_dsmPrescales_Table.h"

class St_dsmPrescalesC : public TChair {
 public:
  static St_dsmPrescalesC* 	instance();
  dsmPrescales_st 	*Struct(Int_t i = 0) 	{return ((St_dsmPrescales*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Int_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  Int_t 	trgId(Int_t i = 0) 	        {return Struct(i)->trgId;}
  Int_t 	dsmPrescale(Int_t i = 0) 	{return Struct(i)->dsmPrescale;}
 protected:
  St_dsmPrescalesC(St_dsmPrescales *table=0) : TChair(table) {}
  virtual ~St_dsmPrescalesC() {if (Table()->IsMarked()) delete GetThisTable(); fgInstance = 0;}
 private:
  static St_dsmPrescalesC* fgInstance;
  ClassDefChair(St_dsmPrescales, dsmPrescales_st )
  ClassDef(St_dsmPrescalesC,1) //C++ TChair for dsmPrescales table class
};
#endif
