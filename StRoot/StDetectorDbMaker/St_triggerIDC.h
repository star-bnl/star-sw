#ifndef St_triggerIDC_h
#define St_triggerIDC_h

#include "TChair.h"
#include "tables/St_triggerID_Table.h"

class St_triggerIDC : public TChair {
 public:
  static St_triggerIDC* 	instance();
  triggerID_st 	*Struct(Int_t i = 0)     	{return ((St_triggerID*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  UInt_t 	idxTrg(Int_t i = 0) 	        {return Struct(i)->idxTrg;}
  UInt_t 	daqTrgId(Int_t i = 0) 	        {return Struct(i)->daqTrgId;}
  UInt_t 	offlineTrgId(Int_t i = 0) 	{return Struct(i)->offlineTrgId;}
  UInt_t 	trgNameVersion(Int_t i = 0) 	{return Struct(i)->trgNameVersion;}
  UInt_t 	trgVersion(Int_t i = 0) 	{return Struct(i)->trgVersion;}
  UInt_t 	threashVersion(Int_t i = 0) 	{return Struct(i)->threashVersion;}
  UInt_t 	psVersion(Int_t i = 0) 	        {return Struct(i)->psVersion;}
 protected:
  St_triggerIDC(St_triggerID *table=0) : TChair(table) {}
  virtual ~St_triggerIDC() {fgInstance = 0;}
 private:
  static St_triggerIDC* fgInstance;
  ClassDefChair(St_triggerID, triggerID_st )
  ClassDef(St_triggerIDC,1) //C++ TChair for triggerID table class
};
#endif
