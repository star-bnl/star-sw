#ifndef St_additionalTriggerIDC_h
#define St_additionalTriggerIDC_h

#include "TChair.h"
#include "tables/St_additionalTriggerID_Table.h"

class St_additionalTriggerIDC : public TChair {
 public:
  static St_additionalTriggerIDC* 	instance();
  additionalTriggerID_st *Struct(Int_t i = 0) 	{return ((St_additionalTriggerID*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  UInt_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  UInt_t 	eventNumber(Int_t i = 0) 	{return Struct(i)->eventNumber;}
  UInt_t 	idxTrg(Int_t i = 0) 	        {return Struct(i)->idxTrg;}
  UInt_t 	daqTrgId(Int_t i = 0) 	        {return Struct(i)->daqTrgId;}
  UInt_t 	offlineTrgId(Int_t i = 0) 	{return Struct(i)->offlineTrgId;}
  UInt_t 	trgNameVersion(Int_t i = 0) 	{return Struct(i)->trgNameVersion;}
  UInt_t 	trgVersion(Int_t i = 0) 	{return Struct(i)->trgVersion;}
  UInt_t 	threashVersion(Int_t i = 0) 	{return Struct(i)->threashVersion;}
  UInt_t 	psVersion(Int_t i = 0) 	        {return Struct(i)->psVersion;}
 protected:
  St_additionalTriggerIDC(St_additionalTriggerID *table=0) : TChair(table) {}
  virtual ~St_additionalTriggerIDC() {if (Table()->IsMarked()) delete GetThisTable(); fgInstance = 0;}
 private:
  static St_additionalTriggerIDC* fgInstance;
  ClassDefChair(St_additionalTriggerID, additionalTriggerID_st )
  ClassDef(St_additionalTriggerIDC,1) //C++ TChair for additionalTriggerID table class
};
#endif
