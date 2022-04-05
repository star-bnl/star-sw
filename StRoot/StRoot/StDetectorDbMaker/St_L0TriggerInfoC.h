#ifndef St_L0TriggerInfoC_h
#define St_L0TriggerInfoC_h

#include "TChair.h"
#include "tables/St_L0TriggerInfo_Table.h"

class St_L0TriggerInfoC : public TChair {
 public:
  static St_L0TriggerInfoC* 	instance();
  L0TriggerInfo_st 	*Struct(Int_t i = 0) 	{return ((St_L0TriggerInfo*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Int_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  Int_t 	daqTriggerId(Int_t i = 0) 	{return Struct(i)->daqTriggerId;}
  Int_t 	offlineTriggerId(Int_t i = 0) 	{return Struct(i)->offlineTriggerId;}
  Int_t 	psL0(Int_t i = 0) 	        {return Struct(i)->psL0;}
  Char_t* 	name(Int_t i = 0) 	        {return Struct(i)->name;}
  UInt_t 	detectorLiveOnBits(Int_t i = 0) {return Struct(i)->detectorLiveOnBits;}
  UInt_t 	detectorLiveOffBits(Int_t i = 0){return Struct(i)->detectorLiveOffBits;}
  UInt_t 	detectorRequest(Int_t i = 0) 	{return Struct(i)->detectorRequest;}
 protected:
  St_L0TriggerInfoC(St_L0TriggerInfo *table=0) : TChair(table) {}
  virtual ~St_L0TriggerInfoC() {fgInstance = 0;}
 private:
  static St_L0TriggerInfoC* fgInstance;
  ClassDefChair(St_L0TriggerInfo, L0TriggerInfo_st )
  ClassDef(St_L0TriggerInfoC,1) //C++ TChair for L0TriggerInfo table class
};
#endif
