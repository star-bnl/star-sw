#ifndef St_triggerInfoC_h
#define St_triggerInfoC_h

#include "TChair.h"
#include "tables/St_triggerInfo_Table.h"

class St_triggerInfoC : public TChair {
 public:
  static St_triggerInfoC* 	instance();
  triggerInfo_st 	*Struct(Int_t i = 0) 	{return ((St_triggerInfo*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                    {return GetNRows();}
  Int_t 	runNumber(Int_t i = 0) 	        {return Struct(i)->runNumber;}
  Int_t 	idxTrg(Int_t i = 0) 	        {return Struct(i)->idxTrg;}
  Int_t 	daqTrgId(Int_t i = 0) 	        {return Struct(i)->daqTrgId;}
  Int_t 	offlineTrgId(Int_t i = 0) 	{return Struct(i)->offlineTrgId;}
  Int_t 	trgNameVersion(Int_t i = 0) 	{return Struct(i)->trgNameVersion;}
  Int_t 	trgVersion(Int_t i = 0) 	{return Struct(i)->trgVersion;}
  Int_t 	threashVersion(Int_t i = 0) 	{return Struct(i)->threashVersion;}
  Int_t 	psVersion(Int_t i = 0) 	        {return Struct(i)->psVersion;}
  Int_t 	psL0(Int_t i = 0) 	        {return Struct(i)->psL0;}
  Char_t* 	name(Int_t i = 0) 	        {return Struct(i)->name;}
  UInt_t 	detectorLiveOnBits(Int_t i = 0) {return Struct(i)->detectorLiveOnBits;}
  UInt_t 	detectorLiveOffBits(Int_t i = 0){return Struct(i)->detectorLiveOffBits;}
  UInt_t 	detectorRequest(Int_t i = 0) 	{return Struct(i)->detectorRequest;}
  Int_t 	idxLevel(Int_t i = 0) 	        {return Struct(i)->idxLevel;}
  Int_t 	algorithmId(Int_t i = 0) 	{return Struct(i)->algorithmId;}
  Float_t 	ps(Int_t i = 0) 	        {return Struct(i)->ps;}
 protected:
  St_triggerInfoC(St_triggerInfo *table=0) : TChair(table) {}
  virtual ~St_triggerInfoC() {fgInstance = 0;}
 private:
  static St_triggerInfoC* fgInstance;
  ClassDefChair(St_triggerInfo, triggerInfo_st )
  ClassDef(St_triggerInfoC,1) //C++ TChair for triggerInfo table class
};
#endif
