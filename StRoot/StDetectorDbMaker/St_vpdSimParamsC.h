#ifndef St_vpdSimParamsC_h
#define St_vpdSimParamsC_h

#include "TChair.h"
#include "tables/St_vpdSimParams_Table.h"

class St_vpdSimParamsC : public TChair {
 public:
  static St_vpdSimParamsC* 	instance();
  vpdSimParams_st 	*Struct(Int_t i = 0) 	const {return ((St_vpdSimParams*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Short_t* 	tubeRes(Int_t i = 0) 	const {return Struct(i)->tubeRes;}
  UChar_t* 	tubeID(Int_t i = 0) 	const {return Struct(i)->tubeID;}
  UChar_t* 	tubeStatusFlag(Int_t i = 0) 	const {return Struct(i)->tubeStatusFlag;}
  UChar_t* 	tubeTriggerFlag(Int_t i = 0) 	const {return Struct(i)->tubeTriggerFlag;}
 protected:
  St_vpdSimParamsC(St_vpdSimParams *table=0) : TChair(table) {}
  virtual ~St_vpdSimParamsC() {fgInstance = 0;}
 private:
  static St_vpdSimParamsC* fgInstance;
  ClassDefChair(St_vpdSimParams, vpdSimParams_st )
  ClassDef(St_vpdSimParamsC,1) //C++ TChair for vpdSimParams table class
};
#endif
