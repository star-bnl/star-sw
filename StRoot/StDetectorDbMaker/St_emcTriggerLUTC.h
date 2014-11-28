#ifndef St_emcTriggerLUTC_h
#define St_emcTriggerLUTC_h

#include "TChair.h"
#include "tables/St_emcTriggerLUT_Table.h"

class St_emcTriggerLUTC : public TChair {
 public:
  emcTriggerLUT_st 	*Struct(Int_t i = 0) 	const {return ((St_emcTriggerLUT*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UInt_t* 	FormulaTag(Int_t i = 0) 	const {return &Struct(i)->FormulaTag[0][0];}
  UInt_t* 	FormulaParameter0(Int_t i = 0) 	const {return &Struct(i)->FormulaParameter0[0][0];}
  UInt_t* 	FormulaParameter1(Int_t i = 0) 	const {return &Struct(i)->FormulaParameter1[0][0];}
  UInt_t* 	FormulaParameter2(Int_t i = 0) 	const {return &Struct(i)->FormulaParameter2[0][0];}
  UInt_t* 	FormulaParameter3(Int_t i = 0) 	const {return &Struct(i)->FormulaParameter3[0][0];}
  UInt_t* 	FormulaParameter4(Int_t i = 0) 	const {return &Struct(i)->FormulaParameter4[0][0];}
  UInt_t* 	FormulaParameter5(Int_t i = 0) 	const {return &Struct(i)->FormulaParameter5[0][0];}
 protected:
  St_emcTriggerLUTC(St_emcTriggerLUT *table=0) : TChair(table) {}
  virtual ~St_emcTriggerLUTC() {}
 private:
  ClassDefChair(St_emcTriggerLUT, emcTriggerLUT_st )
  ClassDef(St_emcTriggerLUTC,1) //C++ TChair for emcTriggerLUT table class
};
class St_bemcTriggerLUTC : public St_emcTriggerLUTC {
 public:
  static St_bemcTriggerLUTC* 	instance();
 protected:
  St_bemcTriggerLUTC(St_emcTriggerLUT *table=0) : St_emcTriggerLUTC(table) {}
  virtual ~St_bemcTriggerLUTC() {fgInstance = 0;}
 private:
  static St_bemcTriggerLUTC* fgInstance;
  ClassDef(St_bemcTriggerLUTC,1) //C++ TChair for emcTriggerLUT table class
};
#endif
