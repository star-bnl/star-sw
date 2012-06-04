#ifndef St_emcTriggerStatusC_h
#define St_emcTriggerStatusC_h

#include "TChair.h"
#include "tables/St_emcTriggerStatus_Table.h"

class St_emcTriggerStatusC : public TChair {
 public:
  emcTriggerStatus_st 	*Struct(Int_t i = 0) 	const {return ((St_emcTriggerStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	PatchStatus(Int_t i = 0) 	const {return Struct(i)->PatchStatus;}
  UChar_t* 	HighTowerStatus(Int_t i = 0) 	const {return Struct(i)->HighTowerStatus;}
  UChar_t* 	TowerStatus(Int_t i = 0) 	const {return &Struct(i)->TowerStatus[0][0];}
 protected:
  St_emcTriggerStatusC(St_emcTriggerStatus *table=0) : TChair(table) {}
  virtual ~St_emcTriggerStatusC() {}
 private:
  ClassDefChair(St_emcTriggerStatus, emcTriggerStatus_st )
  ClassDef(St_emcTriggerStatusC,1) //C++ TChair for emcTriggerStatus table class
};
class St_bemcTriggerStatusC : public St_emcTriggerStatusC {
 public:
  static St_bemcTriggerStatusC* 	instance();
 protected:
  St_bemcTriggerStatusC(St_emcTriggerStatus *table=0) : St_emcTriggerStatusC(table) {}
  virtual ~St_bemcTriggerStatusC() {fgInstance = 0;}
 private:
  static St_bemcTriggerStatusC* fgInstance;
  ClassDef(St_bemcTriggerStatusC,1) //C++ TChair for emcTriggerStatus table class
};
#endif
