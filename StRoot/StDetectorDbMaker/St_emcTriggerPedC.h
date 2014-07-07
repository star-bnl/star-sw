#ifndef St_emcTriggerPedC_h
#define St_emcTriggerPedC_h

#include "TChair.h"
#include "tables/St_emcTriggerPed_Table.h"

class St_emcTriggerPedC : public TChair {
 public:
  emcTriggerPed_st 	*Struct(Int_t i = 0) 	const {return ((St_emcTriggerPed*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UInt_t 	PedShift(Int_t i = 0) 	        const {return Struct(i)->PedShift;}
  UInt_t* 	BitConversionMode(Int_t i = 0) 	const {return &Struct(i)->BitConversionMode[0][0];}
  UInt_t* 	Ped(Int_t i = 0) 	        const {return &Struct(i)->Ped[0][0];}
 protected:
  St_emcTriggerPedC(St_emcTriggerPed *table=0) : TChair(table) {}
  virtual ~St_emcTriggerPedC() {}
 private:
  ClassDefChair(St_emcTriggerPed, emcTriggerPed_st )
  ClassDef(St_emcTriggerPedC,1) //C++ TChair for emcTriggerPed table class
};
class St_bemcTriggerPedC : public St_emcTriggerPedC {
 public:
  static St_bemcTriggerPedC* 	instance();
 protected:
  St_bemcTriggerPedC(St_emcTriggerPed *table=0) : St_emcTriggerPedC(table) {}
  virtual ~St_bemcTriggerPedC() {fgInstance = 0;}
 private:
  static St_bemcTriggerPedC* fgInstance;
  ClassDef(St_bemcTriggerPedC,1) //C++ TChair for emcTriggerPed table class
};
#endif
