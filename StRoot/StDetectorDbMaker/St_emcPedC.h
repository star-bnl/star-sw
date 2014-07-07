#ifndef St_emcPedC_h
#define St_emcPedC_h

#include "TChair.h"
#include "tables/St_emcPed_Table.h"

class St_emcPedC : public TChair {
 public:
  emcPed_st 	*Struct(Int_t i = 0) 	const {return ((St_emcPed*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	Status(Int_t i = 0) 	const {return Struct(i)->Status;}
  short* 	AdcPedestal(Int_t i = 0) 	const {return Struct(i)->AdcPedestal;}
  short* 	AdcPedestalRMS(Int_t i = 0) 	const {return Struct(i)->AdcPedestalRMS;}
  Float_t* 	ChiSquare(Int_t i = 0) 	const {return Struct(i)->ChiSquare;}
 protected:
  St_emcPedC(St_emcPed *table=0) : TChair(table) {}
  virtual ~St_emcPedC() {}
 private:
  ClassDefChair(St_emcPed, emcPed_st )
  ClassDef(St_emcPedC,1) //C++ TChair for emcPed table class
};
class St_bemcPedC : public St_emcPedC {
 public:
  static St_bemcPedC* 	instance();
  St_bemcPedC(St_emcPed *table=0) : St_emcPedC(table) {}
  virtual ~St_bemcPedC() {fgInstance = 0;}
 private:
  static St_bemcPedC* fgInstance;
  ClassDef(St_bemcPedC,1) //C++ TChair for bemcPed table class
};
class St_bprsPedC : public St_emcPedC {
 public:
  static St_bprsPedC* 	instance();
  St_bprsPedC(St_emcPed *table=0) : St_emcPedC(table) {}
  virtual ~St_bprsPedC() {fgInstance = 0;}
 private:
  static St_bprsPedC* fgInstance;
  ClassDef(St_bprsPedC,1) //C++ TChair for bprsPed table class
};
#endif
