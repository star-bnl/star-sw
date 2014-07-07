#ifndef St_smdPedC_h
#define St_smdPedC_h

#include "TChair.h"
#include "tables/St_smdPed_Table.h"

class St_smdPedC : public TChair {
 public:
  smdPed_st 	*Struct(Int_t i = 0) 	const {return ((St_smdPed*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	Status(Int_t i = 0) 	const {return Struct(i)->Status;}
  short* 	AdcPedestal(Int_t i = 0) 	const {return &Struct(i)->AdcPedestal[0][0];}
  short* 	AdcPedestalRMS(Int_t i = 0) 	const {return &Struct(i)->AdcPedestalRMS[0][0];}
 protected:
  St_smdPedC(St_smdPed *table=0) : TChair(table) {}
  virtual  ~St_smdPedC() {}
 private:
  ClassDefChair(St_smdPed, smdPed_st )
  ClassDef(St_smdPedC,1) //C++ TChair for smdPed table class
};
class St_bsmdePedC : public St_smdPedC {
 public:
  static St_bsmdePedC* 	instance();
 protected:
  St_bsmdePedC(St_smdPed *table=0) : St_smdPedC(table) {}
  virtual ~St_bsmdePedC() {fgInstance = 0;}
 private:
  static St_bsmdePedC* fgInstance;
  ClassDef(St_bsmdePedC,1) //C++ TChair for bsmdePed table class
};
class St_bsmdpPedC : public St_smdPedC {
 public:
  static St_bsmdpPedC* 	instance();
 protected:
  St_bsmdpPedC(St_smdPed *table=0) : St_smdPedC(table) {}
  virtual ~St_bsmdpPedC() {fgInstance = 0;}
 private:
  static St_bsmdpPedC* fgInstance;
  ClassDef(St_bsmdpPedC,1) //C++ TChair for bsmdpPed table class
};
#endif
