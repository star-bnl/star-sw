#ifndef St_smdGainC_h
#define St_smdGainC_h

#include "TChair.h"
#include "tables/St_smdGain_Table.h"

class St_smdGainC : public TChair {
 public:
  smdGain_st 	*Struct(Int_t i = 0) 	const {return ((St_smdGain*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	Status(Int_t i = 0) 	const {return Struct(i)->Status;}
  Float_t* 	Gain(Int_t i = 0) 	const {return Struct(i)->Gain;}
 protected:
  St_smdGainC(St_smdGain *table=0) : TChair(table) {}
  virtual ~St_smdGainC() {}
 private:
  ClassDefChair(St_smdGain, smdGain_st )
  ClassDef(St_smdGainC,1) //C++ TChair for smdGain table class
};
class St_bsmdeGainC : public St_smdGainC {
 public:
  static St_bsmdeGainC* 	instance();
 protected:
  St_bsmdeGainC(St_smdGain *table=0) : St_smdGainC(table) {}
  virtual ~St_bsmdeGainC() {fgInstance = 0;}
 private:
  static St_bsmdeGainC* fgInstance;
  ClassDef(St_bsmdeGainC,1) //C++ TChair for smdGain table class
};
class St_bsmdpGainC : public St_smdGainC {
 public:
  static St_bsmdpGainC* 	instance();
 protected:
  St_bsmdpGainC(St_smdGain *table=0) : St_smdGainC(table) {}
  virtual ~St_bsmdpGainC() {fgInstance = 0;}
 private:
  static St_bsmdpGainC* fgInstance;
  ClassDef(St_bsmdpGainC,1) //C++ TChair for smdGain table class
};
#endif
