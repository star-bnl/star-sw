#ifndef St_emcGainC_h
#define St_emcGainC_h

#include "TChair.h"
#include "tables/St_emcGain_Table.h"

class St_emcGainC : public TChair {
 public:
  emcGain_st 	*Struct(Int_t i = 0) 	const {return ((St_emcGain*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	Status(Int_t i = 0) 	const {return Struct(i)->Status;}
  Float_t* 	Gain(Int_t i = 0) 	const {return Struct(i)->Gain;}
 protected:
  St_emcGainC(St_emcGain *table=0) : TChair(table) {}
  virtual ~St_emcGainC() {}
 private:
  ClassDefChair(St_emcGain, emcGain_st )
  ClassDef(St_emcGainC,1) //C++ TChair for emcGain table class
};
class St_bemcGainC : public St_emcGainC {
 public:
  static St_bemcGainC* 	instance();
 protected:
  St_bemcGainC(St_emcGain *table=0) : St_emcGainC(table) {}
  virtual ~St_bemcGainC() {fgInstance = 0;}
 private:
  static St_bemcGainC* fgInstance;
  ClassDef(St_bemcGainC,1) //C++ St_emcGainC for emcGain table class
};
class St_bprsGainC : public St_emcGainC {
 public:
  static St_bprsGainC* 	instance();
 protected:
  St_bprsGainC(St_emcGain *table=0) : St_emcGainC(table) {}
  virtual ~St_bprsGainC() {fgInstance = 0;}
 private:
  static St_bprsGainC* fgInstance;
  ClassDef(St_bprsGainC,1) //C++ St_emcGainC for emcGain table class
};
#endif
