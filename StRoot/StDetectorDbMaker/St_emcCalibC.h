#ifndef St_emcCalibC_h
#define St_emcCalibC_h

#include "TChair.h"
#include "tables/St_emcCalib_Table.h"

class St_emcCalibC : public TChair {
 public:
  emcCalib_st 	*Struct(Int_t i = 0) 	const {return ((St_emcCalib*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()            const {return GetNRows();}
  UChar_t* 	Status(Int_t i = 0) 	const {return Struct(i)->Status;}
  Float_t*  AdcToE(Int_t i = 0) 	const {return &Struct(i)->AdcToE[0][0];}
 protected:
  St_emcCalibC(St_emcCalib *table=0) : TChair(table) {}
 private:
  ClassDefChair(St_emcCalib, emcCalib_st )
  ClassDef(St_emcCalibC,1) //C++ TChair for emcCalib table class
};

class St_bemcCalibC : public St_emcCalibC {
 public:
  static St_bemcCalibC* 	instance();
 protected:
  St_bemcCalibC(St_emcCalib *table=0) : St_emcCalibC(table) {}
  virtual ~St_bemcCalibC() {fgInstance = 0;}
 private:
  static St_bemcCalibC* fgInstance;
  ClassDef(St_bemcCalibC,1) //C++ TChair for bemcCalib table class
};
class St_bprsCalibC : public St_emcCalibC {
 public:
  static St_bprsCalibC* 	instance();
 protected:
  St_bprsCalibC(St_emcCalib *table=0) : St_emcCalibC(table) {}
  virtual ~St_bprsCalibC() {fgInstance = 0;}
 private:
  static St_bprsCalibC* fgInstance;
  ClassDef(St_bprsCalibC,1) //C++ TChair for bprsCalib table class
};
#endif
