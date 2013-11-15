#ifndef St_smdCalibC_h
#define St_smdCalibC_h

#include "TChair.h"
#include "tables/St_smdCalib_Table.h"

class St_smdCalibC : public TChair {
 public:
  smdCalib_st 	*Struct(Int_t i = 0) 	const {return ((St_smdCalib*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	Status(Int_t i = 0) 	const {return Struct(i)->Status;}
  Float_t* 	AdcToE(Int_t i = 0) 	const {return &Struct(i)->AdcToE[0][0];}
 protected:
  St_smdCalibC(St_smdCalib *table=0) : TChair(table) {}
  virtual ~St_smdCalibC() {}
 private:
  ClassDefChair(St_smdCalib, smdCalib_st )
  ClassDef(St_smdCalibC,1) //C++ TChair for smdCalib table class
};
class St_bsmdeCalibC : public St_smdCalibC {
 public:
  static St_bsmdeCalibC* 	instance();
 protected:
  St_bsmdeCalibC(St_smdCalib *table=0) : St_smdCalibC(table) {}
  virtual ~St_bsmdeCalibC() {fgInstance = 0;}
 private:
  static St_bsmdeCalibC* fgInstance;
  ClassDef(St_bsmdeCalibC,1) //C++ St_bsmdeCalibC for bsmdeCalib table class
};
class St_bsmdpCalibC : public St_smdCalibC {
 public:
  static St_bsmdpCalibC* 	instance();
 protected:
  St_bsmdpCalibC(St_smdCalib *table=0) : St_smdCalibC(table) {}
  virtual ~St_bsmdpCalibC() {fgInstance = 0;}
 private:
  static St_bsmdpCalibC* fgInstance;
  ClassDef(St_bsmdpCalibC,1) //C++ St_bsmdpCalibC for bsmdpCalib table class
};
#endif
