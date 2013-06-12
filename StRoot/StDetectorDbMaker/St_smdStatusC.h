#ifndef St_smdStatusC_h
#define St_smdStatusC_h

#include "TChair.h"
#include "tables/St_smdStatus_Table.h"

class St_smdStatusC : public TChair {
 public:
  smdStatus_st 	*Struct(Int_t i = 0) 	const {return ((St_smdStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	Status(Int_t i = 0) 	const {return Struct(i)->Status;}
 protected:
  St_smdStatusC(St_smdStatus *table=0) : TChair(table) {}
  virtual ~St_smdStatusC() {}
 private:
  ClassDefChair(St_smdStatus, smdStatus_st )
  ClassDef(St_smdStatusC,1) //C++ TChair for smdStatus table class
};
class St_bsmdeStatusC : public St_smdStatusC {
 public:
  static St_bsmdeStatusC* 	instance();
 protected:
  St_bsmdeStatusC(St_smdStatus *table=0) : St_smdStatusC(table) {}
  virtual ~St_bsmdeStatusC() {fgInstance = 0;}
 private:
  static St_bsmdeStatusC* fgInstance;
  ClassDef(St_bsmdeStatusC,1) //C++ TChair for bsmdeStatus table class
};
class St_bsmdpStatusC : public St_smdStatusC {
 public:
  static St_bsmdpStatusC* 	instance();
 protected:
  St_bsmdpStatusC(St_smdStatus *table=0) : St_smdStatusC(table) {}
  virtual ~St_bsmdpStatusC() {fgInstance = 0;}
 private:
  static St_bsmdpStatusC* fgInstance;
  ClassDef(St_bsmdpStatusC,1) //C++ TChair for bsmdpStatus table class
};
#endif
