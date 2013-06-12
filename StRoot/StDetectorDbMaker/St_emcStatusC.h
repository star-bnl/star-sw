#ifndef St_emcStatusC_h
#define St_emcStatusC_h

#include "TChair.h"
#include "tables/St_emcStatus_Table.h"

class St_emcStatusC : public TChair {
 public:
  emcStatus_st 	*Struct(Int_t i = 0) 	const {return ((St_emcStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	Status(Int_t i = 0) 	const {return Struct(i)->Status;}
 protected:
  St_emcStatusC(St_emcStatus *table=0) : TChair(table) {}
 private:
  ClassDefChair(St_emcStatus, emcStatus_st )
  ClassDef(St_emcStatusC,1) //C++ TChair for emcStatus table class
};
class St_bemcStatusC : public St_emcStatusC {
 public:
  static St_bemcStatusC* 	instance();
 protected:
  St_bemcStatusC(St_emcStatus *table=0) : St_emcStatusC(table) {}
  virtual ~St_bemcStatusC() {fgInstance = 0;}
 private:
  static St_bemcStatusC* fgInstance;
  ClassDef(St_bemcStatusC,1) //C++ TChair for bemcStatus table class
};
class St_bprsStatusC : public St_emcStatusC {
 public:
  static St_bprsStatusC* 	instance();
 protected:
  St_bprsStatusC(St_emcStatus *table=0) : St_emcStatusC(table) {}
  virtual ~St_bprsStatusC() {fgInstance = 0;}
 private:
  static St_bprsStatusC* fgInstance;
  ClassDef(St_bprsStatusC,1) //C++ TChair for bprsStatus table class
};
#endif
