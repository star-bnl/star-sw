#ifndef St_tofStatusC_h
#define St_tofStatusC_h

#include "TChair.h"
#include "tables/St_tofStatus_Table.h"

class St_tofStatusC : public TChair {
 public:
  static St_tofStatusC* 	instance();
  tofStatus_st 	*Struct(Int_t i = 0) 	const {return ((St_tofStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  unsigned short* 	status(Int_t i = 0) 	const {return Struct(i)->status;}
 protected:
  St_tofStatusC(St_tofStatus *table=0) : TChair(table) {}
  virtual ~St_tofStatusC() {fgInstance = 0;}
 private:
  static St_tofStatusC* fgInstance;
  ClassDefChair(St_tofStatus, tofStatus_st )
  ClassDef(St_tofStatusC,1) //C++ TChair for tofStatus table class
};
#endif
