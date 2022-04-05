#ifndef St_tofStatusC_h
#define St_tofStatusC_h

#include "TChair.h"
#include "tables/St_tofStatus_Table.h"

class St_tofStatusC : public TChair {
 public:
  static St_tofStatusC* 	instance();
  tofStatus_st *Struct(Int_t i = 0) 	const {return ((St_tofStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UShort_t     *status(Int_t i = 0) 	const {return Struct(i)->status;}
  Int_t         status(Int_t trayId, Int_t moduleId, Int_t cellId) {
    enum {mNModule = 32, mNCell = 6, mNTray=120};
    if (trayId   < 1 || trayId   > mNTray   ||
	moduleId < 1 || moduleId > mNModule ||
	cellId   < 1 || cellId   > mNCell     ) return 0;
    Int_t i = (cellId -1) + mNCell*((moduleId-1) + mNModule*(trayId-1));
    return (Int_t) status()[i];
  }
 protected:
  St_tofStatusC(St_tofStatus *table=0) : TChair(table) {}
  virtual ~St_tofStatusC() {fgInstance = 0;}
 private:
  static St_tofStatusC* fgInstance;
  ClassDefChair(St_tofStatus, tofStatus_st )
  ClassDef(St_tofStatusC,1) //C++ TChair for tofStatus table class
};
#endif
