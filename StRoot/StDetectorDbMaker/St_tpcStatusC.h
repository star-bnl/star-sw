#ifndef St_tpcStatusC_h
#define St_tpcStatusC_h

#include "TChair.h"
#include "tables/St_tpcStatus_Table.h"
#include "St_tpcPadPlanesC.h"
class St_tpcStatusC : public TChair {
 public:
  static St_tpcStatusC* 	instance();
  tpcStatus_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	status(Int_t i = 0) 	const {return Struct(i)->status;}
  UChar_t       status(Int_t sector, Int_t row) const {return status()[St_tpcPadPlanesC::instance()->padRows()*(sector-1)+(row-1)];}
  Bool_t        isDead() {return ((status()) && (status()[0]==0xff));}
 protected:
  St_tpcStatusC(St_tpcStatus *table=0) : TChair(table) {}
  virtual ~St_tpcStatusC() {fgInstance = 0;}
 private:
  static St_tpcStatusC* fgInstance;
  ClassDefChair(St_tpcStatus, tpcStatus_st )
  ClassDef(St_tpcStatusC,1) //C++ TChair for tpcStatus table class
};
#endif
