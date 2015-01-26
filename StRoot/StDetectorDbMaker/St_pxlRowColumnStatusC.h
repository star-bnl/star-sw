#ifndef St_pxlRowColumnStatusC_h
#define St_pxlRowColumnStatusC_h

#include "TChair.h"
#include "tables/St_pxlRowColumnStatus_Table.h"

class St_pxlRowColumnStatusC : public TChair {
 public:
  static St_pxlRowColumnStatusC* 	instance();
  pxlRowColumnStatus_st 	*Struct(Int_t i = 0) 	const {return ((St_pxlRowColumnStatus*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	rows(Int_t i = 0) 	const {return Struct(i)->rows;}
  UChar_t* 	cols(Int_t i = 0) 	const {return Struct(i)->cols;}
 protected:
  St_pxlRowColumnStatusC(St_pxlRowColumnStatus *table=0) : TChair(table) {}
  virtual ~St_pxlRowColumnStatusC() {fgInstance = 0;}
 private:
  static St_pxlRowColumnStatusC* fgInstance;
  ClassDefChair(St_pxlRowColumnStatus, pxlRowColumnStatus_st )
  ClassDef(St_pxlRowColumnStatusC,1) //C++ TChair for pxlRowColumnStatus table class
};
#endif
