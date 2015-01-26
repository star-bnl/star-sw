#ifndef St_pxlBadRowColumnsC_h
#define St_pxlBadRowColumnsC_h

#include "TChair.h"
#include "tables/St_pxlBadRowColumns_Table.h"

class St_pxlBadRowColumnsC : public TChair {
 public:
  static St_pxlBadRowColumnsC* 	instance();
  pxlBadRowColumns_st 	*Struct(Int_t i = 0) 	const {return ((St_pxlBadRowColumns*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UInt_t* 	badRowColumns(Int_t i = 0) 	const {return Struct(i)->badRowColumns;}
 protected:
  St_pxlBadRowColumnsC(St_pxlBadRowColumns *table=0) : TChair(table) {}
  virtual ~St_pxlBadRowColumnsC() {fgInstance = 0;}
 private:
  static St_pxlBadRowColumnsC* fgInstance;
  ClassDefChair(St_pxlBadRowColumns, pxlBadRowColumns_st )
  ClassDef(St_pxlBadRowColumnsC,1) //C++ TChair for pxlBadRowColumns table class
};
#endif
