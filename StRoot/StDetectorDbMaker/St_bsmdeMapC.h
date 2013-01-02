#ifndef St_bsmdeMapC_h
#define St_bsmdeMapC_h

#include "TChair.h"
#include "tables/St_bsmdeMap_Table.h"

class St_bsmdeMapC : public TChair {
 public:
  static St_bsmdeMapC* 	instance();
  bsmdeMap_st 	*Struct(Int_t i = 0) 	const {return ((St_bsmdeMap*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t 	m(Int_t i = 0) 	const {return Struct(i)->m;}
  UChar_t 	e(Int_t i = 0) 	const {return Struct(i)->e;}
  UChar_t 	s(Int_t i = 0) 	const {return Struct(i)->s;}
  UChar_t 	rdo(Int_t i = 0) 	const {return Struct(i)->rdo;}
  unsigned short 	rdoChannel(Int_t i = 0) 	const {return Struct(i)->rdoChannel;}
  UChar_t 	wire(Int_t i = 0) 	const {return Struct(i)->wire;}
  UChar_t 	feeA(Int_t i = 0) 	const {return Struct(i)->feeA;}
  Float_t 	eta(Int_t i = 0) 	const {return Struct(i)->eta;}
  Float_t 	phi(Int_t i = 0) 	const {return Struct(i)->phi;}
 protected:
  St_bsmdeMapC(St_bsmdeMap *table=0) : TChair(table) {}
  virtual ~St_bsmdeMapC() {fgInstance = 0;}
 private:
  static St_bsmdeMapC* fgInstance;
  ClassDefChair(St_bsmdeMap, bsmdeMap_st )
  ClassDef(St_bsmdeMapC,1) //C++ TChair for bsmdeMap table class
};
#endif
