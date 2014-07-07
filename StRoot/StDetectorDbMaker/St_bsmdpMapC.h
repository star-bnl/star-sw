#ifndef St_bsmdpMapC_h
#define St_bsmdpMapC_h

#include "TChair.h"
#include "tables/St_bsmdpMap_Table.h"

class St_bsmdpMapC : public TChair {
 public:
  static St_bsmdpMapC* 	instance();
  bsmdpMap_st 	*Struct(Int_t i = 0) 	const {return ((St_bsmdpMap*) Table())->GetTable()+i;}
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
  St_bsmdpMapC(St_bsmdpMap *table=0) : TChair(table) {}
  virtual ~St_bsmdpMapC() {fgInstance = 0;}
 private:
  static St_bsmdpMapC* fgInstance;
  ClassDefChair(St_bsmdpMap, bsmdpMap_st )
  ClassDef(St_bsmdpMapC,1) //C++ TChair for bsmdpMap table class
};
#endif
