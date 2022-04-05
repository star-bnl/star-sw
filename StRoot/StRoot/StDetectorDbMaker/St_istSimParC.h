#ifndef St_istSimParC_h
#define St_istSimParC_h

#include "TChair.h"
#include "tables/St_istSimPar_Table.h"

class St_istSimParC : public TChair {
 public:
  static St_istSimParC* 	instance();
  istSimPar_st 	*Struct(Int_t i = 0) 	const {return ((St_istSimPar*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t 	mode(Int_t i = 0) 	const {return Struct(i)->mode;}
  Float_t 	pCut(Int_t i = 0) 	const {return Struct(i)->pCut;}
  Float_t 	effIst(Int_t i = 0) 	const {return Struct(i)->effIst;}
 protected:
  St_istSimParC(St_istSimPar *table=0) : TChair(table) {}
  virtual ~St_istSimParC() {fgInstance = 0;}
 private:
  static St_istSimParC* fgInstance;
  ClassDefChair(St_istSimPar, istSimPar_st )
  ClassDef(St_istSimParC,1) //C++ TChair for istSimPar table class
};
#endif
