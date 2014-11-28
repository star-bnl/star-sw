#ifndef St_MagFactorC_h
#define St_MagFactorC_h

#include "TChair.h"
#include "tables/St_MagFactor_Table.h"

class St_MagFactorC : public TChair {
 public:
  static St_MagFactorC* 	instance();
  MagFactor_st 	*Struct(Int_t i = 0) 	 {return ((St_MagFactor*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()             {return GetNRows();}
  Float_t 	ScaleFactor(Int_t i = 0) {return Struct(i)->ScaleFactor;}
 protected:
  St_MagFactorC(St_MagFactor *table=0) : TChair(table) {}
  virtual ~St_MagFactorC() {fgInstance = 0;}
 private:
  static St_MagFactorC* fgInstance;
  ClassDefChair(St_MagFactor, MagFactor_st )
  ClassDef(St_MagFactorC,1) //C++ TChair for MagFactor table class
};
#endif
