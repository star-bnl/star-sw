#ifndef St_y1MultC_h
#define St_y1MultC_h

#include "TChair.h"
#include "tables/St_y1Mult_Table.h"

class St_y1MultC : public TChair {
 public:
  static St_y1MultC* 	instance();
  y1Mult_st 	*Struct(Int_t i = 0) 	{return ((St_y1Mult*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()            {return GetNRows();}
  Double_t 	mult(Int_t i = 0) 	{return Struct(i)->mult;}
 protected:
  St_y1MultC(St_y1Mult *table=0) : TChair(table) {}
  virtual ~St_y1MultC() {fgInstance = 0;}
 private:
  static St_y1MultC* fgInstance;
  ClassDefChair(St_y1Mult, y1Mult_st )
  ClassDef(St_y1MultC,1) //C++ TChair for y1Mult table class
};
#endif
