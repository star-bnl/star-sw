#ifndef St_TpcAltroParametersC_h
#define St_TpcAltroParametersC_h

#include "TChair.h"
#include "tables/St_TpcAltroParameters_Table.h"

class St_TpcAltroParametersC : public TChair {
 public:
  static St_TpcAltroParametersC* 	instance();
  TpcAltroParameters_st 	*Struct(Int_t i = 0) 	{return ((St_TpcAltroParameters*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Int_t 	N(Int_t i = 0) 	{return Struct(i)->N;}
  Int_t* 	altro_reg(Int_t i = 0) 	{return Struct(i)->altro_reg;}
 protected:
  St_TpcAltroParametersC(St_TpcAltroParameters *table=0) : TChair(table) {}
  virtual ~St_TpcAltroParametersC() {fgInstance = 0;}
 private:
  static St_TpcAltroParametersC* fgInstance;
  ClassDefChair(St_TpcAltroParameters, TpcAltroParameters_st )
  ClassDef(St_TpcAltroParametersC,1) //C++ TChair for TpcAltroParameters table class
};
#endif
