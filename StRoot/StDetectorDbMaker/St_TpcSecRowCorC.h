#ifndef St_TpcSecRowCorC_h
#define St_TpcSecRowCorC_h

#include "TChair.h"
#include "tables/St_TpcSecRowCor_Table.h"

class St_TpcSecRowCorC : public TChair {
 public:
  TpcSecRowCor_st 	*Struct(Int_t i = 0) 	{return ((St_TpcSecRowCor*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Float_t* 	GainScale(Int_t i = 0) 	        {return Struct(i)->GainScale;}
  Float_t* 	GainRms(Int_t i = 0) 	        {return Struct(i)->GainRms;}
 protected:
  St_TpcSecRowCorC(St_TpcSecRowCor *table=0) : TChair(table) {}
  virtual ~St_TpcSecRowCorC() {}
 private:
  ClassDef(St_TpcSecRowCorC,1) //C++ TChair for TpcSecRowCor table class
};
#endif
