#ifndef St_tpcAltroParamsC_h
#define St_tpcAltroParamsC_h

#include "TChair.h"
#include "tables/St_tpcAltroParams_Table.h"

class St_tpcAltroParamsC : public TChair {
 public:
  static St_tpcAltroParamsC* 	instance();
  tpcAltroParams_st 	*Struct(Int_t i = 0) 	{return ((St_tpcAltroParams*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Int_t 	N(Int_t i = 0) 	{return Struct(i)->N;}
  Int_t  	Threshold(Int_t i = 0) 	{return Struct(i)->Altro_thr;}
  Int_t  	MinSamplesaboveThreshold(Int_t i = 0) 	{return Struct(i)->Altro_seq;}
  Int_t  	K1(Int_t i = 0) 	{return Struct(i)->Altro_K1;}
  Int_t  	K2(Int_t i = 0) 	{return Struct(i)->Altro_K2;}
  Int_t  	K3(Int_t i = 0) 	{return Struct(i)->Altro_K3;}
  Int_t  	L1(Int_t i = 0) 	{return Struct(i)->Altro_L1;}
  Int_t  	L2(Int_t i = 0) 	{return Struct(i)->Altro_L2;}
  Int_t  	L3(Int_t i = 0) 	{return Struct(i)->Altro_L3;}
 protected:
  St_tpcAltroParamsC(St_tpcAltroParams *table=0) : TChair(table) {}
  virtual ~St_tpcAltroParamsC() {fgInstance = 0;}
 private:
  static St_tpcAltroParamsC* fgInstance;
  ClassDefChair(St_tpcAltroParams, tpcAltroParams_st )
  ClassDef(St_tpcAltroParamsC,1) //C++ TChair for tpcAltroParams table class 
};
#endif
