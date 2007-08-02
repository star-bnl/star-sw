#ifndef St_tpcOmegaTauC_h
#define St_tpcOmegaTauC_h

#include "TChair.h"
#include "tables/St_tpcOmegaTau_Table.h"

class St_tpcOmegaTauC : public TChair {
 public:
  static St_tpcOmegaTauC* 	instance();
  tpcOmegaTau_st 	*Struct(Int_t i = 0) {return ((St_tpcOmegaTau*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                 {return GetNRows();}
  Float_t 	tensorV1(Int_t i = 0) 	     {return Struct(i)->tensorV1;}
  Float_t 	tensorV2(Int_t i = 0) 	     {return Struct(i)->tensorV2;}
  Float_t 	getOmegaTauTensorV1()        {return tensorV1();}
  Float_t 	getOmegaTauTensorV2()        {return tensorV2();}
 protected:
  St_tpcOmegaTauC(St_tpcOmegaTau *table=0) : TChair(table) {}
  virtual ~St_tpcOmegaTauC() {SafeDelete(fgInstance);}
 private:
  static St_tpcOmegaTauC* fgInstance;
  ClassDefChair(St_tpcOmegaTau, tpcOmegaTau_st )
  ClassDef(St_tpcOmegaTauC,1) //C++ TChair for tpcOmegaTau table class
};
#endif
