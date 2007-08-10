#ifndef St_tpcPedestalC_h
#define St_tpcPedestalC_h

#include "TChair.h"
#include "tables/St_tpcPedestal_Table.h"

class St_tpcPedestalC : public TChair {
 public:
  static St_tpcPedestalC* 	instance();
  tpcPedestal_st 	*Struct(Int_t i = 0) 	{return ((St_tpcPedestal*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Float_t& 	Pedestal(Int_t sector = 1, Int_t row = 1, Int_t pad = 1) 
  {return Struct(sector-1)->Pedestal[row-1][pad-1];}
  Float_t& 	Rms(Int_t sector = 1, Int_t row = 1, Int_t pad = 1) 
  {return Struct(sector-1)->Rms[row-1][pad-1];}
 protected:
  St_tpcPedestalC(St_tpcPedestal *table=0) : TChair(table) {}
  virtual ~St_tpcPedestalC() {fgInstance = 0;}
 private:
  static St_tpcPedestalC* fgInstance;
  ClassDefChair(St_tpcPedestal, tpcPedestal_st )
  ClassDef(St_tpcPedestalC,1) //C++ TChair for tpcPedestal table class
};
#endif
