#ifndef St_tpcISGainsC_h
#define St_tpcISGainsC_h

#include "TChair.h"
#include "tables/St_tpcISGains_Table.h"

class St_tpcISGainsC : public TChair {
 public:
  static St_tpcISGainsC* 	instance();
  tpcISGains_st 	*Struct(Int_t i = 0) {return ((St_tpcISGains*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                 {return GetNRows();}
  Float_t* 	gain(Int_t i = 0) 	     {return Struct(i)->gain;}
 protected:
  St_tpcISGainsC(St_tpcISGains *table=0) : TChair(table) {}
  virtual ~St_tpcISGainsC() {fgInstance = 0;}
 private:
  static St_tpcISGainsC* fgInstance;
  ClassDefChair(St_tpcISGains, tpcISGains_st )
  ClassDef(St_tpcISGainsC,1) //C++ TChair for tpcISGains table class
};
#endif
