#ifndef St_tpcOSGainsC_h
#define St_tpcOSGainsC_h

#include "TChair.h"
#include "tables/St_tpcOSGains_Table.h"

class St_tpcOSGainsC : public TChair {
 public:
  static St_tpcOSGainsC* 	instance();
  tpcOSGains_st 	*Struct(Int_t i = 0) {return ((St_tpcOSGains*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                 {return GetNRows();}
  Float_t* 	gain(Int_t i = 0) 	     {return Struct(i)->gain;}
 protected:
  St_tpcOSGainsC(St_tpcOSGains *table=0) : TChair(table) {}
  virtual ~St_tpcOSGainsC() {fgInstance = 0;}
 private:
  static St_tpcOSGainsC* fgInstance;
  ClassDefChair(St_tpcOSGains, tpcOSGains_st )
  ClassDef(St_tpcOSGainsC,1) //C++ TChair for tpcOSGains table class
};
#endif
