#ifndef St_tpcPadrowT0C_h
#define St_tpcPadrowT0C_h
#include "TChair.h"
#include "tables/St_tpcPadrowT0_Table.h"

class St_tpcPadrowT0C : public TChair {
 public:
  static St_tpcPadrowT0C* 	instance();
  tpcPadrowT0_st 	*Struct(Int_t i = 0) {return ((St_tpcPadrowT0*) Table())->GetTable()+i;}
  Float_t T0(Int_t sector, Int_t row) {return Struct(sector-1)->T0[row-1];}
 protected:
  St_tpcPadrowT0C(St_tpcPadrowT0 *table=0) : TChair(table) {}
  virtual ~St_tpcPadrowT0C() {fgInstance = 0;}
 private:
  static St_tpcPadrowT0C* fgInstance;
  ClassDefChair(St_tpcPadrowT0, tpcPadrowT0_st )
  ClassDef(St_tpcPadrowT0C,1) //C++ TChair for tpcPadrowT0 table class
};

#endif
