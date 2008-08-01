#ifndef St_tpcT0C_h
#define St_tpcT0C_h
#include "TChair.h"
#include "tables/St_tpcT0_Table.h"

class St_tpcT0C : public TChair {
 public:
  static St_tpcT0C* 	instance();
  tpcT0_st 	*Struct(Int_t i = 0) {return ((St_tpcT0*) Table())->GetTable()+i;}
  Float_t St_tpcT0C::T0(Int_t sector, Int_t row, Int_t pad) {return Struct(sector-1)->T0[row-1][pad-1];}
 protected:
  St_tpcT0C(St_tpcT0 *table=0) : TChair(table) {}
  virtual ~St_tpcT0C() {fgInstance = 0;}
 private:
  static St_tpcT0C* fgInstance;
  ClassDefChair(St_tpcT0, tpcT0_st )
  ClassDef(St_tpcT0C,1) //C++ TChair for tpcT0 table class
};

#endif
