#ifndef St_tpcRDOT0offsetC_h
#define St_tpcRDOT0offsetC_h

#include "TChair.h"
#include "tables/St_tpcRDOT0offset_Table.h"

class St_tpcRDOT0offsetC : public TChair {
 public:
  static St_tpcRDOT0offsetC* 	instance();
  tpcRDOT0offset_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcRDOT0offset*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t* 	isShifted(Int_t i = 0) 	const {return Struct(i)->isShifted;}
  Bool_t        IsShfited(Int_t sector) const {return isShifted()[sector-1];}
  Float_t       T0(Int_t sector, Int_t padrow, Int_t pad) const;
 protected:
  St_tpcRDOT0offsetC(St_tpcRDOT0offset *table=0) : TChair(table) {}
  virtual ~St_tpcRDOT0offsetC() {fgInstance = 0;}
 private:
  static St_tpcRDOT0offsetC* fgInstance;
  ClassDefChair(St_tpcRDOT0offset, tpcRDOT0offset_st )
  ClassDef(St_tpcRDOT0offsetC,1) //C++ TChair for tpcRDOT0offset table class
};
#endif
