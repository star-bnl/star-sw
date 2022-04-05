#ifndef St_TpcEffectivedXC_h
#define St_TpcEffectivedXC_h

#include "TChair.h"
#include "tables/St_TpcEffectivedX_Table.h"

class St_TpcEffectivedXC : public TChair {
 public:
  static St_TpcEffectivedXC* 	instance();
  TpcEffectivedX_st 	*Struct(Int_t i = 0) 	const {return ((St_TpcEffectivedX*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t 	scaleInner(Int_t i = 0) 	const {return Struct(i)->scaleInner;}
  Float_t 	scaleOuter(Int_t i = 0) 	const {return Struct(i)->scaleOuter;}
 protected:
  St_TpcEffectivedXC(St_TpcEffectivedX *table=0) : TChair(table) {}
  virtual ~St_TpcEffectivedXC() {fgInstance = 0;}
 private:
  static St_TpcEffectivedXC* fgInstance;
  ClassDefChair(St_TpcEffectivedX, TpcEffectivedX_st )
  ClassDef(St_TpcEffectivedXC,1) //C++ TChair for TpcEffectivedX table class
};
#endif
