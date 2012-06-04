#ifndef St_tofGeomAlignC_h
#define St_tofGeomAlignC_h

#include "TChair.h"
#include "tables/St_tofGeomAlign_Table.h"

class St_tofGeomAlignC : public TChair {
 public:
  static St_tofGeomAlignC* 	instance();
  tofGeomAlign_st 	*Struct(Int_t i = 0) 	const {return ((St_tofGeomAlign*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t 	z0(Int_t i = 0) 	const {return Struct(i)->z0;}
  Float_t 	x0(Int_t i = 0) 	const {return Struct(i)->x0;}
  Float_t 	phi0(Int_t i = 0) 	const {return Struct(i)->phi0;}
  Float_t 	angle0(Int_t i = 0) 	const {return Struct(i)->angle0;}
 protected:
  St_tofGeomAlignC(St_tofGeomAlign *table=0) : TChair(table) {}
  virtual ~St_tofGeomAlignC() {fgInstance = 0;}
 private:
  static St_tofGeomAlignC* fgInstance;
  ClassDefChair(St_tofGeomAlign, tofGeomAlign_st )
  ClassDef(St_tofGeomAlignC,1) //C++ TChair for tofGeomAlign table class
};
#endif
