#ifndef St_pvpdStrobeDefC_h
#define St_pvpdStrobeDefC_h

#include "TChair.h"
#include "tables/St_pvpdStrobeDef_Table.h"

class St_pvpdStrobeDefC : public TChair {
 public:
  static St_pvpdStrobeDefC* 	instance();
  pvpdStrobeDef_st 	*Struct(Int_t i = 0) 	const {return ((St_pvpdStrobeDef*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	id(Int_t i = 0) 	const {return Struct(i)->id;}
  Int_t 	strobeTdcMin(Int_t i = 0) 	const {return Struct(i)->strobeTdcMin;}
  Int_t 	strobeTdcMax(Int_t i = 0) 	const {return Struct(i)->strobeTdcMax;}
 protected:
  St_pvpdStrobeDefC(St_pvpdStrobeDef *table=0) : TChair(table) {}
  virtual ~St_pvpdStrobeDefC() {fgInstance = 0;}
 private:
  static St_pvpdStrobeDefC* fgInstance;
  ClassDefChair(St_pvpdStrobeDef, pvpdStrobeDef_st )
  ClassDef(St_pvpdStrobeDefC,1) //C++ TChair for pvpdStrobeDef table class
};
#endif
