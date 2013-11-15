#ifndef St_bemcMapC_h
#define St_bemcMapC_h

#include "TChair.h"
#include "tables/St_bemcMap_Table.h"

class St_bemcMapC : public TChair {
 public:
  static St_bemcMapC* 	instance();
  bemcMap_st 	*Struct(Int_t i = 0) 	const {return ((St_bemcMap*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t 	m(Int_t i = 0) 	const {return Struct(i)->m;}
  UChar_t 	e(Int_t i = 0) 	const {return Struct(i)->e;}
  UChar_t 	s(Int_t i = 0) 	const {return Struct(i)->s;}
  unsigned short 	daqID(Int_t i = 0) 	const {return Struct(i)->daqID;}
  UChar_t 	crate(Int_t i = 0) 	const {return Struct(i)->crate;}
  UChar_t 	crateChannel(Int_t i = 0) 	const {return Struct(i)->crateChannel;}
  UChar_t 	TDC(Int_t i = 0) 	const {return Struct(i)->TDC;}
  unsigned short 	triggerPatch(Int_t i = 0) 	const {return Struct(i)->triggerPatch;}
  UChar_t 	jetPatch(Int_t i = 0) 	const {return Struct(i)->jetPatch;}
  unsigned short 	DSM(Int_t i = 0) 	const {return Struct(i)->DSM;}
  Float_t 	eta(Int_t i = 0) 	const {return Struct(i)->eta;}
  Float_t 	phi(Int_t i = 0) 	const {return Struct(i)->phi;}
 protected:
  St_bemcMapC(St_bemcMap *table=0) : TChair(table) {}
  virtual ~St_bemcMapC() {fgInstance = 0;}
 private:
  static St_bemcMapC* fgInstance;
  ClassDefChair(St_bemcMap, bemcMap_st )
  ClassDef(St_bemcMapC,1) //C++ TChair for bemcMap table class
};
#endif
