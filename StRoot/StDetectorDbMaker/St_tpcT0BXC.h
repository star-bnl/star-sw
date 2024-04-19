#ifndef St_tpcT0BXC_h
#define St_tpcT0BXC_h

#include "TChair.h"
#include "tables/St_tpcT0BX_Table.h"

class St_tpcT0BXC : public TChair {
 public:
  static St_tpcT0BXC* 	instance();
  tpcT0BX_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcT0BX*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	detId(Int_t i = 0) 	const {return Struct(i)->detId;}
  Char_t* 	name(Int_t i = 0) 	const {return Struct(i)->name;}
  Float_t 	xmin(Int_t i = 0) 	const {return Struct(i)->xmin;}
  Float_t 	xmax(Int_t i = 0) 	const {return Struct(i)->xmax;}
  Float_t 	tMean(Int_t i = 0) 	const {return Struct(i)->tMean;}
  Float_t 	vMean(Int_t i = 0) 	const {return Struct(i)->vMean;}
  Float_t 	toff(Int_t i = 0) 	const {return Struct(i)->toff;}
  Float_t 	dtoff(Int_t i = 0) 	const {return Struct(i)->dtoff;}
  Float_t 	slope(Int_t i = 0) 	const {return Struct(i)->slope;}
  Float_t 	dslope(Int_t i = 0) 	const {return Struct(i)->dslope;}
  Float_t 	CountPs(Int_t i = 0) 	const {return Struct(i)->CountPs;}
  Float_t 	dCountPs(Int_t i = 0) 	const {return Struct(i)->dCountPs;}
  Double_t      getT0(Double_t values[7]) const;
 protected:
  St_tpcT0BXC(St_tpcT0BX *table=0) : TChair(table) {}
  virtual ~St_tpcT0BXC() {fgInstance = 0;}
 private:
  static St_tpcT0BXC* fgInstance;
  ClassDefChair(St_tpcT0BX, tpcT0BX_st )
  ClassDef(St_tpcT0BXC,1) //C++ TChair for tpcT0BX table class
};
#endif
