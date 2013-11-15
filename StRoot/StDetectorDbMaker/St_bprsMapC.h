#ifndef St_bprsMapC_h
#define St_bprsMapC_h

#include "TChair.h"
#include "tables/St_bprsMap_Table.h"

class St_bprsMapC : public TChair {
 public:
  static St_bprsMapC* 	instance();
  bprsMap_st 	*Struct(Int_t i = 0) 	const {return ((St_bprsMap*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t 	m(Int_t i = 0) 	const {return Struct(i)->m;}
  UChar_t 	e(Int_t i = 0) 	const {return Struct(i)->e;}
  UChar_t 	s(Int_t i = 0) 	const {return Struct(i)->s;}
  UChar_t 	PMTbox(Int_t i = 0) 	const {return Struct(i)->PMTbox;}
  UChar_t 	MAPMT(Int_t i = 0) 	const {return Struct(i)->MAPMT;}
  UChar_t 	pixel(Int_t i = 0) 	const {return Struct(i)->pixel;}
  UChar_t 	rdo(Int_t i = 0) 	const {return Struct(i)->rdo;}
  unsigned short 	rdoChannel(Int_t i = 0) 	const {return Struct(i)->rdoChannel;}
  UChar_t 	wire(Int_t i = 0) 	const {return Struct(i)->wire;}
  UChar_t 	feeA(Int_t i = 0) 	const {return Struct(i)->feeA;}
  UChar_t 	SCA(Int_t i = 0) 	const {return Struct(i)->SCA;}
  UChar_t 	SCAChannel(Int_t i = 0) 	const {return Struct(i)->SCAChannel;}
  UChar_t 	powerSupply(Int_t i = 0) 	const {return Struct(i)->powerSupply;}
  UChar_t 	powerSupplyModule(Int_t i = 0) 	const {return Struct(i)->powerSupplyModule;}
  UChar_t 	powerSupplyChannel(Int_t i = 0) 	const {return Struct(i)->powerSupplyChannel;}
  Float_t 	eta(Int_t i = 0) 	const {return Struct(i)->eta;}
  Float_t 	phi(Int_t i = 0) 	const {return Struct(i)->phi;}
 protected:
  St_bprsMapC(St_bprsMap *table=0) : TChair(table) {}
  virtual ~St_bprsMapC() {fgInstance = 0;}
 private:
  static St_bprsMapC* fgInstance;
  ClassDefChair(St_bprsMap, bprsMap_st )
  ClassDef(St_bprsMapC,1) //C++ TChair for bprsMap table class
};
#endif
