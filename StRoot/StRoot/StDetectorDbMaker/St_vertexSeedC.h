#ifndef St_vertexSeedC_h
#define St_vertexSeedC_h

#include "TChair.h"
#include "tables/St_vertexSeed_Table.h"

class St_vertexSeedC : public TChair {
 public:
  static St_vertexSeedC* 	instance();
  vertexSeed_st 	*Struct(Int_t i = 0) 	const {return ((St_vertexSeed*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t 	x0(Int_t i = 0) 	const {return Struct(i)->x0;}
  Float_t 	dxdz(Int_t i = 0) 	const {return Struct(i)->dxdz;}
  Float_t 	y0(Int_t i = 0) 	const {return Struct(i)->y0;}
  Float_t 	dydz(Int_t i = 0) 	const {return Struct(i)->dydz;}
  Float_t 	err_x0(Int_t i = 0) 	const {return Struct(i)->err_x0;}
  Float_t 	err_dxdz(Int_t i = 0) 	const {return Struct(i)->err_dxdz;}
  Float_t 	err_y0(Int_t i = 0) 	const {return Struct(i)->err_y0;}
  Float_t 	err_dydz(Int_t i = 0) 	const {return Struct(i)->err_dydz;}
  Float_t 	chisq_dof(Int_t i = 0) 	const {return Struct(i)->chisq_dof;}
  Float_t 	weight(Int_t i = 0) 	const {return Struct(i)->weight;}
  Float_t 	stats(Int_t i = 0) 	const {return Struct(i)->stats;}
 protected:
  St_vertexSeedC(St_vertexSeed *table=0) : TChair(table) {}
  virtual ~St_vertexSeedC() {fgInstance = 0;}
 private:
  static St_vertexSeedC* fgInstance;
  ClassDefChair(St_vertexSeed, vertexSeed_st )
  ClassDef(St_vertexSeedC,1) //C++ TChair for vertexSeed table class
};
#endif
