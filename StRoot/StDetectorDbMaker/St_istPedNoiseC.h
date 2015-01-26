#ifndef St_istPedNoiseC_h
#define St_istPedNoiseC_h

#include "TChair.h"
#include "tables/St_istPedNoise_Table.h"

class St_istPedNoiseC : public TChair {
 public:
  static St_istPedNoiseC* 	instance();
  istPedNoise_st 	*Struct(Int_t i = 0) 	const {return ((St_istPedNoise*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  unsigned short* 	cmNoise(Int_t i = 0) 	const {return Struct(i)->cmNoise;}
  unsigned short* 	pedestal(Int_t i = 0) 	const {return Struct(i)->pedestal;}
  unsigned short* 	rmsNoise(Int_t i = 0) 	const {return Struct(i)->rmsNoise;}
 protected:
  St_istPedNoiseC(St_istPedNoise *table=0) : TChair(table) {}
  virtual ~St_istPedNoiseC() {fgInstance = 0;}
 private:
  static St_istPedNoiseC* fgInstance;
  ClassDefChair(St_istPedNoise, istPedNoise_st )
  ClassDef(St_istPedNoiseC,1) //C++ TChair for istPedNoise table class
};
#endif
