#ifndef St_istGainC_h
#define St_istGainC_h

#include "TChair.h"
#include "tables/St_istGain_Table.h"

class St_istGainC : public TChair {
 public:
  static St_istGainC* 	instance();
  istGain_st 	*Struct(Int_t i = 0) 	const {return ((St_istGain*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Float_t* 	gain(Int_t i = 0) 	const {return Struct(i)->gain;}
 protected:
  St_istGainC(St_istGain *table=0) : TChair(table) {}
  virtual ~St_istGainC() {fgInstance = 0;}
 private:
  static St_istGainC* fgInstance;
  ClassDefChair(St_istGain, istGain_st )
  ClassDef(St_istGainC,1) //C++ TChair for istGain table class
};
#endif
