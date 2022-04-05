#ifndef St_istChipConfigC_h
#define St_istChipConfigC_h

#include "TChair.h"
#include "tables/St_istChipConfig_Table.h"

class St_istChipConfigC : public TChair {
 public:
  static St_istChipConfigC* 	instance();
  istChipConfig_st 	*Struct(Int_t i = 0) 	const {return ((St_istChipConfig*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	run(Int_t i = 0) 	const {return Struct(i)->run;}
  UChar_t* 	s(Int_t i = 0) 	const {return Struct(i)->s;}
 protected:
  St_istChipConfigC(St_istChipConfig *table=0) : TChair(table) {}
  virtual ~St_istChipConfigC() {fgInstance = 0;}
 private:
  static St_istChipConfigC* fgInstance;
  ClassDefChair(St_istChipConfig, istChipConfig_st )
  ClassDef(St_istChipConfigC,1) //C++ TChair for istChipConfig table class
};
#endif
