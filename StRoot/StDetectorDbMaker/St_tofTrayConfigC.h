#ifndef St_tofTrayConfigC_h
#define St_tofTrayConfigC_h

#include "TChair.h"
#include "tables/St_tofTrayConfig_Table.h"

class St_tofTrayConfigC : public TChair {
 public:
  static St_tofTrayConfigC* 	instance();
  tofTrayConfig_st 	*Struct(Int_t i = 0) 	const {return ((St_tofTrayConfig*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	entries(Int_t i = 0) 	const {return Struct(i)->entries;}
  short* 	iTray(Int_t i = 0) 	const {return Struct(i)->iTray;}
  short* 	nModules(Int_t i = 0) 	const {return Struct(i)->nModules;}
 protected:
  St_tofTrayConfigC(St_tofTrayConfig *table=0) : TChair(table) {}
  virtual ~St_tofTrayConfigC() {fgInstance = 0;}
 private:
  static St_tofTrayConfigC* fgInstance;
  ClassDefChair(St_tofTrayConfig, tofTrayConfig_st )
  ClassDef(St_tofTrayConfigC,1) //C++ TChair for tofTrayConfig table class
};
#endif
