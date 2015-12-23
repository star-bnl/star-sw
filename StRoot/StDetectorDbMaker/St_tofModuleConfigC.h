#ifndef St_tofModuleConfigC_h
#define St_tofModuleConfigC_h

#include "TChair.h"
#include "tables/St_tofModuleConfig_Table.h"

class St_tofModuleConfigC : public TChair {
 public:
  static St_tofModuleConfigC* 	instance();
  tofModuleConfig_st 	*Struct(Int_t i = 0) 	const {return ((St_tofModuleConfig*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	entries(Int_t i = 0) 	const {return Struct(i)->entries;}
  short* 	iTray(Int_t i = 0) 	const {return Struct(i)->iTray;}
  short* 	iModule(Int_t i = 0) 	const {return Struct(i)->iModule;}
  short* 	iStatModule(Int_t i = 0) 	const {return Struct(i)->iStatModule;}
  short* 	iStatCells(Int_t i = 0) 	const {return Struct(i)->iStatCells;}
  short* 	iChannel(Int_t i = 0) 	const {return Struct(i)->iChannel;}
 protected:
  St_tofModuleConfigC(St_tofModuleConfig *table=0) : TChair(table) {}
  virtual ~St_tofModuleConfigC() {fgInstance = 0;}
 private:
  static St_tofModuleConfigC* fgInstance;
  ClassDefChair(St_tofModuleConfig, tofModuleConfig_st )
  ClassDef(St_tofModuleConfigC,1) //C++ TChair for tofModuleConfig table class
};
#endif
