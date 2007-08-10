#ifndef St_defaultTrgLvlC_h
#define St_defaultTrgLvlC_h

#include "TChair.h"
#include "tables/St_defaultTrgLvl_Table.h"

class St_defaultTrgLvlC : public TChair {
 public:
  static St_defaultTrgLvlC* 	instance();
  defaultTrgLvl_st *Struct(Int_t i = 0) {return ((St_defaultTrgLvl*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()            {return GetNRows();}
  UInt_t 	level(Int_t i = 0) 	{return Struct(i)->level;}
 protected:
  St_defaultTrgLvlC(St_defaultTrgLvl *table=0) : TChair(table) {}
  virtual ~St_defaultTrgLvlC() {fgInstance = 0;}
 private:
  static St_defaultTrgLvlC* fgInstance;
  ClassDefChair(St_defaultTrgLvl, defaultTrgLvl_st )
  ClassDef(St_defaultTrgLvlC,1) //C++ TChair for defaultTrgLvl table class
};
#endif
