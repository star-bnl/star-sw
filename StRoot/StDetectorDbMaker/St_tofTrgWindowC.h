#ifndef St_tofTrgWindowC_h
#define St_tofTrgWindowC_h

#include "TChair.h"
#include "tables/St_tofTrgWindow_Table.h"

class St_tofTrgWindowC : public TChair {
 public:
  static St_tofTrgWindowC* 	instance();
  tofTrgWindow_st 	*Struct(Int_t i = 0) 	const {return ((St_tofTrgWindow*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  unsigned short 	trgWindow_Min(Int_t i = 0) 	const {return Struct(i)->trgWindow_Min;}
  unsigned short 	trgWindow_Max(Int_t i = 0) 	const {return Struct(i)->trgWindow_Max;}
 protected:
  St_tofTrgWindowC(St_tofTrgWindow *table=0) : TChair(table) {}
  virtual ~St_tofTrgWindowC() {fgInstance = 0;}
 private:
  static St_tofTrgWindowC* fgInstance;
  ClassDefChair(St_tofTrgWindow, tofTrgWindow_st )
  ClassDef(St_tofTrgWindowC,1) //C++ TChair for tofTrgWindow table class
};
#endif
