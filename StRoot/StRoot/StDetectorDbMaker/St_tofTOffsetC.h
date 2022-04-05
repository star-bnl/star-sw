#ifndef St_tofTOffsetC_h
#define St_tofTOffsetC_h

#include "St_tofCorrC.h"
#include "tables/St_tofTOffset_Table.h"

class St_tofTOffsetC : public St_tofCorrC {
 public:
  static St_tofTOffsetC* 	instance();
  tofTOffset_st 	*Struct(Int_t i = 0) 	const {return ((St_tofTOffset*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  short 	trayId(Int_t i = 0) 	const {return Struct(i)->trayId;}
  Float_t* 	T0(Int_t i = 0) 	const {return Struct(i)->T0;}
  Float_t       t0(Int_t tray, Int_t module, Int_t cell) const;
 protected:
  St_tofTOffsetC(St_tofTOffset *table=0) : St_tofCorrC(table) {}
  virtual ~St_tofTOffsetC() {fgInstance = 0;}
 private:
  static St_tofTOffsetC* fgInstance;
  ClassDefChair(St_tofTOffset, tofTOffset_st )
  ClassDef(St_tofTOffsetC,1) //C++ TChair for tofTOffset table class
};
#endif
