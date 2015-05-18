#ifndef St_tofTDIGOnTrayC_h
#define St_tofTDIGOnTrayC_h

#include "TChair.h"
#include "tables/St_tofTDIGOnTray_Table.h"

class St_tofTDIGOnTrayC : public TChair {
 public:
  static St_tofTDIGOnTrayC* 	instance();
  tofTDIGOnTray_st 	*Struct(Int_t i = 0) 	const {return ((St_tofTDIGOnTray*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  short 	trayId(Int_t i = 0) 	const {return Struct(i)->trayId;}
  short* 	tdigId(Int_t i = 0) 	const {return Struct(i)->tdigId;}
 protected:
  St_tofTDIGOnTrayC(St_tofTDIGOnTray *table=0) : TChair(table) {}
  virtual ~St_tofTDIGOnTrayC() {fgInstance = 0;}
 private:
  static St_tofTDIGOnTrayC* fgInstance;
  ClassDefChair(St_tofTDIGOnTray, tofTDIGOnTray_st )
  ClassDef(St_tofTDIGOnTrayC,1) //C++ TChair for tofTDIGOnTray table class
};
#endif
