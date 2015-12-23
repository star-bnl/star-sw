#ifndef St_tofZbCorrC_h
#define St_tofZbCorrC_h

#include "St_tofCorrC.h"
#include "tables/St_tofZbCorr_Table.h"

class St_tofZbCorrC : public St_tofCorrC {
 public:
  static St_tofZbCorrC* 	instance();
  tofZbCorr_st 	*Struct(Int_t i = 0) 	const {return ((St_tofZbCorr*) Table())->GetTable()+i;}
  Short_t 	trayId(Int_t i = 0) 	const {return Struct(i)->trayId;}
  Short_t 	moduleId(Int_t i = 0) 	const {return Struct(i)->moduleId;}
  Short_t 	cellId(Int_t i = 0) 	const {return Struct(i)->cellId;}
  Float_t* 	z(Int_t i = 0) 	const {return Struct(i)->z;}
  Float_t* 	corr(Int_t i = 0) 	const {return Struct(i)->corr;}
  Float_t       Corr(Int_t tray, Int_t module, Int_t cell, Float_t x);
 protected:
  St_tofZbCorrC(St_tofZbCorr *table=0);
 private:
  static St_tofZbCorrC* fgInstance;
  ClassDefChair(St_tofZbCorr, tofZbCorr_st )
  ClassDef(St_tofZbCorrC,1) //C++ TChair for tofZbCorr table class
};
#endif
