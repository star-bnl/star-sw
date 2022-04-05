#ifndef St_tofTotbCorrC_h
#define St_tofTotbCorrC_h

#include "St_tofCorrC.h"
#include "tables/St_tofTotbCorr_Table.h"

class St_tofTotbCorrC : public St_tofCorrC {
 public:
  static St_tofTotbCorrC* 	instance();
  tofTotbCorr_st*Struct(Int_t i = 0) 	const {return ((St_tofTotbCorr*) Table())->GetTable()+i;}
  Short_t 	trayId(Int_t i = 0) 	const {return Struct(i)->trayId;}
  Short_t 	moduleId(Int_t i = 0) 	const {return Struct(i)->moduleId;}
  Short_t 	cellId(Int_t i = 0) 	const {return Struct(i)->cellId;}
  Short_t 	tdcId(Int_t i = 0) 	const {return Struct(i)->tdcId;}
  Float_t* 	tot(Int_t i = 0) 	const {return Struct(i)->tot;}
  Float_t* 	corr(Int_t i = 0) 	const {return Struct(i)->corr;}
  Float_t       Corr(Int_t tray, Int_t module, Int_t cell, Float_t x);
 protected:
  St_tofTotbCorrC(St_tofTotbCorr *table=0);
  virtual ~St_tofTotbCorrC() {fgInstance = 0;}
 private:
  static St_tofTotbCorrC* fgInstance;
  ClassDefChair(St_tofTotbCorr, tofTotbCorr_st )
  ClassDef(St_tofTotbCorrC,1) //C++ TChair for tofTotbCorr table class
};
#endif
