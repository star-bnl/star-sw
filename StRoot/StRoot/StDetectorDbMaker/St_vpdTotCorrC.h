#ifndef St_vpdTotCorrC_h
#define St_vpdTotCorrC_h

#include "St_tofCorrC.h"
#include "tables/St_vpdTotCorr_Table.h"
class St_vpdTotCorrC : public St_tofCorrC {
 public:
  static St_vpdTotCorrC* 	instance();
  vpdTotCorr_st 	*Struct(Int_t i = 0) 	const {return ((St_vpdTotCorr*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  short 	tubeId(Int_t i = 0) 	const {return Struct(i)->tubeId;}
  Float_t* 	tot(Int_t i = 0) 	const {return Struct(i)->tot;}
  Float_t* 	corr(Int_t i = 0) 	const {return Struct(i)->corr;}
  short 	corralgo(Int_t i = 0) 	const {return Struct(i)->corralgo;}
  Float_t       Corr(Int_t i, Float_t x);
 protected:
  St_vpdTotCorrC(St_vpdTotCorr *table=0) : St_tofCorrC(table) {}
  virtual ~St_vpdTotCorrC() {fgInstance = 0;}
 private:
  static St_vpdTotCorrC* fgInstance;
  ClassDefChair(St_vpdTotCorr, vpdTotCorr_st )
  ClassDef(St_vpdTotCorrC,1) //C++ St_tofCorrC for vpdTotCorr table class
};
#endif
