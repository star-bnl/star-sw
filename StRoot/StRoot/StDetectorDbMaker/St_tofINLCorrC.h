#ifndef St_tofINLCorrC_h
#define St_tofINLCorrC_h

#include "TChair.h"
#include "tables/St_tofINLCorr_Table.h"

class St_tofINLCorrC : public TChair {
 public:
  static St_tofINLCorrC* 	instance();
  tofINLCorr_st 	*Struct(Int_t i = 0) 	const {return ((St_tofINLCorr*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  short 	tdigId(Int_t i = 0) 	const {return Struct(i)->tdigId;}
  short 	tdcChanId(Int_t i = 0) 	const {return Struct(i)->tdcChanId;}
  Float_t* 	INLCorr(Int_t i = 0) 	const {return Struct(i)->INLCorr;}
 protected:
  St_tofINLCorrC(St_tofINLCorr *table=0) : TChair(table) {}
  virtual ~St_tofINLCorrC() {fgInstance = 0;}
 private:
  static St_tofINLCorrC* fgInstance;
  ClassDefChair(St_tofINLCorr, tofINLCorr_st )
  ClassDef(St_tofINLCorrC,1) //C++ TChair for tofINLCorr table class
};
#endif
