#ifndef St_tofINLSCorrC_h
#define St_tofINLSCorrC_h

#include "TChair.h"
#include "tables/St_tofINLSCorr_Table.h"

class St_tofINLSCorrC : public TChair {
 public:
  static St_tofINLSCorrC* 	instance();
  tofINLSCorr_st 	*Struct(Int_t i = 0) 	const {return ((St_tofINLSCorr*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  short 	tdigId(Int_t i = 0) 	const {return Struct(i)->tdigId;}
  short 	tdcChanId(Int_t i = 0) 	const {return Struct(i)->tdcChanId;}
  short* 	INLCorr(Int_t i = 0) 	const {return Struct(i)->INLCorr;}
 protected:
  St_tofINLSCorrC(St_tofINLSCorr *table=0) : TChair(table) {}
  virtual ~St_tofINLSCorrC() {fgInstance = 0;}
 private:
  static St_tofINLSCorrC* fgInstance;
  ClassDefChair(St_tofINLSCorr, tofINLSCorr_st )
  ClassDef(St_tofINLSCorrC,1) //C++ TChair for tofINLSCorr table class
};
#endif
