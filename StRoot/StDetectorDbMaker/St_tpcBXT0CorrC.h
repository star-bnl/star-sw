#ifndef St_tpcBXT0CorrC_h
#define St_tpcBXT0CorrC_h

#include "TChair.h"
#include "tables/St_tpcBXT0Corr_Table.h"

class St_tpcBXT0CorrC : public TChair {
 public:
  tpcBXT0Corr_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcBXT0Corr*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	type(Int_t i = 0) 	const {return Struct(i)->type;}
  Int_t 	idx(Int_t i = 0) 	const {return Struct(i)->idx;}
  Int_t 	nrows(Int_t i = 0) 	const {return Struct(i)->nrows;}
  Int_t 	npar(Int_t i = 0) 	const {return Struct(i)->npar;}
  Double_t 	OffSet(Int_t i = 0) 	const {return Struct(i)->OffSet;}
  Double_t 	min(Int_t i = 0) 	const {return Struct(i)->min;}
  Double_t 	max(Int_t i = 0) 	const {return Struct(i)->max;}
  Double_t* 	a(Int_t i = 0) 	const {return Struct(i)->a;}
  Char_t* 	comment(Int_t i = 0) 	const {return Struct(i)->comment;}
 protected:
  St_tpcBXT0CorrC(St_tpcBXT0Corr *table=0) : TChair(table) {}
 private:
  ClassDefChair(St_tpcBXT0Corr, tpcBXT0Corr_st )
  ClassDef(St_tpcBXT0CorrC,1) //C++ TChair for tpcBXT0Corr table class
};
#endif
