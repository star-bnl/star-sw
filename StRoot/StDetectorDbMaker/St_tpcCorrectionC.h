#ifndef St_tpcCorrectionC_h
#define St_tpcCorrectionC_h

#include "TChair.h"
#include "tables/St_tpcCorrection_Table.h"

class St_tpcCorrectionC : public TChair {
 public:
  St_tpcCorrectionC(St_tpcCorrection *table=0) : TChair(table) {}
  virtual ~St_tpcCorrectionC() {}
  tpcCorrection_st 	*Struct(Int_t i = 0) 	const {return ((St_tpcCorrection*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	type(Int_t i = 0) 	const {return Struct(i)->type;}
  Int_t 	idx(Int_t i = 0) 	const {return Struct(i)->idx;}
  Int_t 	nrows(Int_t i = 0) 	const {return Struct(i)->nrows;}
  Int_t 	npar(Int_t i = 0) 	const {return Struct(i)->npar;}
  Double_t 	OffSet(Int_t i = 0) 	const {return Struct(i)->OffSet;}
  Double_t 	min(Int_t i = 0) 	const {return Struct(i)->min;}
  Double_t 	max(Int_t i = 0) 	const {return Struct(i)->max;}
  Double_t* 	a(Int_t i = 0) 	        const {return Struct(i)->a;}
  Double_t CalcCorrection(Int_t i, Double_t x, Double_t z = 0, Int_t NparMax = -1);
  Double_t SumSeries(tpcCorrection_st *cor, Double_t x, Double_t z = 0, Int_t NparMax = -1);
  ClassDefChair(St_tpcCorrection, tpcCorrection_st )
  ClassDef(St_tpcCorrectionC,1) //C++ TChair for tpcCorrection table class
};
#endif
