#ifndef St_tpcCorrectionC_h
#define St_tpcCorrectionC_h
#include "TChair.h"
#include "tables/St_tpcCorrection_Table.h"

class St_tpcCorrectionC : public TChair {
 public:
  St_tpcCorrectionC (St_tpcCorrection *table=0) : TChair(table) {}
  virtual ~St_tpcCorrectionC() {}
  Double_t CalcCorrection(Int_t i, Double_t x, Double_t z = 0);
  Double_t SumSeries(tpcCorrection_st *cor, Double_t x, Double_t z = 0);
  ClassDefChair(St_tpcCorrection, tpcCorrection_st )
  ClassDef(St_tpcCorrectionC,1) //C++ TChair for tpcCorrection table class
};
#endif
