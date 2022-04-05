#ifndef St_svtCorrectionC_h
#define St_svtCorrectionC_h
#include "TChair.h"
#include "tables/St_svtCorrection_Table.h"

class St_svtCorrectionC : public TChair {
 public:
  St_svtCorrectionC (St_svtCorrection *table=0) : TChair(table) {}
  virtual ~St_svtCorrectionC() {}
  static Double_t STcheb(Int_t N, Double_t *par, Double_t x);
  Double_t CalcCorrection(Int_t layer, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t u);
  svtCorrection_st *pCorrection(Int_t layer, Int_t ladder, Int_t wafer, Int_t hybrid);
 private:
  ClassDefChair(St_svtCorrection, svtCorrection_st )
  ClassDef(St_svtCorrectionC,1) //C++ TChair for svtCorrection table class
};
#endif
