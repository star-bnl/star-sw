#ifndef St_tpcPadResponseC_h
#define St_tpcPadResponseC_h
#include "TChair.h"
#include "tables/St_tpcPadResponse_Table.h"

class St_tpcPadResponseC : public TChair {
 public:
  St_tpcPadResponseC (St_tpcPadResponse *table=0) : TChair(table) {}
  Float_t  innerGasGainFluctuation() ;
  Float_t  outerGasGainFluctuation() ;
  Float_t  innerPadResponseSigma()   ;
  Float_t  outerPadResponseSigma()   ;
  Float_t  innerWirePadCoupling()    ;
  Float_t  outerWirePadCoupling()    ;
  Float_t  innerRowNormalization()   ;
  Float_t  outerRowNormalization()   ;
  Float_t  BoundaryOfStepFunctions(Int_t i) ;
  Float_t  innerChargeFractionConstants(Int_t i) ;
  Float_t  outerChargeFractionConstants(Int_t i) ;
  Float_t  errorFunctionRange() ;
  Int_t    errorFunctionEntry() ;
  Float_t  longitudinalDiffusionConstant() ;
  Float_t  transverseDiffusionConstant() ;
  Float_t  InnerOuterFactor() ;
  
  ClassDefChair(St_tpcPadResponse, tpcPadResponse_st )
  ClassDef(St_tpcPadResponseC,1) //C++ TChair for tpcPadResponse table class
};
#endif
