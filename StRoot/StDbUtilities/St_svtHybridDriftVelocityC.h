#ifndef St_svtHybridDriftVelocityC_h
#define St_svtHybridDriftVelocityC_h
#include "TChair.h"
#include "TArrayI.h"
#include "tables/St_svtHybridDriftVelocity_Table.h"

class St_svtHybridDriftVelocityC : public TChair {
 public:
  St_svtHybridDriftVelocityC (St_svtHybridDriftVelocity *table=0);
  virtual   ~St_svtHybridDriftVelocityC() {fgsvtHybridDriftVelocityC = 0;}
  
  static St_svtHybridDriftVelocityC *instance()  {return fgsvtHybridDriftVelocityC;}
  void                               Init();
  Double_t                           CalcTransLength(Double_t x)     {return x*mAnodePitch;}
  Double_t                           UnCalcTransLength(Double_t x)   {return x/mAnodePitch;}
  Double_t                           CalcDriftLength(Int_t barrel, Int_t ladder, Int_t wafer, 
						     Int_t hybrid, Double_t timeBin, Double_t anode) 
  {return CalcDriftLength(p(barrel, ladder, wafer, hybrid),timeBin,anode);}
  Double_t                           UnCalcDriftLength(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid,Double_t x)
  {return UnCalcDriftLength(p(barrel, ladder, wafer, hybrid),x);}
  Double_t                           CalcDriftLength(svtHybridDriftVelocity_st *p, Double_t timeBin, Double_t anode);
  Double_t                           UnCalcDriftLength(svtHybridDriftVelocity_st *p,Double_t x);
  Double_t                           DriftVelocity(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid) 
  {return DriftVelocity(p(barrel, ladder, wafer, hybrid));}
  Double_t                           DriftVelocity(svtHybridDriftVelocity_st *p);
  Double_t                           uHat(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t timeBin) 
  {return uHat(p(barrel, ladder, wafer, hybrid),timeBin);}
  Double_t                           uHat(svtHybridDriftVelocity_st *p, Double_t timeBin);
  Double_t                           vHat(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t anode) 
  {return vHat(p(barrel, ladder, wafer, hybrid),anode);}
  Double_t                           vHat(svtHybridDriftVelocity_st *p, Double_t anode);
  Double_t                           CalcU(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t timeBin, Double_t anode);
  Double_t                           CalcV(Int_t hybrid, Double_t x);
  Double_t                           UnCalcU(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t timeBin);
  Double_t                           UnCalcV(Int_t hybrid, Double_t x);
  svtHybridDriftVelocity_st         *p(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid);
  Bool_t                             IsValidDriftRegion(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t timeBin);
  static void                        SetAnodePitch(Double_t pitch  = 0.0250) {mAnodePitch  = pitch;}
  static void                        SetWaferLength(Double_t length= 2.9928) {mWaferLength = length;}
  static void                        SetWaferWidth(Double_t width  = 3.0000) {mWaferWidth  = width;}
  static void                        SetSamplingFrequency(Double_t samplingFrequency = 25000000.0) {mSamplingFrequency = samplingFrequency;}

  static Double_t                    AnodePitch()        {return mAnodePitch;}
  static Double_t                    WaferLength()       {return mWaferLength;}
  static Double_t                    WaferWidth()        {return mWaferWidth;}
  static Double_t                    SamplingFrequency() {return mSamplingFrequency;}
  static Double_t                    STcheb(Int_t N, Double_t *par, Double_t x);
 private:
  static St_svtHybridDriftVelocityC *fgsvtHybridDriftVelocityC;
  static Double_t                    mAnodePitch;
  static Double_t                    mWaferLength;
  static Double_t                    mWaferWidth;
  static Double_t                    mSamplingFrequency;
  static Double_t                    mNoAnodes;
  ClassDefChair(St_svtHybridDriftVelocity, svtHybridDriftVelocity_st )
  ClassDef(St_svtHybridDriftVelocityC,1) //C++ TChair for svtHybridDriftVelocity table class
};
#endif
