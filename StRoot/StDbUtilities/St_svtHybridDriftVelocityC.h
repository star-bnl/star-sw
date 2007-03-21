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
  Double_t                           CalcDriftLength(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Double_t timeBin) 
  {return CalcDriftLength(p(barrel, ladder, wafer, hybrid),timeBin);}
  Double_t                           UnCalcDriftLength(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid,Double_t x)
  {return UnCalcDriftLength(p(barrel, ladder, wafer, hybrid),x);}
  Double_t                           CalcDriftLength(svtHybridDriftVelocity_st *p, Double_t timeBin);
  Double_t                           UnCalcDriftLength(svtHybridDriftVelocity_st *p,Double_t x);
  Double_t                           DriftVelocity(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid) 
  {return DriftVelocity(p(barrel, ladder, wafer, hybrid));}
  Double_t                           DriftVelocity(svtHybridDriftVelocity_st *p);
  svtHybridDriftVelocity_st         *p(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid);
  static void                        SetAnodePitch(Double_t pitch  = 0.0250) {mAnodePitch  = pitch;}
  static void                        SetWaferLength(Double_t length= 2.9928) {mWaferLength = length;}
  static void                        SetWaferWidth(Double_t width  = 3.0000) {mWaferWidth  = width;}
  static void                        SetSamplingFrequency(Double_t samplingFrequency = 25000000.0) {mSamplingFrequency = samplingFrequency;}

  static Double_t                    AnodePitch()        {return mAnodePitch;}
  static Double_t                    WaferLength()       {return mWaferLength;}
  static Double_t                    WaferWidth()        {return mWaferWidth;}
  static Double_t                    SamplingFrequency() {return mSamplingFrequency;}
 private:
  static St_svtHybridDriftVelocityC *fgsvtHybridDriftVelocityC;
  static Double_t                    mAnodePitch;
  static Double_t                    mWaferLength;
  static Double_t                    mWaferWidth;
  static Double_t                    mSamplingFrequency;
  ClassDefChair(St_svtHybridDriftVelocity, svtHybridDriftVelocity_st )
  ClassDef(St_svtHybridDriftVelocityC,1) //C++ TChair for svtHybridDriftVelocity table class
};
#endif
