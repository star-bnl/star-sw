//////////////////////////////////////////////////////////////////////////
//
// StPmtSignal
// 
// This class provides the response of PMT.
// It is C++ version of fortran code developed by V.Rykov.
//
//////////////////////////////////////////////////////////////////////////
#ifndef STAR_StPmtSignal
#define STAR_StPmtSignal
#include <TNamed.h>
#include <TArrayF.h>
#include <TRandom.h>

class StPmtSignal : public TNamed {
protected:
  // Characteristics of PMT
  Int_t    mPmt;              // Type of PMT 
  Int_t    mNumDynodes;       // ndyn
  Float_t  mPmtGain;          // pmtgain
  Float_t  mProbPhotoCatode;  // cnoise
  Float_t  mProbPhotoDynode;  // dnoise
  TArrayF  mNodeVoltage;      // base

  Float_t  mFromPheToAdc;   // gain in V.Rykov's notation
  Float_t  mMeanAdcPed;     // in ADC-counts
  Float_t  mRmsAdcPed;      // in ADC-counts

  TRandom  mRandom;
public:
  StPmtSignal();
  StPmtSignal(Int_t );  
  ~StPmtSignal() {/* Nothing */};
  void print(Int_t);

  void    init();
  void    initPmtOne();
  Float_t getFromPheToAdc() const {return mFromPheToAdc;}
  Float_t getMeanAdcPed()   const {return mMeanAdcPed;}
  Float_t getRmsAdcPed()    const {return mRmsAdcPed;}
  Int_t   getAdc(Int_t nphe, Int_t iver);

  void setFromPheToAdc(Float_t var) {mFromPheToAdc = var;}
  void setMeanAdcPed(Float_t var)   {mMeanAdcPed   = var;}
  void setRmsAdcPed(Float_t var)    {mRmsAdcPed    = var;}
  void setAllParameters(Float_t,Float_t,Float_t);
  void printParameters();

 ClassDef(StPmtSignal, 1)  
};

#endif
