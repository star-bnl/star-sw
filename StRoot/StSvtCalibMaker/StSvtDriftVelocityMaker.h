/***************************************************************************
 *
 * $Id: StSvtDriftVelocityMaker.h,v 1.1 2002/01/18 19:57:43 willson Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Drift Velocity Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDriftVelocityMaker.h,v $
 * Revision 1.1  2002/01/18 19:57:43  willson
 * Drift Velocity Calculation v.1
 *
 *
 **************************************************************************/

#ifndef STSVTDRIFTVELOCITYMAKER_H
#define STSVTDRIFTVELOCITYMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <string>

#include "TH1.h"
#include "TH2.h"

class StEvent;
class StTrack;
class StSvtHit;
class StChain;
class StSvtData;
class StSvtHybridDriftVelocity;
class StSvtHybridCollection;
class StSvtHitCollection;

class StSvtDriftVelocityMaker : public StMaker {
  private:
  StSvtHybridCollection* mSvtData;                    //!
  StSvtHybridCollection* mSvtDriftVeloc;  //!
  TH1D** mHybridDriftVelocityHisto; //!
  TH2D** mHybridDriftVelocity2DHisto; //!
  TH1D* mGlobalDriftVelocityHisto; //!
  TH2D* mGlobalDriftVelocity2DHisto; //!
  TH1D* mCalculatedDriftVelocity; //!
  Int_t mEventCounter;
  Int_t mHitCounter;
  Int_t mNumBinsDV;    // Number of time bins to use in drift velocity calculation.  Default=128
  Int_t mMaximumDV;    // Max Time Bin to look at. Default=127
  Int_t mMinimumDV;    // Min Time Bin to look at. Default=0
  bool  mRaw;          // Raw data?  false=StEvent data
  bool mDebug;         // If true, make 2D histos, and keep everything.  Also, save to TH1D instead of ascii file
  
  /* To calculate drift velocity, take the 1D histo of hits vs. time.  Find the average number of hits
     in some number of time bins in the middle of the histogram.  
       For mMoveFoward=true:
       Start in the middle of the 1D histo, and move up the time bins until the number of hits per time bin 
       are LESS THAN  [average*mFraction].  Record this time bin, j, and calculate drift velocity with this formula:
          Drift Velocity = 3.0 cm/Drift Time  (cm/sec)
	  Drift Time = (j-mMinimumDV-mT0Guess)*(128.0/mNumBinsDV)*(0.00000004 sec/time bin)

       For mMoveForward=false:
       Start at the end of the 1D histo, and move up the time bins until the number of hits per time bin are
       GREATER THAN  [average*mFraction].  Record this time bin, j, and calculate drift velocity the same way.

       mT0Guess = T0 guess (units: time bucket bins)   */
  
  double mFraction;  // Default: 0.5
  double mT0Guess;   // Default: 0.5
  bool mMoveForward; // Default: true

 protected:

 public: 
  StSvtDriftVelocityMaker(const char *name="SvtDriftVelocity");
  virtual       ~StSvtDriftVelocityMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  //  virtual Int_t  Clear();

  bool accept(StEvent* event);
  bool accept(StSvtHit* hit);

  virtual Int_t  SetSvtData();
  virtual Int_t  SetSvtDriftVelocity();
  virtual Int_t  FillHistogramsRaw();
  virtual Int_t  FillHistogramsStEvent();
  virtual Int_t  CalcDriftVelocity();
  virtual Int_t  GetInjectorLine(float peak);
  virtual Int_t  GetInjectorLine(float* peak);
  virtual Float_t  GetClosestToLine(float peak1, float peak2);
  virtual Float_t  GetTimeZero(int anode);
  virtual Float_t  GetDistanceInjectorLine(int line);
  virtual Float_t                FitVelocity(int nInjectorsFired, float* peak, float t0);
  virtual Float_t  CalcV1(int anode, float velocity);
  virtual Float_t  CalcV2(int anode, float velocity);
  virtual Float_t  CalcV3(int anode, float velocity);
  virtual Int_t  FillAllAnodes();
  virtual Int_t  WriteToDb(Text_t *timestamp);
  TH1D* GetHybridHisto(int i) {return mHybridDriftVelocityHisto[i];}; //!
  TH2D* GetHybrid2DHisto(int i) {return mHybridDriftVelocity2DHisto[i];}; //!
  TH1D* GetGlobalHisto() {return mGlobalDriftVelocityHisto;}; //!
  TH2D* GetGlobal2DHisto() {return mGlobalDriftVelocity2DHisto;}; //!
  TH1D* GetFinalHisto() {return mCalculatedDriftVelocity;}; //!
  Int_t GetNumEvents() {return mEventCounter;}; //!
  Int_t GetNumHits() {return mHitCounter;}; //!
  Int_t GetNumBinsDV() {return mNumBinsDV;}; //!
  Int_t GetMaximumDV() {return mMaximumDV;}; //!
  Int_t GetMinimumDV() {return mMinimumDV;}; //!
  bool DebugIsOn() {return mDebug;}; //!
  bool DVCalcDoesMoveForward() {return mMoveForward;}; //!
  double GetDVFraction() {return mFraction;}; //!
  double GetDVT0Guess() {return mT0Guess;}; //!
  void SetNumBinsDV(const Int_t x) {mNumBinsDV = x;}; //!
  void SetMaximumDV(const Int_t x) {mMaximumDV = x;}; //!
  void SetMinimumDV(const Int_t x) {mMinimumDV = x;}; //!
  void SetDebug(const bool x) {mDebug = x;}; //!
  void SetMoveForward(const bool x) {mMoveForward = x;}; //!
  void SetFraction(const double x) {mFraction = x;}; //!
  void SetT0Guess(const double x) {mT0Guess = x;}; //!

  ClassDef(StSvtDriftVelocityMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


