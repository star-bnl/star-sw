/***************************************************************************
 *
 * $Id: StSvtDriftVelocityMaker.h,v 1.7 2004/01/26 23:11:44 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Drift Velocity Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDriftVelocityMaker.h,v $
 * Revision 1.7  2004/01/26 23:11:44  perev
 * Leak off
 *
 * Revision 1.6  2003/09/10 19:47:34  perev
 * ansi corrs
 *
 * Revision 1.5  2003/09/07 03:49:05  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.4  2002/03/04 17:06:48  willson
 * Laser spot positions recorded
 *
 * Revision 1.3  2002/02/06 00:10:45  willson
 * File entries, variable names.
 *
 * Revision 1.2  2002/01/18 20:53:47  willson
 * Some bug fixes by Helen
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
  StSvtHybridCollection* mSvtData;        //!
  StSvtData* mSvtRawData;                 //!
  StSvtHybridCollection* mSvtDriftVeloc;  //!
  int mNHybridDriftVelocityHisto;
  TH1D** mHybridDriftVelocityHisto; //!
  TH2D** mHybridDriftVelocity2DHisto; //!
  TH1D* mGlobalDriftVelocityHisto; //!
  TH2D* mGlobalDriftVelocity2DHisto; //!
  TH1D* mCalculatedDriftVelocity; //!
  TH1D* mLaserSpotDistL07B3_1; //!
  TH1D* mLaserSpotDistL15B3_1; //!
  TH1D* mLaserSpotDistL15B3_2; //!
  Int_t mEventCounter;
  Int_t mHitCounter;
  Int_t mNumTimeBins;    // Number of time bins to use in drift velocity calculation.  Default=128
  Int_t mMaximumTB;      // Max Time Bin to look at. Default=128
  Int_t mMinimumTB;      // Min Time Bin to look at. Default=0
  bool  mRaw;            // Raw data?  false=StEvent data
  bool mDebug;           // If true, make 2D histos, and keep everything.  Also, save to TH1D instead of ascii file
  
  /* To calculate drift velocity, take the 1D histo of hits vs. time.  Find the average number of hits
     in some number of time bins in the middle of the histogram.  
       For mMoveFoward=true:
       Start in the middle of the 1D histo, and move up the time bins until the number of hits per time bin 
       is LESS THAN  [average*mFraction].  Record this time bin, j, and calculate drift velocity with this formula:
          Drift Velocity = 3.0 cm/Drift Time  (cm/sec)
	  Drift Time = (j-mMinimumTB-mT0Guess)*(128.0/mNumTimeBins)*(0.00000004 sec/time bucket)

       For mMoveForward=false:
       Start at the end of the 1D histo, and move down the time bins until the number of hits per time bin are
       GREATER THAN  [average*mFraction].  Record this time bin, j, and calculate drift velocity the same way.

       mT0Guess = T0 guess (units: time bucket bins **NOT Time Buckets!**)   */
  
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
  virtual Int_t  SetSvtRawData();
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
  TH1D* GetLaserSpotDistL07B3_1() {return mLaserSpotDistL07B3_1;}; //!
  TH1D* GetLaserSpotDistL15B3_1() {return mLaserSpotDistL15B3_1;}; //!
  TH1D* GetLaserSpotDistL15B3_2() {return mLaserSpotDistL15B3_2;}; //!
  Int_t GetNumEvents() {return mEventCounter;}; //!
  Int_t GetNumHits() {return mHitCounter;}; //!
  Int_t GetNumTimeBins() {return mNumTimeBins;}; //!
  Int_t GetMaximumTB() {return mMaximumTB;}; //!
  Int_t GetMinimumTB() {return mMinimumTB;}; //!
  bool DebugIsOn() {return mDebug;}; //!
  bool DVCalcDoesMoveForward() {return mMoveForward;}; //!
  double GetDVFraction() {return mFraction;}; //!
  double GetDVT0Guess() {return mT0Guess;}; //!
  void SetNumTimeBins(const Int_t x) {mNumTimeBins = x;}; //!
  void SetMaximumTB(const Int_t x) {mMaximumTB = x;}; //!
  void SetMinimumTB(const Int_t x) {mMinimumTB = x;}; //!
  void SetDebug(int x) {mDebug = (x!=0);}; //!
  void SetMoveForward(const bool x) {mMoveForward = x;}; //!
  void SetFraction(const double x) {mFraction = x;}; //!
  void SetT0Guess(const double x) {mT0Guess = x;}; //!

  ClassDef(StSvtDriftVelocityMaker,0)   //StAF chain virtual base class for Makers
};

#endif


