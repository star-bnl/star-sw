/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 *         Adam Kisiel, Warsaw University of Technology
 ***************************************************************************
 *
 * Description : ThPair wich return :
 *               RealMomentum : HiddenInfo Momentum or Particle Momentum
 *                              according to 
 *                              UseHiddenMomentum()/ UseParticleMomentum()
 *               EmPoint : EmPoint is distributed randomly in a Double
 *                         Gaussian distribution.Size is fixed by SetSize1() 
 *                         SetSize2() and 
 *                         Ref Frame is fixed by SetRCMS()...
 *                         The probability that the particle comes from the
 *                         first source is set by SetProb1().
 *               Pid : to be fixed
 *               MeasMomentum : Particle Momentum
 *
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef ST_HBT_THPAIR_DOUBLE_GAUSS_HH
#define ST_HBT_THPAIR_DOUBLE_GAUSS_HH

#include "TRandom2.h"

#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Base/StHbtThPair.hh"
#include "StHbtMaker/ThCorrFctn/StHbtMomRes.hh"
#include "StHbtMaker/ThCorrFctn/StHbtSmearedHiddenInfo.h"
#include "StHbtMaker/ThCorrFctn/StHbtEvtGenHiddenInfo.hh"
#include "StHbtMaker/ThCorrFctn/StHbtShiftedHiddenInfo.h"

enum RefFrameDG{RCMSDG,LCMSDG,PRFDG};
enum HiddenInfoType{EVTGEN,SMEAR,SHIFT};

class StHbtThPairDoubleGauss : public StHbtThPair{


public:
  StHbtThPairDoubleGauss();
  virtual ~StHbtThPairDoubleGauss();
  virtual void Set(const StHbtPair* aPair);

  void SetSizes(double aXYZ1, double aT1, double aXYZ2, double aT2);
  void SetSizes(double aX1,double aY1,double aZ1, double aT1,double aX2,double aY2,double aZ2, double aT2);
  void SetSize1(double aXYZ,double aT );
  void SetSize1(double aX,double aY,double aZ, double aT);
  void SetSize2(double aXYZ,double aT );
  void SetSize2(double aX,double aY,double aZ, double aT);
  void SetPositionShift(double aX, double aY, double aZ, double aT);
  void SetFirstProb(double amProb);
  void UseHiddenMomentum();
  void UseParticleMomentum();

  void UseHiddenPid();
  void UseFixedPid( int const tPid1, double const tMass1);  
  void UseFixedPid( int const tPid1,double const tMass1, int const tPid2,double const tMass2 ); 
 
  void SetBoostRCMS(double aPlab,double aMBeam, double aMTarget);
  
  void SetRCMS();
  void SetLCMS();
  void SetPRF();
  void SetCoreHalo();
  void SetTwoSources();
  void SetRadialGaus();
  void SetPRFGaus();
  
  void SetResolutionMult(const double mult);
  void SetMomentumShift(const double shift);

  void UseSmearedHiddenInfo();
  void UseShiftedHiddenInfo();
  void UseEvtGenHiddenInfo();
  void Write();
  

 protected:
  TRandom2           mRand;
  bool               mUseHidMom;
  bool               mUseHidPid;
  bool               mCoreHalo;
  double             mSizeX1,mSizeY1,mSizeZ1,mSizeX2,mSizeY2,mSizeZ2;
  double             mTime1,mTime2;
  double             mXShift, mYShift, mZShift, mTShift;
  double             mProb1;
  RefFrameDG         mRef;
  double             mBetaRCMS;
  double             mGammaRCMS;
  double             mResMult;
  double             mShift;
  HiddenInfoType     mHiddenInfoType;

  StHbtLorentzVector mPos1;
  StHbtLorentzVector mPos2;

  StHbtLorentzVector mMom1;  //because StHbtParticle.FourMomentum() is not a ref
  StHbtLorentzVector mMom2;

  StHbt3DHisto*      mPosDist1;
  StHbt3DHisto*      mPosDist2;
  StHbt2DHisto*      mPosPtDist1;
  StHbt2DHisto*      mPosPtDist2;
  StHbt1DHisto*      mTDist1;
  StHbt1DHisto*      mTDist2;

  double mMassSq1,mMassSq2;

  // 3 protected Step called from  Set(...) public function member
  void SetMomentum_PID( const StHbtPair* );
  void SetPosition( const StHbtPair* );
  void BoostPosition();
  StHbtLorentzVector *GenerateFreezeOut(int partno);

#ifdef __ROOT__
  ClassDef(StHbtThPairDoubleGauss,1)
#endif
};

#endif
