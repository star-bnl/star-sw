/***************************************************************************
 *
 *  
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 *         Adam Kisiel, Warsaw University of Technology
 ***************************************************************************
 *
 * Description : implementation of StHbtThPAirGAuss
 *
 ***************************************************************************
 *
 *  
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtThPairDoubleGauss.h"
#include "StHbtMaker/Infrastructure/StHbtParticle.hh"
#include "StHbtMaker/Base/StHbtFsiWeight.hh"

#ifdef __ROOT__
ClassImp(StHbtThPairDoubleGauss)
#endif

StHbtThPairDoubleGauss::StHbtThPairDoubleGauss() : StHbtThPair(){
  mEmPoint1=&mPos1;
  mEmPoint2=&mPos2;
  mUseHidMom=false;
  mUseHidPid=false;
  mMomentum1=&mMom1;
  mMomentum2=&mMom2;
  mRef=RCMSDG;
  mSizeX1=mSizeY1=mSizeZ1=1.;
  mTime1=0;
  mSizeX2=mSizeY2=mSizeZ2=1.;
  mTime2=0;
  mProb1=0.5;
  mBetaRCMS=0.;
  mGammaRCMS=1.;
  mCoreHalo=1;
}; 

StHbtThPairDoubleGauss::~StHbtThPairDoubleGauss() {};

void StHbtThPairDoubleGauss::Set(const StHbtPair* aPair){
  SetMomentum_PID (aPair);
  SetPosition();
  BoostPosition();

  mMeasPair=aPair;
  mWeightOk=false;

}

void StHbtThPairDoubleGauss::SetMomentum_PID(const StHbtPair* aPair ){

  const StHbtSmearedHiddenInfo* tSmearedHidInf1=0;
  const StHbtSmearedHiddenInfo* tSmearedHidInf2=0;
  const StHbtEvtGenHiddenInfo* tEvtGenHidInf1=0;
  const StHbtEvtGenHiddenInfo* tEvtGenHidInf2=0;
  const StHbtShiftedHiddenInfo* tShiftedHidInf1=0;
  const StHbtShiftedHiddenInfo* tShiftedHidInf2=0;



  if (mUseHidMom||mUseHidPid) {
    if (mHiddenInfoType == SMEAR) {
      tSmearedHidInf1=dynamic_cast<const StHbtSmearedHiddenInfo*>
	(aPair->track1()->HiddenInfo()); 
      tSmearedHidInf2=dynamic_cast<const StHbtSmearedHiddenInfo*>
	(aPair->track2()->HiddenInfo());
      if ((tSmearedHidInf1==0)||(tSmearedHidInf2==0)) {
	if (tSmearedHidInf1==0) {
	  StHbtMomRes *mMomRes1 = new StHbtMomRes(mPid1);
	  mMomRes1->setMult(mResMult);
	  aPair->track1()->SetHiddenInfo(new StHbtSmearedHiddenInfo(aPair->track1()->FourMomentum(), mPid1, &mRand, mMomRes1));
	  tSmearedHidInf1 = dynamic_cast<const StHbtSmearedHiddenInfo*>(aPair->track1()->HiddenInfo());
	  delete mMomRes1;
	} 
	if (tSmearedHidInf2==0) {
	  StHbtMomRes *mMomRes2 = new StHbtMomRes(mPid2);
	  mMomRes2->setMult(mResMult);
	  aPair->track2()->SetHiddenInfo(new StHbtSmearedHiddenInfo(aPair->track2()->FourMomentum(), mPid2, &mRand, mMomRes2));
	  tSmearedHidInf2 = dynamic_cast<const StHbtSmearedHiddenInfo*>(aPair->track2()->HiddenInfo());
	  delete mMomRes2;
	} 
      }
    }
    if (mHiddenInfoType == SHIFT) {
      tShiftedHidInf1=dynamic_cast<const StHbtShiftedHiddenInfo*>
	(aPair->track1()->HiddenInfo()); 
      tShiftedHidInf2=dynamic_cast<const StHbtShiftedHiddenInfo*>
	(aPair->track2()->HiddenInfo());
      if ((tShiftedHidInf1==0)||(tShiftedHidInf2==0)) {
	if (tShiftedHidInf1==0) {
	  StHbtMomRes *mMomRes1 = new StHbtMomRes(mPid1);
	  mMomRes1->setMult(mResMult);
	  aPair->track1()->SetHiddenInfo(new StHbtShiftedHiddenInfo(aPair->track1()->FourMomentum(), mPid1, &mRand, mMomRes1, mShift, PHISHIFT));
	  tShiftedHidInf1 = dynamic_cast<const StHbtShiftedHiddenInfo*>(aPair->track1()->HiddenInfo());
	  delete mMomRes1;
	} 
	if (tShiftedHidInf2==0) {
	  StHbtMomRes *mMomRes2 = new StHbtMomRes(mPid2);
	  mMomRes2->setMult(mResMult);
	  aPair->track2()->SetHiddenInfo(new StHbtShiftedHiddenInfo(aPair->track2()->FourMomentum(), mPid2, &mRand, mMomRes2, mShift, PHISHIFT));
	  tShiftedHidInf2 = dynamic_cast<const StHbtShiftedHiddenInfo*>(aPair->track2()->HiddenInfo());
	  delete mMomRes2;
	} 
      }
    }
    else {
      tEvtGenHidInf1=dynamic_cast<const StHbtEvtGenHiddenInfo*>
	(aPair->track1()->HiddenInfo()); 
      tEvtGenHidInf2=dynamic_cast<const StHbtEvtGenHiddenInfo*>
	(aPair->track2()->HiddenInfo());
    }
  }
  
  if (mUseHidMom&&tSmearedHidInf2&&tSmearedHidInf1) {
    mMomentum1=&(tSmearedHidInf1->getMomentum());
    mMomentum2=&(tSmearedHidInf1->getMomentum());
  }
  else if (mUseHidMom&&tShiftedHidInf2&&tShiftedHidInf1) {
    mMomentum1=&(tShiftedHidInf1->getMomentum());
    mMomentum2=&(tShiftedHidInf2->getMomentum());
  }
  else if (mUseHidMom&&tEvtGenHidInf2&&tEvtGenHidInf1) {
    mMomentum1=&(tEvtGenHidInf1->getFreezeOutMomEn());
    mMomentum2=&(tEvtGenHidInf2->getFreezeOutMomEn());
  }
  else{
    mMom1=aPair->track1()->FourMomentum();
    mMom1.setE(sqrt(mMassSq1+mMom1.vect().mag2()));
    mMom2=aPair->track2()->FourMomentum();
    mMom2.setE(sqrt(mMassSq2+mMom2.vect().mag2()));
    if ((!mUseHidMom)&&(mBetaRCMS>0)) {
      double tE=mMom1.e();
      mMom1.setE(mGammaRCMS*(tE-mBetaRCMS*mMom1.pz()));
      mMom1.setPz(mGammaRCMS*(mMom1.pz()-mBetaRCMS*tE));
      tE=mMom2.e();
      mMom2.setE(mGammaRCMS*(tE-mBetaRCMS*mMom2.pz()));
      mMom2.setPz(mGammaRCMS*(mMom2.pz()-mBetaRCMS*tE));		 
    }
  }
}
    
void StHbtThPairDoubleGauss::SetPosition() {
  /*
   * Set the random freeze-out space coordinates
   * according to either of two models:
   * Core-Halo - the coordinates are randomly generated
   * either with core (1st) or halo (2nd) parametrization
   * TwoSources - The first particle coordinates are generated 
   * from the 1st set of parameters, coordinates for te second - 
   * from the second set.
   */

  float x,y,z,t;
  mRand.Rannor(x,y);
  mRand.Rannor(z,t);
  if (mCoreHalo) {
    if (mRand.Uniform() < mProb1) {
      mPos1.setX(x*mSizeX1);
      mPos1.setY(y*mSizeY1);
      mPos1.setZ(z*mSizeZ1);
      mPos1.setT(t*mTime1);
      mRand.Rannor(x,y);
      mRand.Rannor(z,t);
      mPos2.setX(x*mSizeX1);
      mPos2.setY(y*mSizeY1);
      mPos2.setZ(z*mSizeZ1);
      mPos2.setT(t*mTime1);
    }
    else {
      mPos1.setX(x*mSizeX2);
      mPos1.setY(y*mSizeY2);
      mPos1.setZ(z*mSizeZ2);
      mPos1.setT(t*mTime2);
      mRand.Rannor(x,y);
      mRand.Rannor(z,t);
      mPos2.setX(x*mSizeX2);
      mPos2.setY(y*mSizeY2);
      mPos2.setZ(z*mSizeZ2);
      mPos2.setT(t*mTime2);
    }
  }
  else{
    mPos1.setX(x*mSizeX1);
    mPos1.setY(y*mSizeY1);
    mPos1.setZ(z*mSizeZ1);
    mPos1.setT(t*mTime1);
    mRand.Rannor(x,y);
    mRand.Rannor(z,t);
    mPos2.setX(x*mSizeX2);
    mPos2.setY(y*mSizeY2);
    mPos2.setZ(z*mSizeZ2);
    mPos2.setT(t*mTime2);
  }

};

void StHbtThPairDoubleGauss::BoostPosition(){
  double tBeta;
  double tGamma;
  double tT;
  switch (mRef) {
  case RCMSDG: break;
  case LCMSDG:
    tBeta=(mMomentum1->pz()+mMomentum2->pz())/(mMomentum1->e()+mMomentum2->e());
     tGamma=sqrt(1/1-tBeta*tBeta);
     tT=mPos1.t();
    mPos1.setT(tGamma*(tT-tBeta*mPos1.z()));
    mPos1.setZ(tGamma*(tBeta*mPos1.z()-tBeta*tT));
    tT=mPos2.t();
    mPos2.setT(tGamma*(tT-tBeta*mPos2.z()));
    mPos2.setZ(tGamma*(mPos2.z()-tBeta*tT)); 
    break;
  case PRFDG:
    StHbtLorentzVector tBoost=*mMomentum1+*mMomentum2;
    mPos1=mPos1.boost(tBoost);
    mPos2=mPos2.boost(tBoost);  
    break;
  }
};

inline void StHbtThPairDoubleGauss::SetResolutionMult(const double mult) {
  mResMult = mult;
}

inline void StHbtThPairDoubleGauss::SetMomentumShift(const double shift)
{
  mShift = shift;
}

inline void StHbtThPairDoubleGauss::UseSmearedHiddenInfo()
{
  mHiddenInfoType = SMEAR;
}

inline void StHbtThPairDoubleGauss::UseShiftedHiddenInfo()
{
  mHiddenInfoType = SHIFT;
}

inline void StHbtThPairDoubleGauss::UseEvtGenHiddenInfo()
{
  mHiddenInfoType = EVTGEN;
}

inline  void          StHbtThPairDoubleGauss::SetSizes(double aXYZ1, double aT1, double aXYZ2, double aT2) 
{mSizeX1=mSizeY1=mSizeZ1=aXYZ1; mTime1=aT1; mSizeX2=mSizeY2=mSizeZ2=aXYZ2; mTime2=aT2; }
inline  void          StHbtThPairDoubleGauss::SetSizes(double aX1,double aY1,double aZ1, double aT1,double aX2,double aY2,double aZ2, double aT2)
{mSizeX1=aX1; mSizeY1=aY1; mSizeZ1=aZ1; mTime1=aT1; mSizeX2=aX2; mSizeY2=aY2; mSizeZ2=aZ2; mTime2=aT2; }
inline  void          StHbtThPairDoubleGauss::SetSize1(double aSize,double aTime) {mSizeX1=aSize;mSizeY1=aSize;mSizeZ1=aSize;mTime1=aTime;};
inline  void          StHbtThPairDoubleGauss::SetSize1(double aSizeX,double aSizeY, double aSizeZ,double aTime)
{mSizeX1=aSizeX;mSizeY1=aSizeY;mSizeZ1=aSizeZ;mTime1=aTime;};
inline  void          StHbtThPairDoubleGauss::SetSize2(double aSize,double aTime) {mSizeX2=aSize;mSizeY2=aSize;mSizeZ2=aSize;mTime2=aTime;};
inline  void          StHbtThPairDoubleGauss::SetSize2(double aSizeX,double aSizeY, double aSizeZ,double aTime)
{mSizeX2=aSizeX;mSizeY2=aSizeY;mSizeZ2=aSizeZ;mTime2=aTime;};

inline  void          StHbtThPairDoubleGauss::UseHiddenMomentum(){mUseHidMom=1;};
inline  void          StHbtThPairDoubleGauss::UseParticleMomentum(){
  mUseHidMom=0;
  if (mUseHidPid){
    mMomentum1=&mMom1;
    mMomentum2=&mMom2;
  }
};

inline  void          StHbtThPairDoubleGauss::UseHiddenPid() {
  mUseHidPid=true;
  if (mUseHidMom){
    mMomentum1=&mMom1;
    mMomentum2=&mMom2;
  }
};

inline  void          StHbtThPairDoubleGauss::UseFixedPid( int const tPid1,double const tMass1,int const tPid2, double const tMass2) {
  mUseHidPid=false;mPid1=tPid1;mPid2=tPid2;mMassSq1=tMass1*tMass1;mMassSq2=tMass2*tMass2; }; 
inline  void          StHbtThPairDoubleGauss::UseFixedPid( int const tPid1,double const tMass1) {
  mUseHidPid=false;mPid1=tPid1;mPid2=tPid1;mMassSq1=tMass1*tMass1;mMassSq2=tMass1*tMass1; }; 

inline  void          StHbtThPairDoubleGauss::SetRCMS() {mRef=RCMSDG;};
inline  void          StHbtThPairDoubleGauss::SetLCMS(){mRef=LCMSDG;};
inline  void          StHbtThPairDoubleGauss::SetPRF(){mRef=PRFDG;};

inline  void          StHbtThPairDoubleGauss::SetBoostRCMS(double aPlab,double aMBeam, double aMTarget){
  double tEBeamLab=sqrt(aPlab*aPlab+aMBeam*aMBeam);
  mGammaRCMS=(tEBeamLab+aMTarget)/sqrt(aMBeam*aMBeam+aMTarget*aMTarget+2*tEBeamLab*aMTarget);
  mBetaRCMS=sqrt(1.-1/(mGammaRCMS*mGammaRCMS));
}

inline  void          StHbtThPairDoubleGauss::SetFirstProb(double aProb1) { 
  if (aProb1<0.0) mProb1 = 0.0;
  else if (aProb1>1.0) mProb1 = 1.0;
  else mProb1 = aProb1;
}

inline  void          StHbtThPairDoubleGauss::SetCoreHalo() { mCoreHalo=1; }
inline  void          StHbtThPairDoubleGauss::SetTwoSources() { mCoreHalo=0; }

