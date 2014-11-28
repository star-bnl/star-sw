/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : implementation of StHbtThPAirGAuss
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtThPairGauss.h"
#include "StHbtMaker/ThCorrFctn/StHbtEvtGenHiddenInfo.hh"
#include "StHbtMaker/Infrastructure/StHbtParticle.hh"
#include "StHbtMaker/Base/StHbtFsiWeight.hh"

#ifdef __ROOT__
ClassImp(StHbtThPairGauss)
#endif

StHbtThPairGauss::StHbtThPairGauss() : StHbtThPair(){
  mEmPoint1=&mPos1;
  mEmPoint2=&mPos2;
  mUseHidMom=false;
  mUseHidPid=false;
  mMomentum1=&mMom1;
  mMomentum2=&mMom2;
  mRef=RCMS;
  mSizeX=mSizeY=mSizeZ=1.;
  mTime=0;
  mBetaRCMS=0.;
  mGammaRCMS=1.;
}; 

StHbtThPairGauss::~StHbtThPairGauss() 
{ /* no-op */ };

void StHbtThPairGauss::Set(const StHbtPair* aPair){
  SetMomentum_PID (aPair);
  SetPosition();
  BoostPosition();

  mMomParCalculated=0;
  mPosParCalculated=0;

  mMeasPair=aPair;
  mWeightOk=false;

}

void StHbtThPairGauss::SetMomentum_PID( const StHbtPair* aPair ){

  const StHbtEvtGenHiddenInfo* tEvtGenHidInf1=0;
  const StHbtEvtGenHiddenInfo* tEvtGenHidInf2=0;
  if (mUseHidMom||mUseHidPid) {
    tEvtGenHidInf1=dynamic_cast<const StHbtEvtGenHiddenInfo*>
    (aPair->track1()->HiddenInfo());
    tEvtGenHidInf2=dynamic_cast<const StHbtEvtGenHiddenInfo*>
    (aPair->track2()->HiddenInfo()); 
    if ((tEvtGenHidInf1==0)||(tEvtGenHidInf2==0)) {
      cout << "Fatal Error in StHbtThPairGauss : "<< endl; 
      cout << "    HiddenInfo does NOT inherit from StHbtEvtGenHiddenInfo , Or it is NULL " << endl;
      exit(0);
    } 
  }
  if (mUseHidMom&&mUseHidPid) {
    mMomentum1=new StHbtLorentzVector(*(tEvtGenHidInf1->getFreezeOutMomEn()));
    mMomentum2=new StHbtLorentzVector(*(tEvtGenHidInf2->getFreezeOutMomEn()));
  }else{
    mMom1=aPair->track1()->FourMomentum();
    mMom1.setE(::sqrt(mMassSq1+mMom1.vect().mag2()));
    mMom2=aPair->track2()->FourMomentum();
    mMom2.setE(::sqrt(mMassSq2+mMom2.vect().mag2()));
    if ((!mUseHidMom)&&(mBetaRCMS>0)) {
      double tE=mMom1.e();
      mMom1.setE(mGammaRCMS*(tE-mBetaRCMS*mMom1.pz()));
      mMom1.setPz(mGammaRCMS*(mMom1.pz()-mBetaRCMS*tE));
      tE=mMom2.e();
      mMom2.setE(mGammaRCMS*(tE-mBetaRCMS*mMom2.pz()));
      mMom2.setPz(mGammaRCMS*(mMom2.pz()-mBetaRCMS*tE));		 
    }
  }
  if (mUseHidPid) {
    mPid1=tEvtGenHidInf1->getPid();
    mPid2=tEvtGenHidInf2->getPid();
  }
}

void StHbtThPairGauss::SetPosition() {
  float x,y,z,t;
  mRand.Rannor(x,y);
  mRand.Rannor(z,t);
  mPos1.setX(x*mSizeX);
  mPos1.setY(y*mSizeY);
  mPos1.setZ(z*mSizeZ);
  mPos1.setT(t*mTime);
  mRand.Rannor(x,y);
  mRand.Rannor(z,t);
  mPos2.setX(x*mSizeX);
  mPos2.setY(y*mSizeY);
  mPos2.setZ(z*mSizeZ);
  mPos2.setT(t*mTime);
};

void StHbtThPairGauss::BoostPosition(){
  double tBeta;
  double tGamma;
  double tT;
  switch (mRef) {
  case RCMS: break;
  case LCMS:
    tBeta=(mMomentum1->pz()+mMomentum2->pz())/(mMomentum1->e()+mMomentum2->e());
     tGamma=::sqrt(1/1-tBeta*tBeta);
     tT=mPos1.t();
    mPos1.setT(tGamma*(tT-tBeta*mPos1.z()));
    mPos1.setZ(tGamma*(tBeta*mPos1.z()-tBeta*tT));
    tT=mPos2.t();
    mPos2.setT(tGamma*(tT-tBeta*mPos2.z()));
    mPos2.setZ(tGamma*(mPos2.z()-tBeta*tT)); 
    break;
  case PRF:
    StHbtLorentzVector tBoost=*mMomentum1+*mMomentum2;
    mPos1=mPos1.boost(tBoost);
    mPos2=mPos2.boost(tBoost);  
    break;
  }
};

inline  void          StHbtThPairGauss::SetSize(double aSize,double aTime){mSizeX=aSize;mSizeY=aSize;mSizeZ=aSize;mTime=aTime;};
inline  void          StHbtThPairGauss::SetSize(double aSizeX,double aSizeY, double aSizeZ,double aTime)
{mSizeX=aSizeX;mSizeY=aSizeY;mSizeZ=aSizeZ;mTime=aTime;};

inline  void          StHbtThPairGauss::UseHiddenMomentum(){mUseHidMom=1;};
inline  void          StHbtThPairGauss::UseParticleMomentum(){
  mUseHidMom=0;
  if (mUseHidPid){
    mMomentum1=&mMom1;
    mMomentum2=&mMom2;
  }
};

inline  void          StHbtThPairGauss::UseHiddenPid() {
  mUseHidPid=true;
  if (mUseHidMom){
    mMomentum1=&mMom1;
    mMomentum2=&mMom2;
  }
};

inline  void          StHbtThPairGauss::UseFixedPid( int const tPid1,double const tMass1,int const tPid2, double const tMass2) {
  mUseHidPid=false;mPid1=tPid1;mPid2=tPid2;mMassSq1=tMass1*tMass1;mMassSq2=tMass2*tMass2;}; 
inline  void          StHbtThPairGauss::UseFixedPid( int const tPid1,double const tMass1) {
  mUseHidPid=false;mPid1=tPid1;mPid2=tPid1;mMassSq1=tMass1*tMass1;mMassSq2=tMass1*tMass1;}; 

inline  void          StHbtThPairGauss::SetRCMS() {mRef=RCMS;};
inline  void          StHbtThPairGauss::SetLCMS(){mRef=LCMS;};
inline  void          StHbtThPairGauss::SetPRF(){mRef=PRF;};

inline  void          StHbtThPairGauss::SetBoostRCMS(double aPlab,double aMBeam, double aMTarget){
  double tEBeamLab=::sqrt(aPlab*aPlab+aMBeam*aMBeam);
  mGammaRCMS=(tEBeamLab+aMTarget)/::sqrt(aMBeam*aMBeam+aMTarget*aMTarget+2*tEBeamLab*aMTarget);
  mBetaRCMS=::sqrt(1.-1/(mGammaRCMS*mGammaRCMS));
}
  
