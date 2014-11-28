/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Implementaton of StHbtThPairGaussFit
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtThPairGaussFit.h"

StHbtThPairGaussFit::StHbtThPairGaussFit(): StHbtThPairGauss() 
{ /* no-op */};  

StHbtThPairGaussFit::~StHbtThPairGaussFit()
{ /* no-op */};  

void StHbtThPairGaussFit::Set(const StHbtPair* aPair){
  SetMomentum_PID (aPair);
  SetPosition();
  if (mRef!=RCMS) {
    mSourceDist1=*mEmPoint1;
    mSourceDist2=*mEmPoint2;
    BoostPosition();
  }
  mMeasPair=aPair;
  mWeightOk=false;
}

double StHbtThPairGaussFit::GetRejectionProb2Size (double aX, double aY, double aZ, double aT){
  return 1.0;
  /*
  double tCorSizeX,tCorSizeY,tCorSizeZ,tCorSizeT;
  if (mSizeX==aX) { tCorSizeX=0.;}
  else {tCorSizeX=1./(aX*aX)-1./(mSizeX*mSizeX);}
  if (mSizeY==aY) { tCorSizeY=0.;}
  else {tCorSizeY=1./(aY*aY)-1./(mSizeY*mSizeY);}
  if (mSizeZ==aZ) { tCorSizeZ=0.;}
  else {tCorSizeZ=1./(aZ*aZ)-1./(mSizeZ*mSizeZ);}
  if (mTime==aT) { tCorSizeT=0.;}
  else {tCorSizeT=1./(aT*aT)-1./(mTime*mTime);}
  const StHbtLorentzVector  *tEM1;
  const StHbtLorentzVector *tEM2;
  if(mRef==RCMS){
    tEM1=mEmPoint1;
    tEM2=mEmPoint2;
  } else {
    tEM1= &mSourceDist1;
    tEM2= &mSourceDist2;
  }
  return exp(-0.5*((::pow(tEM1->x(),2)+::pow(tEM2->x(),2))*tCorSizeX
		   +(::pow(tEM1->y(),2)+::pow(tEM2->y(),2))*tCorSizeY
		   +(::pow(tEM1->z(),2)+::pow(tEM2->z(),2))*tCorSizeZ
		   +(::pow(tEM1->t(),2)+::pow(tEM2->t(),2))*tCorSizeT));
  */
}

void StHbtThPairGaussFit::setVariables(const StHbtPair*){
  
}
