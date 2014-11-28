/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : implementation of StHbtThPair
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include <Stiostream.h>  
#include <Stsstream.h>
#include <stdlib.h>

#include "StHbtMaker/Base/StHbtThPair.hh"
#include "StHbtMaker/Base/StHbtFsiWeight.hh"

#ifdef __ROOT__
ClassImp(StHbtThPair)
#endif

StHbtThPair::StHbtThPair(){
  mMomentum1=mMomentum2=mEmPoint1=mEmPoint2=0;
  mPid1=mPid2=0;
  mMeasPair=0;
  mWeight=0;
  mWeightNum=mWeightDen=1.;
  mWeightOk=false;
  mPairPurity=1;
};

void StHbtThPair::setVariables(const StHbtPair*){
  /* no-op */
}

void StHbtThPair::setMomRes1(int aPid){
  if(aPid==7 || aPid==8){// pion
    mMomRes1=1;
    mPLoss1[0]=-0.00275; // (PRec-Pmc)P = [0]+[1]*P^[2]
    mPLoss1[1]=-0.000335;
    mPLoss1[2]=-2.052;
    mPtRes1[0]=0.0178; // Dpt/pt = [0] + [1]*Pt^[2]
    mPtRes1[1]=7.95e-5;      
    mPtRes1[2]=-2.273;
    mPhiRes1[0]=0.0642; // DPhi = [0] +[1]*P^[2]
    mPhiRes1[1]=0.0322;
    mPhiRes1[2]=-1.511;
    mThetaRes1[0]=0.0779; // DTheta = [0] + P^[2]
    mThetaRes1[1]=0.0445;
    mThetaRes1[2]=-1.528;
  }
  if(aPid==11 || aPid==12){//kaon
    mMomRes1=1;
  }
}
void StHbtThPair::setMomRes2(int aPid){
  if(aPid==7 || aPid==8){// pion
    mMomRes2=1;
    mPLoss2[0]=-0.00275; // (PRec-Pmc)P = [0]+[1]*P^[2]
    mPLoss2[1]=-0.000335;
    mPLoss2[2]=-2.052;
    mPtRes2[0]=0.0178; // Dpt/pt = [0] + [1]*Pt^[2]
    mPtRes2[1]=7.95e-5;      
    mPtRes2[2]=-2.273;
    mPhiRes2[0]=0.0642; // DPhi = [0] +[1]*P^[2]
    mPhiRes2[1]=0.0322;
    mPhiRes2[2]=-1.511;
    mThetaRes2[0]=0.0779; // DTheta = [0] + P^[2]
    mThetaRes2[1]=0.0445;
    mThetaRes2[2]=-1.528;
  }
  if(aPid==11 || aPid==12){//kaon
    mMomRes2=1;
  }
}


void StHbtThPair::UpdateWeight() {
  if (mWeight) {
    mWeightNum=mWeight->GetWeight(this);
    mWeightNum=(mWeightNum-1.)*mPairPurity+1.;
    mWeightDen=mWeight->GetWeightDen();
    if(mWeightNum<=0. || mWeightNum>1000.){
      ostrstream tCom;
      tCom << "echo ---> " << mWeightNum << " " 
	   << mEmPoint1 << " " << mEmPoint2 << " " 
	   << mMomentum1 << " " << mMomentum2 << " >> Err.txt" << ends;
      system(tCom.str());
      mWeightNum=1;
    }
  } else {
    cout << "StHbtThPair Error - No Weight Generator plugged - set to 1 " <<endl;
    mWeightNum=mWeightDen=1.;
  }
  mWeightOk=true;
}

StHbtString StHbtThPair::Report() {
  ostrstream tStr; 
  tStr << "Default StHbtThPair Report" << endl;
  if (mWeight) {
    tStr << mWeight->Report() << endl;
  } else {
    tStr <<   "No Weight Generator plugged - Weight set to 1 " << endl;
  }
  StHbtString returnThis = tStr.str();
  return returnThis;
}

double StHbtThPair::RealqSideCMS() const {
    double x1 = mMomentum1->x();  double y1 = mMomentum1->y();
    double x2 = mMomentum2->x();  double y2 = mMomentum2->y();

    double xt = x1+x2;  double yt = y1+y2;
    double k1 = ::sqrt(xt*xt+yt*yt);

    double tmp = 2.0*(x1*y2-x2*y1)/k1;

    return (tmp);
}
double StHbtThPair::RealqOutCMS() const {
    double dx = mMomentum1->x() - mMomentum2->x();
    double xt = mMomentum1->x() + mMomentum2->x();
    
    double dy = mMomentum1->y() - mMomentum2->y();
    double yt = mMomentum1->y() + mMomentum2->y();

    double k1 = (::sqrt(xt*xt+yt*yt));
    double k2 = (dx*xt+dy*yt);
    double tmp = k2/k1;
    return (tmp);
}
double StHbtThPair::RealqLongCMS() const {
    double dz = mMomentum1->z() - mMomentum2->z();
    double zz = mMomentum1->z() + mMomentum2->z();

    double dt = mMomentum1->t() - mMomentum2->t();
    double tt = mMomentum1->t() + mMomentum2->t();

    double beta = zz/tt;
    double gamma = 1.0/::sqrt(1.0 - beta*beta);

    double temp = gamma*(dz - beta*dt);
    return (temp);
}

//________________________________
double StHbtThPair::RealqOutPf() const
{
  double dt = mMomentum1->t() - mMomentum2->t();
  double tt = mMomentum1->t() + mMomentum2->t();
  
  double xt = mMomentum1->x() + mMomentum2->x();
  double yt = mMomentum1->y() + mMomentum2->y();
  
  double k1 = ::sqrt(xt*xt + yt*yt);
  double bOut = k1/tt;
  double gOut = 1.0/::sqrt(1.0 - bOut*bOut);
  
  double temp = gOut*(this->RealqOutCMS() - bOut*dt);
  return (temp);
}

//___________________________________
double StHbtThPair::RealqSidePf() const
{
 return(this->RealqSideCMS());
}

//___________________________________

double StHbtThPair::RealqLongPf() const
{
 return(this->RealqLongCMS());
}

//___________________________________

void StHbtThPair::calcMomParameters() const{ // fortran like function! faster?
  mMomParCalculated=1;

  double tPx1(mMomentum1->px());
  double tPy1(mMomentum1->py());
  double tPz1(mMomentum1->pz());
  double tE1(mMomentum1->e());
  double tPx(tPx1+mMomentum2->px());
  double tPy(tPy1+mMomentum2->py());
  double tPz(tPz1+mMomentum2->pz());
  double tE(tE1 +mMomentum2->e());
  mPt = tPx*tPx + tPy*tPy;
  double tP = tPz*tPz;
  double tMt = tE*tE - tP;
  tP += mPt;
  tP = ::sqrt(tP);
  double tM =   ::sqrt(tMt - mPt);
  tMt = ::sqrt(tMt);
  mPt = ::sqrt(mPt);
  mBetat = mPt/tMt;
  mUt = mPt/tMt;

  // Boost to LCMS
  double tBeta = tPz/tE;
  double tGamma = tE/tMt;	    
  mKStarLong = tGamma * (tPz1 - tBeta * tE1);
  double tE1L = tGamma * (tE1  - tBeta * tPz1);

  // Rotate in transverse plane
  mKStarOut  = ( tPx1*tPx + tPy1*tPy)/mPt;
  mKStarSide = (-tPx1*tPy + tPy1*tPx)/mPt;

  // Boost to pair cms
  mKStarOut = tMt/tM * (mKStarOut - mPt/tMt * tE1L);

  mKStar = ::sqrt(mKStarOut*mKStarOut+mKStarSide*mKStarSide+
		mKStarLong*mKStarLong);

  mCVK = (mKStarOut*mPt + mKStarLong*tPz)/mKStar/mCVK;
  mKStarLong = (tPz>=0)* mKStarLong - (tPz<0)*mKStarLong;

}


void StHbtThPair::calcPosParameters() const{ // fortran like function! faster?
  mPosParCalculated=1;

  double tPx = mMomentum1->px()+mMomentum2->px();
  double tPy = mMomentum1->py()+mMomentum2->py();
  double tPz = mMomentum1->pz()+mMomentum2->pz();
  double tE = mMomentum1->e()+mMomentum2->e();
  double tPt = tPx*tPx + tPy*tPy;
  double tMt = tE*tE - tPz*tPz;
  double tM =   ::sqrt(tMt - tPt);
  tMt = ::sqrt(tMt);
  tPt = ::sqrt(tPt);
   
  double tDX = mEmPoint1->x()-mEmPoint2->x();
  double tDY = mEmPoint1->y()-mEmPoint2->y();
  mRLong = mEmPoint1->z()-mEmPoint2->z();
  mDTime = mEmPoint1->t()-mEmPoint2->t();
  
  mRTrans = tDX>0.? ::sqrt(tDX*tDX+tDY*tDY) : -1.*::sqrt(tDX*tDX+tDY*tDY);
  mROut = (tDX*tPx + tDY*tPy)/tPt;
  mRSide = (-tDX*tPy + tDY*tPx)/tPt;
  mRSidePairCMS = mRSide;

  // Lab -> LCMS -> PRF method
  double tBeta = tPz/tE;
  double tGamma = tE/tMt;
  mRLongPairCMS = tGamma*(mRLong - tBeta* mDTime);
  mDTimePairLCMS = tGamma*(mDTime - tBeta* mRLong);
  tBeta = tPt/tMt;
  tGamma = tMt/tM;
  mROutPairCMS = tGamma*(mROut - tBeta* mDTimePairLCMS);
  mDTimePairCMS = tGamma*(mDTimePairLCMS - tBeta* mROut);
  mRStar = ::sqrt(mROutPairCMS*mROutPairCMS + mRSidePairCMS*mRSidePairCMS +
		mRLongPairCMS*mRLongPairCMS);


  /* 
     // Lab -> PRF method
  double tRS12 = DX*Px+DY*Py+mRLong*Pz;
  double tH1 = (tRS12/(E+M) - mDTime) / M;
  double DXPairCMS = DX + Px* tH1;
  double DYPairCMS = DY + Py* tH1;
  mRLongPairCMS = mRLong + Pz* tH1;
  //mRLong = Pz*mRLong>0? mRLong : -mRLong;
  //mRLongPairCMS = Pz*mRLongPairCMS>0? mRLongPairCMS : -mRLongPairCMS;
  mDTimePairCMS = (E* mDTime - tRS12) / M;
  mRStar = ::sqrt(DXPairCMS*DXPairCMS + DYPairCMS*DYPairCMS +
		mRLongPairCMS * mRLongPairCMS);
  mROutPairCMS = (DXPairCMS*Px + DYPairCMS*Py)/Ptrans;
  mRSidePairCMS = (-DXPairCMS*Py + DYPairCMS*Px)/Ptrans;
  */
}
