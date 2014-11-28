/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 * 
 * Description : Create pair from StHbtEvtGenHiddenInfo
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtEvtGenPair.h"
#include "StHbtMaker/ThCorrFctn/StHbtEvtGenHiddenInfo.hh"
#include "StHbtMaker/Infrastructure/StHbtParticle.hh"

#include "TRandom.h"
#include "TMath.h"

ClassImp(StHbtEvtGenPair)

StHbtEvtGenPair::StHbtEvtGenPair(short aDecoralate) : 
  StHbtThPair(),mDecoralate(aDecoralate) {
  if(mDecoralate==2){
    mNStoredPos=100;
    mPosArray1 = new StHbtLorentzVector[mNStoredPos];
    mValidPos1 = new short[mNStoredPos];
    mPosArray2 = new StHbtLorentzVector[mNStoredPos];
    mValidPos2 = new short[mNStoredPos];
    for(int ti=0; ti< mNStoredPos; ti++){
      mValidPos1[ti]=0;
      mValidPos2[ti]=0;
    }
  }
}

StHbtEvtGenPair::~StHbtEvtGenPair(){
  if(mDecoralate==2){
    delete[] mPosArray1;
    delete[] mValidPos1;
    delete[] mPosArray2;
    delete[] mValidPos2;
  }
}

void StHbtEvtGenPair::setVariables(const StHbtPair* aPair){
  double tTimeShift = 3.75; 
  double SpaceShift = -4.21;   if(SpaceShift){/*touch*/};
 
  StHbtEvtGenHiddenInfo* tEvtGenHiddenInfoV[2];
  tEvtGenHiddenInfoV[0] =
    (StHbtEvtGenHiddenInfo*)(aPair->track1()->getHiddenInfo());
  tEvtGenHiddenInfoV[1] =
    (StHbtEvtGenHiddenInfo*)(aPair->track2()->getHiddenInfo());

  mMomentum1=tEvtGenHiddenInfoV[0]->getFreezeOutMomEn();
  mMomentum2=tEvtGenHiddenInfoV[1]->getFreezeOutMomEn();

  for(int ti=0; ti<2; ti++){
    StHbtEvtGenHiddenInfo* tEvtGenHiddenInfo = tEvtGenHiddenInfoV[ti];

    if(tEvtGenHiddenInfo->posHaveNotBeenModified()){
      StHbtLorentzVector* tEmPoint=tEvtGenHiddenInfo->getEmPoint();

      switch(mDecoralate){
      case 1: // Randomize phi
	{
	  static TRandom tRand;
	  double tR = tEmPoint->perp();
	  double tPhi = tRand.Rndm()*2.*TMath::Pi();
	  tEmPoint->setX(tR*cos(tPhi));
	  tEmPoint->setY(tR*sin(tPhi));
	  break;
	}
      case 2: // Randomize x and p
	{	  
	  static TRandom tRand;
	  static StHbtLorentzVector tVect;
	  int tIndex = (int)(tRand.Rndm()*mNStoredPos);
	  StHbtLorentzVector* tPosArray;
	  short* tValidPos;
	  if(ti==0){
	    tPosArray = mPosArray1;
	    tValidPos = mValidPos1;
	  }
	  else{
	    tPosArray = mPosArray2;
	    tValidPos = mValidPos2;
	  }	  
	  if(tValidPos[tIndex]==0){
	    tPosArray[tIndex]=(*tEmPoint);
	    tValidPos[tIndex]=1;
	  }
	  else{
	    tVect = (*tEmPoint);
	    (*tEmPoint) = tPosArray[tIndex];
	    tPosArray[tIndex] =  tVect;
	  }	
	  break;
        }  
      case 3: // set time to zero in particle LCMS
	{
	  if(ti==1) tEmPoint->setT(tEmPoint->t()+tTimeShift/mMomentum2->e()*
				   mMomentum2->mt());
	  break;
	}
      case 4: // both 1 and 3
	{
	  static TRandom tRand;
	  double tR = tEmPoint->perp();
	  double tPhi = tRand.Rndm()*2.*TMath::Pi();
	  tEmPoint->setX(tR*cos(tPhi));
	  tEmPoint->setY(tR*sin(tPhi));
	  if(ti==1) tEmPoint->setT(tEmPoint->t()+tTimeShift/mMomentum2->e()*
				   mMomentum2->mt());
	  break;
	}      
      case 5: // Generate a static gaussian in rOut
	{
	  static TRandom tRand;
	  static double sigma=9.0;
	  static double mu=3.0;
	  double mRandVar[3];
	  if (ti==0)
	    {
	      tEmPoint->setX(0.0);
	      tEmPoint->setY(0.0);
	      tEmPoint->setZ(0.0);
	      tEmPoint->setT(0.0);
	    }
	  else
	    {
	      mRandVar[0] = tRand.Gaus(0.,1.);
	      mRandVar[1] = tRand.Gaus(0.,1.);
	      mRandVar[2] = tRand.Gaus(0.,1.);
	  

	      double tPx = mMomentum1->x()+mMomentum2->x();
	      double tPy = mMomentum1->y()+mMomentum2->y();
	      double tPz = mMomentum1->z()+mMomentum2->z();
	      double tE  = mMomentum1->e()+mMomentum2->e();
	      double tPt = tPx*tPx + tPy*tPy;
	      //mCVK = tPz*tPz;
	      double tMt = tE*tE - tPz*tPz;//mCVK;
	      //mCVK += tPt;
	      //mCVK = ::sqrt(mCVK);
	      double tM =   ::sqrt(tMt - tPt);
	      tMt = ::sqrt(tMt);
	      tPt = ::sqrt(tPt);
	  
	      double tROut = mRandVar[0]*sigma+mu;
	      double tRSide = mRandVar[1]*sigma;
	      double ttz = mRandVar[2]*sigma;
	      double ttt = tROut;
	      //	      mX2[2] = mRandVar[2]*sigma;
	      //	      mX2[3] = tROut; // =0 | Just a computing trick for the boost
	  
	      tROut  *= (tMt/tM); // Rout*gammaT
	      ttt    *= (tPt/tM); // Rout*betaT*gammaT
	      double ttDTime = ttt; 
	      ttt += (tPz/tE*ttz);
	      ttt *= (tE/tMt);
	      ttz += (tPz/tE*ttDTime); 
	      ttz *= (tE/tMt);
	  
	      tPx /= tPt;
	      tPy /= tPt;
	  
	      tEmPoint->setX(tROut*tPx-tRSide*tPy);
	      tEmPoint->setY(tROut*tPy+tRSide*tPx);
	      tEmPoint->setZ(ttz);
	      tEmPoint->setT(ttt);
	    }
	  break;
	}      
	//      case 4:
      	//{
	  //	  if(ti==1) {
	    
	  //tEmPoint->setT(tEmPoint->t()+tTimeShift/mMomentum2->e()*
	  //		   mMomentum2->mt());
	//break;
	//}
      }
      tEvtGenHiddenInfo->setPosHaveBeenModified();      
    }
  }
  mEmPoint1 = tEvtGenHiddenInfoV[0]->getEmPoint();
  mEmPoint2 = tEvtGenHiddenInfoV[1]->getEmPoint();
  mPid1=tEvtGenHiddenInfoV[0]->getPdgPid();
  mPid2=tEvtGenHiddenInfoV[1]->getPdgPid();

  mMomParCalculated=0;
  mPosParCalculated=0;

  mMeasPair=aPair;
  mWeightOk=false;
}

