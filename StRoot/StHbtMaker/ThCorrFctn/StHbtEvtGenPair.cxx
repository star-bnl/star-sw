/***************************************************************************
 *
 *  
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 * 
 * Description : Create pair from StHbtEvtGenHiddenInfo
 *
 ***************************************************************************
 *
 *  
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtEvtGenPair.h"
#include "StHbtMaker/ThCorrFctn/StHbtEvtGenHiddenInfo.hh"
#include "StHbtMaker/Infrastructure/StHbtParticle.hh"

#include "TRandom.h"
#include "TMath.h"

ClassImp(StHbtEvtGenPair)

StHbtEvtGenPair::StHbtEvtGenPair(short aDecoralate=0) : 
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
  double SpaceShift = -4.21;
 
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
}

