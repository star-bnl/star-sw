/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description :implementation of StHbtThCFGaussFit 
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtThCFGaussFit.h"

#include "StHbtMaker/ThCorrFctn/StHbtThCFGaussSizeCollection.hh"
#include "StHbtMaker/ThCorrFctn/StHbtThCFGaussSize.h"
#include "StHbtMaker/Base/StHbtThCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

#include "TRandom.h"
#include <Stsstream.h>

#ifdef __ROOT__
  ClassImp(StHbtThCFGaussFit)
#endif

StHbtThCFGaussFit::StHbtThCFGaussFit(): 
  mMaxX(0), mMaxY(0),  mMaxZ(0), mMaxT(0) 
{ /* no-op */ };

StHbtThCFGaussFit::~StHbtThCFGaussFit() 
{ /* no-op */ } ;

void  StHbtThCFGaussFit::AddCorrFctn(const StHbtThCorrFctn *aCF){
  if (mSizeColl.size()<=0) {
    cout << "StHbtThCFGaussFit - At Least One Gaussian Size must be plugged before " << endl;
    cout << " StHbtThCFGaussFit - Warning - " << aCF->GetName() << " has NOT been added " << endl;
  } 
  StHbtThCFGaussSizeIterator iter;
  for (iter=mSizeColl.begin(); iter!=mSizeColl.end();iter++){
    (*iter)->AddCorrFctn(aCF);
  }
}

void  StHbtThCFGaussFit::AddSize(const char* aName, double aX, double aY, double aZ, double aT){
  if (aX>mMaxX) { mMaxX=aX;mPair.SetSize(mMaxX,mMaxY,mMaxZ,mMaxT);};
  if (aY>mMaxY) { mMaxY=aY;mPair.SetSize(mMaxX,mMaxY,mMaxZ,mMaxT);};
  if (aZ>mMaxZ) { mMaxZ=aZ;mPair.SetSize(mMaxX,mMaxY,mMaxZ,mMaxT);};
  if (aT>mMaxT) { mMaxT=aT;mPair.SetSize(mMaxX,mMaxY,mMaxZ,mMaxT);};
  if (mSizeColl.size()>0) {
    StHbtThCFGaussSize* tFirstSize=(*(mSizeColl.begin()));
    mSizeColl.push_back(tFirstSize->Copy(aName,aX,aY,aZ,aT));
  } else {
    mSizeColl.push_back(new StHbtThCFGaussSize(aName,aX,aY,aZ,aT));
  };
};

void StHbtThCFGaussFit::AddRealPair( const StHbtPair* aPair) {};

void StHbtThCFGaussFit::AddMixedPair( const StHbtPair* aPair) {
  if (mSizeColl.size()>0) {
    mPair.Set(aPair);
    double tProb=mRand.Rndm(); if(tProb){/*nothing*/};
    StHbtThCFGaussSizeIterator iter;
    for (iter=mSizeColl.begin(); iter!=mSizeColl.end();iter++){
      //      if (mPair.GetRejectionProb2Size((*iter)->GetSizeX(),(*iter)->GetSizeY(),
      //				 (*iter)->GetSizeZ(),(*iter)->GetTime())>tProb)
	(*iter)->FillPair(&mPair);
    }
  }
} 
    

void StHbtThCFGaussFit::Finish() {
  if (mSizeColl.size()>0) {
    StHbtThCFGaussSizeIterator iter;
    for (iter=mSizeColl.begin(); iter!=mSizeColl.end();iter++){
	(*iter)->Finish();
    }
  }  
}

StHbtString StHbtThCFGaussFit::Report() {
  ostrstream tStr; 
  tStr << "Correlation - Gaussian Fit Manager Report" << endl;
  tStr << mPair.Report() ;

  tStr << mSizeColl.size() << " Sizes defined " << endl;
  
 if (mSizeColl.size()>0) {
    StHbtThCFGaussSizeIterator iter;
    for (iter=mSizeColl.begin(); iter!=mSizeColl.end();iter++){
	tStr <<	(*iter)->Report() << endl;
    }
  } 
 StHbtString returnThis = tStr.str();
 return returnThis;
}

inline void StHbtThCFGaussFit::SetWeight(StHbtFsiWeight* aWeight) {mPair.SetWeight(aWeight);};
inline void StHbtThCFGaussFit::UseHiddenMomentum() {mPair.UseHiddenMomentum();};
inline void StHbtThCFGaussFit::UseParticleMomentum() {mPair.UseParticleMomentum();};

inline void StHbtThCFGaussFit::UseHiddenPid() {mPair.UseHiddenPid();};
inline void StHbtThCFGaussFit::UseFixedPid( int const tPid1, double const tMass1) 
{mPair.UseFixedPid( tPid1,  tMass1) ;};  
inline void StHbtThCFGaussFit::UseFixedPid( int const tPid1,double const tMass1, int const tPid2,double const tMass2 ) 
{mPair.UseFixedPid(tPid1, tMass1,  tPid2, tMass2 );}; 

inline void StHbtThCFGaussFit::SetBoostRCMS(double aPlab,double aMBeam, double aMTarget)
 {mPair.SetBoostRCMS(aPlab,aMBeam, aMTarget);};  
inline void StHbtThCFGaussFit::SetRCMS() {mPair.SetRCMS();};  
inline void StHbtThCFGaussFit::SetLCMS() {mPair.SetLCMS();};
inline void StHbtThCFGaussFit::SetPRF() {mPair.SetPRF();};    

inline void StHbtThCFGaussFit::AddSize(const char* aName, double aXYZ, double aT){
  AddSize(aName,aXYZ,aXYZ,aXYZ,aT);};

inline StHbtThCFGaussSizeCollection *StHbtThCFGaussFit::getCollection() { return &mSizeColl; }
