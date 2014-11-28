/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : implementtaion of StHbtThPairEvtGen
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtThPairEvtGen.h"
#include "StHbtMaker/ThCorrFctn/StHbtEvtGenHiddenInfo.hh"
#include "StHbtMaker/Infrastructure/StHbtParticle.hh"
#include "StHbtMaker/Base/StHbtFsiWeight.hh"

#ifdef __ROOT__
ClassImp(StHbtThPairEvtGen)
#endif

StHbtThPairEvtGen::StHbtThPairEvtGen() : StHbtThPair() 
{ /* no-op */ };

void StHbtThPairEvtGen::Set(const StHbtPair* aPair){
  const StHbtEvtGenHiddenInfo* tEvtGenHidInf1=dynamic_cast<const StHbtEvtGenHiddenInfo*>
    (aPair->track1()->HiddenInfo());
  const StHbtEvtGenHiddenInfo* tEvtGenHidInf2=dynamic_cast<const StHbtEvtGenHiddenInfo*>
    (aPair->track2()->HiddenInfo());
  if ((tEvtGenHidInf1==0)||(tEvtGenHidInf2==0)) {
    cout << "Error in StHbtThPairEvtGen : "<< endl; 
    cout << "    HiddenInfo does NOT inherit from StHbtEvtGenHiddenInfo , Or it is NULL " << endl;
    exit(0);
  } else {
    mMomentum1=new StHbtLorentzVector(*(tEvtGenHidInf1->getFreezeOutMomEn()));
    mMomentum2=new StHbtLorentzVector(*(tEvtGenHidInf2->getFreezeOutMomEn()));
    mEmPoint1=new StHbtLorentzVector(*(tEvtGenHidInf1->getEmPoint()));
    mEmPoint2=new StHbtLorentzVector(*(tEvtGenHidInf2->getEmPoint()));
    mPid1=tEvtGenHidInf1->getPid();
    mPid2=tEvtGenHidInf2->getPid(); 
  }    
  
  mMeasPair=aPair;
  mWeightOk=false;
  
}

