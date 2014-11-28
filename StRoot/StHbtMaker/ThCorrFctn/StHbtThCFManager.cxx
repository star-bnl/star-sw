/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description :
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtThCFManager.h"
#include "StHbtMaker/Base/StHbtThPair.hh"
#include "StHbtMaker/Base/StHbtThCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include <Stsstream.h>

#ifdef __ROOT__
ClassImp(StHbtThCFManager)
#endif

StHbtThCFManager::StHbtThCFManager() : StHbtCorrFctn() {
  mThPair=0;
};

StHbtThCFManager::~StHbtThCFManager() 
{ /* no-op */ };

void StHbtThCFManager::AddMixedPair(const StHbtPair* aPair) {

  if (mThPair) {
    mThPair->Set(aPair);
    StHbtThCorrFctnIterator iter;
    for (iter=mThCorrFctnColl.begin(); iter!=mThCorrFctnColl.end();iter++){
      (*iter)->AddNum(mThPair);
      (*iter)->AddDen(mThPair);
    }
  }
}

void StHbtThCFManager::AddRealPair(const StHbtPair*)
{/* do nothing*/};

void StHbtThCFManager::Finish() {

  StHbtThCorrFctnIterator iter;
  for (iter=mThCorrFctnColl.begin(); iter!=mThCorrFctnColl.end();iter++){
    (*iter)->Finish();
  }
}


StHbtString StHbtThCFManager::Report() {
  ostrstream tStr; 
  tStr << "Theoretical Correlation Function Manager Report" << endl;
  if (!(mThPair)) {
    tStr << "ERROR : No Theoretical Pair Plugged " << endl;
  } else {
    tStr << mThPair->Report() << endl;
  };
  tStr << mThCorrFctnColl.size() << " Correlations Functions Plugged : " << endl;
  StHbtThCorrFctnIterator iter;
  for (iter=mThCorrFctnColl.begin(); iter!=mThCorrFctnColl.end();iter++){
    tStr <<(*iter)->Report() << endl;
    }
  StHbtString returnThis = tStr.str();
  return returnThis;
}

inline void StHbtThCFManager::AddCorrFctn(StHbtThCorrFctn* aCorrFctn){
  mThCorrFctnColl.push_back(aCorrFctn);};

inline void  StHbtThCFManager::SetThPair(StHbtThPair* aThPair) {mThPair=aThPair;};

