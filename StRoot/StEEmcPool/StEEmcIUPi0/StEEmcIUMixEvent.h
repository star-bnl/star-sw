#ifndef __StEEmcIUMixEvent_h__
#define __StEEmcIUMixEvent_h__

#include <TObject.h>
#include "StEEmcIUPair.h"
#include "StEEmcIUPoint.h"
#include "StEEmcIUCluster.h"
#include "StEEmcIUSmdCluster.h"

/// copied from muDst
#include "StEvent/StEventInfo.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StEventSummary.h"
#include "StEvent/StL0Trigger.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StBbcTriggerDetector.h"
#include "StEvent/StEmcTriggerDetector.h"

#include "StarClassLibrary/StThreeVectorF.hh"

#define MAX_PAIRS 5 

class StEEmcIUMixEventClust : public TObject {

 public:
  StEEmcIUMixEventClust(){ /* nada */ }
  ~StEEmcIUMixEventClust(){ /* nada */ }

  Int_t   index;           /**< tower index of highest tower in event  */  
  Float_t eTower;          /**< energy in towers                       */
  Float_t ePre1;           /**< energy in pre1                         */
  Float_t ePre2;           /**< energy in pre2                         */
  Float_t ePost;           /**< energy in postshower                   */
  Float_t eSmd;            /** energy in smd                           */

  Float_t etaTower;        /**< mean eta of tower cluster */
  Float_t etaPre1;         /**< mean eta of pre1 cluster  */
  Float_t etaPre2;         /**< mean eta of pre2 cluster  */
  Float_t etaPost;         /**< mean eta of post cluster  */
  Float_t etaSmd;          /**< mean eta of smd response  */

  Int_t   nTower;
  Int_t   nPre1;
  Int_t   nPre2;
  Int_t   nPost;

  void Clear(Option_t *opts="");

 private:
 protected:
  ClassDef(StEEmcIUMixEventClust,1);
};


class StEEmcIUMixEventHead : public TObject {

 public:
  StEEmcIUMixEventHead(){ /* nada */ };
  ~StEEmcIUMixEventHead(){ /* nada */ };

  Int_t mEventId;
  Int_t mEventNumber;
  Int_t mRunId;
  Int_t mRunNumber;

  Int_t numberOfCandidates(){ return mNumberOfCandidates; }
  Int_t mNumberOfCandidates; 

  void setEvent( StMuEvent *event );

  void Clear(Option_t *opts="");

 private:
 protected:
  ClassDef(StEEmcIUMixEventHead,1);

};


class StEEmcIUMixEvent : public TObject {

 public:

  StEEmcIUMixEvent();
  ~StEEmcIUMixEvent(){ /* nada */ };

  void addPair( StEEmcIUPair  p );
  void setEvent( StMuEvent *event );
  void setSpin4(Int_t s){ mSpin4=s; } 

  void Clear(Option_t *opts="");

  /// Items to be copied from StMuEvent
  Int_t                   mEventId;
  Int_t                   mEventNumber;
  Int_t                   mRunId;
  Int_t                   mRunNumber;

  StEventInfo             mEventInfo;
  StRunInfo               mRunInfo;  
  StL0Trigger             mL0trigger;
  StMuTriggerIdCollection mMuTriggerIdCollection;  
  StBbcTriggerDetector    mBbcTrigger;
  StEmcTriggerDetector    mEmcTrigger;

  StThreeVectorF          mPrimaryVertex;

  Double_t                mMagneticField;

  /// Ped subtracted ADC values from mEEanalysis.
  /// Index is etawise
  Float_t mADC[720];
  UShort_t mStat[720];
  Float_t sum3x3(Int_t index);
  Float_t sum3x3();
  Float_t htdsm();
  Float_t tpdsm();
  Int_t pedEEmcDSM_HT[90];
  Int_t pedEEmcDSM_TP[90];
  Int_t npi0();
  Int_t deta(){ return deta(0); }
  Int_t dphi(){ return dphi(0); }
  Int_t deta(Int_t i);
  Int_t dphi(Int_t i);


  Int_t mSpin4; 
  Int_t bx7;
  Int_t bx48;
  Int_t bxStar; 
  
  /// From EEMC point-maker
  Int_t         nPairs;
  Float_t       mMass[MAX_PAIRS]; //[nPairs]
  Float_t       mPT[MAX_PAIRS];   //[nPairs]
  Float_t       mEta[MAX_PAIRS];   //[nPairs] 
  Float_t       mEEmcEta[MAX_PAIRS];   //[nPairs] 
  Float_t       mPhi[MAX_PAIRS];   //[nPairs] 
  Float_t       mZgg[MAX_PAIRS];  //[nPairs];
  Float_t       mEnergy[MAX_PAIRS]; //[nPairs]
  Float_t       mEpre1[MAX_PAIRS];  //[nPairs]
  Float_t       mEpre2[MAX_PAIRS];  //[nPairs]
  Float_t       mEpost[MAX_PAIRS];  //[nPairs]
  Float_t       mEsmdu[MAX_PAIRS];  //[nPairs]
  Float_t       mEsmdv[MAX_PAIRS];  //[nPairs]
  Float_t       mZvertex[MAX_PAIRS]; //[nPairs]
  Float_t       mPhigg[MAX_PAIRS]; //[nPairs] 

  Int_t         mTower1[MAX_PAIRS]; //[nPairs]
  Int_t         mTower2[MAX_PAIRS]; //[nPairs] 
  Float_t       mEnergy1[MAX_PAIRS]; //[nPairs]
  Float_t       mEnergy2[MAX_PAIRS]; //[nPairs] 

  Float_t       mEnergyRatio[MAX_PAIRS];

  Float_t       mTotalEnergyT;
  Float_t       mTotalEnergyP;
  Float_t       mTotalEnergyQ;
  Float_t       mTotalEnergyR; 
  Float_t       mTotalEnergyU;
  Float_t       mTotalEnergyV; 

  Int_t mNumberT[MAX_PAIRS];
  Int_t mNumberR[MAX_PAIRS];
  Int_t mNumberU[MAX_PAIRS];
  Int_t mNumberV[MAX_PAIRS];

 private:
 protected:
  ClassDef(StEEmcIUMixEvent,1);

};

#endif
