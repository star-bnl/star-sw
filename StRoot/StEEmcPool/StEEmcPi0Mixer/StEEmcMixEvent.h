#ifndef __StEEmcMixEvent_h__
#define __StEEmcMixEvent_h__

#include <TObject.h>
#include "StEEmcPair.h"
#include "StEEmcPool/StEEmcPointMaker/StEEmcPoint.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcCluster.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcSmdCluster.h"

/// copied from muDst
#include "StEvent/StEventInfo.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StEventSummary.h"
#include "StEvent/StL0Trigger.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StBbcTriggerDetector.h"
#include "StEvent/StEmcTriggerDetector.h"

#define MAX_PAIRS 5 


class StEEmcMixEvent : public TObject {

 public:

  StEEmcMixEvent();
  ~StEEmcMixEvent(){ /* nada */ };

  void addPair( StEEmcPair  p );
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

  Float_t       mTotalEnergyT;
  Float_t       mTotalEnergyP;
  Float_t       mTotalEnergyQ;
  Float_t       mTotalEnergyR; 
  Float_t       mTotalEnergyU;
  Float_t       mTotalEnergyV; 

 private:
 protected:
  ClassDef(StEEmcMixEvent,1);

};

#endif
