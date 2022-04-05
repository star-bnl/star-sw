#ifndef __StEEmcMixEvent_h__
#define __StEEmcMixEvent_h__

#include <TObject.h>
#include "TClonesArray.h"
#include "StEEmcPair.h"
#include "StEEmcPoint.h"
#include "StEEmcCluster.h"
#include "StEEmcSmdCluster.h"

/// copied from muDst
#include "StEvent/StEventInfo.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StEventSummary.h"
#include "StEvent/StL0Trigger.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"


class StEEmcMixEvent : public TObject {

 public:

  StEEmcMixEvent();
  ~StEEmcMixEvent(){ /* nada */ };

  void addPoint ( StEEmcPoint p );
  void addPair  ( StEEmcPair  p );
  void addMixed ( StEEmcPair  p );
  void addTower ( StEEmcTower t ); 

  void setLeadingPair( StEEmcPair &p );

  void addCluster( StEEmcSmdCluster c);

  void setEvent( StMuEvent *event );

  void setTotalEnergy( Float_t e );
  void setTotalPoints( Float_t e );
  void setEnergySeen( Float_t e );

  void setTotalEnergyU( Int_t s, Float_t e );
  void setTotalEnergyV( Int_t s, Float_t e );

  void setHighTower( StEEmcTower &t );

  void Clear(Option_t *opts="");

 private:
 protected:

  /// Items to be copied from StMuEvent
  Int_t                   mEventId;
  Int_t                   mEventNumber;
  Int_t                   mRunId;
  Int_t                   mRunNumber;
  StEventInfo             mEventInfo;
  StRunInfo               mRunInfo;  
  StMuTriggerIdCollection mMuTriggerIdCollection; 
  StL0Trigger             mL0trigger;
  Double_t                mMagneticField;
  
  /// From EEMC point-maker
  Int_t         nReal;
  Int_t         nMixed;
  TClonesArray *mRealPairs;
  TClonesArray *mMixedPairs;

  /// Pair which contains the two most energetic points
  StEEmcPair mLeadingPair;

  Int_t         nPoints;
  TClonesArray *mPoints;

  Int_t nClustersU;
  TClonesArray *mClustersU;

  Int_t nClustersV;
  TClonesArray *mClustersV;

  /// Total energy in eemc
  Float_t       mEtotal;
  Float_t       mEpoints;
  Float_t       mEseen;

  Float_t       mEtotalU[12];
  Float_t       mEtotalV[12];

  /// Hit towers from A2E maker
  Int_t nTowers;
  TClonesArray *mTowers; 

  /// Maximum tower
  StEEmcTower mHighTower;

  ClassDef(StEEmcMixEvent,1);

};

inline void StEEmcMixEvent::setTotalEnergy(Float_t t){mEtotal=t;}
inline void StEEmcMixEvent::setEnergySeen(Float_t s){mEseen=s;}

inline void StEEmcMixEvent::setTotalEnergyU(Int_t s, Float_t e){mEtotalU[s]=e;}
inline void StEEmcMixEvent::setTotalEnergyV(Int_t s, Float_t e){mEtotalV[s]=e;}

inline void StEEmcMixEvent::setHighTower ( StEEmcTower &t ){ mHighTower=t; }

inline void StEEmcMixEvent::setLeadingPair( StEEmcPair &p ) { mLeadingPair = p; }

#endif
