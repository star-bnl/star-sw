// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 4 September 2009
//

#ifndef TOWER_TO_JET_INDEX_H
#define TOWER_TO_JET_INDEX_H

#ifndef StEnumerations_hh
#define StEnumerations_hh

#define kUnknownIdentifier             0
#define kTpcIdentifier                 1
#define kBarrelEmcTowerIdentifier      9
#define kEndcapEmcTowerIdentifier     13

enum StDetectorId {
  kUnknownId            = kUnknownIdentifier,
  kTpcId                = kTpcIdentifier,
  kBarrelEmcTowerId     = kBarrelEmcTowerIdentifier,
  kEndcapEmcTowerId     = kEndcapEmcTowerIdentifier,
};

#endif // StEnumerations_hh

#include "TLorentzVector.h"

class TowerToJetIndex : public TLorentzVector {
public:
  TowerToJetIndex(int jetIndex = -1)
    : mJetIndex(jetIndex)
    , mTowerId(0)
    , mDetectorId(kUnknownId)
    , mAdc(0)
    , mPedestal(0)
    , mRms(0)
    , mStatus(0)
  {
  }

  int   jetIndex  () const { return mJetIndex  ; }
  int   towerId   () const { return mTowerId   ; }
  int   detectorId() const { return mDetectorId; }
  int   adc       () const { return mAdc       ; }
  float pedestal  () const { return mPedestal  ; }
  float rms       () const { return mRms       ; }
  int   status    () const { return mStatus    ; }

  void setJetIndex  (int jetIndex  ) { mJetIndex   = jetIndex  ; }
  void setTowerId   (int towerId   ) { mTowerId    = towerId   ; }
  void setDetectorId(int detectorId) { mDetectorId = detectorId; }
  void setAdc       (int adc       ) { mAdc        = adc       ; }
  void setPedestal  (float pedestal) { mPedestal   = pedestal  ; }
  void setRms       (float rms     ) { mRms        = rms       ; }
  void setStatus    (int status    ) { mStatus     = status    ; }

private:
  int   mJetIndex;
  int   mTowerId;
  int   mDetectorId;
  int   mAdc;
  float mPedestal;
  float mRms;
  int   mStatus;

  ClassDef(TowerToJetIndex, 1);
};

#endif // TOWER_TO_JET_INDEX_H
