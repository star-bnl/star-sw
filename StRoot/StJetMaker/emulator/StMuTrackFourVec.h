// -*- mode: c++;-*-
// $Id: StMuTrackFourVec.h,v 1.5 2009/09/04 17:30:20 pibero Exp $
#ifndef STMUTRACKFOURVEC_H
#define STMUTRACKFOURVEC_H

#include "StJetFinder/AbstractFourVec.h"

#include "StMuTrackEmu.h"
#include "StMuTowerEmu.h"

#include "StDetectorId.h"

#include <TLorentzVector.h>

#include <iostream>
#include <string>

class StMuTrackFourVec : public AbstractFourVec {

public:
    
  StMuTrackFourVec() : _track(0), index(0), mDetId(kUnknownId), mCharge(0) { }

  StMuTrackFourVec(StMuTrackEmu* track, StMuTowerEmu* tower, const TLorentzVector& p, double charge, Int_t i, int detectorId)
    : _track(track)
    , _tower(tower)
    , _vec(p)
    , index(i)
    , mDetId(detectorId)
    , mCharge(charge)
  { }

  virtual ~StMuTrackFourVec()
  {
    if (_track) delete _track; _track = 0;
    if (_tower) delete _tower; _tower = 0;
  }
    
  ///momenta
  double pt() const { return _vec.Pt(); }
  double px() const { return _vec.Px(); } 
  double py() const { return _vec.Py(); }
  double pz() const { return _vec.Pz(); }

  ///angles
  double phi()      const { return _vec.Phi(); }
  double eta()      const { return _vec.Eta(); }
  
  //4-th component
  double eT()   const { return _vec.Et(); }

  double e()    const { return _vec.E(); }
  double mass() const { return _vec.M(); }

  //charge
  double charge() const { return mCharge; }

  // Mu StjTrack
  StMuTrackEmu* track() const { return _track; }

  // Mu StjTower
  StMuTowerEmu* tower() const { return _tower; }

  //Index of the track/tower/cluster/point in the container that it came from
  Int_t getIndex(void) const { return index; }
    
  //Id of the detector that generated this 4-vector
  int detectorId() const {return mDetId;}
    
private:

  StMuTrackEmu* _track;
  StMuTowerEmu* _tower;
  TLorentzVector _vec;
  Int_t index;
  int mDetId;
  double mCharge;
};

#endif // STMUTRACKFOURVEC_H
