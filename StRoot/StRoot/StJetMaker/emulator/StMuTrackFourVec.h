// -*- mode: c++;-*-
// $Id: StMuTrackFourVec.h,v 1.6 2009/12/09 05:12:12 pibero Exp $
#ifndef STMUTRACKFOURVEC_H
#define STMUTRACKFOURVEC_H

#include "StJetFinder/AbstractFourVec.h"

#include "StMuTrackEmu.h"
#include "StMuTowerEmu.h"
#include "StMcTrackEmu.h"

#include "StDetectorId.h"

#include <TLorentzVector.h>

#include <iostream>
#include <string>

class StMuTrackFourVec : public AbstractFourVec {

public:
    
  StMuTrackFourVec() : _track(0), _tower(0), _mctrack(0), _index(0), _detId(kUnknownId), _charge(0) {}

  StMuTrackFourVec(StMuTrackEmu* track, StMuTowerEmu* tower, StMcTrackEmu* mctrack, const TLorentzVector& p, double charge, int i, int detectorId)
    : _track(track)
    , _tower(tower)
    , _mctrack(mctrack)
    , _vec(p)
    , _index(i)
    , _detId(detectorId)
    , _charge(charge)
  {
  }

  virtual ~StMuTrackFourVec()
  {
    if (_track) delete _track; _track = 0;
    if (_tower) delete _tower; _tower = 0;
    if (_mctrack) delete _mctrack; _mctrack = 0;
  }
    
  ///momenta
  double pt() const { return _vec.Pt(); }
  double px() const { return _vec.Px(); } 
  double py() const { return _vec.Py(); }
  double pz() const { return _vec.Pz(); }

  ///angles
  double phi() const { return _vec.Phi(); }
  double eta() const { return _vec.Eta(); }
  
  //4-th component
  double   eT() const { return _vec.Et(); }
  double    e() const { return _vec.E (); }
  double mass() const { return _vec.M (); }

  //charge
  double charge() const { return _charge; }

  // Mu StjTrack
  StMuTrackEmu* track() const { return _track; }

  // Mu StjTower
  StMuTowerEmu* tower() const { return _tower; }

  // Mc StjTrack
  StMcTrackEmu* mctrack() const { return _mctrack; }

  //Index of the track/tower/cluster/point in the container that it came from
  int getIndex() const { return _index; }
    
  //Id of the detector that generated this 4-vector
  int detectorId() const { return _detId; }
    
private:

  StMuTrackEmu*  _track;
  StMuTowerEmu*  _tower;
  StMcTrackEmu*  _mctrack;
  TLorentzVector _vec;
  int            _index;
  int            _detId;
  double         _charge;
};

#endif // STMUTRACKFOURVEC_H
