// -*- mode: c++;-*-
// $Id: StMuTrackFourVec.h,v 1.9 2008/05/09 03:33:00 tai Exp $
#ifndef StMuTrackFourVec_HH
#define StMuTrackFourVec_HH

#include "StMuTrackEmu.h"

#include "StJetFinder/AbstractFourVec.h"

#include "StarClassLibrary/StLorentzVectorF.hh"
#include "StDetectorId.h"

#include <iostream>
#include <string>

#include "StarClassLibrary/StParticleTypes.hh"

class StMuTrackFourVec : public AbstractFourVec {

public:
    
  StMuTrackFourVec() : _track(0), index(0), mDetId(kUnknownId), mCharge(0) { }

  StMuTrackFourVec(StSpinJet::StMuTrackEmu* t, StLorentzVectorF P, Int_t i, StDetectorId detId)
    : _track(t), mVec(P), index(i), mDetId(detId), mCharge(!t ? 0 : (double)t->charge())
  { }

  virtual ~StMuTrackFourVec() { if(_track) delete _track; _track = 0; }
    
  ///momenta
  double pt() const { return mVec.perp(); }
  double px() const { return mVec.px(); } 
  double py() const { return mVec.py(); }
  double pz() const { return mVec.pz(); }

  ///angles
  double phi()      const { return mVec.phi(); }
  double eta()      const { return mVec.pseudoRapidity(); }
  
  //4-th component
  double eT()   const { return ::sqrt(e()*e()*pt()*pt()/(p()*p())); }

  double e()    const { return mVec.e(); }
  double mass() const { return mVec.m(); }

  //charge
  double charge() const { return mCharge; }

  // Mu Track (null if it's an emc tower/hit/point) this will change soon
  //  StMuTrack* particle() const {return mTrack;}
  StSpinJet::StMuTrackEmu* track() const { return _track; }

  //Index of the track/tower/cluster/point in the container that it came from
  Int_t getIndex(void) const { return index; }
    
  //Id of the detector that generated this 4-vector
  StDetectorId detectorId() const {return mDetId;}
    
  const StLorentzVectorF& vec() const {return mVec;}
    
private:

  double p() const  { return mVec.vect().mag(); }

  StSpinJet::StMuTrackEmu *_track;
  StLorentzVectorF mVec;
  Int_t index;
  StDetectorId mDetId;
  double mCharge;
};

inline ostream& operator<<(ostream& os, const StMuTrackFourVec& f)
{
  std::string idstring;
  StDetectorId mDetId(f.detectorId());

  if (mDetId == kTpcId)
    idstring = "kTpcId";
  else if (mDetId == kBarrelEmcTowerId)
    idstring = "kBarrelEmcTowerId";
  else if (mDetId == kEndcapEmcTowerId)
    idstring = "kEndcapEmcTowerId";
  else
    idstring = "kUnknown";
    
  return os << "index:\t" << f.getIndex() << "\tP:\t" << f.vec() << "\tdetId:\t" << f.detectorId() << "\t" << idstring;

}
#endif // StMuTrackFourVec_HH
