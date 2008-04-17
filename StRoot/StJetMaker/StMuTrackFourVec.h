// -*- mode: c++;-*-
// $Id: StMuTrackFourVec.h,v 1.5 2008/04/17 20:12:12 tai Exp $
#ifndef StMuTrackFourVec_HH
#define StMuTrackFourVec_HH

#include "StJetFinder/AbstractFourVec.h"

#include "StarClassLibrary/StLorentzVectorF.hh"
#include "StDetectorId.h"

#include <iostream>
#include <string>

class StMuTrack;

class StMuTrackFourVec : public AbstractFourVec
{
public:
    
  StMuTrackFourVec(StMuTrack* track, StLorentzVectorF P, Int_t i, StDetectorId detId);
  StMuTrackFourVec();
  virtual ~StMuTrackFourVec() {};
    
  ///momenta
  double pt() const { return mVec.perp();}
  double px() const { return mVec.px(); } 
  double py() const { return mVec.py(); }
  double pz() const { return mVec.pz(); }
  double p() const  { return mVec.vect().mag(); }

  ///angles
  double theta()    const { return mVec.theta(); }
  double phi()      const { return mVec.phi(); }
  double eta()      const { return mVec.pseudoRapidity(); }
  double rapidity() const { return mVec.rapidity(); }

  ///4-th component
  double eT()   const { return ::sqrt(e()*e()*pt()*pt()/(p()*p())); }
  double eZ()   const { return eT()*sinh(eta()); }
  double e()    const { return mVec.e(); }
  double mass() const { return mVec.m(); }

  ///charge
  double charge() const { return mCharge; }

  ////Mu Track (null if it's an emc tower/hit/point) this will change soon
  StMuTrack* particle() const {return mTrack;}

  ///Index of the track/tower/cluster/point in the container that it came from
  Int_t getIndex(void) const { return index; }
    
  ///Id of the detector that generated this 4-vector
  StDetectorId detectorId() const {return mDetId;}
    
  void Init(StMuTrack* track, StLorentzVectorF P, Int_t i, StDetectorId detId);

  const StLorentzVectorF& vec() const {return mVec;}
    
private:
  StMuTrack* mTrack;
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
