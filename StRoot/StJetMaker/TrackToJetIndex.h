// -*- mode: c++;-*-
// $Id: TrackToJetIndex.h,v 1.1 2008/06/01 02:14:29 tai Exp $
#ifndef TRACKTOJETINDEX_H
#define TRACKTOJETINDEX_H

#include <ostream>
#include <string>

#include "StDetectorId.h"
#include "TLorentzVector.h"

class TrackToJetIndex : public TLorentzVector
{
public:
  TrackToJetIndex(int ji=-1, int ti=-1, StDetectorId id=kUnknownId);
	
  virtual ~TrackToJetIndex() {};
    
  void setJetIndex(int n) {mJetIndex=n;}
  int jetIndex() const {return mJetIndex;}

  // Note, trackIndex is the index of the track in the primaryTracks array, if detectorId==kTpcId.
  // If detectorId==kBemcTowerId, it is the tower index (actually software id)
  void setTrackIndex(int n) {mTrackIndex=n;}
  int trackIndex() const {return mTrackIndex;}

  //Does this come from EEMC, BEMC, or TPC
  void setDetectorId(StDetectorId v) {mDetId=v;}
  StDetectorId detectorId() const {return mDetId;}
	
  //Cache extra info if it's from the TPC
  void setCharge(Short_t v) {mCharge = v;}
  void setNhits(unsigned short v) {mNhits = v;}
  void setNhitsPoss(unsigned short v) {mNhitsPoss = v;}
  void setNhitsDedx(unsigned short v) {mNhitsDedx = v;}
  void setNhitsFit(unsigned short v) {mNhitsFit =v;}
  void setNsigmaPion(double v) {mNsigmaPion = v;}
  void setTdca(double v) {mTdca = v;} //jan 27, 2007
  void setTdcaz(double v) {mTdcaz = v;} //jan 27, 2007
  void setTdcaxy(double v) {mTdcaxy = v;} //jan 27, 2007
  void setetaext(double v) {metaext = v;}
  void setphiext(double v) {mphiext = v;}
	
  Short_t charge() const {return mCharge;}
  unsigned short nHits() const {return mNhits;}     //< Return total number of hits on track.
    unsigned short nHitsPoss() const {return mNhitsPoss;} //< Return number of possible hits on track.
      unsigned short nHitsDedx() const {return mNhitsDedx;} //< Return number of hits used for dEdx. 
    unsigned short nHitsFit() const {return mNhitsFit;}  //< Return total number of hits used in fit. 
    double nSigmaPion() const {return mNsigmaPion;}      //< Rdistance to the calculated dE/dx band for pions in units of sigma.
    double Tdca() const {return mTdca;} //jan 27, 2007	
    double Tdcaz() const {return mTdcaz;} //jan 27, 2007	
    double Tdcaxy() const {return mTdcaxy;} //jan 27, 2007
    double etaext() const {return metaext;}
    double phiext() const {return mphiext;}
private:
    int mJetIndex;
    int mTrackIndex;
    StDetectorId mDetId;
	
    Short_t mCharge;
    unsigned short mNhits;
    unsigned short mNhitsPoss;
    unsigned short mNhitsDedx;
    unsigned short mNhitsFit;
    double mNsigmaPion;
    double mTdca; //jan 27, 2007	
    double mTdcaz; //jan 27, 2007
    double mTdcaxy; //jan 27, 2007
    double metaext;
    double mphiext;
    ClassDef(TrackToJetIndex,2)
};

inline ostream& operator<<(ostream& os, const TrackToJetIndex& t)
{
  std::string idstring;
    StDetectorId mDetId = t.detectorId();
    if (mDetId==kTpcId) {
		idstring = "kTpcId";
    }
    else if (mDetId==kBarrelEmcTowerId) {
		idstring = "kBarrelEmcTowerId";
    }
    else if (mDetId==kEndcapEmcTowerId) {
		idstring = "kEndcapEmcTowerId";
    }
    else {
		idstring = "kUnknown";
    }
    
    return os <<"jetIndex:\t"<<t.jetIndex()<<"\ttrackIndex:\t"<<t.trackIndex()<<"\tdetId:\t"<<t.detectorId()<<"\t"<<idstring;
}


#endif // TRACKTOJETINDEX_H
