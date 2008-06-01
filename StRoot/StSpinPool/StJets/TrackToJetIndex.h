// -*- mode: c++;-*-
// $Id: TrackToJetIndex.h,v 1.5 2008/06/01 18:37:58 tai Exp $
#ifndef TRACKTOJETINDEX_H
#define TRACKTOJETINDEX_H

#include "TLorentzVector.h"

#include <ostream>
#include <string>


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


class TrackToJetIndex : public TLorentzVector
{
public:
  TrackToJetIndex(int ji=-1, int ti=-1, StDetectorId id=kUnknownId);
	
  virtual ~TrackToJetIndex() {};
    
  int jetIndex() const {return mJetIndex;}

  int trackIndex() const {return mTrackIndex;}
  // if detectorId == kTpcId,            the index of the track in the primaryTracks
  // If detectorId == kBarrelEmcTowerId, the tower index (software id)
  // If detectorId == kEndcapEmcTowerId, the tower ID

  StDetectorId detectorId() const   { return mDetId;      }
	
  Short_t        charge()     const { return mCharge;     }
  unsigned short nHits()      const { return mNhits;      } //< Return total number of hits on track.
  unsigned short nHitsPoss()  const { return mNhitsPoss;  } //< Return number of possible hits on track.
  unsigned short nHitsDedx()  const { return mNhitsDedx;  } //< Return number of hits used for dEdx. 
  unsigned short nHitsFit()   const { return mNhitsFit;   } //< Return total number of hits used in fit. 
  double         nSigmaPion() const { return mNsigmaPion; } //< Rdistance to the calculated dE/dx band for pions in units of sigma.
  double         Tdca()       const { return mTdca;       }
  double         Tdcaz()      const { return mTdcaz;      }
  double         Tdcaxy()     const { return mTdcaxy;     }
  double         etaext()     const { return metaext;     }
  double         phiext()     const { return mphiext;     }
  double         dEdx()       const { return mdEdx;       }

  void setJetIndex(int n)            { mJetIndex = n; }
  void setTrackIndex(int n)          { mTrackIndex = n; }
  void setDetectorId(StDetectorId v) { mDetId = v; }

  void setCharge(Short_t v)           { mCharge = v; }
  void setNhits(unsigned short v)     { mNhits = v; }
  void setNhitsPoss(unsigned short v) { mNhitsPoss = v; }
  void setNhitsDedx(unsigned short v) { mNhitsDedx = v; }
  void setNhitsFit(unsigned short v)  { mNhitsFit = v; }
  void setNsigmaPion(double v)        { mNsigmaPion = v; }
  void setTdca(double v)              { mTdca = v; }
  void setTdcaz(double v)             { mTdcaz = v; }
  void setTdcaxy(double v)            { mTdcaxy = v; }
  void setetaext(double v)            { metaext = v; }
  void setphiext(double v)            { mphiext = v; }
  void setdEdx(double v)              { mdEdx = v; }
	

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
  double mdEdx;

  ClassDef(TrackToJetIndex,3)

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
