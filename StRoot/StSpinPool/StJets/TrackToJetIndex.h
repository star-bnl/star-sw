// -*- mode: c++;-*-
// $Id: TrackToJetIndex.h,v 1.10 2012/06/11 14:57:05 fisyak Exp $
#ifndef TRACKTOJETINDEX_H
#define TRACKTOJETINDEX_H

class StJet;

#include "TLorentzVector.h"
#include "TRef.h"

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
  TrackToJetIndex(int jetIndex = -1, int trackIndex = -1, StDetectorId detId = kUnknownId, StJet* jet = 0);

  int jetIndex() const { return mJetIndex; }
  int trackIndex() const {return mTrackIndex; }
  StJet* jet() const { return (StJet*)mJet.GetObject(); }

  short          trackId()    const { return mTrackId; }
  StDetectorId   detectorId() const { return mDetId  ; }
  short          flag()       const { return mFlag; }
  short          charge()     const { return mCharge;     }
  unsigned short nHits()      const { return mNhits;      } //< Return total number of hits on track.
  unsigned short nHitsPoss()  const { return mNhitsPoss;  } //< Return number of possible hits on track.
  unsigned short nHitsDedx()  const { return mNhitsDedx;  } //< Return number of hits used for dEdx. 
  unsigned short nHitsFit()   const { return mNhitsFit;   } //< Return total number of hits used in fit. 
  double         nSigmaPion() const { return mNsigmaPion; } //< Rdistance to the calculated dE/dx band for pions in units of sigma.
  double         nSigmaElectron() const { return mNsigmaElectron; }
  double         nSigmaKaon() const { return mNsigmaKaon; }
  double         nSigmaProton() const { return mNsigmaProton; }
  double         Tdca()       const { return mTdca;       }
  double         Tdcaz()      const { return mTdcaz;      }
  double         Tdcaxy()     const { return mTdcaxy;     }
  double         etaext()     const { return metaext;     }
  double         phiext()     const { return mphiext;     }
  double         dEdx()       const { return mdEdx;       }
  double         jt()         const { return localMomentum().Perp(); }
  double         ps()         const { return localMomentum().Px  (); }
  double         pn()         const { return localMomentum().Py  (); }
  double         pl()         const { return localMomentum().Pz  (); }
  double         frag()       const;

  TVector3 momentum() const { return Vect(); }
  TVector3 localMomentum() const;

  // For particle jets
  short id    () const { return trackIndex(); }	// line in Pythia record
  short pdg   () const { return trackId();    }	// PDG code
  short status() const { return flag();       }	// 1=stable, 2=unstable, 3=incoming parton

  void setJetIndex(int n)             { mJetIndex = n; }
  void setTrackIndex(int n)           { mTrackIndex = n; }
  void setDetectorId(StDetectorId v)  { mDetId = v; }
  void setFlag(short v)               { mFlag = v; }
  void setCharge(short v)             { mCharge = v; }
  void setNhits(unsigned short v)     { mNhits = v; }
  void setNhitsPoss(unsigned short v) { mNhitsPoss = v; }
  void setNhitsDedx(unsigned short v) { mNhitsDedx = v; }
  void setNhitsFit(unsigned short v)  { mNhitsFit = v; }
  void setNsigmaPion(double v)        { mNsigmaPion = v; }
  void setNsigmaElectron(double v) { mNsigmaElectron = v; }
  void setNsigmaKaon(double v) { mNsigmaKaon = v; }
  void setNsigmaProton(double v) { mNsigmaProton = v; }
  void setTdca(double v)              { mTdca = v; }
  void setTdcaz(double v)             { mTdcaz = v; }
  void setTdcaxy(double v)            { mTdcaxy = v; }
  void setetaext(double v)            { metaext = v; }
  void setphiext(double v)            { mphiext = v; }
  void setdEdx(double v)              { mdEdx = v; }
  void setTrackId(int v)              { mTrackId = v; }

  // For particle jets
  void setId(short v)     { setTrackIndex(v); }
  void setPdg(short v)    { setTrackId(v);    }
  void setStatus(short v) { setFlag(v);       }

private:
  int mJetIndex;
  int mTrackIndex;
  StDetectorId mDetId;
  short mFlag;
  short mCharge;
  unsigned short mNhits;
  unsigned short mNhitsPoss;
  unsigned short mNhitsDedx;
  unsigned short mNhitsFit;
  double mNsigmaPion;
  double mNsigmaElectron;
  double mNsigmaKaon;
  double mNsigmaProton;
  double mTdca; //jan 27, 2007	
  double mTdcaz; //jan 27, 2007
  double mTdcaxy; //jan 27, 2007
  double metaext;
  double mphiext;
  double mdEdx;
  short  mTrackId;
  TRef mJet;

  ClassDef(TrackToJetIndex, 6);
};

inline std::ostream& operator<<(std::ostream& os, const TrackToJetIndex& t)
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
