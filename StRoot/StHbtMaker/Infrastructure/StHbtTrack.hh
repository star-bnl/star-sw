/***************************************************************************
 *
 * $Id: StHbtTrack.hh,v 1.4 1999/07/19 14:24:06 hardtke Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    Intermediate format for particle.  This is built from the
 *    input particle format (e.g. StTrack of StEvent) and presented to
 *    the Analyses for ParticleCuts.
 *
 ***************************************************************************
 *
 * $Log: StHbtTrack.hh,v $
 * Revision 1.4  1999/07/19 14:24:06  hardtke
 * modifications to implement uDST
 *
 * Revision 1.3  1999/07/06 22:33:23  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.2  1999/06/29 17:50:27  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtTrack_hh
#define StHbtTrack_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StPhysicalHelixD.hh"

class StHbtTrack{
public:
  StHbtTrack(){/* no-op*/};
  ~StHbtTrack(){/* no-op*/};

  char Charge() const;
  unsigned short NHits() const;
  unsigned short NHitsPossible() const;
  float NSigmaPion() const;
  float NSigmaKaon() const;
  float NSigmaProton() const;
  float dEdx() const;
  float DCAz() const;
  float DCAxy() const;
  float ChiSquaredXY() const;
  float ChiSquaredZ() const;
  StHbtThreeVector P() const;
  StPhysicalHelixD& Helix();

  void SetCharge(const char&);
  void SetNHits(const unsigned short&);
  void SetNHitsPossible(const unsigned short&);
  void SetNSigmaPion(const float&);
  void SetNSigmaKaon(const float&);
  void SetNSigmaProton(const float&);
  void SetdEdx(const float&);
  void SetDCAxy(const float&);
  void SetDCAz(const float&);
  void SetChiSquaredXY(const float&);
  void SetChiSquaredZ(const float&);
  void SetP(const StHbtThreeVector&);
  void SetHelix(const StPhysicalHelixD&);

private:
  char mCharge;
  int mNHits;
  unsigned short mNHitsPoss; 
  float mNSigmaPion;
  float mNSigmaKaon;
  float mNSigmaProton;
  float mdEdx;
  float mDCA[2];
  float mChiSq[2];
  StHbtThreeVector mP;
  StPhysicalHelixD mHelix;
};

inline void StHbtTrack::SetNHits(const unsigned short& nh){mNHits=nh;}
inline void StHbtTrack::SetNHitsPossible(const unsigned short& nh){
mNHitsPoss=nh;}
inline void StHbtTrack::SetCharge(const char& ch){mCharge=ch;}
inline void StHbtTrack::SetNSigmaPion(const float& x){mNSigmaPion = x;}
inline void StHbtTrack::SetNSigmaKaon(const float& x){mNSigmaKaon = x;}
inline void StHbtTrack::SetNSigmaProton(const float& x){mNSigmaProton = x;}
inline void StHbtTrack::SetdEdx(const float& x){mdEdx = x;}
inline void StHbtTrack::SetDCAxy(const float& x){mDCA[0] = x;}
inline void StHbtTrack::SetDCAz(const float& x){mDCA[1] = x;}
inline void StHbtTrack::SetChiSquaredXY(const float& x){mChiSq[0] = x;}
inline void StHbtTrack::SetChiSquaredZ(const float& x){mChiSq[1] = x;}
inline void StHbtTrack::SetP(const StHbtThreeVector& p){mP = p;}
inline void StHbtTrack::SetHelix(const StPhysicalHelixD& h){mHelix = h;}

inline char StHbtTrack::Charge() const {return mCharge;}
inline unsigned short StHbtTrack::NHits() const {return mNHits;}
inline unsigned short StHbtTrack::NHitsPossible() const {return mNHitsPoss;}
inline float StHbtTrack::NSigmaPion() const {return mNSigmaPion;}
inline float StHbtTrack::NSigmaKaon() const {return mNSigmaKaon;}
inline float StHbtTrack::NSigmaProton() const {return mNSigmaProton;}
inline float StHbtTrack::dEdx() const {return mdEdx;}
inline float StHbtTrack::DCAxy() const {return mDCA[0];}
inline float StHbtTrack::DCAz() const {return mDCA[1];}
inline float StHbtTrack::ChiSquaredXY() const {return mChiSq[0];}
inline float StHbtTrack::ChiSquaredZ() const {return mChiSq[1];}
inline StHbtThreeVector StHbtTrack::P() const {return mP;}
inline StPhysicalHelixD& StHbtTrack::Helix() {return mHelix;}

#endif
