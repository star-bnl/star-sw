/***************************************************************************
 *
 * $Id: StHbtTrack.hh,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
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
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtTrack_hh
#define StHbtTrack_hh

#include "StThreeVector.hh"

class StHbtTrack{
public:
  StHbtTrack(){/* no-op*/};
  ~StHbtTrack(){/* no-op*/};

  int NHits() const;
  float NSigmaPion() const;
  float NSigmaKaon() const;
  float NSigmaProton() const;
  float Pt() const;
  float DCA() const;
  int Charge() const;
  StThreeVector<double> P() const;

  void SetNHits(const int&);
  void SetNSigmaPion(const float&);
  void SetNSigmaKaon(const float&);
  void SetNSigmaProton(const float&);
  void SetPt(const float&);
  void SetDCA(const float&);
  void SetP(const StThreeVector<double>&);
  void SetCharge(const int&);

private:
  int mNHits;
  float mNSigmaPion;
  float mNSigmaKaon;
  float mNSigmaProton;
  float mPt;
  float mDCA;
  StThreeVector<double> mP;
  int mCharge;
};

inline void StHbtTrack::SetNHits(const int& nh){mNHits=nh;}
inline void StHbtTrack::SetCharge(const int& ch){mCharge=ch;}
inline void StHbtTrack::SetNSigmaPion(const float& x){mNSigmaPion = x;}
inline void StHbtTrack::SetNSigmaKaon(const float& x){mNSigmaKaon = x;}
inline void StHbtTrack::SetNSigmaProton(const float& x){mNSigmaProton = x;}
inline void StHbtTrack::SetPt(const float& x){mPt = x;}
inline void StHbtTrack::SetDCA(const float& x){mDCA = x;}
inline void StHbtTrack::SetP(const StThreeVector<double>& p){mP = p;}

inline int StHbtTrack::Charge() const {return mCharge;}
inline int StHbtTrack::NHits() const {return mNHits;}
inline float StHbtTrack::NSigmaPion() const {return mNSigmaPion;}
inline float StHbtTrack::NSigmaKaon() const {return mNSigmaKaon;}
inline float StHbtTrack::NSigmaProton() const {return mNSigmaProton;}
inline float StHbtTrack::Pt() const {return mPt;} 
inline float StHbtTrack::DCA() const {return mDCA;}
inline StThreeVector<double> StHbtTrack::P() const {return mP;}

#endif
