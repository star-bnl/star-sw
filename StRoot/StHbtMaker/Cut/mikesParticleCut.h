/***************************************************************************
 *
 * $Id: mikesParticleCut.h,v 1.1.1.1 1999/06/29 16:02:56 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a simple particle cut that selects on phasespace, #hits, DCA, and PID          
 *
 ***************************************************************************
 *
 * $Log: mikesParticleCut.h,v $
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef mikesParticleCut_hh
#define mikesParticleCut_hh

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StHbtMaker/Base/StHbtParticleCut.hh"

class mikesParticleCut : public StHbtParticleCut {

public:

  mikesParticleCut();
  //~mikesParticleCut();

  virtual bool Pass(const StHbtTrack*);

  virtual string Report();


  void SetNSigmaPion(const float& lo, const float& hi);
  void SetNSigmaKaon(const float& lo, const float& hi);
  void SetNSigmaProton(const float& lo, const float& hi);

  void SetNHits(const int& lo, const int& hi);
  void SetPt(const float& lo, const float& hi);
  void SetRapidity(const float& lo, const float& hi);
  void SetDCA(const float& lo, const float& hi);
  void SetCharge(const int&);

  virtual void SetMass(const double& mass);  // need this in derived class so Cint can handle it

private:   // here are the quantities I want to cut on...

  int               mCharge;
  float             mNSigmaPion[2];
  float             mNSigmaKaon[2];
  float             mNSigmaProton[2];
  int               mNHits[2];
  float             mPt[2];
  float             mRapidity[2];
  float             mDCA[2];

  long              mNTracksPassed;
  long              mNTracksFailed;

  ClassDef(mikesParticleCut, 1)

};

inline void mikesParticleCut::SetMass(const double& mass) {mMass = mass;}

inline void mikesParticleCut::SetNSigmaPion(const float& lo, const float& hi){mNSigmaPion[0]=lo; mNSigmaPion[1]=hi;}
inline void mikesParticleCut::SetNSigmaKaon(const float& lo, const float& hi){mNSigmaKaon[0]=lo; mNSigmaKaon[1]=hi;}
inline void mikesParticleCut::SetNSigmaProton(const float& lo, const float& hi){mNSigmaProton[0]=lo; mNSigmaProton[1]=hi;}

inline void mikesParticleCut::SetNHits(const int& lo, const int& hi){mNHits[0]=lo;mNHits[1]=hi;}
inline void mikesParticleCut::SetPt(const float& lo, const float& hi){mPt[0]=lo; mPt[1]=hi;}
inline void mikesParticleCut::SetRapidity(const float& lo,const float& hi){mRapidity[0]=lo; mRapidity[1]=hi;}
inline void mikesParticleCut::SetDCA(const float& lo,const float& hi){mDCA[0]=lo; mDCA[1]=hi;}
inline void mikesParticleCut::SetCharge(const int& ch){mCharge = ch;}

#endif
