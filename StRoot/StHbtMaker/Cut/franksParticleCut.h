 /***************************************************************************
 *
 * $Id: franksParticleCut.h,v 1.1 1999/09/05 02:58:11 lisa Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a simple particle cut that selects on phasespace, #hits, DCA, and PID          
 *
 ***************************************************************************
 *
 * $Log: franksParticleCut.h,v $
 * Revision 1.1  1999/09/05 02:58:11  lisa
 * add ASCII microDST reader/writer AND franksParticle cuts
 *
 * Revision 1.2  1999/07/06 22:33:21  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef franksParticleCut_hh
#define franksParticleCut_hh

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StHbtMaker/Base/StHbtParticleCut.hh"
//#include "StHbtMaker/Base/StHbtHistoStyle.hh"
//#include "StHbtMaker/Infrastructure/StHbtParticleCutMoniHandler.h" 

class franksParticleCut : public StHbtParticleCut
//, public StHbtParticleCutMoniHandler 
{

 public:

  franksParticleCut();
  ~franksParticleCut();
  
  virtual bool Pass(const StHbtTrack*);

  virtual StHbtString Report();


  void SetNSigmaPion(const float& lo, const float& hi);
  void SetNSigmaKaon(const float& lo, const float& hi);
  void SetNSigmaProton(const float& lo, const float& hi);

  void SetNHits(const int& lo, const int& hi);
  void SetP(const float& lo, const float& hi);
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
  float             mP[2];
  float             mPt[2];
  float             mRapidity[2];
  float             mDCA[2];

  long              mNTracksPassed;
  long              mNTracksFailed;

  ClassDef(franksParticleCut, 1)

};

inline void franksParticleCut::SetMass(const double& mass) {mMass = mass;}

inline void franksParticleCut::SetNSigmaPion(const float& lo, const float& hi){mNSigmaPion[0]=lo; mNSigmaPion[1]=hi;}
inline void franksParticleCut::SetNSigmaKaon(const float& lo, const float& hi){mNSigmaKaon[0]=lo; mNSigmaKaon[1]=hi;}
inline void franksParticleCut::SetNSigmaProton(const float& lo, const float& hi){mNSigmaProton[0]=lo; mNSigmaProton[1]=hi;}

inline void franksParticleCut::SetNHits(const int& lo, const int& hi){mNHits[0]=lo;mNHits[1]=hi;}
inline void franksParticleCut::SetP(const float& lo, const float& hi){mP[0]=lo; mP[1]=hi;}
inline void franksParticleCut::SetPt(const float& lo, const float& hi){mPt[0]=lo; mPt[1]=hi;}
inline void franksParticleCut::SetRapidity(const float& lo,const float& hi){mRapidity[0]=lo; mRapidity[1]=hi;}
inline void franksParticleCut::SetDCA(const float& lo,const float& hi){mDCA[0]=lo; mDCA[1]=hi;}
inline void franksParticleCut::SetCharge(const int& ch){mCharge = ch;}

#endif
