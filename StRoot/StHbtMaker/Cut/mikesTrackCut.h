/***************************************************************************
 *
 * $Id:
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a simple particle cut that selects on phasespace, #hits, DCA, and PID          
 *
 ***************************************************************************
 *
 * $Log:
 **************************************************************************/

#ifndef mikesTrackCut_hh
#define mikesTrackCut_hh

//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtTrackCut.h"

class mikesTrackCut : public StHbtTrackCut {

public:

  mikesTrackCut();
  //~mikesTrackCut();

  virtual bool Pass(const StHbtTrack*);

  virtual StHbtString Report();


  void SetNSigmaPion(const float& lo, const float& hi);
  void SetNSigmaKaon(const float& lo, const float& hi);
  void SetNSigmaProton(const float& lo, const float& hi);

  void SetNHits(const int& lo, const int& hi);
  void SetPt(const float& lo, const float& hi);
  void SetRapidity(const float& lo, const float& hi);
  void SetDCA(const float& lo, const float& hi);
  void SetCharge(const int&);


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

#ifdef __ROOT__ 
  ClassDef(mikesTrackCut, 1)
#endif
};


inline void mikesTrackCut::SetNSigmaPion(const float& lo, const float& hi){mNSigmaPion[0]=lo; mNSigmaPion[1]=hi;}
inline void mikesTrackCut::SetNSigmaKaon(const float& lo, const float& hi){mNSigmaKaon[0]=lo; mNSigmaKaon[1]=hi;}
inline void mikesTrackCut::SetNSigmaProton(const float& lo, const float& hi){mNSigmaProton[0]=lo; mNSigmaProton[1]=hi;}

inline void mikesTrackCut::SetNHits(const int& lo, const int& hi){mNHits[0]=lo;mNHits[1]=hi;}
inline void mikesTrackCut::SetPt(const float& lo, const float& hi){mPt[0]=lo; mPt[1]=hi;}
inline void mikesTrackCut::SetRapidity(const float& lo,const float& hi){mRapidity[0]=lo; mRapidity[1]=hi;}
inline void mikesTrackCut::SetDCA(const float& lo,const float& hi){mDCA[0]=lo; mDCA[1]=hi;}
inline void mikesTrackCut::SetCharge(const int& ch){mCharge = ch;}

#endif
