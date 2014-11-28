 /***************************************************************************
 *
 * $Id: 
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a simple particle cut that selects on phasespace, #hits, DCA, and PID          
 *
 ***************************************************************************
 *
 * $Log:
 *
 **************************************************************************/

#ifndef helensLaPTrackCut_hh
#define helensLaPTrackCut_hh


//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtTrackCut.h"

class helensLaPTrackCut : public StHbtTrackCut
{

 public:

  helensLaPTrackCut();
  helensLaPTrackCut(helensLaPTrackCut& );
  ~helensLaPTrackCut();
  
  virtual bool Pass(const StHbtTrack*);

  virtual StHbtString Report();


  void SetNSigmaElectron(const float& lo, const float& hi);
  void SetNSigmaPion(const float& lo, const float& hi);
  void SetNSigmaKaon(const float& lo, const float& hi);
  void SetNSigmaProton(const float& lo, const float& hi);
  void SetNSigmaAntiElectron(const float& lo, const float& hi);
  void SetNSigmaAntiPion(const float& lo, const float& hi);
  void SetNSigmaAntiKaon(const float& lo, const float& hi);
  void SetNSigmaAntiProton(const float& lo, const float& hi);


  void SetNHits(const int& lo, const int& hi);
  void SetP(const float& lo, const float& hi);
  void SetPt(const float& lo, const float& hi);
  void SetPx(const float& lo, const float& hi);
  void SetPy(const float& lo, const float& hi);
  void SetPz(const float& lo, const float& hi);
  void SetRapidity(const float& lo, const float& hi);
  void SetDCA(const float& lo, const float& hi);
  void SetCharge(const int&);

  helensLaPTrackCut* Clone();

private:   // here are the quantities I want to cut on...

  int               mCharge;
  float             mNSigmaElectron[2];
  float             mNSigmaPion[2];
  float             mNSigmaKaon[2];
  float             mNSigmaProton[2];
  float             mNSigmaAntiElectron[2];
  float             mNSigmaAntiPion[2];
  float             mNSigmaAntiKaon[2];
  float             mNSigmaAntiProton[2];
  int               mNHits[2];
  float             mP[2];
  float             mPt[2];
  float             mPx[2];
  float             mPy[2];
  float             mPz[2];
  float             mRapidity[2];
  float             mDCA[2];

  long              mNTracksPassed;
  long              mNTracksFailed;

#ifdef __ROOT__
  ClassDef(helensLaPTrackCut, 1)
#endif
};


inline void helensLaPTrackCut::SetNSigmaElectron(const float& lo, const float& hi){mNSigmaElectron[0]=lo; mNSigmaElectron[1]=hi;}
inline void helensLaPTrackCut::SetNSigmaPion(const float& lo, const float& hi){mNSigmaPion[0]=lo; mNSigmaPion[1]=hi;}
inline void helensLaPTrackCut::SetNSigmaKaon(const float& lo, const float& hi){mNSigmaKaon[0]=lo; mNSigmaKaon[1]=hi;}
inline void helensLaPTrackCut::SetNSigmaProton(const float& lo, const float& hi){mNSigmaProton[0]=lo; mNSigmaProton[1]=hi;}
inline void helensLaPTrackCut::SetNSigmaAntiElectron(const float& lo, const float& hi){
    mNSigmaAntiElectron[0]=lo; mNSigmaAntiElectron[1]=hi;}
inline void helensLaPTrackCut::SetNSigmaAntiPion(const float& lo, const float& hi){
    mNSigmaAntiPion[0]=lo; mNSigmaAntiPion[1]=hi;}
inline void helensLaPTrackCut::SetNSigmaAntiKaon(const float& lo, const float& hi){
    mNSigmaAntiKaon[0]=lo; mNSigmaAntiKaon[1]=hi;}
inline void helensLaPTrackCut::SetNSigmaAntiProton(const float& lo, const float& hi){
    mNSigmaAntiProton[0]=lo; mNSigmaAntiProton[1]=hi;}

inline void helensLaPTrackCut::SetNHits(const int& lo, const int& hi){mNHits[0]=lo;mNHits[1]=hi;}
inline void helensLaPTrackCut::SetP(const float& lo, const float& hi){mP[0]=lo; mP[1]=hi;}
inline void helensLaPTrackCut::SetPt(const float& lo, const float& hi){mPt[0]=lo; mPt[1]=hi;}
inline void helensLaPTrackCut::SetPx(const float& lo, const float& hi){mPx[0]=lo; mPx[1]=hi;}
inline void helensLaPTrackCut::SetPy(const float& lo, const float& hi){mPy[0]=lo; mPy[1]=hi;}
inline void helensLaPTrackCut::SetPz(const float& lo, const float& hi){mPz[0]=lo; mPz[1]=hi;}
inline void helensLaPTrackCut::SetRapidity(const float& lo,const float& hi){mRapidity[0]=lo; mRapidity[1]=hi;}
inline void helensLaPTrackCut::SetDCA(const float& lo,const float& hi){mDCA[0]=lo; mDCA[1]=hi;}
inline void helensLaPTrackCut::SetCharge(const int& ch){mCharge = ch;}
inline helensLaPTrackCut* helensLaPTrackCut::Clone() { helensLaPTrackCut* c = new helensLaPTrackCut(*this); return c;}

#endif
