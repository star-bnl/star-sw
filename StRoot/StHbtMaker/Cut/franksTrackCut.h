 /***************************************************************************
 *
 *  
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    a simple particle cut that selects on phasespace, #hits, DCA, and PID          
 *
 ***************************************************************************
 *
 * 
 *
 **************************************************************************/

#ifndef franksTrackCut_hh
#define franksTrackCut_hh


//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtTrackCut.h"

class franksTrackCut : public StHbtTrackCut
{

 public:

  franksTrackCut();
  franksTrackCut(franksTrackCut& );
  ~franksTrackCut();
  
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

  franksTrackCut* Clone();

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
  ClassDef(franksTrackCut, 1)
#endif
};


inline void franksTrackCut::SetNSigmaElectron(const float& lo, const float& hi){mNSigmaElectron[0]=lo; mNSigmaElectron[1]=hi;}
inline void franksTrackCut::SetNSigmaPion(const float& lo, const float& hi){mNSigmaPion[0]=lo; mNSigmaPion[1]=hi;}
inline void franksTrackCut::SetNSigmaKaon(const float& lo, const float& hi){mNSigmaKaon[0]=lo; mNSigmaKaon[1]=hi;}
inline void franksTrackCut::SetNSigmaProton(const float& lo, const float& hi){mNSigmaProton[0]=lo; mNSigmaProton[1]=hi;}
inline void franksTrackCut::SetNSigmaAntiElectron(const float& lo, const float& hi){
    mNSigmaAntiElectron[0]=lo; mNSigmaAntiElectron[1]=hi;}
inline void franksTrackCut::SetNSigmaAntiPion(const float& lo, const float& hi){
    mNSigmaAntiPion[0]=lo; mNSigmaAntiPion[1]=hi;}
inline void franksTrackCut::SetNSigmaAntiKaon(const float& lo, const float& hi){
    mNSigmaAntiKaon[0]=lo; mNSigmaAntiKaon[1]=hi;}
inline void franksTrackCut::SetNSigmaAntiProton(const float& lo, const float& hi){
    mNSigmaAntiProton[0]=lo; mNSigmaAntiProton[1]=hi;}

inline void franksTrackCut::SetNHits(const int& lo, const int& hi){mNHits[0]=lo;mNHits[1]=hi;}
inline void franksTrackCut::SetP(const float& lo, const float& hi){mP[0]=lo; mP[1]=hi;}
inline void franksTrackCut::SetPt(const float& lo, const float& hi){mPt[0]=lo; mPt[1]=hi;}
inline void franksTrackCut::SetPx(const float& lo, const float& hi){mPx[0]=lo; mPx[1]=hi;}
inline void franksTrackCut::SetPy(const float& lo, const float& hi){mPy[0]=lo; mPy[1]=hi;}
inline void franksTrackCut::SetPz(const float& lo, const float& hi){mPz[0]=lo; mPz[1]=hi;}
inline void franksTrackCut::SetRapidity(const float& lo,const float& hi){mRapidity[0]=lo; mRapidity[1]=hi;}
inline void franksTrackCut::SetDCA(const float& lo,const float& hi){mDCA[0]=lo; mDCA[1]=hi;}
inline void franksTrackCut::SetCharge(const int& ch){mCharge = ch;}
inline franksTrackCut* franksTrackCut::Clone() { franksTrackCut* c = new franksTrackCut(*this); return c;}

#endif
