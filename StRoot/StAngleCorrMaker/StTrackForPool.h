#ifndef StTrackForPool_HH
#define StTrackForPool_HH

///////////////////////////////////////////////////////////////////////////////
//
// StTrackForPool
//
// Description: 
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Craig Ogilvie, MIT
//  Matt Horsley,  YALE
// History:
//
///////////////////////////////////////////////////////////////////////////////

#include "TString.h"

class StTrackForPool {
private:
  Int_t mId; 
  Double_t mPsuedoRap;
  Double_t mPx,mPy,mPz;
  Int_t mCharge; 

  // track characteristics
  Double_t mChi2;
  Int_t mNTPCPoints;

public:
  StTrackForPool();
  StTrackForPool(Double_t px, Double_t py, Double_t pz);
  ~StTrackForPool();
  void SetMomentum(Double_t px, Double_t py, Double_t pz);
  void GetMomentum(Double_t& px, Double_t& py, Double_t& pz);
  void GetMomentum(Double_t& p);
  void GetPt(Double_t& pt);
  void GetTrackIDNumber(Int_t& gid);
  void SetTrackIDNumber(Int_t sid);
  void GetPseudoRapidity(Double_t& pRap);
  void SetChiSquared(Double_t chiXY);
  void SetNTPCPoints(Int_t npoints);
  Double_t GetChiSquared();
  Int_t GetNTPCPoints();
  void  SetCharge(Int_t charge);
  Int_t GetCharge();

};

#endif
