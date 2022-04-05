// -*- mode: C++ -*-

//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Lab
// 3 August 2015
//

#ifndef STJ_TRACK_REGION_H
#define STJ_TRACK_REGION_H

// STAR
#include "StjTrackList.h"
#include <iostream>
using namespace std;
//Local
#include "StjAbstractTrackRegion.h"

class StjTrackRegion : public StjAbstractTrackRegion {
public: 
  StjTrackRegion(float phiplus, float phiminus, float deta) : mphiplus(phiplus),  mphiminus(phiminus), mdeta(deta){}
  virtual ~StjTrackRegion() {}
 
  StjTrackList Do(const StjTrackList &trackList, const StJetCandidate* leadingjet, const TString bname);
  float phiplus() const {return mphiplus;}
  float phiminus() const {return mphiminus;}
  float deta() const { return mdeta;}
  void setphiplus(float phiplus) { mphiplus = phiplus;} 
  void setphiminus(float phiminus) { mphiminus = phiminus;} 
  void setdeta(float deta) {mdeta = deta;}
private: 
  float mphiplus;
  float mphiminus;
  float mdeta;
  ClassDef(StjTrackRegion,0);
};

#endif // STJ_TRACK_REGION_H 
