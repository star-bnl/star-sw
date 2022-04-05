// -*- mode: C++ -*-

//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Lab
// 10 August 2015
//

#ifndef STJ_MCPARTICLE_REGION_H
#define STJ_MCPARTICLE_REGION_H

// STAR
#include "StjTrackList.h"
#include <iostream>
using namespace std;
//Local
#include "StjAbstractMCParticleRegion.h"

class StjMCParticleRegion : public StjAbstractMCParticleRegion {
public: 
  StjMCParticleRegion(float phiplus, float phiminus, float deta) : mphiplus(phiplus),  mphiminus(phiminus), mdeta(deta){}
  virtual ~StjMCParticleRegion() {}
 
  StjMCParticleList Do(const StjMCParticleList &mcParticleList, const StJetCandidate* leadingjet, const TString bname);
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
  ClassDef(StjMCParticleRegion,0);
};

#endif // STJ_MCPARTICLE_REGION_H 
