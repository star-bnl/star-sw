// -*- mode: C++ -*-

//
// Grant Webb <gdwebb@bnl.gov>
// 
// 10 August 2015
//

#ifndef STJ_ABSTRACT_MCPARTICLE_REGION_H
#define STJ_ABSTRACT_MCPARTICLE_REGION_H

// ROOT
#include "TObject.h"

// STAR
#include "StjMCParticleList.h"
#include "StSpinPool/StJetEvent/StJetCandidate.h"

class StjAbstractMCParticleRegion : public TObject {
public:
  StjAbstractMCParticleRegion() {}
  virtual ~StjAbstractMCParticleRegion() {}

  StjMCParticleList operator()(const StjMCParticleList& particleList, const StJetCandidate* jet, const TString name)
  {
    return Do(particleList, jet, name);
  }

  virtual StjMCParticleList Do(const StjMCParticleList& particleList, const StJetCandidate* jet, const TString name) = 0;

  ClassDef(StjAbstractMCParticleRegion,0);
};

#endif //STJ_ABSTRACT_MCPARTICLE_REGION_H
