//StKonstConeJetFinder.h
//adapted from M.L. Miller (Yale Software)
//07/02
//Thomas Henry
//08/02

//adapted from Akio Ogawa's work

#ifndef StKonstConeJetFinder_HH
#define StKonstConeJetFinder_HH

//std
#include <map>
#include <vector>
#include <cmath>
using std::map;
using std::vector;

//StJetFinder
#include "StJetFinderKonst.h"
#include "StKonstJetFinder.h"
#include "StJetFinder.h"
#include "Functors.h"
#include "StJetEtCell.h"

class StKonstConeJetFinder : public StKonstJetFinder
{
public:
    //cstr-dstr
    StKonstConeJetFinder(StKonstJetFinderPars& pars) : StKonstJetFinder(pars) {};
    virtual ~StKonstConeJetFinder();

    //inherited interface
    virtual Int_t kfindJets(Int_t numTracks, Float_t *pt, Float_t *eta,
      Float_t *phi, Float_t *mass,
      StProtoJet* jets, StProtoJet** tracks) {
        return jetFinder.FindCone(numTracks, pt, eta, phi, mass,
          jets, tracks);
      };

    void setConeRadius(Double_t v) { mPars.coneRadius = v; };
    void setEtSeed(Double_t v) { mPars.etSeed = v; };

protected:
    StKonstConeJetFinder() {} ; ///Not implemented, must pass pars at construction time!

  ClassDef(StKonstConeJetFinder, 1)
};

#endif

