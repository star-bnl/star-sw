//StKonstKtJetFinder.h
//adapted from M.L. Miller (Yale Software)
//07/02
//Thomas Henry
//08/02

//adapted from Akio Ogawa's work

#ifndef StKonstKtJetFinder_HH
#define StKonstKtJetFinder_HH

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

class StKonstKtJetFinder : public StKonstJetFinder
{
public:
    //cstr-dstr
    StKonstKtJetFinder(StKonstJetFinderPars& pars) : StKonstJetFinder(pars) {};
    virtual ~StKonstKtJetFinder();

    //inherited interface
    virtual Int_t kfindJets(Int_t numTracks, Float_t *pt, Float_t *eta,
      Float_t *phi, Float_t *mass,
      StProtoJet* jets, StProtoJet** tracks) {
        return jetFinder.FindKt(numTracks, pt, eta, phi, mass,
          jets, tracks);
      };

    void setRadius(Double_t v) { jetFinder.setConeRadius(v); };
    Double_t getRadius(void) { return jetFinder.getConeRadius(); };

protected:
    StKonstKtJetFinder() {}; ///Not implemented, must pass pars at construction time!

  ClassDef(StKonstKtJetFinder, 1)
};

#endif

