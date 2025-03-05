//usr/bin/env root4star -l -b -q $0'('$1', '$2')'; exit $?
#include "gen.C"
#include "TString.h"

void single_particle_gun( Int_t nevents=5000, Int_t rngSeed=541522,
                          TString particle="mu-", Int_t nParticles=1,
                          Float_t _minPt=0.1, Float_t _maxPt=1.0,
                          Float_t _minEta=2.5, Float_t _maxEta=4.0,
                          Float_t _minPhi=0.0, Float_t _maxPhi=2.0*TMath::Pi()
                        )
{
  nameParticle = particle;
  numParticles = nParticles;
  minPt = _minPt;
  maxPt = _maxPt;
  minEta = _minEta;
  maxEta = _maxEta;
  minPhi = _minPhi;
  maxPhi = _maxPhi;

  TString safeName = particle;
  safeName.ReplaceAll("+", "plus");
  safeName.ReplaceAll("-", "minus");
  fzdFilename = TString::Format("single_particle_gun_%s_%dEvents_%dPerEvent_Pt_%0.2fto%0.2f_Eta_%0.2fto%0.2f_Phi_%0.2fto%0.2f.fzd", safeName.Data(), nevents, numParticles, minPt, maxPt, minEta, maxEta, minPhi, maxPhi);
  primaryName = TString::Format("single_particle_gun_%s_%dEvents_%dPerEvent_Pt_%0.2fto%0.2f_Eta_%0.2fto%0.2f_Phi_%0.2fto%0.2f.root", safeName.Data(), nevents, numParticles, minPt, maxPt, minEta, maxEta, minPhi, maxPhi);
  cout << "Writing output to: " << fzdFilename << endl;
  gen( nevents, rngSeed );
}
