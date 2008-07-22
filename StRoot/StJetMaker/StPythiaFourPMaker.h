// -*- mode: c++;-*-
// $Id: StPythiaFourPMaker.h,v 1.5 2008/07/22 06:36:54 tai Exp $
#ifndef STPYTHIAFOURPMAKER_HH
#define STPYTHIAFOURPMAKER_HH

#include "StFourPMaker.h"

#include <MCParticleList.h>

#include <vector>

class StMcEventMaker;
class StMCAsymMaker;
class StMuTrackFourVec;

namespace StSpinJet {
  class StJetMC;
  class StJetMCParticleListCut;
}

class StPythiaFourPMaker : public StFourPMaker {

public:
    
  StPythiaFourPMaker(const char *name, StMCAsymMaker* sim = 0, StMcEventMaker* mc = 0)
    : StFourPMaker(name) { }
  virtual ~StPythiaFourPMaker() { };
    
  FourList &getTracks() { return tracks; };
  Int_t numTracks(void) { return tracks.size(); };

  Int_t Init();
  Int_t Make();
    
  void Clear(Option_t* opt);
    
private:

  FourList tracks;

  StSpinJet::StJetMC* _mc;
  StSpinJet::StJetMCParticleListCut* _cut;

  ClassDef(StPythiaFourPMaker,1)
};


#endif // STPYTHIAFOURPMAKER_HH
