// -*- mode: c++;-*-
// $Id: StPythiaFourPMaker.h,v 1.8 2008/08/02 22:43:05 tai Exp $
#ifndef STPYTHIAFOURPMAKER_H
#define STPYTHIAFOURPMAKER_H

#include "StFourPMaker.h"

#include <StjMCParticleList.h>

#include <vector>

class StMcEventMaker;
class StMCAsymMaker;
class StMuTrackFourVec;

namespace StSpinJet {
  class StjMC;
  class StjMCParticleListCut;
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

  StSpinJet::StjMC* _mc;
  StSpinJet::StjMCParticleListCut* _cut;

  ClassDef(StPythiaFourPMaker,1)
};


#endif // STPYTHIAFOURPMAKER_H
