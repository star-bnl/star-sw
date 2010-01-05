// -*- mode: c++;-*-
// $Id: StPythiaFourPMaker.h,v 1.10 2009/08/14 14:51:17 pibero Exp $
#ifndef STPYTHIAFOURPMAKER_H
#define STPYTHIAFOURPMAKER_H

#include "StFourPMaker.h"

#include <StjMCParticleList.h>

#include <vector>

class StMcEventMaker;
class StMCAsymMaker;
class StMuTrackFourVec;

class StjMC;
class StjMCParticleListCut;

class StPythiaFourPMaker : public StFourPMaker {

public:
    
  StPythiaFourPMaker(const char *name = "StPythiaFourPMaker") : StFourPMaker(name) { }
  virtual ~StPythiaFourPMaker() { };
    
  FourList &getTracks() { return tracks; };
  Int_t numTracks(void) { return tracks.size(); };

  Int_t Init();
  Int_t Make();
    
  void Clear(Option_t* opt);
    
private:

  FourList tracks;

  StjMC* _mc;
  StjMCParticleListCut* _cut;

  ClassDef(StPythiaFourPMaker,2)
};


#endif // STPYTHIAFOURPMAKER_H
