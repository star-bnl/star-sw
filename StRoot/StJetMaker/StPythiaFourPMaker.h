// -*- mode: c++;-*-
// $Id: StPythiaFourPMaker.h,v 1.11 2009/12/09 05:12:02 pibero Exp $
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

  const StThreeVectorF& getVertex() const { return vertex; }
  FourList &getTracks() { return tracks; };
  Int_t numTracks(void) { return tracks.size(); };

  Int_t Init();
  Int_t Make();
    
  void Clear(Option_t* opt);
    
private:

  StThreeVectorF vertex;
  FourList tracks;

  StjMC* _mc;
  StjMCParticleListCut* _cut;

  ClassDef(StPythiaFourPMaker,2)
};


#endif // STPYTHIAFOURPMAKER_H
