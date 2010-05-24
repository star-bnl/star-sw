// -*- mode: c++;-*-
// $Id: StPythiaFourPMaker.h,v 1.14 2010/05/24 23:11:35 pibero Exp $
#ifndef STPYTHIAFOURPMAKER_H
#define STPYTHIAFOURPMAKER_H

#include "StFourPMaker.h"

#include <vector>

class StMcEventMaker;
class StMCAsymMaker;
class StMuTrackFourVec;

class StjMC;
class StjMCParticleListCut;

class StPythiaFourPMaker : public StFourPMaker {

public:
    
  StPythiaFourPMaker(const char *name = "StPythiaFourPMaker") : StFourPMaker(name) { }
  virtual ~StPythiaFourPMaker() { }

  Int_t Init();
  Int_t Make();
    
  void Clear(Option_t* opt);
    
private:

  StjMC* _mc;
  StjMCParticleListCut* _cut;

  ClassDef(StPythiaFourPMaker,0)
};

#endif // STPYTHIAFOURPMAKER_H
