// -*- mode: C++ -*-

//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 19 July 2007
//

#ifndef ST_GAMMA_PYTHIA_EVENT_MAKER_H
#define ST_GAMMA_PYTHIA_EVENT_MAKER_H

class StMcVertex;
class StGammaPythiaEvent;

#include "StMaker.h"

class StGammaPythiaEventMaker : public StMaker {
public:
  StGammaPythiaEventMaker(const char* name = "GammaPythia") : StMaker(name), mPythia(0) {}
  ~StGammaPythiaEventMaker() {}

  void SetPythia(StGammaPythiaEvent* pythia) { mPythia = pythia; }
  int Make();

private:
  void collectDecayPhotons();
  void collectDecayPhotons(StMcVertex* vertex);

  StGammaPythiaEvent* mPythia;

  ClassDef(StGammaPythiaEventMaker, 1);
};

#endif
