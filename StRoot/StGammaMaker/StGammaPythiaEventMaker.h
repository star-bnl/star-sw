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

  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StGammaPythiaEventMaker.h,v 1.2 2008/06/30 14:58:42 jwebb Exp $ built "__DATE__" "__TIME__; return cvs;}


private:
  void collectDecayPhotons();
  void collectDecayPhotons(StMcVertex* vertex);

  StGammaPythiaEvent* mPythia;

  ClassDef(StGammaPythiaEventMaker, 1);
};

#endif
