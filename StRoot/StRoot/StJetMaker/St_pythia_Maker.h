// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 15 Dec 2009
//

#ifndef ST_PYTHIA_MAKER_H
#define ST_PYTHIA_MAKER_H

// ROOT forward declarations
class TChain;

// STAR forward declarations
class StPythiaEvent;

// STAR includes
#include "StMaker.h"

class St_pythia_Maker : public StMaker {
public:
  St_pythia_Maker(const char* name = "St_pythia_Maker") : StMaker(name) {}

  int Init();
  int Make();
  void SetFile(const char* filename) { mFileName = filename; }
  StPythiaEvent* GetEvent() const { return mEvent; }

private:
  TString mFileName;
  TChain* mChain;
  StPythiaEvent* mEvent;

  ClassDef(St_pythia_Maker,1)
};

#endif	// ST_PYTHIA_MAKER_H
