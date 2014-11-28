// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 4 May 2010
//
// Saves Pythia record for every event into a designated file.
// This maker must run after St_geant_Maker which builds the
// g2t tables needed.

#ifndef ST_PYTHIA_EVENT_MAKER_H
#define ST_PYTHIA_EVENT_MAKER_H

// ROOT
class TFile;
class TTree;
class TVector3;
class TParticle;

// STAR
class StPythiaEvent;

// ROOT
#include "StMaker.h"

class StPythiaEventMaker : public StMaker {
public:
  StPythiaEventMaker(const char* name = "PythiaEvent")
    : StMaker(name)
    , mFileName("pythia.root")
    , mFile(0)
    , mTree(0)
    , mPythiaEvent(0)
  {
  }

  void Clear(Option_t* option = "");
  int  Init();
  int  Make();
  int  Finish();
  void SetPythiaFile(const char* filename) { mFileName = filename; }

private:
  void getEvent();
  void getPythia();
  void getVertex();
  void getParticles();
  void getAsymmetries();

  TString mFileName;
  TFile* mFile;
  TTree* mTree;
  StPythiaEvent* mPythiaEvent;

  ClassDef(StPythiaEventMaker,0);
};

ostream& operator<<(ostream&, const TVector3&);
ostream& operator<<(ostream&, const TParticle&);
ostream& operator<<(ostream&, const StPythiaEvent&);

#endif	// ST_PYTHIA_EVENT_MAKER_H
