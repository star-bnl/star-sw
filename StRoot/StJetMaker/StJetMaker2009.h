// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 27 May 2010
//

#ifndef ST_JET_MAKER_2009_H
#define ST_JET_MAKER_2009_H

class TTree;
class StAnaPars;
class StJetEvent;
class StJetFinder;
class StjPrimaryVertex;
class StJetVertex;
class StProtoJet;
class StJetCandidate;
class StMuTrackEmu;
class StJetTrack;
class StMuTowerEmu;
class StJetTower;
class StMcTrackEmu;
class StJetParticle;

#include <vector>
#include "StMaker.h"
#include "StJetFinder/StJetPars.h"
#include "StSpinPool/StJetEvent/StJetEvent.h"

class StJetMaker2009 : public StMaker {
public:
  StJetMaker2009(const char* name = "StJetMaker2009")
    : StMaker(name)
    , mFile(0)
    , mTree(0)
  {
  }

  void Clear(Option_t* option = "");
  int  Init();
  int  Make();
  int  Finish();

  // Setters
  void addBranch(const char* name, StAnaPars* anapars, StJetPars* jetpars);
  void setJetFile(const char* filename);

  // Getters
  TTree* tree();
  StJetEvent* event(const char* branchname);

protected:
  void addJet(const StProtoJet& protojet, StJetEvent* event, StJetVertex* vertex);
  void copyVertex(const StjPrimaryVertex& vertex, StJetVertex* jetvertex);
  void copyTrack(const StMuTrackEmu*, StJetTrack*);
  void copyTower(const StMuTowerEmu*, StJetTower*);
  void copyParticle(const StMcTrackEmu*, StJetParticle*);

struct StJetBranch {
    StJetBranch(const char* name, StAnaPars* anapars, StJetPars* jetpars)
      : name(name)
      , anapars(anapars)
      , jetpars(jetpars)
      , jetfinder(jetpars->constructJetFinder())
      , event(new StJetEvent)
    {
    }

    TString name;
    StAnaPars* anapars;
    StJetPars* jetpars;
    StJetFinder* jetfinder;
    StJetEvent* event;
  };

  vector<StJetBranch*> mJetBranches;
  TString mFileName;
  TFile* mFile;
  TTree* mTree;

  ClassDef(StJetMaker2009,0);
};

#endif
