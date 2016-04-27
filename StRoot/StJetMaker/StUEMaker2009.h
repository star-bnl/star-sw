// -*- mode:c++ -*-
//
// Grant Webb <gdwebb@bnl.edu>
// Brookhaven National Laboratory
// 30 July 2015
//

#ifndef ST_UE_MAKER_2009_H
#define ST_UE_MAKER_2009_H

class TTree;
class StJetCandidate;
//class StUECandidate;
class StJetEvent;
class StUeEvent;
class StJetSkimEvent;
class StPythiaEvent;
class StJetVertex;
class StJetMaker2009;
class StJetSkimEvent;
#include <vector>
#include "StMaker.h"
#include "StSpinPool/StJetEvent/StUeEvent.h"
class StUEMaker2009 : public StMaker { 

public:
  StUEMaker2009(const char* name = "StUEMaker2009")
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
  void addBranch(const char* name, StAnaPars* anapars, const char* jetname);// StRegionPars* regionPars);
  void setUeFile(const char* filename);

  // Getters
  TTree* tree();
  StUeEvent* event(const char* branchname);

StJetMaker2009 *mJetMaker;
StJetSkimEventMaker *mSkimMaker;


private:
  StJetCandidate* findLeadingJet(StJetVertex *vertex);
  // void AddRegion(StJetCandidate *leadingjet, StjTrackList trackList, StUeEvent *event);
  // void addJet(const StProtoJet& protojet, StJetEvent* event, StJetVertex* vertex);
  void copyVertex(const StjPrimaryVertex& vertex, StJetVertex* jetvertex);

  void copyTrack(const StjTrack&, StJetTrack*);
  void copyTower(const StjTowerEnergy&, StJetTower*);
  void copyParticle(const StjMCParticle&, StJetParticle*);

  struct StUeBranch {
    StUeBranch(const char* name,  StAnaPars* anapars, const char* jetname)//, StUEAnaPars* ueanapars, StUEPars* uepars)
      : name(name)
      , anapars(anapars)
      , jetname(jetname)
      // , jetfinder(jetpars->constructJetFinder())
      , event(new StUeEvent)
    {
    }

    TString name;
    StAnaPars* anapars;
    TString jetname;
    // StJetPars* jetpars;
    //  StJetFinder* jetfinder;
    StUeEvent* event;
  };
  void addTracks(const StjTrackList&, StUeEvent*);
  void addTowers(const StjTowerEnergyList&, StUeEvent*);
  void addParticles(const StjMCParticleList&, StUeEvent*);
  vector<StUeBranch*> mUeBranches;
  TString mFileName;
  TFile* mFile;
  TTree* mTree;

  ClassDef(StUEMaker2009,2);
};

#endif
