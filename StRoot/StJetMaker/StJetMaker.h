// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.59 2014/08/06 11:43:22 jeromel Exp $
#ifndef STJETMAKER_H
#define STJETMAKER_H

// ROOT
class TTree;

// STAR
class StMuDstMaker;
class StFourPMaker;
class StJetPars;
class StppAnaPars;
class StJets;
class StJetEvent;
class StjeParticleCollector;
class StjeJetFinderRunner;
class StjeJetCuts;
class StjeTreeWriter;
class AbstractFourVec;

// C++ STL
#include <list>
#include <vector>

using namespace std;

// STAR
#include "StMaker.h"
#include "StJetFinder/StProtoJet.h"

class StJetMaker : public StMaker {
public:
  StJetMaker(const char* name, StMuDstMaker* uDstMaker, const char* outputFile);
  virtual ~StJetMaker();

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  TTree* tree() const;
    
  void SetTreeWriter(StjeTreeWriter* treeWriter);

  void addAnalyzer(const StppAnaPars* anapars, StJetPars* jetpars, StFourPMaker* fourPMaker, const char* name);

  StJets* getStJets(const char* branchName = "ConeJets12") const;
  StJetEvent* getStJetEvent(const char* branchname = "ConeJets12") const;

  StjeTreeWriter* getTreeWriter() { return _treeWriter; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetMaker.h,v 1.59 2014/08/06 11:43:22 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:
  struct StJetBranch {
    TString name;
    vector<const AbstractFourVec*> particles;
    list<StProtoJet> protojets;
    StjeParticleCollector* particleCollector;
    StjeJetFinderRunner* jetFinder;
    StjeJetCuts* jetCuts;

    StJetBranch(const StppAnaPars* anapars, StJetPars* jetpars, StFourPMaker* fourPMaker, const char* name);

    void clear()
    {
      particles.clear();
      protojets.clear();
    }
  };

  vector<StJetBranch*> mJetBranches;

  StjeTreeWriter* _defaultTreeWriter;
  StjeTreeWriter* _treeWriter;

  ClassDef(StJetMaker,0);
};

#endif // STJETMAKER_H
