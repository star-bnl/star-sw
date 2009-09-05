// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.56 2009/09/05 22:15:55 pibero Exp $
#ifndef STJETMAKER_H
#define STJETMAKER_H

#include "StMaker.h"

#include "emulator/StJetMakerBackwordCompatibility.h"

#include <vector>
#include <map>

class TTree;

class StMuDstMaker;
class StFourPMaker;
class StJetPars;
class StppAnaPars;
class StJets;

class StjeParticleCollector;
class StjeJetFinderRunner;
class StjeJetCuts;

class StjeTreeWriter;

class StJetMaker : public StMaker {

public:

  StJetMaker(const char* name, StMuDstMaker* uDstMaker, const char* outputFile);
  virtual ~StJetMaker();

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  TTree* tree() const;
    
  void SetTreeWriter(StjeTreeWriter* treeWriter);

  void addAnalyzer(const StppAnaPars*, StJetPars*, StFourPMaker*, const char* anaName);

  // To be removed. Please do not call this method.
  StJets* getStJets(const char* branchName = "ConeJets12") const;

  typedef StJetMakerBackwordCompatibility::jetBranchesMap jetBranchesMap;

  // To be removed. Please do not call this method.
  jetBranchesMap& getJets() const { return _backwordCompatibility->getJets(); }
  //

  StjeTreeWriter* getTreeWriter() { return _treeWriter; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetMaker.h,v 1.56 2009/09/05 22:15:55 pibero Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  vector<StjeParticleCollector*> _particleCollectorList;

  vector<StjeJetFinderRunner*> _jetFinderList;

  vector<StjeJetCuts*> _jetCutsList;

  StjeTreeWriter* _defaultTreeWriter;
  StjeTreeWriter* _treeWriter;

  StJetMakerBackwordCompatibility* _backwordCompatibility;

  ClassDef(StJetMaker, 0)

};

#endif // STJETMAKER_H
