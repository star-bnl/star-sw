// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.52 2008/08/03 00:26:16 tai Exp $
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

  StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile);
  virtual ~StJetMaker();

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  TTree* tree() const;
    
  void SetTreeWriter(StjeTreeWriter *treeWriter);

  void addAnalyzer(const StppAnaPars*, StJetPars*, StFourPMaker*, const char* anaName);

  StJets* getStJets(const char* branchName = "ConeJets12")
  {
    return _stjetsMap[string(branchName)];
  }
    
  typedef StJetMakerBackwordCompatibility::jetBranchesMap jetBranchesMap;

  // To be removed. Please do not call this method.
  jetBranchesMap& getJets() const { return _backwordCompatibility->getJets(); }
  //

  StjeTreeWriter* getTreeWriter() { return _treeWriter; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetMaker.h,v 1.52 2008/08/03 00:26:16 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  std::vector<StjeParticleCollector*> _particleCollectorList;

  std::vector<StjeJetFinderRunner*> _jetFinderList;

  std::vector<StjeJetCuts*> _jetCutsList;

  std::map<std::string, StJets*> _stjetsMap;

  StjeTreeWriter *_defaultTreeWriter;
  StjeTreeWriter *_treeWriter;

  StJetMakerBackwordCompatibility *_backwordCompatibility;

  ClassDef(StJetMaker, 0)

};

#endif // STJETMAKER_H
