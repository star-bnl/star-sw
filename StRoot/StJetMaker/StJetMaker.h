// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.47 2008/07/22 17:40:59 tai Exp $
#ifndef STJETMAKER_HH
#define STJETMAKER_HH

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

namespace StSpinJet {
  class StParticleCollector;
  class StJetFinderRunner;
  class StJetCuts;
}

class StJetTreeWriter;

class StJetMaker : public StMaker {

public:

  StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile);
  virtual ~StJetMaker();

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  TTree* tree() const;
    
  void SetTreeWriter(StJetTreeWriter *treeWriter);

  void addAnalyzer(const StppAnaPars*, StJetPars*, StFourPMaker*, const char* anaName);

  StJets* getStJets(const char* branchName = "ConeJets12")
  {
    return _stjetsMap[string(branchName)];
  }
    
  typedef StSpinJet::StJetMakerBackwordCompatibility::jetBranchesMap jetBranchesMap;

  // To be removed. Please do not call this method.
  jetBranchesMap& getJets() const { return _backwordCompatibility->getJets(); }
  //

  StJetTreeWriter* getTreeWriter() { return _treeWriter; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetMaker.h,v 1.47 2008/07/22 17:40:59 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  std::vector<StSpinJet::StParticleCollector*> _particleCollectorList;

  std::vector<StSpinJet::StJetFinderRunner*> _jetFinderList;

  std::vector<StSpinJet::StJetCuts*> _jetCutsList;

  std::map<std::string, StJets*> _stjetsMap;

  StJetTreeWriter *_defaultTreeWriter;
  StJetTreeWriter *_treeWriter;

  StSpinJet::StJetMakerBackwordCompatibility *_backwordCompatibility;

  ClassDef(StJetMaker, 0)

};

#endif // STJETMAKER_HH
