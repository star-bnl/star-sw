// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.51 2008/08/02 23:10:06 tai Exp $
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

namespace StSpinJet {
  class StjeParticleCollector;
  class StjeJetFinderRunner;
  class StjeJetCuts;
}

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
    
  typedef StSpinJet::StJetMakerBackwordCompatibility::jetBranchesMap jetBranchesMap;

  // To be removed. Please do not call this method.
  jetBranchesMap& getJets() const { return _backwordCompatibility->getJets(); }
  //

  StjeTreeWriter* getTreeWriter() { return _treeWriter; }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StJetMaker.h,v 1.51 2008/08/02 23:10:06 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  std::vector<StSpinJet::StjeParticleCollector*> _particleCollectorList;

  std::vector<StSpinJet::StjeJetFinderRunner*> _jetFinderList;

  std::vector<StSpinJet::StjeJetCuts*> _jetCutsList;

  std::map<std::string, StJets*> _stjetsMap;

  StjeTreeWriter *_defaultTreeWriter;
  StjeTreeWriter *_treeWriter;

  StSpinJet::StJetMakerBackwordCompatibility *_backwordCompatibility;

  ClassDef(StJetMaker, 0)

};

#endif // STJETMAKER_H
