// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.32 2008/04/21 20:00:28 tai Exp $
#ifndef STJETMAKER_HH
#define STJETMAKER_HH

#include "StMaker.h"

#include "StJetMakerBackwordCompatibility.h"

#include <string>
#include <vector>

class TTree;
class StppJetAnalyzer;
class StppJetAnalyzer2;
class StMuDstMaker;
class StFourPMaker;
class StJetPars;
class StppAnaPars;

namespace StSpinJet {
  class StJetTreeWriter;
}

class StJetMaker : public StMaker {

public:

  StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile);
    
  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  TTree* tree() const;
    
  void addAnalyzer(const StppAnaPars*, StJetPars*, StFourPMaker*, const char* anaName);
    
  typedef StSpinJet::StJetMakerBackwordCompatibility::jetBranchesMap jetBranchesMap;
  jetBranchesMap& getJets() const { return _backwordCompatibility->getJets(); }

private:

  std::vector<StppJetAnalyzer*> _jetFinderList;

  void findJets();
  
  StSpinJet::StJetTreeWriter *_treeWriter;

  StSpinJet::StJetMakerBackwordCompatibility *_backwordCompatibility;

  ClassDef(StJetMaker, 0)

};

#endif // STJETMAKER_HH
