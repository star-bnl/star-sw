// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.28 2008/04/20 23:34:26 tai Exp $
#ifndef STJETMAKER_HH
#define STJETMAKER_HH

#include "StMaker.h"
#include "StppJetAnalyzer.h"
#include "AnalyzerCtl.h"

#include <string>
#include <vector>

class TTree;
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
    
  void addAnalyzer(const StppAnaPars*, const StJetPars*, StFourPMaker*, const char* anaName);
    
  // for backword compatability
  typedef std::map<std::string, StppJetAnalyzer*> jetBranchesMap;
  // for backword compatability
  jetBranchesMap& getJets() { return mJetBranches; }

private:

  // for backword compatability
  jetBranchesMap  mJetBranches;

  std::vector<StppJetAnalyzer*> _jetFinderList;

  void findJets();
  
  StSpinJet::StJetTreeWriter *_treeWriter;

  ClassDef(StJetMaker, 0)
};

#endif // STJETMAKER_HH
