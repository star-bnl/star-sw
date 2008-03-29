// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.23 2008/03/29 18:45:22 tai Exp $

#ifndef STJETMAKER_HH
#define STJETMAKER_HH

#include "StMaker.h"
#include "StppJetAnalyzer.h"

#include <string>
#include <vector>

class TFile;
class TTree;
class StMuDstMaker;
class StFourPMaker;
class StJetPars;
class StppAnaPars;
class StProtoJet;
class StJets;



class StJetMaker : public StMaker {

public:


  StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile);
    
  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  TTree* tree() { return mJetTree; }
    
  void addAnalyzer(const StppAnaPars*, const StJetPars*, StFourPMaker*, const char* anaName);
    
  // for backword compatability
  typedef std::map<std::string, StppJetAnalyzer*> jetBranchesMap;
  // for backword compatability
  jetBranchesMap& getJets() { return mJetBranches; }

private:

  // for backword compatability
  jetBranchesMap  mJetBranches;

  struct AnalyzerCtl {
    std::string mBranchName;
    StppJetAnalyzer* mAnalyzer;
    StJets *mJets;
  };

  std::vector<AnalyzerCtl> mAnalyzerCtl;

  void fillTree(StJets& jets, StppJetAnalyzer* analyzer, StFourPMaker* fourPMaker);
  void fillJet(StJets &jets, StProtoJet& pj);

  StMuDstMaker*   mMuDstMaker;

  std::string mOutName;
  TFile *mOutFile;
  TTree *mJetTree;

  ClassDef(StJetMaker, 0)
};

#endif // STJETMAKER_HH
