// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.22 2008/03/28 00:54:27 tai Exp $

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
    
private:

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
