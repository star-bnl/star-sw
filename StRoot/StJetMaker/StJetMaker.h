// -*- mode: c++;-*-
// $Id: StJetMaker.h,v 1.21 2008/03/28 00:14:56 tai Exp $

#ifndef StJetMaker_h
#define StJetMaker_h

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

/*!
  \class StJetMaker
  \author T.Henry (Texas A&M)
  StJetMaker is used to (i) run the jet finding algorithms and (ii) store the results in a TTree.
  Multiple algorithms can be run simultaneously, via the use of the addAnalyzer() method.
  The TTree is a collection of branches, each branch containing an instance of the StJets class.
  The number of branches is dynamic and equal to the number of calls to addAnalyzer().  That is,
  one branch per jet analysis.  The TTree can be analyzed later using the StJetReader class.
 */

class StJetMaker : public StMaker {

public:

  StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputFile);
    
  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  ///Access to the Tree of StJets branches
  TTree* tree() { return mJetTree; }
    
  ///Construct a new jet analysis.
  void addAnalyzer(const StppAnaPars*, const StJetPars*, StFourPMaker*, const char* anaName);
    
private:

  struct AnalyzerCtl {
    std::string mBranchName;
    StppJetAnalyzer* mAnalyzer;
    StJets *mJets;
  };

  std::vector<AnalyzerCtl> mAnalyzerCtl;

  void fillTree(StJets& jets, StppJetAnalyzer* thisAna, StFourPMaker* fourPMaker);
  void fillJet(StJets &jets, StProtoJet& pj);

  StMuDstMaker*   mMuDstMaker;

  std::string mOutName;
  TFile *mOutFile;
  TTree *mJetTree;

  ClassDef(StJetMaker, 0)
};

#endif // StJetMaker_h

// $Log: StJetMaker.h,v $
// Revision 1.21  2008/03/28 00:14:56  tai
// *** empty log message ***
//
