// $Id: StJetMaker.cxx,v 1.41 2008/04/20 21:38:50 tai Exp $

#include "StJetMaker.h"

#include "TFile.h"
#include "TTree.h"

#include "StMessMgr.h"

#include "StEvent.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StJetFinder/StProtoJet.h"

#include "StJets.h"
#include "StJet.h"
#include "StFourPMakers/StFourPMaker.h"
#include "StFourPMakers/StBET4pMaker.h"
#include "StFourPMakers/StMuEmcPosition.h"

#include "StJetTreeWriter.h"

using namespace std;
using namespace StSpinJet;

ClassImp(StJetMaker)
  
StJetMaker::StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputName) 
  : StMaker(name)
  , mAnalyzerCtl(0)
  , mMuDstMaker(uDstMaker)
  , mOutName(outputName)
  , mJetTree(0)
  , _treeWriter(new StJetTreeWriter(*uDstMaker, string(outputName)))
{

}

void StJetMaker::addAnalyzer(const StppAnaPars* ap, const StJetPars* jp, StFourPMaker* fp, const char* name)
{
  AnalyzerCtl anaCtl;
  anaCtl.mBranchName = name;
  anaCtl.mAnalyzer = new StppJetAnalyzer(ap, jp, fp);
  anaCtl.mJets = new StJets();

  mAnalyzerCtl.push_back(anaCtl);

  // for backword compatability
  mJetBranches[name] = anaCtl.mAnalyzer;
  anaCtl.mAnalyzer->setmuDstJets(anaCtl.mJets);
}

Int_t StJetMaker::Init() 
{
  _treeWriter->Init();

  mJetTree  = new TTree("jet", "jetTree");

  for(std::vector<AnalyzerCtl>::iterator it = mAnalyzerCtl.begin(); it != mAnalyzerCtl.end(); ++it) {
    mJetTree->Branch ((*it).mBranchName.c_str(), "StJets", &((*it).mJets));
  }

  return StMaker::Init();
}

Int_t StJetMaker::Make()
{
  findJets();
  _treeWriter->fillJetTree(mAnalyzerCtl, mJetTree);
  return kStOk;
}

Int_t StJetMaker::Finish()
{
  _treeWriter->Finish();
  delete mOutFile;
  mOutFile = 0;

  StMaker::Finish();
  return kStOK;
}

void StJetMaker::findJets()
{
  for(std::vector<AnalyzerCtl>::iterator it = mAnalyzerCtl.begin(); it != mAnalyzerCtl.end(); ++it) {
    StppJetAnalyzer* analyzer = (*it).mAnalyzer;
    analyzer->findJets();
  }
}
