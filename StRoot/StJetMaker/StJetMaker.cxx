// $Id: StJetMaker.cxx,v 1.42 2008/04/20 23:34:26 tai Exp $
#include "StJetMaker.h"

#include "StMessMgr.h"
#include "StJetTreeWriter.h"

#include "StJets.h"

using namespace std;
using namespace StSpinJet;

ClassImp(StJetMaker)
  
StJetMaker::StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputName) 
  : StMaker(name)
  , _treeWriter(new StJetTreeWriter(*uDstMaker, string(outputName)))
{

}

void StJetMaker::addAnalyzer(const StppAnaPars* ap, const StJetPars* jp, StFourPMaker* fp, const char* name)
{

  StppJetAnalyzer* anAnalyzer = new StppJetAnalyzer(ap, jp, fp);
  StJets *aStJets = new StJets();

  AnalyzerCtl anaCtl;
  anaCtl.mBranchName = name;
  anaCtl.mAnalyzer = anAnalyzer;
  anaCtl.mJets = aStJets;

  _treeWriter->push_back(anaCtl);
  _jetFinderList.push_back(anaCtl.mAnalyzer);

  // for backword compatability
  mJetBranches[name] = anAnalyzer;
  anAnalyzer->setmuDstJets(aStJets);
}

Int_t StJetMaker::Init() 
{
  _treeWriter->Init();

  return StMaker::Init();
}

Int_t StJetMaker::Make()
{
  findJets();
  _treeWriter->fillJetTree();
  return kStOk;
}

Int_t StJetMaker::Finish()
{
  _treeWriter->Finish();

  StMaker::Finish();
  return kStOK;
}

void StJetMaker::findJets()
{
  for(vector<StppJetAnalyzer*>::iterator jetFinder = _jetFinderList.begin(); jetFinder != _jetFinderList.end(); ++jetFinder) {
    (*jetFinder)->findJets();
  }
}

TTree* StJetMaker::tree() const 
{
  return _treeWriter->jetTree();
 }
