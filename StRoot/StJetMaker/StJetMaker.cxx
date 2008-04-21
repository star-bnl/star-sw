// $Id: StJetMaker.cxx,v 1.43 2008/04/21 00:24:56 tai Exp $
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

  StppJetAnalyzer* analyzer = new StppJetAnalyzer(ap, jp, fp);
  StJets *stJets = new StJets();

  _treeWriter->addAnalyzer(analyzer, stJets, name);

  _jetFinderList.push_back(analyzer);

  // for backword compatability
  mJetBranches[name] = analyzer;
  analyzer->setmuDstJets(stJets);
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
