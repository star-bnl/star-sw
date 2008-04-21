// $Id: StJetMaker.cxx,v 1.47 2008/04/21 19:14:17 tai Exp $
#include "StJetMaker.h"

#include "StJetTreeWriter.h"
#include "StppJetAnalyzer.h"

using namespace std;
using namespace StSpinJet;

ClassImp(StJetMaker)
  
StJetMaker::StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputName) 
  : StMaker(name)
  , _treeWriter(new StJetTreeWriter(*uDstMaker, string(outputName)))
  , _backwordCompatibility(new StJetMakerBackwordCompatibility)
{

}

void StJetMaker::addAnalyzer(const StppAnaPars* ap, StJetPars* jp, StFourPMaker* fp, const char* name)
{
  StppJetAnalyzer* analyzer = new StppJetAnalyzer(ap, jp, fp);

  _treeWriter->addJetFinder(fp, &(analyzer->getJets()), name);

  _jetFinderList.push_back(analyzer);

  _backwordCompatibility->addAnalyzer(analyzer, _treeWriter->getLastStJets(), name);
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
