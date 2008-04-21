// $Id: StJetMaker.cxx,v 1.50 2008/04/21 21:20:25 tai Exp $
#include "StJetMaker.h"

#include "StJetTreeWriter.h"
#include "StppJetAnalyzer.h"
#include "StppJetAnalyzer2.h"

#include <StJetFinder/StProtoJet.h>

#include <string>
#include <list>

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
  list<StProtoJet>* protoJetList = new list<StProtoJet>;

  _treeWriter->addJetFinder(fp, protoJetList, name);

  _jetFinderList.push_back(new StppJetAnalyzer2(ap, jp, fp, *protoJetList));

  _backwordCompatibility->addAnalyzer(new StppJetAnalyzer(*protoJetList), _treeWriter->getLastStJets(), name);
}

Int_t StJetMaker::Init() 
{
  _treeWriter->Init();

  return kStOk;
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

  return kStOK;
}

void StJetMaker::findJets()
{
  for(vector<StppJetAnalyzer2*>::iterator jetFinder = _jetFinderList.begin(); jetFinder != _jetFinderList.end(); ++jetFinder) {
    (*jetFinder)->findJets();
  }
}

TTree* StJetMaker::tree() const 
{
  return _treeWriter->jetTree();
}
