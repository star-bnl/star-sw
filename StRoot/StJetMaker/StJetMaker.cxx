// $Id: StJetMaker.cxx,v 1.54 2008/05/01 21:31:35 tai Exp $
#include "StJetMaker.h"

#include "StParticleCollector.h"
#include "StJetFinderRunner.h"
#include "StJetTreeWriter.h"

#include "StppJetAnalyzer.h"

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

  _particleCollectorList.push_back(new StParticleCollector(ap, fp, *protoJetList));

  _jetFinderList.push_back(new StJetFinderRunner(ap, jp, *protoJetList));

  _treeWriter->addJetFinder(fp, protoJetList, name);


  _backwordCompatibility->addAnalyzer(new StppJetAnalyzer(*protoJetList), _treeWriter->getLastStJets(), name);
}

Int_t StJetMaker::Init() 
{
  for(vector<StJetFinderRunner*>::iterator jetFinder = _jetFinderList.begin(); jetFinder != _jetFinderList.end(); ++jetFinder) {
    (*jetFinder)->Init();
  }

  _treeWriter->Init();

  return kStOk;
}

Int_t StJetMaker::Make()
{
  for(vector<StParticleCollector*>::iterator particleCollector = _particleCollectorList.begin(); particleCollector != _particleCollectorList.end(); ++particleCollector) {
    (*particleCollector)->Do();
  }

  for(vector<StJetFinderRunner*>::iterator jetFinder = _jetFinderList.begin(); jetFinder != _jetFinderList.end(); ++jetFinder) {
    (*jetFinder)->Run();
  }

  _treeWriter->fillJetTree();

  return kStOk;
}

Int_t StJetMaker::Finish()
{
  _treeWriter->Finish();

  return kStOK;
}

TTree* StJetMaker::tree() const 
{
  return _treeWriter->jetTree();
}
