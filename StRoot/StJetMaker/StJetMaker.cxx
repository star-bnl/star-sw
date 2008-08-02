// $Id: StJetMaker.cxx,v 1.67 2008/08/02 22:21:09 tai Exp $
#include "StJetMaker.h"

#include "StParticleCollector.h"
#include "StJetFinderRunner.h"
#include "StjeJetCuts.h"
#include "StjTreeWriter.h"
#include "StjeDefaultJetTreeWriter.h"
#include "StppJetAnalyzer.h"

#include <StJetFinder/StProtoJet.h>

#include <StSpinPool/StJets/StJets.h>

#include <string>
#include <list>
#include <vector>

using namespace std;
using namespace StSpinJet;

ClassImp(StJetMaker)
  
StJetMaker::StJetMaker(const Char_t *name, StMuDstMaker* uDstMaker, const char *outputName) 
  : StMaker(name)
  , _defaultTreeWriter(new StjeDefaultJetTreeWriter(*uDstMaker, string(outputName)))
  , _treeWriter(_defaultTreeWriter)
  , _backwordCompatibility(new StJetMakerBackwordCompatibility)
{

}

StJetMaker::~StJetMaker()
{
  delete _defaultTreeWriter;
  delete _backwordCompatibility;
}


void StJetMaker::addAnalyzer(const StppAnaPars* ap, StJetPars* jp, StFourPMaker* fp, const char* name)
{
  list<StProtoJet>* protoJetList = new list<StProtoJet>;

  vector<const AbstractFourVec*> *particleList = new vector<const AbstractFourVec*>;

  StJets* stjets = new StJets();

  _particleCollectorList.push_back(new StParticleCollector(ap, fp, *particleList));

  _jetFinderList.push_back(new StJetFinderRunner(jp, *particleList, *protoJetList));

  _jetCutsList.push_back(new StjeJetCuts(ap, *protoJetList));

  _treeWriter->addJetFinder(fp, particleList, protoJetList, name, stjets);

  _stjetsMap[string(name)] = stjets;

  _backwordCompatibility->addAnalyzer(new StppJetAnalyzer(*protoJetList), _treeWriter, name);
}

void StJetMaker::SetTreeWriter(StjTreeWriter *treeWriter)
{
  _treeWriter = treeWriter;
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

  for(vector<StjeJetCuts*>::iterator jetCuts = _jetCutsList.begin(); jetCuts != _jetCutsList.end(); ++jetCuts) {
    (*jetCuts)->Apply();
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
