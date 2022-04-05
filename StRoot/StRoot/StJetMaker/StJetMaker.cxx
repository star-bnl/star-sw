// $Id: StJetMaker.cxx,v 1.75 2010/06/05 03:13:13 pibero Exp $

// ROOT
#include "TTree.h"

// STAR
#include "StJetFinder/StProtoJet.h"
#include "StSpinPool/StJets/StJets.h"
#include "StSpinPool/StJetEvent/StJetEvent.h"

// Local
#include "StjeParticleCollector.h"
#include "StjeJetFinderRunner.h"
#include "StjeJetCuts.h"
#include "StjeTreeWriter.h"
#include "StjeDefaultJetTreeWriter.h"
#include "StppJetAnalyzer.h"
#include "StJetMaker.h"

ClassImp(StJetMaker);

StJetMaker::StJetMaker(const char* name, StMuDstMaker* uDstMaker, const char* outputName) 
  : StMaker(name)
  , _defaultTreeWriter(new StjeDefaultJetTreeWriter(*uDstMaker, outputName))
  , _treeWriter(_defaultTreeWriter)
{
}

StJetMaker::~StJetMaker()
{
  delete _defaultTreeWriter;
}

StJetMaker::StJetBranch::StJetBranch(const StppAnaPars* anapars, StJetPars* jetpars, StFourPMaker* fourPMaker, const char* name)
  : name(name)
  , particleCollector(new StjeParticleCollector(anapars,fourPMaker,this->particles))
  , jetFinder(new StjeJetFinderRunner(jetpars,this->particles,this->protojets))
  , jetCuts(new StjeJetCuts(anapars,this->protojets))
{
}

void StJetMaker::addAnalyzer(const StppAnaPars* anapars, StJetPars* jetpars, StFourPMaker* fourPMaker, const char* name)
{
  StJetBranch* jetBranch = new StJetBranch(anapars,jetpars,fourPMaker,name);
  mJetBranches.push_back(jetBranch);
  _treeWriter->addJetFinder(fourPMaker,&jetBranch->particles,&jetBranch->protojets,name,new StJets);
}

void StJetMaker::SetTreeWriter(StjeTreeWriter* treeWriter)
{
  _treeWriter = treeWriter;
}

Int_t StJetMaker::Init() 
{
  for (size_t iBranch = 0; iBranch < mJetBranches.size(); ++iBranch)
    mJetBranches[iBranch]->jetFinder->Init();

  _treeWriter->Init();

  return kStOk;
}

Int_t StJetMaker::Make()
{
  for (size_t iBranch = 0; iBranch < mJetBranches.size(); ++iBranch) {
    StJetBranch* jetBranch = mJetBranches[iBranch];
    _treeWriter->fillJetTreeHeader(iBranch);
    for (size_t iVertex = 0; iVertex < jetBranch->particleCollector->numberOfVertices(); ++iVertex) {
      jetBranch->particleCollector->Do(iVertex);
      jetBranch->jetFinder->Run();
      jetBranch->jetCuts->Apply();
      _treeWriter->fillJetTree(iBranch,iVertex);
    }
  }

  _treeWriter->jetTree()->Fill();

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

StJets* StJetMaker::getStJets(const char* branchName) const
{
  TTree* jetTree = _treeWriter->jetTree();
  if (jetTree) {
    TBranch* branch = jetTree->GetBranch(branchName);
    if (branch) return *(StJets**)branch->GetAddress();
  }
  return 0;
}

StJetEvent* StJetMaker::getStJetEvent(const char* branchName) const
{
  TTree* jetTree = _treeWriter->jetTree();
  if (jetTree) {
    TBranch* branch = jetTree->GetBranch(branchName);
    if (branch) return *(StJetEvent**)branch->GetAddress();
  }
  return 0;
}
