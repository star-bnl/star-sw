// $Id: StJetMCParticleMaker.cxx,v 1.1 2008/07/25 01:05:41 tai Exp $
#include "StJetMCParticleMaker.h"


#include "MCParticleCut.h"
#include "MCParticleCutEta.h"
#include "MCParticleCutEtaForStatus.h"
#include "MCParticleCutStatus.h"

#include "StJetMCParticleListWriter.h"

#include "StJetMCMuDst.h"

#include "StJetMCParticleListCut.h"

#include <TDirectory.h>

#include <iostream>

using namespace std;
using namespace StSpinJet;
using namespace StJetMCParticleCut;

ClassImp(StJetMCParticleMaker)
  

StJetMCParticleMaker::StJetMCParticleMaker(const Char_t *name, TDirectory* file, StMaker* uDstMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
{ }

Int_t StJetMCParticleMaker::Init()
{
  _mc = new StJetMCMuDst(_uDstMaker);

  _mcCut = new StJetMCParticleListCut();

  int goodStatus[] = {1, 3};
  _mcCut->addCut(new MCParticleCutStatus(2, goodStatus));
  _mcCut->addCut(new MCParticleCutEtaForStatus(-2.0, 2.0, 1));

  _writer = new StJetMCParticleListWriter("mcParticles", _file);

  return kStOk;
}

Int_t StJetMCParticleMaker::Make()
{
  MCParticleList theList = _mc->getMCPartilceList();

  theList = (*_mcCut)(theList);

  _writer->Fill(theList);

  return kStOk;

}

Int_t StJetMCParticleMaker::Finish()
{
  _writer->Finish();

  return kStOk;
}
