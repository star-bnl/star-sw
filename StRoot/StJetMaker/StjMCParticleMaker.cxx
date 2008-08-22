// $Id: StjMCParticleMaker.cxx,v 1.4 2008/08/22 17:32:52 tai Exp $
#include "StjMCParticleMaker.h"


#include "StjMCParticleCut.h"
#include "StjMCParticleCutEta.h"
#include "StjMCParticleCutEtaForStatus.h"
#include "StjMCParticleCutStatus.h"

#include "StjMCParticleListWriter.h"

#include "StjMCMuDst.h"

#include "StjMCParticleListCut.h"

#include <TDirectory.h>

#include <iostream>

using namespace std;

ClassImp(StjMCParticleMaker)
  

StjMCParticleMaker::StjMCParticleMaker(const Char_t *name, TDirectory* file, StMaker* uDstMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
{ }

Int_t StjMCParticleMaker::Init()
{
  _mc = new StjMCMuDst(_uDstMaker);

  _mcCut = new StjMCParticleListCut();

  int goodStatus[] = {1, 3};
  _mcCut->addCut(new StjMCParticleCutStatus(2, goodStatus));
  _mcCut->addCut(new StjMCParticleCutEtaForStatus(-2.0, 2.0, 1));

  _writer = new StjMCParticleListWriter("mcParticles", _file);

  return kStOk;
}

Int_t StjMCParticleMaker::Make()
{
  StjMCParticleList theList = _mc->getMCParticleList();

  theList = (*_mcCut)(theList);

  _writer->Fill(theList);

  return kStOk;

}

Int_t StjMCParticleMaker::Finish()
{
  _writer->Finish();

  return kStOk;
}
