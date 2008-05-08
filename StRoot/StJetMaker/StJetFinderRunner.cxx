// $Id: StJetFinderRunner.cxx,v 1.9 2008/05/08 02:31:46 tai Exp $
#include "StJetFinderRunner.h"

#include <StJetFinder/StJetFinder.h>
#include <StJetFinder/StJetPars.h>

namespace StSpinJet {

StJetFinderRunner::StJetFinderRunner(StJetPars* pars, const ParticleList& particleList, ProtoJetList& protoJets)
  : _jetFinder(pars->constructJetFinder())
  , _particleList(particleList)
  , _protoJetList(protoJets)
{

}

StJetFinderRunner::~StJetFinderRunner()
{

}

void StJetFinderRunner::Init()
{
  _jetFinder->Init();
}

void StJetFinderRunner::Run()
{
  _jetFinder->findJets(_protoJetList, _particleList);
}

}
