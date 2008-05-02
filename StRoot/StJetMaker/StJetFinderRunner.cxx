// $Id: StJetFinderRunner.cxx,v 1.6 2008/05/02 17:07:11 tai Exp $
#include "StJetFinderRunner.h"

#include <StJetFinder/StProtoJet.h>
#include <StJetFinder/StJetFinder.h>
#include <StJetFinder/StJetPars.h>

#include <vector>

using namespace std;

namespace StSpinJet {

StJetFinderRunner::StJetFinderRunner(StJetPars* pars, ParticleList& particleList, ProtoJetList& protoJets)
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
  _protoJetList.clear();
  
  for(vector<const AbstractFourVec*>::const_iterator particle = _particleList.begin(); particle != _particleList.end(); ++particle) {
    _protoJetList.push_back(StProtoJet(*particle));
  }

  _jetFinder->findJets(_protoJetList);
}

}
