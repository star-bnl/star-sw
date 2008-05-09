// $Id: StJetFinderRunner.cxx,v 1.10 2008/05/09 02:14:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
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
