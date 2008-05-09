// $Id: StJetFinderRunner.cxx,v 1.11 2008/05/09 02:17:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
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
