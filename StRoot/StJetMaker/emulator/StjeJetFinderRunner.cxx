// $Id: StjeJetFinderRunner.cxx,v 1.2 2008/08/03 00:26:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjeJetFinderRunner.h"

#include <StJetFinder/StJetFinder.h>
#include <StJetFinder/StJetPars.h>

StjeJetFinderRunner::StjeJetFinderRunner(StJetPars* pars, const ParticleList& particleList, ProtoJetList& protoJets)
  : _jetFinder(pars->constructJetFinder())
  , _particleList(particleList)
  , _protoJetList(protoJets)
{

}

StjeJetFinderRunner::~StjeJetFinderRunner()
{

}

void StjeJetFinderRunner::Init()
{
  _jetFinder->Init();
}

void StjeJetFinderRunner::Run()
{
  _jetFinder->findJets(_protoJetList, _particleList);
}
