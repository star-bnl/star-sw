// $Id: StjeJetFinderRunner.cxx,v 1.1 2008/08/02 23:10:19 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjeJetFinderRunner.h"

#include <StJetFinder/StJetFinder.h>
#include <StJetFinder/StJetPars.h>

namespace StSpinJet {

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

}
