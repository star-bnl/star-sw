// $Id: StJetCuts.cxx,v 1.1 2008/05/01 21:55:13 tai Exp $
#include "StJetCuts.h"

#include <StJetFinder/StProtoJet.h>
#include <StJetFinder/StJetFinder.h>
#include <StJetFinder/StJetPars.h>

#include <algorithm>
#include <iterator>

using namespace std;

namespace StSpinJet {

StJetCuts::StJetCuts(const StppAnaPars* ap, StJetPars* pars, ProtoJetList& protoJets)
  : _jetFinder(pars->constructJetFinder())
  , _protoJetList(protoJets)
  , _anaPar(*ap)
{

}

StJetCuts::~StJetCuts()
{

}

void StJetCuts::Init()
{
  _jetFinder->Init();
}

void StJetCuts::Run()
{
  _jetFinder->findJets(_protoJetList);

  applyCutsOnJets();
}

void StJetCuts::applyCutsOnJets()
{
  ProtoJetList newList;

  for (ProtoJetList::iterator jet = _protoJetList.begin(); jet != _protoJetList.end(); ++jet) {

    if(shouldNotKeep(*jet)) continue;

    newList.push_back(*jet);

  }

  _protoJetList.clear();

  copy(newList.begin(), newList.end(), back_inserter(_protoJetList));
}

bool StJetCuts::shouldNotKeep(StProtoJet &pj)
{
  if(pj.pt() <= _anaPar.mJetPtMin) return true;

  if(fabs(pj.eta()) >= _anaPar.mJetEtaMax) return true;

  if(fabs(pj.eta()) <= _anaPar.mJetEtaMin) return true;

  if((int)pj.numberOfParticles() < _anaPar.mJetNmin)  return true;

  return false;
}

}
