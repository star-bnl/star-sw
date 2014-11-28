// $Id: StjeJetCuts.cxx,v 1.2 2008/08/03 00:26:51 tai Exp $
#include "StjeJetCuts.h"

#include <StJetFinder/StProtoJet.h>
#include <StJetFinder/StJetPars.h>

#include <algorithm>
#include <iterator>

using namespace std;

StjeJetCuts::StjeJetCuts(const StppAnaPars* ap, ProtoJetList& protoJets)
  : _protoJetList(protoJets)
  , _anaPar(*ap)
{

}

StjeJetCuts::~StjeJetCuts()
{

}

void StjeJetCuts::Apply()
{
  ProtoJetList newList;

  for (ProtoJetList::iterator jet = _protoJetList.begin(); jet != _protoJetList.end(); ++jet) {

    if(shouldNotKeep(*jet)) continue;

    newList.push_back(*jet);

  }

  _protoJetList.clear();

  copy(newList.begin(), newList.end(), back_inserter(_protoJetList));
}

bool StjeJetCuts::shouldNotKeep(StProtoJet &pj)
{
  if(pj.pt() <= _anaPar.mJetPtMin) return true;

  if(fabs(pj.eta()) >= _anaPar.mJetEtaMax) return true;

  if(fabs(pj.eta()) <= _anaPar.mJetEtaMin) return true;

  if((int)pj.numberOfParticles() < _anaPar.mJetNmin)  return true;

  return false;
}
