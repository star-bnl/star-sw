// $Id: StJetFinderRunner.cxx,v 1.1 2008/05/01 17:44:50 tai Exp $
#include "StJetFinderRunner.h"


//StJetFinder
#include <StJetFinder/FourVec.h>
#include <StJetFinder/StProtoJet.h>
#include <StJetFinder/StJetFinder.h>
#include <StJetFinder/StJetPars.h>


//StMuDst
#include <StMuDSTMaker/COMMON/StMuTrack.h>

//StJetMaker
#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StFourPMakers/StFourPMaker.h"

#include <ctime>
#include <vector>

using namespace std;

namespace StSpinJet {

StJetFinderRunner::StJetFinderRunner(const StppAnaPars* ap, StJetPars* pars, StFourPMaker* fp, ProtoJetList& protoJets)
  : _jetFinder(pars->constructJetFinder())
  , _protoJetList(protoJets)
  , _fourPMaker(fp)
  , _anaPar(*ap)
{

}

StJetFinderRunner::~StJetFinderRunner()
{

}

void StJetFinderRunner::Init()
{
  _jetFinder->Init();
}

void StJetFinderRunner::findJets()
{
  collectFourMomentum();

  _jetFinder->findJets(_protoJetList);

  applyCutsOnJets();
}


void StJetFinderRunner::collectFourMomentum()
{
  vector<AbstractFourVec*> &particleList = _fourPMaker->getTracks();

  _protoJetList.clear();

  for(vector<AbstractFourVec*>::iterator particle = particleList.begin(); particle  != particleList.end(); ++particle) {

    if(shoudNotPassToJetFinder(*particle)) continue;

    _protoJetList.push_back(StProtoJet(*particle));

  }
}


bool StJetFinderRunner::shoudNotPassToJetFinder(AbstractFourVec* particle)
{
  StMuTrackFourVec* p = dynamic_cast<StMuTrackFourVec*>(particle);

  if (p == 0)
    return true;
  if (p->pt() <= _anaPar.mPtMin)
    return true;
  if (fabs(p->eta()) >= _anaPar.mEtaMax)
    return true;

  if(isChargedTrack(p)) {
    StMuTrack* track = p->particle();
    if (track->flag() <= _anaPar.mFlagMin)
      return true;
    if (track->nHits() <= _anaPar.mNhits)
      return true;
  }


  return false;
}
	
bool StJetFinderRunner::isChargedTrack(StMuTrackFourVec* p)
{
  return p->particle() != 0;
}

void StJetFinderRunner::applyCutsOnJets()
{
  ProtoJetList newList(_protoJetList);

  _protoJetList.clear();

  for (ProtoJetList::iterator jet = newList.begin(); jet != newList.end(); ++jet) {

    if(shouldNotKeep(*jet)) continue;

    _protoJetList.push_back(*jet);

  }
}

bool StJetFinderRunner::shouldNotKeep(StProtoJet &pj)
{
  if (pj.pt() <= _anaPar.mJetPtMin)
    return true;
  if (fabs(pj.eta()) >= _anaPar.mJetEtaMax)
    return true;
  if (fabs(pj.eta()) <= _anaPar.mJetEtaMin)
    return true;
  if ((int)pj.numberOfParticles() < _anaPar.mJetNmin)
    return true;

  return false;
}

}
