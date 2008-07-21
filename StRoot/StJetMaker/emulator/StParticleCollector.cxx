// $Id: StParticleCollector.cxx,v 1.1 2008/07/21 02:00:26 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StParticleCollector.h"

#include <StJetFinder/AbstractFourVec.h>

#include "StMuTrackFourVec.h"
#include "StFourPMakers/StFourPMaker.h"

#include <vector>

using namespace std;

namespace StSpinJet {

StParticleCollector::StParticleCollector(const StppAnaPars* ap, StFourPMaker* fp, ParticleList& particleList)
  : _fourPMaker(fp)
  , _particleList(particleList)
  , _anaPar(*ap)
{

}

StParticleCollector::~StParticleCollector()
{

}

void StParticleCollector::Do()
{
  const vector<AbstractFourVec*> &particleList = _fourPMaker->getTracks();
  
  _particleList.clear();
  
  for(vector<AbstractFourVec*>::const_iterator particle = particleList.begin(); particle != particleList.end(); ++particle) {
  
    if(shoudNotPassToJetFinder(*particle)) continue;
  
    _particleList.push_back(*particle);
  }

}


bool StParticleCollector::shoudNotPassToJetFinder(const AbstractFourVec* particle) const
{
  const StMuTrackFourVec* p = dynamic_cast<const StMuTrackFourVec*>(particle);

  if (p == 0) return true;

  if (p->pt() <= _anaPar.mPtMin) return true;

  if (fabs(p->eta()) >= _anaPar.mEtaMax) return true;

  if(isChargedTrack(p)) {

    StMuTrackEmu* track = p->track();
    if (track->flag() <= _anaPar.mFlagMin) return true;

    if (track->nHits() <= _anaPar.mNhits)  return true;
  }

  return false;
}
	
bool StParticleCollector::isChargedTrack(const StMuTrackFourVec* p) const
{
  return p->track() != 0;
}


}

