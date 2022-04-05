// $Id: StjeParticleCollector.cxx,v 1.3 2010/04/24 04:15:35 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjeParticleCollector.h"

#include <StJetFinder/AbstractFourVec.h>

#include <emulator/StMuTrackFourVec.h>
#include <StFourPMaker.h>

#include <vector>

using namespace std;

StjeParticleCollector::StjeParticleCollector(const StppAnaPars* ap, StFourPMaker* fp, ParticleList& particleList)
  : _fourPMaker(fp)
  , _particleList(particleList)
  , _anaPar(*ap)
{
}

void StjeParticleCollector::Do(int iVertex)
{
  const vector<AbstractFourVec*> &particleList = _fourPMaker->getVertexNodes()[iVertex].tracks;
  
  _particleList.clear();
  
  for(vector<AbstractFourVec*>::const_iterator particle = particleList.begin(); particle != particleList.end(); ++particle) {
  
    if(shoudNotPassToJetFinder(*particle)) continue;
  
    _particleList.push_back(*particle);
  }
}

size_t StjeParticleCollector::numberOfVertices() const
{
  return  _fourPMaker->getVertexNodes().size();
}

bool StjeParticleCollector::shoudNotPassToJetFinder(const AbstractFourVec* particle) const
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
	
bool StjeParticleCollector::isChargedTrack(const StMuTrackFourVec* p) const
{
  return p->track() != 0;
}
