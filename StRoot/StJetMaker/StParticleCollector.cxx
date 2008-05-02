// $Id: StParticleCollector.cxx,v 1.4 2008/05/02 16:15:34 tai Exp $
#include "StParticleCollector.h"

#include <StJetFinder/AbstractFourVec.h>
#include <StJetFinder/StProtoJet.h>

#include <StMuDSTMaker/COMMON/StMuTrack.h>

#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StFourPMakers/StFourPMaker.h"

#include <vector>

using namespace std;

namespace StSpinJet {

StParticleCollector::StParticleCollector(const StppAnaPars* ap, StFourPMaker* fp, ProtoJetList& protoJets)
  : _protoJetList(protoJets)
  , _fourPMaker(fp)
  , _anaPar(*ap)
{

}

StParticleCollector::~StParticleCollector()
{

}

void StParticleCollector::Do()
{
  const vector<AbstractFourVec*> &particleList = _fourPMaker->getTracks();

  _protoJetList.clear();

  for(vector<AbstractFourVec*>::const_iterator particle = particleList.begin(); particle  != particleList.end(); ++particle) {

    if(shoudNotPassToJetFinder(*particle)) continue;

    _protoJetList.push_back(StProtoJet(*particle));

  }
}


bool StParticleCollector::shoudNotPassToJetFinder(const AbstractFourVec* particle) const
{
  const StMuTrackFourVec* p = dynamic_cast<const StMuTrackFourVec*>(particle);

  if (p == 0) return true;

  if (p->pt() <= _anaPar.mPtMin) return true;

  if (fabs(p->eta()) >= _anaPar.mEtaMax) return true;

  if(isChargedTrack(p)) {

    StMuTrack* track = p->particle();
    if (track->flag() <= _anaPar.mFlagMin) return true;

    if (track->nHits() <= _anaPar.mNhits)  return true;

  }

  return false;
}
	
bool StParticleCollector::isChargedTrack(const StMuTrackFourVec* p) const
{
  return p->particle() != 0;
}


}

