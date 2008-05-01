// $Id: StParticleCollector.cxx,v 1.2 2008/05/01 21:28:39 tai Exp $
#include "StParticleCollector.h"

#include <StJetFinder/AbstractFourVec.h>
#include <StJetFinder/StProtoJet.h>


#include <StMuDSTMaker/COMMON/StMuTrack.h>

#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StFourPMakers/StFourPMaker.h"

#include <ctime>
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
  vector<AbstractFourVec*> &particleList = _fourPMaker->getTracks();

  _protoJetList.clear();

  for(vector<AbstractFourVec*>::iterator particle = particleList.begin(); particle  != particleList.end(); ++particle) {

    if(shoudNotPassToJetFinder(*particle)) continue;

    _protoJetList.push_back(StProtoJet(*particle));

  }
}


bool StParticleCollector::shoudNotPassToJetFinder(AbstractFourVec* particle)
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
	
bool StParticleCollector::isChargedTrack(StMuTrackFourVec* p)
{
  return p->particle() != 0;
}


}

