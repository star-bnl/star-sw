// $Id: StppJetAnalyzer2.cxx,v 1.5 2008/04/22 00:15:05 tai Exp $
#include "StppJetAnalyzer2.h"


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

ClassImp(StppJetAnalyzer2)
    
StppJetAnalyzer2::StppJetAnalyzer2(const StppAnaPars* ap, StJetPars* pars, StFourPMaker* fp, ProtoJetList& protoJets)
  : _jetFinder(pars->constructJetFinder())
  , _protoJetList(protoJets)
  , _fourPMaker(fp)
  , _anaPar(*ap)
{

}

StppJetAnalyzer2::~StppJetAnalyzer2()
{

}

void StppJetAnalyzer2::Init()
{
  _jetFinder->Init();
}

void StppJetAnalyzer2::findJets()
{
  collectFourMomentum();

  _jetFinder->findJets(_protoJetList);

  applyCuts();
}


void StppJetAnalyzer2::collectFourMomentum()
{
  vector<AbstractFourVec*> &tracks = _fourPMaker->getTracks();

  _protoJetList.clear();

  for (vector<AbstractFourVec*>::iterator i = tracks.begin(); i != tracks.end(); ++i) {
    if (accept4p(dynamic_cast<StMuTrackFourVec*>(*i))) {
      _protoJetList.push_back(StProtoJet(*i));
    }
  }
}


bool StppJetAnalyzer2::accept4p(StMuTrackFourVec* p)
{
  if (p == 0)
    return false;
  if (p->pt() <= _anaPar.mPtMin)
    return false;
  if (fabs(p->eta()) >= _anaPar.mEtaMax)
    return false;

  if(isChargedTrack(p)) {
    StMuTrack* track = p->particle();
    if (track->flag() <= _anaPar.mFlagMin)
      return false;
    if (track->nHits() <= _anaPar.mNhits)
      return false;
  }


  return true;
}
	
bool StppJetAnalyzer2::isChargedTrack(StMuTrackFourVec* p)
{
  return p->particle() != 0;
}

void StppJetAnalyzer2::applyCuts()
{
  ProtoJetList newList;
  for (ProtoJetList::iterator it=_protoJetList.begin(); it!=_protoJetList.end(); ++it)  {
    newList.push_back(*it);
  }
  _protoJetList.clear();

  for (ProtoJetList::iterator it=newList.begin(); it!=newList.end(); ++it) {
    if(acceptJet(*it)) _protoJetList.push_back(*it);
  }
}

bool StppJetAnalyzer2::acceptJet(StProtoJet &pj)
{
  if (pj.pt() <= _anaPar.mJetPtMin)
    return false;
  if (fabs(pj.eta()) >= _anaPar.mJetEtaMax)
    return false;
  if (fabs(pj.eta()) <= _anaPar.mJetEtaMin)
    return false;
  if ((int)pj.numberOfParticles() < _anaPar.mJetNmin)
    return false;

  return true;
}
