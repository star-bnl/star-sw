// $Id: StppJetAnalyzer2.cxx,v 1.3 2008/04/21 21:20:26 tai Exp $
#include "StppJetAnalyzer2.h"


//StJetFinder
#include "StJetFinder/FourVec.h"
#include "StJetFinder/StProtoJet.h"
#include "StJetFinder/StJetFinder.h"


//StMuDst
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//StJetMaker
#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StFourPMakers/StFourPMaker.h"

#include <ctime>
#include <vector>

using namespace std;

ClassImp(StppJetAnalyzer2)
    
StppJetAnalyzer2::StppJetAnalyzer2(const StppAnaPars* ap, StJetPars* pars, StFourPMaker* fp, JetList& protoJets)
  : mFinder(pars->constructJetFinder())
  , mProtoJets(protoJets)
  , mFourPMaker(fp)
  , mPars(*ap)
{

}

StppJetAnalyzer2::~StppJetAnalyzer2()
{

}

void StppJetAnalyzer2::findJets()
{
  fillLists();

  clock_t start = clock();
  mFinder->findJets(mProtoJets);
  clock_t stop = clock();

  double time = (double)(stop-start)/(double)(CLOCKS_PER_SEC);
  LOG_DEBUG << "\ttime to find jets:\t" << time << endm;

  acceptJets();
}


void StppJetAnalyzer2::fillLists()
{
  vector<AbstractFourVec*> &tracks = mFourPMaker->getTracks();

  mProtoJets.clear();

  for (vector<AbstractFourVec*>::iterator i = tracks.begin(); i != tracks.end(); ++i) {
    if (accept4p(dynamic_cast<StMuTrackFourVec*>(*i))) {
      mProtoJets.push_back(StProtoJet(*i));
    }
  }
}


bool StppJetAnalyzer2::accept4p(StMuTrackFourVec* p)
{
  if (p == 0)
    return false;
  if (p->pt() <= mPars.mPtMin)
    return false;
  if (fabs(p->eta()) >= mPars.mEtaMax)
    return false;

  if(isChargedTrack(p)) {
    StMuTrack* track = p->particle();
    if (track->flag() <= mPars.mFlagMin)
      return false;
    if (track->nHits() <= mPars.mNhits)
      return false;
  }


  return true;
}
	
bool StppJetAnalyzer2::isChargedTrack(StMuTrackFourVec* p)
{
  return p->particle() != 0;
}

void StppJetAnalyzer2::acceptJets()
{
  JetList newList;
  for (JetList::iterator it=mProtoJets.begin(); it!=mProtoJets.end(); ++it)  {
    newList.push_back(*it);
  }
  mProtoJets.clear();

  for (JetList::iterator it=newList.begin(); it!=newList.end(); ++it) {
    if(acceptJet(*it)) mProtoJets.push_back(*it);
  }
}

bool StppJetAnalyzer2::acceptJet(StProtoJet &pj)
{
  if (pj.pt() <= mPars.mJetPtMin)
    return false;
  if (fabs(pj.eta()) >= mPars.mJetEtaMax)
    return false;
  if (fabs(pj.eta()) <= mPars.mJetEtaMin)
    return false;
  if ((int)pj.numberOfParticles() < mPars.mJetNmin)
    return false;

  return true;
}

void StppJetAnalyzer2::print()
{
  cout <<"\t---   Contents of mProtoJets ---"<<endl;
  for (JetList::const_iterator it2=mProtoJets.begin(); it2!=mProtoJets.end(); ++it2) {
    cout <<*it2<<endl;
  }
}

