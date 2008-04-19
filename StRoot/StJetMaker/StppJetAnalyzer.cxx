// $Id: StppJetAnalyzer.cxx,v 1.12 2008/04/19 00:58:40 tai Exp $
//
// Author List: M.L. Miller
//              Thomas Henry
//              Tai Sakuma

#include "StppJetAnalyzer.h"

#include "StMessMgr.h"
#include "Stiostream.h"

//StJetFinder
#include "StJetFinder/FourVec.h"
#include "StJetFinder/StProtoJet.h"
#include "StJetFinder/StJetFinder.h"
#include "StJetFinder/StKtCluJetFinder.h"
#include "StJetFinder/StConeJetFinder.h"
#include "StJetFinder/StCdfChargedConeJetFinder.h"


//StMuDst
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//StJetMaker
#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StFourPMakers/StFourPMaker.h"

#include <ctime>
#include <vector>

using namespace std;

ClassImp(StppJetAnalyzer)
ClassImp(StppAnaPars)
    
    
StppJetAnalyzer::StppJetAnalyzer(const StppAnaPars* ap, const StJetPars* pars, StFourPMaker* fp)
  : mFinder(0)
  , mProtoJets(0)
  , mFourPMaker(fp)
  , mPars(*ap)
  , muDstJets(0)
{
  cout <<"StppJetAnalyzer::StppJetAnalyzer()"<<endl;
    
    //Set the finder!
    
  if (dynamic_cast<const StKtCluPars*>(pars)) {
    const StKtCluPars* temp = dynamic_cast<const StKtCluPars*>(pars);
    cout <<"StppJetAnalyzer, instantiate StKtCluJetFinder"<<endl;
    StKtCluJetFinder* jf = new StKtCluJetFinder(*temp);
    mFinder = jf;
  }
  else if (dynamic_cast<const StCdfChargedConePars*>(pars)) { //note, this if MUST come before the StConePars test
    cout <<"StppJetAnalyzer, instantiate StCdfChargedConeJetFinder"<<endl;
    const StCdfChargedConePars* temp = dynamic_cast<const StCdfChargedConePars*>(pars);
    StCdfChargedConeJetFinder* jf = new StCdfChargedConeJetFinder(*temp);
    mFinder = jf;
    jf->print();
  }
  else if (dynamic_cast<const StConePars*>(pars)) {	
    cout <<"StppJetAnalyzer, instantiate StConeJetFinder"<<endl;
    const StConePars* temp = dynamic_cast<const StConePars*>(pars);
    StConeJetFinder* jf = new StConeJetFinder(*temp);
    mFinder = jf;
    jf->print();
  }
  else {
    cout <<"StppJetAnalyzer, unkown algorithm"<<endl;
    abort();
  }

}

StppJetAnalyzer::~StppJetAnalyzer()
{

}

void StppJetAnalyzer::findJets()
{
  fillLists();

  clock_t start = clock();
  mFinder->findJets(mProtoJets);
  clock_t stop = clock();

  double time = (double)(stop-start)/(double)(CLOCKS_PER_SEC);
  LOG_DEBUG << "\ttime to find jets:\t" << time << endm;

  acceptJets();
}


void StppJetAnalyzer::fillLists()
{
  vector<AbstractFourVec*> &tracks = mFourPMaker->getTracks();

  mProtoJets.clear();

  for (vector<AbstractFourVec*>::iterator i = tracks.begin(); i != tracks.end(); ++i) {
    if (accept4p(dynamic_cast<StMuTrackFourVec*>(*i))) {
      mProtoJets.push_back(StProtoJet(*i));
    }
  }
}


bool StppJetAnalyzer::accept4p(StMuTrackFourVec* p)
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
	
bool StppJetAnalyzer::isChargedTrack(StMuTrackFourVec* p)
{
  return p->particle() != 0;
}

void StppJetAnalyzer::acceptJets()
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

bool StppJetAnalyzer::acceptJet(StProtoJet &pj)
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

void StppJetAnalyzer::clear()
{
  mProtoJets.clear();
}

void StppJetAnalyzer::print()
{
  cout <<"\t---   Contents of mProtoJets ---"<<endl;
  for (JetList::const_iterator it2=mProtoJets.begin(); it2!=mProtoJets.end(); ++it2) {
    cout <<*it2<<endl;
  }
}

