//StppCdfChargedConeJetAnalyzer.cxx
//M.L. Miller (Yale Software)
//12/02

//std
#include <list>
#include <time.h>
#include <algorithm>
#include "Stiostream.h"
#include <math.h>
using namespace std;

//ROOT
#include "TFile.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TH2.h"

//StJetFinder
#include "StJetFinder/StCdfChargedConeJetFinder.h"

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//StSpinMaker
#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StppEvent.h"
#include "StppJetAnalyzer.h"
#include "StppCdfChargedConeJetAnalyzer.h"

ClassImp(StppCdfChargedConeJetAnalyzer)

StppCdfChargedConeJetAnalyzer::StppCdfChargedConeJetAnalyzer(int nEta, double etaMin,
    double etaMax, int nPhi, double phiMin, double phiMax)
{
    cout <<"StppCdfChargedConeJetAnalyzer::StppCdfChargedConeJetAnalyzer()"<<endl;

    mDebug = false;
    
    mEmcAccepted=false;
    mTpcAccepted=false;
    mFpdAccepted=false;
    mPtMin=0.1;
    mEtaMax=1.4;
    mNhits=10;
    
    mJetPtMin=0.0;
    mJetEtaMax=100.;
    mJetNmin=0;

    //Mike's Cone Jet Finder
    StConeJetFinder::StConeJetFinderPars conePars;
    conePars.mEtaMax = etaMax;
    conePars.mEtaMin = etaMin;
    conePars.mPhiMax = phiMax;
    conePars.mPhiMin = phiMin;
    conePars.mNeta = nEta;
    conePars.mNphi = nPhi;
    mFinder = new StCdfChargedConeJetFinder(conePars);
    mFinder->setR(1.0);
    mFinder->setDebug(mDebug);
    mFinder->setAssocEtMin(.01);
    mFinder->setSeedEtMin(1.0);

    mNtuple = 0;
}

StppCdfChargedConeJetAnalyzer::~StppCdfChargedConeJetAnalyzer()
{
    cout <<"StppCdfChargedConeJetAnalyzer::~StppCdfChargedConeJetAnalyzer()"<<endl;
    if (mFile) {
	mFile->Write();
	mFile->Close();
    }
}

void StppCdfChargedConeJetAnalyzer::setPerformMinimization(bool v) 
{
  ((StCdfChargedConeJetFinder*) mFinder)->setPerformMinimization(v);
}

bool StppCdfChargedConeJetAnalyzer::performMinimization(void) 
{
  return ((StCdfChargedConeJetFinder*) mFinder)->performMinimization();
}
        
void StppCdfChargedConeJetAnalyzer::setAddMidpoints(bool v) 
{
  ((StCdfChargedConeJetFinder*) mFinder)->setAddMidpoints(v);
}

bool StppCdfChargedConeJetAnalyzer::addMidpoints(void) 
{
  return ((StCdfChargedConeJetFinder*) mFinder)->addMidpoints();
}
        
void StppCdfChargedConeJetAnalyzer::setDoSplitMerge(bool v) 
{
 ((StCdfChargedConeJetFinder*) mFinder)->setDoSplitMerge(v);
}

bool StppCdfChargedConeJetAnalyzer::doSplitMerge() 
{
  return ((StCdfChargedConeJetFinder*) mFinder)->doSplitMerge();
}

void StppCdfChargedConeJetAnalyzer::setSplitFraction(double v) 
{ 
  ((StCdfChargedConeJetFinder*) mFinder)->setSplitFraction(v); 
}

double StppCdfChargedConeJetAnalyzer::splitFraction(void) 
{ 
  return ((StCdfChargedConeJetFinder*) mFinder)->splitFraction(); 
}

void StppCdfChargedConeJetAnalyzer::setDoMidpointFix(bool v)
{
    return ((StCdfChargedConeJetFinder*) mFinder)->setDoMidpointFix(v);
}

bool StppCdfChargedConeJetAnalyzer::doMidpointFix() const
{
    return ((StCdfChargedConeJetFinder*) mFinder)->doMidpointFix();
}

void StppCdfChargedConeJetAnalyzer::setRequireStableMidpoints(bool v)
{
    return ((StCdfChargedConeJetFinder*) mFinder)->setRequireStableMidpoints(v);
}


bool StppCdfChargedConeJetAnalyzer::requiredStableMidpoints() const
{
    return ((StCdfChargedConeJetFinder*) mFinder)->requiredStableMidpoints();
}
