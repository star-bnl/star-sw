//StppMikeConeJetAnalyzer.cxx
//Adapted from M.L. Miller (Yale Software)
//07/02
//Thomas Henry
//08/02

//std
#include <list>
#include <time.h>
#include <algorithm>
#include <iostream>
#include <math.h>
using namespace std;

//ROOT
#include "TFile.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TH2.h"

//StJetFinder
#include "StJetFinder/StConeJetFinder.h"

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//StSpinMaker
#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StppEvent.h"
#include "StppJetAnalyzer.h"
#include "StppMikeConeJetAnalyzer.h"

ClassImp(StppMikeConeJetAnalyzer)

StppMikeConeJetAnalyzer::StppMikeConeJetAnalyzer(int nEta, double etaMin,
    double etaMax, int nPhi, double phiMin, double phiMax)
{
    cout <<"StppMikeConeJetAnalyzer::StppMikeConeJetAnalyzer()"<<endl;

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
    mFinder = new StConeJetFinder(conePars);
    mFinder->setR(1.0);
    mFinder->setDebug(mDebug);
    mFinder->setAssocEtMin(.01);
    mFinder->setSeedEtMin(1.0);

    mNtuple = 0;
}

StppMikeConeJetAnalyzer::~StppMikeConeJetAnalyzer()
{
    cout <<"StppMikeConeJetAnalyzer::~StppMikeConeJetAnalyzer()"<<endl;
    if (mFile) {
	mFile->Write();
	mFile->Close();
    }
}

void StppMikeConeJetAnalyzer::setPerformMinimization(bool v) 
{
  ((StConeJetFinder*) mFinder)->setPerformMinimization(v);
}

bool StppMikeConeJetAnalyzer::performMinimization(void) 
{
  return ((StConeJetFinder*) mFinder)->performMinimization();
}
        
void StppMikeConeJetAnalyzer::setAddMidpoints(bool v) 
{
  ((StConeJetFinder*) mFinder)->setAddMidpoints(v);
}

bool StppMikeConeJetAnalyzer::addMidpoints(void) 
{
  return ((StConeJetFinder*) mFinder)->addMidpoints();
}
        
void StppMikeConeJetAnalyzer::setDoSplitMerge(bool v) 
{
 ((StConeJetFinder*) mFinder)->setDoSplitMerge(v);
}

bool StppMikeConeJetAnalyzer::doSplitMerge() 
{
  return ((StConeJetFinder*) mFinder)->doSplitMerge();
}

void StppMikeConeJetAnalyzer::setSplitFraction(double v) 
{ 
  ((StConeJetFinder*) mFinder)->setSplitFraction(v); 
}

double StppMikeConeJetAnalyzer::splitFraction(void) 
{ 
  return ((StConeJetFinder*) mFinder)->splitFraction(); 
}

