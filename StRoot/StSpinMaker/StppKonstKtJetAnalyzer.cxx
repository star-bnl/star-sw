//StppKonstKtJetAnalyzer.cxx
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
#include "StJetFinder/FourVec.h"
#include "StJetFinder/StProtoJet.h"
#include "StJetFinder/StJetFinder.h"
#include "StJetFinder/StKonstKtJetFinder.h"

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//StSpinMaker
#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StppEvent.h"
#include "StppJetAnalyzer.h"

#include "StppKonstKtJetAnalyzer.h"

ClassImp(StppKonstKtJetAnalyzer)

StppKonstKtJetAnalyzer::StppKonstKtJetAnalyzer()
{
    cout <<"StppKonstKtJetAnalyzer::StppKonstKtJetAnalyzer()"<<endl;

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

    //kt-cluster jet finder
    StKonstJetFinder::StKonstJetFinderPars pars;  // Pars only apply to Cone Alg.
    mFinder = new StKonstKtJetFinder(pars);
    mFinder->setDebug(mDebug);
    
    mNtuple = 0;
}

StppKonstKtJetAnalyzer::~StppKonstKtJetAnalyzer()
{
    cout <<"StppKonstKtJetAnalyzer::~StppKonstKtJetAnalyzer()"<<endl;
    if (mFile) {
	mFile->Write();
	mFile->Close();
    }
}
