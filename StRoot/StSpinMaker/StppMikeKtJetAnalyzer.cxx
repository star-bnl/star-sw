//StppMikeKtJetAnalyzer.cxx
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
#include "StJetFinder/StKtCluJetFinder.h"
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
#include "StppMikeKtJetAnalyzer.h"

ClassImp(StppMikeKtJetAnalyzer)

StppMikeKtJetAnalyzer::StppMikeKtJetAnalyzer()
{
    cout <<"StppMikeKtJetAnalyzer::StppMikeKtJetAnalyzer()"<<endl;

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
    mFinder = new StKtCluJetFinder();
    mFinder->setR(1.4);
    mFinder->setDebug(mDebug);
    
    mNtuple = 0;
}

StppMikeKtJetAnalyzer::~StppMikeKtJetAnalyzer()
{
    cout <<"StppMikeKtJetAnalyzer::~StppMikeKtJetAnalyzer()"<<endl;
    if (mFile) {
	mFile->Write();
	mFile->Close();
    }
}
