//StppJetAnalyzer.cxx
//M.L. Miller (Yale Software)
//07/02
//Modified by Thomas Henry
//08/02 StppJetAnalyzer converted to an interface class

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
#include "StppuDstMaker.h"
#include "StppJetAnalyzer.h"

ClassImp(StppJetAnalyzer)

StppJetAnalyzer::StppJetAnalyzer()
{
    cout <<"StppJetAnalyzer::StppJetAnalyzer()"<<endl;

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

    mNtuple = 0;

    // mFinder get set by the children of this class

    muDstJets = new StJets();
    muDstJets->Clear();
}

void StppJetAnalyzer::setDebug(bool v)
{
    mDebug =v;
    mFinder->setDebug(mDebug);
}

void StppJetAnalyzer::openFile(const char* ifile)
{
    cout <<"StppJetAnalyzer::openFile()"<<endl;

    if (!ifile) {
	cout <<"Error, ifile==0"<<endl;
	abort();
    }

    mFile = new TFile(ifile,"RECREATE");
    cout <<"opened file:\t"<<mFile->GetName()<<endl;
    bookHists();
    bookNtuple();
}

StppJetAnalyzer::~StppJetAnalyzer()
{
    cout <<"StppJetAnalyzer::~StppJetAnalyzer()"<<endl;
    if (mFile) {
	mFile->Write();
	mFile->Close();
    }
    delete muDstJets;
}

void StppJetAnalyzer::bookNtuple()
{
    mNtuple = new TNtuple("jetNtuple","The Jet Daughter Ntuple",
			  //we add particles w/ a ref to the properties of the jet they belong to
			  "pt:pz:et:eta:phi:mass:charge:pdg_id:lp:lcp:tpcLcp:dTpc:jetEt:jetEta:jetPhi:jetNcell:jetCharge:jetType");
}

void StppJetAnalyzer::fillNtuple(StppEvent* e)
{
    mEvent = e;
}

void StppJetAnalyzer::bookHists()
{
   // mPythiaEt = new TH1D("pythiaEt","Et of 2-2 outgoing Pythia partons", 40, 0., 60.);
   mEt = new TH1D("coneEt","Et of grid/cone jets", 40, 0., 60.);
   // mClusterEt = new TH1D("clusterEt","Et of kt-cluster jets", 40, 0., 60.);
}

void StppJetAnalyzer::setR(double v)
{
    mFinder->setR(v);
}

void StppJetAnalyzer::setSeedEtMin(double v)
{
    mFinder->setSeedEtMin(v);
}

void StppJetAnalyzer::setAssocEtMin(double v)
{
    mFinder->setAssocEtMin(v);
}

void StppJetAnalyzer::clear()
{
    //clearAndDestroy four list
    for (FourList::iterator it=mFourList.begin(); it!=mFourList.end(); ++it) {
	AbstractFourVec* temp = *it;
        if(temp != NULL)
	  delete temp;
	temp=0;
    }
    mFourList.clear();
    
    //clear protoJets
    mProtoJets.clear();
}

void StppJetAnalyzer::print()
{
    cout <<"\t---   Contents of mProtoJets ---"<<endl;
    for (JetList::const_iterator it2=mProtoJets.begin(); it2!=mProtoJets.end(); ++it2) {
	cout <<*it2<<endl;
    }
}

bool StppJetAnalyzer::accept(StMuTrack* p)
{
    return (p
	    && p->flag()>0
	    && p->pt()>mPtMin
	    && fabs(p->eta())<mEtaMax
	    && p->nHits()>mNhits
	    );
}

bool StppJetAnalyzer::accept(StMuTrackFourVec* p)
{
    if(p->particle())
      return(accept(p->particle()));
    return (p
	    && p->pt()>mPtMin
	    && fabs(p->eta())<mEtaMax
	    );
}

bool StppJetAnalyzer::accept(const StProtoJet& pj)
{
    return (
	    pj.pt()>mPtMin
	    && fabs(pj.eta())<mEtaMax
	    );
}

void StppJetAnalyzer::acceptJets(void)
{
    JetList newList;
    for (JetList::iterator it=mProtoJets.begin(); it!=mProtoJets.end(); ++it)
    {
      newList.push_back(*it);
    }
    mProtoJets.clear();
    //loop on jets
    for (JetList::iterator it=newList.begin(); it!=newList.end(); ++it) 
    {
	if(acceptJet(*it)) 
	    mProtoJets.push_back(*it);
    }
}

bool StppJetAnalyzer::acceptJet(StProtoJet &pj)
{
    return (
	    (pj.pt() > mJetPtMin)
	    && (fabs(pj.eta()) < mJetEtaMax)
	    && (fabs(pj.eta()) > mJetEtaMin)
            && (pj.numberOfParticles() >= mJetNmin)
	    );
}

void StppJetAnalyzer::fillLists()
{
    TClonesArray* tempArray = mEvent->tracks;
    TClonesArray& particles = *(tempArray);
    int last = particles.GetLast();
    int nFinal = 0;
    for (int i=0; i<=last; ++i) {
	StMuTrack* p = static_cast<StMuTrack*>(particles[i]);
	if (accept(p)) {
	    StMuTrackFourVec* temp = new StMuTrackFourVec(p, i);
	    mFourList.push_back( temp) ; //for ownership
	    StProtoJet tempPj(temp);
	    if (accept(tempPj)) { //check to make sure that nothing is wrong w/ protojet, as well!!!
		mProtoJets.push_back( StProtoJet(temp) ); //for jet finding
		++nFinal;
	    }
	}
    }
}

void StppJetAnalyzer::fillLists(StMuTrackFourVec* tracks, int numTracks)
{
    for (int i=0; i < numTracks; i++) {
	if (accept(&tracks[i])) {
	    mFourList.push_back(&tracks[i]) ; //for ownership
	    StProtoJet tempPj(&tracks[i]);
	    if (accept(tempPj)) { //check to make sure that nothing is wrong w/ protojet, as well!!!
		mProtoJets.push_back( tempPj ); //for jet finding
	    }
	}
    }
}

void StppJetAnalyzer::setEvent(StppEvent* e)
{
    clear();
    mEvent=e;
    fillLists();
}

void StppJetAnalyzer::setFourVec(StMuTrackFourVec* tracks, int numTracks)
{
    mFourList.clear();
    mProtoJets.clear();

    fillLists(tracks, numTracks);
}

void StppJetAnalyzer::findJets()
{
    clock_t start = clock();
    mFinder->findJets(mProtoJets);
    clock_t stop = clock();
    double time = (double)(stop-start)/(double)(CLOCKS_PER_SEC);
    if (mFinder->debug()) {
	cout <<"\ttime to find jets:\t"<<time<<endl;
    }
    acceptJets();
}

void StppJetAnalyzer::fillHists()
{
    if (!mFile) {return;}
    
    //loop on jets
    for (JetList::iterator it=mProtoJets.begin(); it!=mProtoJets.end(); ++it) {
	StProtoJet& pj = *it;
	if (accept(pj) ) {
	    mEt->Fill( pj.eT() );
	}
    }
}

void StppJetAnalyzer::addBranch(const char *name, void *stppudst) 
{
  TTree* ppuDst = (TTree *) stppudst;
  ppuDst->Branch (name, "StJets", &muDstJets, 64000, 99);   
}
