/***************************************************************************
*
* $Id: StChargedPionMaker.cxx,v 1.10 2008/01/08 17:33:15 kocolosk Exp $
*
* Author:  Adam Kocoloski
***************************************************************************
*
* Description:  Collects charged pions from muDst.  Intent is to use 
* StJetSkimMaker in the same chain to get all spin-related event info
*
***************************************************************************
*
* $Log: StChargedPionMaker.cxx,v $
* Revision 1.10  2008/01/08 17:33:15  kocolosk
* StChargedPionMaker fills a full StChargedPionEvent on its own now
* Added trigger prescales and extra simulator info to Event
* Added detectorEta() definition to Jet
* Removed unused Header class
*
* Revision 1.9  2007/12/31 19:53:04  kocolosk
* new tree structure separate from StJetSkimEvent
*
* Revision 1.8  2007/06/06 13:01:31  kocolosk
* muDst is a pointer ... oops
*
* Revision 1.7  2007/06/05 19:32:19  kocolosk
* meaning of primary first point changed btw runs -- best to get vertex from event
*
* Revision 1.6  2007/05/03 19:40:19  kocolosk
* Rewrite of StChargedPionTrack to remove redundant / useless info.
* Single particles takes up 75% less space now.
*
* Revision 1.5  2007/03/12 15:01:49  kocolosk
* use StChargedPionTrack instead of StMuTrack so we can read the trees offline
*
* Revision 1.4  2007/03/10 16:30:45  kocolosk
* correction to 1.3
*
* Revision 1.3  2007/03/10 16:26:43  kocolosk
* log each new file in job
*
* Revision 1.2  2007/03/08 22:13:59  kocolosk
* stores StMuTracks directly
*
* Revision 1.1  2007/02/02 13:59:41  kocolosk
* new Maker StChargedPionMaker intended to be used with StJetSkimEventMaker for spin analysis
*
**************************************************************************/

//ROOT headers
#include "TFile.h"
#include "TTree.h"
#include "TClonesArray.h"
#include "TChain.h"
#include "TUnixSystem.h"

//StarClassLibrary
#include "SystemOfUnits.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

//logger
#include "StMessMgr.h"

//my headers
#include "StChargedPionMaker.h"
#include "StChargedPionEvent.h"
#include "StChargedPionVertex.h"
#include "StChargedPionTrack.h"
#include "StChargedPionJet.h"
#include "StChargedPionJetParticle.h"

//StDetectorDbMaker
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"

//StJetMaker
#include "StJetMaker/StJetSkimEvent.h"
#include "StJetMaker/StJets.h"
#include "StJetMaker/StJet.h"

//StSpinDbMaker
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

//StEmcTriggerMaker
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"

ClassImp(StChargedPionMaker)

StChargedPionMaker::StChargedPionMaker(const char *name, const char *outputfile) 
{
    LOG_INFO << "calling constructor" << endm;
    
    mFile = new TFile(outputfile,"RECREATE");

    mBadTracks = new TH1D("badTracks","tracks failing quality cuts",4,0.5,4.5);

    time_t rawtime;
    time(&rawtime);
    char title[200];
    sprintf(title, "created %s", ctime(&rawtime));
    mTree = new TTree("tree",title);
    mEvent = new StChargedPionEvent();
    mTree->Branch("event", "StChargedPionEvent", &mEvent);
    
    Long64_t autosave = 1000000000; //1GB
    mTree->SetAutoSave(autosave);
    mTree->SetMaxTreeSize(autosave);
    
    muDstMaker = NULL;
    spDbMaker  = NULL;
    emcTrgMaker= NULL;
    
    theSystem = new TUnixSystem();
    
    mJetFile = NULL;
    mJetTree = NULL;
    mJets = new StJets();
    
    LOG_INFO << "finished constructor" << endm; 
}

StChargedPionMaker::~StChargedPionMaker()
{
    LOG_DEBUG << "calling destructor" << endm;
    
    delete mEvent;
    mTree->Delete();
    mFile->Delete();
    
    if(mJetFile) mJetFile->Close();
    delete mJets;
    
    LOG_DEBUG << "finished destructor" << endm;
}

void StChargedPionMaker::Clear(const char*)
{
    mEvent->Clear();
    StMaker::Clear();
}

Int_t StChargedPionMaker::Init()
{
    muDstMaker  = dynamic_cast<StMuDstMaker*>(GetMaker("MuDst")); assert(muDstMaker);
    spDbMaker   = dynamic_cast<StSpinDbMaker*>(GetMaker("spinDb"));
    emcTrgMaker = dynamic_cast<StEmcTriggerMaker*>(GetMaker("bemctrigger")); 
    
    this->Clear();
    
    LOG_INFO << "init OK" << endm;
    return StMaker::Init();
}

Int_t StChargedPionMaker::InitRun(int runnumber) {
    std::ostringstream os;
    if(runnumber < 7000000) {
        os << "/star/institutions/mit/common/run5/jets/jets_" << runnumber << ".tree.root";        
    }
    else {
        // best confirm this one with Murad first
        os << "/star/institutions/mit/common/run6/jets/jets_" << runnumber << ".tree.root";
    }
    
    if(mJetFile) mJetFile->Close();
    mJetTree = NULL;
    mJetFile = TFile::Open(os.str().c_str());
    if(mJetFile) mJetTree = (TTree*) mJetFile->Get("jet");
    if(mJetTree) {
        if(runnumber < 7000000) {
            mJetTree->SetBranchAddress("ConeJets", &mJets);            
        }
        else {
            mJetTree->SetBranchAddress("ConeJets12", &mJets);            
        }
        mJetTree->BuildIndex("mRunId","mEventId");
    }
    
    return StMaker::InitRun(runnumber);
}

Int_t StChargedPionMaker::Make()
{
    //get pointers to useful objects
    TChain* chain               = muDstMaker->chain(); assert(chain);
    StMuDst* muDst              = muDstMaker->muDst(); assert(muDst);
    StMuEvent* event            = muDst->event(); assert(event);
    
    //have we changed files?
    TString inputFile(chain->GetFile()->GetName());
    if(currentFile !=  inputFile){
        LOG_INFO << "opened file " << inputFile << endm;
        currentFile = inputFile;
        mEvent->setMuDstName( theSystem->BaseName(currentFile.Data()) );
    }
    
    //basic event info
    mEvent->setRunId( event->runNumber() );
    mEvent->setEventId( event->eventNumber() );
    mEvent->setBx7( event->l0Trigger().bunchCrossingId7bit(event->runId()) );
    mEvent->setBbcTimeBin( event->bbcTriggerDetector().onlineTimeDifference() );
    
    //spin DB
    int bx48 =  event->l0Trigger().bunchCrossingId();
    mEvent->setSpinBit( spDbMaker->spin4usingBX48(bx48) );
    mEvent->setPolValid( spDbMaker->isValid() );
    mEvent->setPolLong( spDbMaker->isPolDirLong() );
    mEvent->setPolTrans( spDbMaker->isPolDirTrans() );
    mEvent->setBxingMasked( spDbMaker->isMaskedUsingBX48(bx48) );
    mEvent->setBxingOffset( spDbMaker->offsetBX48minusBX7(bx48, mEvent->bx7()) );
    
    //vertices
    StChargedPionVertex vertex;
    unsigned int nVertices = muDst->numberOfPrimaryVertices();
    for(unsigned int i=0; i<nVertices; ++i){
        assert(muDst->primaryVertex(i));
        StMuPrimaryVertex* muVert = muDst->primaryVertex(i);
        vertex.SetX( muVert->position().x() );
        vertex.SetY( muVert->position().y() );
        vertex.SetZ( muVert->position().z() );
        vertex.setRanking( muVert->ranking() );
        mEvent->addVertex(&vertex);
    }
    
    //minbias simu trigger
    int Npmt=event->bbcTriggerDetector().numberOfPMTs();
    bool eastBBC(false), westBBC(false);
    for (int pmt=0;pmt<Npmt;pmt++){
        if(event->bbcTriggerDetector().adc(pmt) > 5) {
            if(pmt<16) eastBBC = true;
            if(pmt>23 && pmt<40) westBBC = true;
        }
    }
    if(eastBBC && westBBC) {
        mEvent->addSimuTrigger(96011);  
        mEvent->addSimuTrigger(117011);
    }
    
    //triggers and prescales
    map<int,float> prescaleMap = StDetectorDbTriggerID::instance()->getTotalPrescales();
    for (map<int,float>::iterator it=prescaleMap.begin(); it!=prescaleMap.end(); ++it) {
        int trigId = it->first;
        
        mEvent->setPrescale(trigId, it->second);
        
        if( event->triggerIdCollection().nominal().isTrigger(trigId) ) {
            mEvent->addTrigger(trigId);
        }
        
        if ( emcTrgMaker->isTrigger(trigId) ) {
            mEvent->addSimuTrigger(trigId);
        }
        
        map<int,int> m( emcTrgMaker->barrelTowersAboveThreshold(trigId) );
        for(map<int,int>::const_iterator iter=m.begin(); iter!=m.end(); iter++) {
            mEvent->addHighTower(iter->first, iter->second);
        }
        
        m = emcTrgMaker->barrelTriggerPatchesAboveThreshold(trigId);
        for(map<int,int>::const_iterator iter=m.begin(); iter!=m.end(); iter++) {
            mEvent->addTriggerPatch(iter->first, iter->second);
        }
        
        m = emcTrgMaker->barrelJetPatchesAboveThreshold(trigId);
        for(map<int,int>::const_iterator iter=m.begin(); iter!=m.end(); iter++) {
            mEvent->addJetPatch(iter->first, iter->second);
        }
    }
    
    //now for the tracks
    for(unsigned int vertex_index=0; vertex_index<nVertices; vertex_index++){
        muDst->setVertexIndex(vertex_index);
        TObjArray* primaryTracks = muDst->primaryTracks();
        StMuTrack* track;
        StMuTrack* global;
        int nentries = muDst->numberOfPrimaryTracks();
        assert(nentries==primaryTracks->GetEntries());
        for(int i=0; i<nentries; i++){
            track = muDst->primaryTracks(i);
            global = track->globalTrack();
            
            //sanity checks
            switch(track->bad()){
                case(0):
                    break;
                case(10):
                    mBadTracks->Fill(kFlagged);
                case(20):
                    mBadTracks->Fill(kBadHelix);
                case(30):
                    mBadTracks->Fill(kBadOuterHelix);
                    continue;
            }
            
            if(!global){
                mBadTracks->Fill(kMissingGlobal);
                continue;
            }
            
            //cuts
            if(track->pt() < 2.)                continue;
            if(TMath::Abs(track->eta()) > 2.1)  continue;
            
            StChargedPionTrack cpTrack(this->chargedPionTrack(track));
            cpTrack.setB(event->eventSummary().magneticField()*kilogauss);
            cpTrack.setVertex(muDst->primaryVertex()->position());
            
            mEvent->addTrack(&cpTrack);
            
            //can do some unit tests here to make sure cpTrack is OK
            /*cout << "------------------ cpTrack QA tests ------------------" << endl;
            if(fabs(cpTrack.globalPt() - global->pt()) > 1.e-4)
                printf("cp pt  = %12.8f     gl pt  = %12.8f     pr pt  = %12.8f\n",cpTrack.globalPt(track->firstPoint()),global->pt(),track->pt());
            if(fabs(cpTrack.globalPhi() - global->phi()) > 1.e-3)
                printf("cp phi = %12.8f     gl phi = %12.8f     pr phi = %12.8f\n",cpTrack.globalPhi(track->firstPoint()),global->phi(),track->phi());
            if(fabs(cpTrack.globalEta() - global->eta()) > 1.e-4)
                printf("cp eta = %12.8f     gl eta = %12.8f     pr eta = %12.8f\n",cpTrack.globalEta(track->firstPoint()),global->eta(),track->eta());
            if(fabs(cpTrack.globalDca().mag() - global->dca().mag()) > 1.e-6)
                printf("cp dca = %12.8f     gl dca = %12.8f     pr dca = %12.8f\n",cpTrack.globalDca().mag(),global->dca().mag(),track->dca().mag());
            cout << "------------------------------------------------------" << endl;*/
        }
    }
    
    //and the jets
    if(mJetTree) {
        mJetTree->GetEntryWithIndex(mEvent->runId(), mEvent->eventId());
        translateJets(mJets, mEvent);
    }

    mTree->Fill();
    
    return StMaker::Make();
}

Int_t StChargedPionMaker::Finish()
{
    mFile->cd();
    mTree->Write();
    mBadTracks->Write();
    mFile->Close();
    LOG_INFO << "finished OK"<<endm;
    return StMaker::Finish();
}

StChargedPionTrack & StChargedPionMaker::chargedPionTrack(StMuTrack *muTrack)
{
    StChargedPionTrack *cpTrack = new StChargedPionTrack();
    
    cpTrack->setId(muTrack->id());
    cpTrack->setFlag(muTrack->flag());
    
    cpTrack->setVertexIndex(muTrack->vertexIndex());
    
    cpTrack->setNHits(muTrack->nHits());
    cpTrack->setNHitsPoss(muTrack->nHitsPoss());
    cpTrack->setNHitsDedx(muTrack->nHitsDedx());
    cpTrack->setNHitsFit(muTrack->nHitsFit());
    
    cpTrack->setNSigmaElectron(muTrack->nSigmaElectron());
    cpTrack->setNSigmaPion(muTrack->nSigmaPion());
    cpTrack->setNSigmaKaon(muTrack->nSigmaKaon());
    cpTrack->setNSigmaProton(muTrack->nSigmaProton());
    cpTrack->setDedx(muTrack->dEdx());
    
    cpTrack->setChi2(muTrack->chi2());
    cpTrack->setChi2prob(muTrack->chi2prob());
    
    cpTrack->setPtEtaPhi(muTrack->pt(),muTrack->eta(),muTrack->phi());
    
    cpTrack->setCharge(muTrack->charge());      
    
    cpTrack->setGlobalLastPoint(muTrack->globalTrack()->lastPoint());
    cpTrack->setGlobalHelix(muTrack->globalTrack()->helix());
    
    return (*cpTrack);
}

void StChargedPionMaker::translateEvent(StJetSkimEvent *skimEvent, StChargedPionEvent *ev) {
    ev->setRunId(skimEvent->runId());
    ev->setEventId(skimEvent->eventId());
    ev->setBx7(skimEvent->bx7());
    ev->setBbcTimeBin(skimEvent->bbcTimeBin());
    ev->setMuDstName( basename(skimEvent->mudstFileName().GetString().Data()) );
    
    ev->setSpinBit(skimEvent->spin4usingBx48());
    ev->setPolValid(skimEvent->isValid());
    ev->setPolLong(skimEvent->isPolLong());
    ev->setPolTrans(skimEvent->isPolTrans());
    ev->setBxingMasked(skimEvent->isMaskedUsingBx48());
    ev->setBxingOffset(skimEvent->offsetBx48minusBX7());
    
    for(int i=0; i<skimEvent->trigHeaders()->GetEntriesFast(); i++) {
        StJetSkimTrigHeader *h = static_cast<StJetSkimTrigHeader*>(skimEvent->trigHeaders()->At(i));
        ev->setPrescale(h->trigId, h->prescale);
    }
    
    for(int i=0; i<skimEvent->triggers()->GetEntriesFast(); i++) {
        StJetSkimTrig *t = static_cast<StJetSkimTrig*>(skimEvent->triggers()->At(i));
        if(t->didFire() > 0) ev->addTrigger(t->trigId());
        if(t->shouldFire() > 0) ev->addSimuTrigger(t->trigId());
        
        map<int,int> m(t->towersAboveThreshold(0));
        for(map<int,int>::const_iterator it=m.begin(); it!=m.end(); it++) {
            ev->addHighTower(it->first, it->second);
        }
        
        m = t->triggerPatchesAboveThreshold(0);
        for(map<int,int>::const_iterator it=m.begin(); it!=m.end(); it++) {
            ev->addTriggerPatch(it->first, it->second);
        }
        
        m = t->jetPatchesAboveThreshold(0);
        for(map<int,int>::const_iterator it=m.begin(); it!=m.end(); it++) {
            ev->addJetPatch(it->first, it->second);
        }
    }
    
    if(skimEvent->eBbc() > 0 && skimEvent->wBbc() > 0) {
        if(ev->runId() > 6000000 && ev->runId() < 7000000) {
            ev->addSimuTrigger(96011);
        }
        else if(ev->runId() > 7000000 && ev->runId() < 8000000) {
            ev->addSimuTrigger(117001);
        }
    }
    
    StChargedPionVertex v;
    for(int i=0; i<skimEvent->vertices()->GetEntriesFast(); i++) {
        StJetSkimVert *skv = static_cast<StJetSkimVert*>(skimEvent->vertices()->At(i));
        v.SetX( skv->position()[0] );
        v.SetY( skv->position()[1] );
        v.SetZ( skv->position()[2] );
        v.setRanking( skv->ranking() );
        ev->addVertex(&v);
    }
}

void StChargedPionMaker::translateJet(StJet* oldJet, vector<TrackToJetIndex*> particles, StChargedPionJet* jet) {    
    *static_cast<TLorentzVector*>(jet) = *static_cast<TLorentzVector*>(oldJet);
    
    jet->setCharge(oldJet->charge);
    jet->setNTpcTracks(oldJet->nTracks);
    jet->setNBarrelTowers(oldJet->nBtowers);
    jet->setNEndcapTowers(oldJet->nEtowers);
    jet->setTpcEtSum(oldJet->tpcEtSum);
    jet->setBarrelEtSum(oldJet->btowEtSum);
    jet->setEndcapEtSum(oldJet->etowEtSum);
    jet->setVertexZ(oldJet->zVertex);
    
    StChargedPionJetParticle particle;
    StThreeVectorF dca(0.0);
    for(unsigned i=0; i<particles.size(); i++) {
        particle.SetPt ( particles[i]->Pt()  );
        particle.SetEta( particles[i]->Eta() );
        particle.SetPhi( particles[i]->Phi() );
        particle.SetE( particles[i]->E() );
        particle.setIndex( particles[i]->trackIndex() );
        particle.setDetectorId( particles[i]->detectorId() );
        particle.setCharge( particles[i]->charge());
        particle.setNHits( particles[i]->nHits() );
        particle.setNHitsFit( particles[i]->nHitsFit() );
        particle.setNHitsPoss( particles[i]->nHitsPoss() );
        particle.setNHitsDEdx( particles[i]->nHitsDedx() );
        particle.setNSigmaPion( particles[i]->nSigmaPion() );
        
        dca.setY( float(particles[i]->Tdcaxy()) );
        dca.setZ( float(particles[i]->Tdcaz()) );
        particle.setGlobalDca( dca );
        jet->addParticle(&particle);
    }
}

void StChargedPionMaker::translateJets(StJets *jets, StChargedPionEvent *ev) {
    StChargedPionJet myjet;
    StChargedPionVertex *v = ev->vertex(0);
    
    TClonesArray *oldJets = jets->jets();
    for(int i=0; i<oldJets->GetEntriesFast(); i++) {
        StJet *oldJet = static_cast<StJet*>(oldJets->At(i));
        translateJet(oldJet, jets->particles(i), &myjet);
        if(v) myjet.setVertexZ(v->z());
        ev->addJet(&myjet);
        myjet.Clear();
    }
}

