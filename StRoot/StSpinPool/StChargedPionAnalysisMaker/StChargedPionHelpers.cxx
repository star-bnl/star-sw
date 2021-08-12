// $Id: StChargedPionHelpers.cxx,v 1.8 2013/10/09 14:43:39 stevens4 Exp $

#include "TLorentzVector.h"

#include "StChargedPionHelpers.h"

#include "StChargedPionBaseEv.h"
#include "StChargedPionEvent.h"
#include "StChargedPionJet.h"
#include "StChargedPionMcEvent.h"
#include "StChargedPionTrack.h"
#include "StChargedPionVertex.h"

#include "StSpinPool/StJetSkimEvent/StJetSkimEvent.h"
#include "StSpinPool/StJets/StJets.h"
#include "StSpinPool/StJets/StJet.h"
#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StSpinPool/StMCAsymMaker/StPythiaEvent.h"

ClassImp(StChargedPionHelpers)

StChargedPionHelpers::~StChargedPionHelpers() { }

void StChargedPionHelpers::
translateEvent(StJetSkimEvent *skimEvent, StChargedPionBaseEv *ev) {
    ev->setRunId(skimEvent->runId());
    ev->setEventId(skimEvent->eventId());
    ev->setBbcTimeBin(skimEvent->bbcTimeBin());
    const char *baseName = strrchr(skimEvent->mudstFileName().GetString().Data(), '/');
    baseName = baseName + 1;
    ev->setMuDstName( baseName );
    
    StChargedPionEvent *data = dynamic_cast<StChargedPionEvent*>(ev);
    if(data) {
        data->setBx7(skimEvent->bx7());

        data->setSpinBit(skimEvent->spin4usingBx48());
        data->setPolValid(skimEvent->isValid());
        data->setPolLong(skimEvent->isPolLong());
        data->setPolTrans(skimEvent->isPolTrans());
        data->setBxingMasked(skimEvent->isMaskedUsingBx48());
        data->setBxingOffset(skimEvent->offsetBx48minusBX7());

        for(int i=0; i<skimEvent->trigHeaders()->GetEntriesFast(); i++) {
            StJetSkimTrigHeader *h = static_cast<StJetSkimTrigHeader*>(skimEvent->trigHeaders()->At(i));
            data->setPrescale(h->trigId, h->prescale);
        }

        for(int i=0; i<skimEvent->triggers()->GetEntriesFast(); i++) {
            StJetSkimTrig *t = static_cast<StJetSkimTrig*>(skimEvent->triggers()->At(i));
            if(t->didFire() > 0) data->addTrigger(t->trigId());
            if(t->shouldFire() > 0) data->addSimuTrigger(t->trigId());

            map<int,int> *m;
            m = &(t->towersAboveThreshold(0));
            for(map<int,int>::const_iterator it=m->begin(); it!=m->end(); it++) {
                data->addHighTower(it->first, it->second);
            }
            delete m;

            m = &(t->triggerPatchesAboveThreshold(0));
            for(map<int,int>::const_iterator it=m->begin(); it!=m->end(); it++) {
                data->addTriggerPatch(it->first, it->second);
            }
            delete m;

            m = &(t->jetPatchesAboveThreshold(0));
            for(map<int,int>::const_iterator it=m->begin(); it!=m->end(); it++) {
                data->addJetPatch(it->first, it->second);
            }
            delete m;
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

void StChargedPionHelpers::
translateJet(StJet* oldJet, vector<TLorentzVector*> particles, StChargedPionJet* jet) {
    *static_cast<TLorentzVector*>(jet) = *static_cast<TLorentzVector*>(oldJet);
    
    jet->setCharge(oldJet->charge);
    jet->setNTpcTracks(oldJet->nTracks);
    jet->setNBarrelTowers(oldJet->nBtowers);
    jet->setNEndcapTowers(oldJet->nEtowers);
    jet->setTpcEtSum(oldJet->tpcEtSum);
    jet->setBarrelEtSum(oldJet->btowEtSum);
    jet->setEndcapEtSum(oldJet->etowEtSum);
    jet->setVertexZ(oldJet->zVertex);
    
    for(unsigned i=0; i<oldJet->geomTriggers().size(); i++) {
        jet->addTrigger(oldJet->geomTriggers()[i]);
    }
    
    StChargedPionJetParticle particle;
    StThreeVectorF dca;
    TowerToJetIndex *myTower;
    TrackToJetIndex *myTrack;
    for( unsigned i=0; i<particles.size(); i++ )
    {
        particle.SetPt ( particles[i]->Pt()  );
        particle.SetEta( particles[i]->Eta() );
        particle.SetPhi( particles[i]->Phi() );
        particle.SetE( particles[i]->E() );

	if ( (myTower = dynamic_cast<TowerToJetIndex *>(particles[i])) )
	{
	    particle.setIndex( myTower->towerId() );
	    particle.setDetectorId( StDetectorId(myTower->detectorId()) );
	    particle.setCharge( 0 );
	}
	else if ( (myTrack = dynamic_cast<TrackToJetIndex *>(particles[i])) )
	{
	    particle.setIndex( myTrack->trackIndex() );
	    particle.setDetectorId( myTrack->detectorId() );
	    particle.setCharge( myTrack->charge());
	    particle.setNHits( myTrack->nHits() );
	    particle.setNHitsFit( myTrack->nHitsFit() );
	    particle.setNHitsPoss( myTrack->nHitsPoss() );
	    particle.setNHitsDEdx( myTrack->nHitsDedx() );
	    particle.setNSigmaPion( myTrack->nSigmaPion() );
	    
	    dca.setY( float(myTrack->Tdcaxy()) );
	    dca.setZ( float(myTrack->Tdcaz()) );
	    particle.setGlobalDca( dca );
	}
        jet->addParticle(&particle);
    }
}

void StChargedPionHelpers::
translateJets(StJets *jets, StChargedPionBaseEv *ev) {
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

void StChargedPionHelpers::
translateJets(StJets *jets, StChargedPionMcEvent *ev) {
    StChargedPionMcJet myjet;
    StChargedPionVertex *v = ev->vertex(0);
    
    TClonesArray *oldJets = jets->jets();
    for(int i=0; i<oldJets->GetEntriesFast(); i++) {
        StJet *oldJet = static_cast<StJet*>(oldJets->At(i));
        translateJet(oldJet, jets->particles(i), &myjet);
        if(v) myjet.setVertexZ(v->z());
        ev->mcJets().push_back(myjet);
        myjet.Clear();
    }
}

void StChargedPionHelpers::
translatePythia(const StPythiaEvent *py, StChargedPionMcEvent *ev) {
    //remove const-ness until we have const versions of pythia->particle()
    StPythiaEvent *pythia = const_cast<StPythiaEvent*>(py);
    
    ev->parton1() = StChargedPionLorentzVector(pythia->particle(0)->Px(),
                                               pythia->particle(0)->Py(),
                                               pythia->particle(0)->Pz(),
                                               pythia->particle(0)->Energy());
    ev->parton2() = StChargedPionLorentzVector(pythia->particle(1)->Px(),
                                               pythia->particle(1)->Py(),
                                               pythia->particle(1)->Pz(),
                                               pythia->particle(1)->Energy());
    ev->parton3() = StChargedPionLorentzVector(pythia->particle(2)->Px(),
                                               pythia->particle(2)->Py(),
                                               pythia->particle(2)->Pz(),
                                               pythia->particle(2)->Energy());
    ev->parton4() = StChargedPionLorentzVector(pythia->particle(3)->Px(),
                                               pythia->particle(3)->Py(),
                                               pythia->particle(3)->Pz(),
                                               pythia->particle(3)->Energy());
    ev->setProcessId(pythia->processId());
    ev->setX1(pythia->x1());
}

void StChargedPionHelpers::
translateMinimc(const StMiniMcEvent *mini, StChargedPionMcEvent *ev) {
    ev->mcVertex() = StChargedPion3Vector(mini->mcVertexX(),
                                          mini->mcVertexY(),
                                          mini->mcVertexZ());
    
    TClonesArray* mcTracks      = mini->tracks(MC);
    TClonesArray* matchedPairs  = mini->tracks(MATCHED);
    TClonesArray* mergedPairs   = mini->tracks(MERGED);
    TClonesArray* splitPairs    = mini->tracks(SPLIT);
    TClonesArray* contamPairs   = mini->tracks(CONTAM);
    TClonesArray* ghostPairs    = mini->tracks(GHOST);

    StTinyMcTrack *t;
    StMiniMcPair *p;
    for(int i=0; i<mcTracks->GetEntriesFast(); i++) {
        t = static_cast<StTinyMcTrack*>(mcTracks->At(i));
        if(t->ptMc() > 1.7) ev->mcTracks().push_back(*t);
    }
    
    //note: the approach I used here should end up duplicating the tiny mc tracks
    //in each of these clones arrays.  Alternative would be to find the pointer to
    //the original tiny mc track in ev->mcTracks() that has the same ID.  That's
    //likely a CPU-intensive process (but who cares?)
    
    //recoKey wasn't filled for the 2005 simulations, so I have to fall back to
    //pT matching here
    
    for(int i=0; i<matchedPairs->GetEntriesFast(); i++) {
        p = static_cast<StMiniMcPair*>(matchedPairs->At(i));
        if(p->ptPr() > 2.0) {
            ev->matchedPairs().push_back(*p);
        }
    }
    
    for(int i=0; i<mergedPairs->GetEntriesFast(); i++) {
        p = static_cast<StMiniMcPair*>(mergedPairs->At(i));
        if(p->ptPr() > 2.0) {
            ev->mergedPairs().push_back(*p);
        }
    }
    
    for(int i=0; i<splitPairs->GetEntriesFast(); i++) {
        p = static_cast<StMiniMcPair*>(splitPairs->At(i));
        if(p->ptPr() > 2.0) {
            ev->splitPairs().push_back(*p);
        }
    }
    
    for(int i=0; i<contamPairs->GetEntriesFast(); i++) {
        p = static_cast<StMiniMcPair*>(contamPairs->At(i));
        if(p->ptPr() > 2.0) {
            ev->contamPairs().push_back(*p);
        }
    }
    
    for(int i=0; i<ghostPairs->GetEntriesFast(); i++) {
        p = static_cast<StMiniMcPair*>(ghostPairs->At(i));
        if(p->ptPr() > 2.0) {
            ev->ghostPairs().push_back(*p);
        }
    }
}

void StChargedPionHelpers::
translateMuDst(StChargedPionEvent *ev) {
    translateMuDst(static_cast<StChargedPionBaseEv*>(ev));
    
    ev->setBx7( StMuDst::event()->l0Trigger().bunchCrossingId7bit(ev->runId()) );
    ev->setRunInfo( StMuDst::event()->runInfo() );
    
    // see StDaqLib/TRG/trgStructures2005.h => L2RESULTS_OFFSET_DIJET==14
    if(ev->runId() > 7000000) 
        ev->setL2Result( StMuDst::event()->L2Result().GetArray() + 14 );
}

void StChargedPionHelpers::
translateMuDst(StChargedPionMcEvent *ev) {
    translateMuDst(static_cast<StChargedPionBaseEv*>(ev));
}

void StChargedPionHelpers::
translateMuDst(StChargedPionBaseEv *ev) {
    //basic event info
    ev->setRunId( StMuDst::event()->runNumber() );
    ev->setEventId( StMuDst::event()->eventNumber() );
    ev->setBbcTimeBin( StMuDst::event()->bbcTriggerDetector().onlineTimeDifference() );
    
    //vertices
    StChargedPionVertex vertex;
    unsigned int nVertices = StMuDst::numberOfPrimaryVertices();
    for(unsigned int i=0; i<nVertices; ++i){
        assert(StMuDst::primaryVertex(i));
        StMuPrimaryVertex* muVert = StMuDst::primaryVertex(i);
        vertex.SetX( muVert->position().x() );
        vertex.SetY( muVert->position().y() );
        vertex.SetZ( muVert->position().z() );
        vertex.setRanking( muVert->ranking() );
        ev->addVertex(&vertex);
    }
    
    //now for the tracks
    for(unsigned int vertex_index=0; vertex_index<nVertices; vertex_index++){
        StMuDst::setVertexIndex(vertex_index);
        TObjArray* primaryTracks = StMuDst::primaryTracks();
        const StMuTrack* track;
        const StMuTrack* global;
        int nentries = StMuDst::numberOfPrimaryTracks();
        assert(nentries==primaryTracks->GetEntries());
        for(int i=0; i<nentries; i++){
            track = StMuDst::primaryTracks(i);
            global = track->globalTrack();
            
            //sanity checks
            switch(track->bad()){
                case(0):
                    break;
                case(10):
                    // mBadTracks->Fill(kFlagged);
                case(20):
                    // mBadTracks->Fill(kBadHelix);
                case(30):
                    // mBadTracks->Fill(kBadOuterHelix);
                    continue;
            }
            
            if(!global){
                // mBadTracks->Fill(kMissingGlobal);
                continue;
            }
            
            //cuts
            if(track->pt() < 2.)                continue;
            if(TMath::Abs(track->eta()) > 2.1)  continue;
            
            StChargedPionTrack cpTrack;
            translateTrack(track, &cpTrack);
            cpTrack.setB(StMuDst::event()->eventSummary().magneticField()*kilogauss);
            cpTrack.setVertex(StMuDst::primaryVertex()->position());
            
            ev->addTrack(&cpTrack);
        }
    }
}

void StChargedPionHelpers::
translateTrack(const StMuTrack *mu, StChargedPionTrack *cp) {
    cp->setId(mu->id());
    cp->setFlag(mu->flag());
    
    cp->setVertexIndex(mu->vertexIndex());
    
    cp->setNHits(mu->nHits());
    cp->setNHitsPoss(mu->nHitsPoss());
    cp->setNHitsDedx(mu->nHitsDedx());
    cp->setNHitsFit(mu->nHitsFit());
    
    cp->setNSigmaElectron(mu->nSigmaElectron());
    cp->setNSigmaPion(mu->nSigmaPion());
    cp->setNSigmaKaon(mu->nSigmaKaon());
    cp->setNSigmaProton(mu->nSigmaProton());
    cp->setDedx(mu->dEdx());
    
    cp->setChi2(mu->chi2());
    cp->setChi2prob(mu->chi2prob());
    
    cp->setPtEtaPhi(mu->pt(),mu->eta(),mu->phi());
    
    cp->setCharge(mu->charge());      
    
    cp->setGlobalLastPoint(mu->globalTrack()->lastPoint());
    cp->setGlobalHelix(mu->globalTrack()->helix());
}

/*****************************************************************************
 * $Log: StChargedPionHelpers.cxx,v $
 * Revision 1.8  2013/10/09 14:43:39  stevens4
 * Add const to char* in 2 lines to compile on 5.34.09 and SL6.4 on rplay18
 *
 * Revision 1.7  2010/07/16 20:24:08  rfatemi
 * Changes in code to preserve backward compatibility with changes in jet code.  Affects info about particles in jets.
 *
 * Revision 1.5  2009/12/02 21:17:31  fine
 * Fix StMuTrack interface
 *
 * Revision 1.4  2009/09/25 14:26:37  fine
 * fix compilation error on SL5/64-bit machine
 *
 * Revision 1.3  2009/04/02 18:25:42  kocolosk
 * fixed paths to jet codes
 *
 * Revision 1.2  2008/12/29 15:58:28  kocolosk
 * removed commented code and added Id and Log as needed
 *
 * Revision 1.1  2008/07/17 17:06:30  kocolosk
 * big-bang integration StChargedPionMcEvent framework
 *
 *****************************************************************************/
