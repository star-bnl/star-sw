/***************************************************************************
*
* $Id: StChargedPionMaker.cxx,v 1.8 2007/06/06 13:01:31 kocolosk Exp $
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
#include "StChargedPionTrack.h"
#include "StChargedPionMaker.h"

ClassImp(StChargedPionMaker)

StChargedPionMaker::StChargedPionMaker(const char *name, const char *outputfile) 
{
    LOG_INFO << "calling constructor" << endm;
    
    StChargedPionTrack::Class()->IgnoreTObjectStreamer();
    
    mFile = new TFile(outputfile,"RECREATE");

    mBadTracks = new TH1D("badTracks","tracks failing quality cuts",4,0.5,4.5);

    mTree = new TTree("chargedPionTree","charged pions from data");
    mPrimaries = new TClonesArray("StChargedPionTrack",10);
    
    mTree->Branch("run",&mRun,"run/I");
    mTree->Branch("event",&mEvent,"event/I");
    mTree->Branch("ntracks",&mNTracks,"ntracks/I");
    mTree->Branch("primaries",&mPrimaries);
    Long64_t autosave = 1000000000; //1GB
    autosave *= 10;
    mTree->SetAutoSave(autosave);
    mTree->SetMaxTreeSize(autosave);    
    muDstMaker = NULL;
    
        
    LOG_INFO << "finished constructor" << endm; 
}

StChargedPionMaker::~StChargedPionMaker()
{
    LOG_DEBUG << "calling destructor" << endm;
    
    mPrimaries->Delete();
    mTree->Delete();
    mFile->Delete();
    
    LOG_DEBUG << "finished destructor" << endm;
}

void StChargedPionMaker::Clear(const char*)
{
    mRun = -1;
    mEvent = -1;
    mNTracks = 0;
    mPrimaries->Clear();
    
    StMaker::Clear();
}

Int_t StChargedPionMaker::Init()
{
    muDstMaker  = dynamic_cast<StMuDstMaker*>(GetMaker("MuDst")); assert(muDstMaker);
    
    this->Clear();
    
    LOG_INFO << "init OK" << endm;
    return StMaker::Init();
}

Int_t StChargedPionMaker::Make()
{
    //get pointers to useful objects
    TChain* chain               = muDstMaker->chain(); assert(chain);
    StMuDst* muDst              = muDstMaker->muDst(); assert(muDst);
    StMuEvent* event            = muDst->event(); assert(event);

    //basic event info
    mRun    = event->runNumber();
    mEvent  = event->eventNumber();
    
    //have we changed files?
    TString inputFile(chain->GetFile()->GetName());
    if(currentFile !=  inputFile){
        LOG_INFO << "opened file " << inputFile << endm;
        currentFile = inputFile;
    }
    
    //now for the tracks
    unsigned int nVertices = muDst->numberOfPrimaryVertices();
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
//          if(global->dca().mag() > 3.)        continue;
//          if(track->nHitsFit() < 20)          continue;
            
            StChargedPionTrack cpTrack(this->chargedPionTrack(track));
            cpTrack.setB(event->eventSummary().magneticField()*kilogauss);
            cpTrack.setVertex(muDst->primaryVertex()->position());
            new ( (*mPrimaries)[mPrimaries->GetLast()+1] ) StChargedPionTrack(cpTrack);
            mNTracks++;
            
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
    
    //be VERY careful here -- better to get current primary vertex from MuDst
    //Run 5 -- primary first point == global first point
    //Run 6 -- primary first point == vertex
    //cpTrack->setVertex(muTrack->firstPoint());
    
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
