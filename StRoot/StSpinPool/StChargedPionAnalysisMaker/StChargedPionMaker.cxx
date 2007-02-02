/***************************************************************************
*
* $Id: StChargedPionMaker.cxx,v 1.1 2007/02/02 13:59:41 kocolosk Exp $
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
* Revision 1.1  2007/02/02 13:59:41  kocolosk
* new Maker StChargedPionMaker intended to be used with StJetSkimEventMaker for spin analysis
*
**************************************************************************/

//ROOT headers
#include "TFile.h"
#include "TTree.h"
#include "TClonesArray.h"
#include "TChain.h"

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
#include "TChargedPionEvent.h"
#include "StChargedPionMaker.h"

ClassImp(StChargedPionMaker)

StChargedPionMaker::StChargedPionMaker(const char *name, const char *outputfile) 
{
	LOG_DEBUG << "calling constructor" << endm;
	
	mFile = new TFile(outputfile,"RECREATE");
	mTree = new TTree("chargedPionTree","charged pions from data");
	mTracks = new TClonesArray("TChargedPion",10);
	mPion = new TChargedPion();
	
	mTree->Branch("run",&mRun,"run/I");
	mTree->Branch("event",&mEvent,"event/I");
	mTree->Branch("fnumber",&mFileNumber,"fnumber/I");
	mTree->Branch("ntracks",&mNTracks,"ntracks/I");
	mTree->Branch("tracks",&mTracks);
	Long64_t autosave = 1000000000; //1GB
	autosave *= 10;
	mTree->SetAutoSave(autosave);
	mTree->SetMaxTreeSize(autosave);	
	muDstMaker = NULL;
	
	LOG_DEBUG << "finished constructor" << endm; 
}

StChargedPionMaker::~StChargedPionMaker()
{
	LOG_DEBUG << "calling destructor" << endm;
	
	mPion->Delete();
	mTracks->Delete();
	mTree->Delete();
	mFile->Delete();
	
	LOG_DEBUG << "finished destructor" << endm;
}

void StChargedPionMaker::Clear(const char*)
{
	mRun = -1;
	mEvent = -1;
	mFileNumber = -1;
	mNTracks = 0;
	mPion->Clear();
	mTracks->Clear();
	
	StMaker::Clear();
}

Int_t StChargedPionMaker::Init()
{
	muDstMaker	= dynamic_cast<StMuDstMaker*>(GetMaker("MuDst")); assert(muDstMaker);
	
	LOG_INFO << "init OK" << endm;
	return StMaker::Init();
}

Int_t StChargedPionMaker::Make()
{
	//get pointers to useful objects
	TChain* chain				= muDstMaker->chain(); assert(chain);
	StMuDst* muDst				= muDstMaker->muDst(); assert(muDst);
	StMuEvent* event			= muDst->event(); assert(event);

	//basic event info
	mRun	= event->runNumber();
	mEvent	= event->eventNumber();
	
	TString inputfile(chain->GetFile()->GetName());
	int index1 = inputfile.Index(".MuDst");
	TString filenumber(inputfile(index1-7,7));
	mFileNumber = filenumber.Atoi();
	
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
			if(!global){
				LOG_WARN << "no global found for R"<<mRun<< ", event "<<mEvent<<", key "<<track->id()<<", so skip it"<<endm;
				continue;
			}
			
			if(track->pt()<1.) continue;
			
			//basics
			mPion->pt			= track->pt();
			mPion->eta			= track->eta();
			mPion->phi			= track->phi();
			mPion->nHits		= track->nHits();
			mPion->nFitPoints	= track->nHitsFit();
			mPion->nDedxPoints	= track->nHitsDedx();
			mPion->nHitsPossible= track->nHitsPoss();
			mPion->charge		= track->charge();
			mPion->chi2			= track->chi2();
			mPion->key			= track->id();
			mPion->flag			= track->flag();
			
			//global info
			mPion->globalPt		= global->pt();
			mPion->globalEta	= global->eta();
			mPion->globalPhi	= global->phi();
			
			//dedx info
			mPion->dEdx			= track->dEdx();
			mPion->nSigmaPion	= track->nSigmaPion();
			mPion->nSigmaElectron= track->nSigmaElectron();
			mPion->nSigmaProton	= track->nSigmaProton();
			mPion->nSigmaKaon	= track->nSigmaKaon();
			
			mPion->vertexIndex = vertex_index;
			
			//TVector3 stuff (vertex,dca,first,last)
			StThreeVectorF stdca = track->dcaGlobal(mPion->vertexIndex);
			mPion->dca.SetXYZ(stdca.x(),stdca.y(),stdca.z());
			
			StThreeVectorF stfp = global->firstPoint();
			mPion->firstPoint.SetXYZ(stfp.x(),stfp.y(),stfp.z());
			
			StThreeVectorF stlp = global->lastPoint();
			mPion->lastPoint.SetXYZ(stlp.x(),stlp.y(),stlp.z());
			
			LOG_DEBUG<<"adding track with pt "<<mPion->pt<<endm;
			new ( (*mTracks)[mTracks->GetLast()+1] ) TChargedPion(*mPion);
			mPion->Clear();
			mNTracks++;
		}
	}
	
	mTree->Fill();
	
	return StMaker::Make();
}

Int_t StChargedPionMaker::Finish()
{
	mFile->cd();
	mTree->Write();
	mFile->Close();
	LOG_INFO << "finished OK"<<endm;
	return StMaker::Finish();
}
