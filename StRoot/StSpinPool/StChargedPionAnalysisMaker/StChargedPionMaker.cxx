/***************************************************************************
*
* $Id: StChargedPionMaker.cxx,v 1.3 2007/03/10 16:26:43 kocolosk Exp $
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

ClassImp(StChargedPionMaker)

StChargedPionMaker::StChargedPionMaker(const char *name, const char *outputfile) 
{
	LOG_DEBUG << "calling constructor" << endm;
	
	mFile = new TFile(outputfile,"RECREATE");
	mTree = new TTree("chargedPionTree","charged pions from data");
	mPrimaries = new TClonesArray("StMuTrack",10);
	mGlobals = new TClonesArray("StMuTrack",10);
	
	mTree->Branch("run",&mRun,"run/I");
	mTree->Branch("event",&mEvent,"event/I");
	mTree->Branch("ntracks",&mNTracks,"ntracks/I");
	mTree->Branch("primaries",&mPrimaries);
	mTree->Branch("globals",&mGlobals);
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
	
	mPrimaries->Delete();
	mGlobals->Delete();
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
	mGlobals->Clear();
	
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
	
	//have we changed files?
	TString inputFile(chain->GetFile()->GetName());
	if(currentFile !=  inputFile){
		LOG_INFO << "finished " << currentFile << "and opened " << inputFile << endm;
		currentFile = inputfile;
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
			
			if(!global){
				LOG_WARN << "no global found for R"<<mRun<< ", event "<<mEvent<<", key "<<track->id()<<", so skip it"<<endm;
				continue;
			}
			
			//cuts
			if(track->pt() < 2.)					continue;
			if(TMath::Abs(track->eta()) > 1.)		continue;
			if(track->dca(vertex_index).mag() > 1.)	continue;
			if(track->nHitsFit() < 20)				continue;
			
			new ( (*mPrimaries)[mPrimaries->GetLast()+1] )	StMuTrack(*track);
			new ( (*mGlobals)[mGlobals->GetLast()+1] )		StMuTrack(*global);
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
