//Xianglei Zhu 
//Skeleton embedding PicoDst analysis macro with StPicoDstMaker 
//Run it with the wrapper in ACLIC mode, CINT mode for debug ONLY

#ifndef __CINT__
#include "TROOT.h"
#include "TSystem.h"
#include <iostream>
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"
#include "TTreeHelper.h"
#include "TDatime.h"
#include "StarRoot/TUnixTime.h"
#include "StChain.h"
#include "StMessMgr.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoMcTrack.h"
#include "StPicoEvent/StPicoMcVertex.h"
#include "StPicoEvent/StPicoArrays.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StBTofHeader.h"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#endif

void makePicoDstQA(TString InputFileList, Int_t nFiles = 1, Int_t nEvents = 0, TString OutputFile = "test.histo.root" );

void makePicoDstQA(TString InputFileList, Int_t nFiles, Int_t nEvents, TString OutputFile ) 
{
 
  // Load libraries for CINT mode
#ifdef __CINT__
  gROOT   -> Macro("loadMuDst.C");
  gSystem->Load("StPicoEvent");
  gSystem->Load("StPicoDstMaker");
#endif

  // List of member links in the chain
  StChain*                    chain  =  new StChain ;

  StPicoDstMaker* picoDstMaker = new StPicoDstMaker(StPicoDstMaker::IoRead,InputFileList,"picoDst");

  if ( nEvents == 0 )  nEvents = 10000000 ;       // Take all events in nFiles if nEvents = 0

  // ---------------- modify here according to your QA purpose --------------------------
  TFile *tags_output = new TFile( OutputFile, "recreate" ) ;
  tags_output->cd();

  //book histograms or trees if you need
  TH1F *hPhiMc = new TH1F("hPhiMc","Phi of Mc tracks",200,-TMath::Pi(),TMath::Pi());
  TH1F *hPtMc = new TH1F("hPtMc","Pt of Mc tracks",200,0,5.0);
  TH1F *hSelPtMc = new TH1F("hSelPtMc","Pt of selected Mc tracks",38,0.2,4.0);
  TH1F *hEtaMc = new TH1F("hEtaMc","Eta of Mc tracks",200,-2.0,2.0);
  TH1F *hPhi = new TH1F("hPhi","Phi of matched RC tracks",200,-TMath::Pi(),TMath::Pi());
  TH1F *hPt = new TH1F("hPt","Pt of matched RC tracks",200,0,5.0);
  TH1F *hSelPt = new TH1F("hSelPt","Pt of selected matched RC tracks",38,0.2,4.0);
  TH1F *hEffPt = new TH1F("hEffPt","Efficiency in pt bins",38,0.2,4.0);
  TH1F *hEta = new TH1F("hEta","Eta of matched RC tracks",200,-2.0,2.0);
  hEffPt->Sumw2();
  hSelPt->Sumw2();
  hSelPtMc->Sumw2();

  // ---------------- end of histogram and tree booking --------------------------------

  // Loop over the links in the chain
  Int_t iInit = chain -> Init() ;
  if (iInit) chain->FatalErr(iInit,"on init");
  
  // chain -> EventLoop(1,nEvents) ;  //will output lots of useless debugging info.
  Int_t istat = 0, i = 1;
  while (i <= nEvents && istat != 2) {
     if(i%10==0)cout << endl << "== Event " << i << " start ==" << endl;
     chain->Clear();
     istat = chain->Make(i);

     if (istat == 2)
	  cout << "Last  event processed. Status = " << istat << endl;
     if (istat == 3)
	  cout << "Error event processed. Status = " << istat << endl;
     i++;

     if(istat != kStOK)continue; //skip those suspectible events
     
  // ---------------- modify here according to your QA purpose --------------------------
     //cout<<"In event #. "<<i-1<<" Maker status "<<istat<<endl;

     StPicoDst* mPicoDst = picoDstMaker->picoDst();
     if(!mPicoDst) {
	  LOG_WARN << " No PicoDst " << endm; continue;
     }

     StPicoEvent* mPicoEvent = mPicoDst->event();
     if(!mPicoEvent) {
	  LOG_WARN << " No PicoEvent " << endm; continue;
     }

     //trigger
     if ( ! mPicoEvent->isTrigger(610001) && ! mPicoEvent->isTrigger(610011) && ! mPicoEvent->isTrigger(610021) && ! mPicoEvent->isTrigger(610031) && ! mPicoEvent->isTrigger(610041) && ! mPicoEvent->isTrigger(610051) ) continue ;
     //Vz
     if ( fabs(mPicoEvent->primaryVertex().Z()) > 75.0 ) continue ;
     //Vr
     if ( mPicoEvent->primaryVertex().Perp() > 2.0 ) continue ;
     
     //fill MC histograms
     //The MC arrays in PicoDst
     Int_t NoMuMcVertices = mPicoDst->numberOfMcVertices();
     Int_t NoMuMcTracks = mPicoDst->numberOfMcTracks();
     LOG_INFO <<"# of MC tracks = "<< NoMuMcTracks <<" # of MC vertices = "<< NoMuMcVertices << endm;
     if (! NoMuMcVertices || ! NoMuMcTracks) {
	  LOG_WARN << "Ev. " << i  << " has no MC information ==> skip it" << endm;
	  continue;
     }
     Int_t nMc = 0;

     // Loop for MC tracks
     for(Int_t itrk=0; itrk<NoMuMcTracks; itrk++){
	  StPicoMcTrack *mcTrack = (StPicoMcTrack *) mPicoDst->mcTrack(itrk);
	  if (! mcTrack) continue;

	  // Select only Triggered Mc Vertex, i.e. the MC track should originate from PV (IdVx=1)
	  Int_t IdVx = mcTrack->idVtxStart();
	  if (IdVx != 1) continue;

	  const int Gid = mcTrack->geantId();

	  nMc++;  // # of MC tracks
	  if(Gid==11){//k+
		hPtMc->Fill(mcTrack->p().Perp());
		hPhiMc->Fill(mcTrack->p().Phi());
		hEtaMc->Fill(mcTrack->p().PseudoRapidity());
		if(fabs(mcTrack->p().PseudoRapidity())<0.5)hSelPtMc->Fill(mcTrack->p().Perp());
	  }
	  else {
	     LOG_WARN << "Gid: "<<Gid<<" in Ev. "<<i<<endm;
	  }
     }

     //fill Event QA histograms
     Int_t nTracks = mPicoDst->numberOfTracks();
     StPicoTrack* ptrack ; 
     for(Int_t i=0; i<nTracks; i++)                // Main loop for Iterating over tracks
     {
	  ptrack = mPicoDst->track(i);  // Pointer to a track
	  if(!ptrack->isPrimary())continue;

	  if (ptrack->idTruth() <= 0 || ptrack->idTruth() > NoMuMcTracks) {
	     //cout << "Illegal idTruth " << ptrack->idTruth() << " The track is ignored" << endl;
	     continue;
	  }
	  StPicoMcTrack *mcTrack = (StPicoMcTrack *) mPicoDst->mcTrack(ptrack->idTruth()-1);
	  if (!mcTrack) {
	     LOG_WARN << "Inconsistency in mcArray(1), ignored" << endm;
	     continue;
	  }
	  if (mcTrack->id() != ptrack->idTruth()) {
	     LOG_WARN << "Mismatched idTruth " << ptrack->idTruth() << " and mcTrack Id " <<  mcTrack->id() 
		  << " this track is ignored" <<  endm;
	  }
	  Int_t idMcVx = mcTrack->idVtxStart();
	  while (idMcVx != 1) {
	     StPicoMcVertex *mcVertex = (StPicoMcVertex *) mPicoDst->mcVertex(idMcVx-1);
	     Int_t idMcTrack = mcVertex->idOfParentTrack();
	     if (! idMcTrack) break;
	     StPicoMcTrack *mcTrackP = (StPicoMcTrack *) mPicoDst->mcTrack(idMcTrack-1);
	     idMcVx = mcTrackP->idVtxStart();
	     if (! idMcVx) break;
	  }
	  if (idMcVx != 1) continue; //this MC track is not eventually originated from PV

	  if(mcTrack->geantId() != 11) continue;
	  if(mcTrack->idVtxStart() != 1) {
	     LOG_WARN<<"mc track may not directly originate from PV!"<<endm;
	  }
	  if(ptrack->qaTruth()<50.) continue;

	  if(ptrack->nHits()<=15)continue;
	  //if(ptrack->flag()<=0)continue;
	  if(abs(ptrack->charge())!=1) continue;

	  TVector3 p = ptrack->pMom();
	  hPhi->Fill(p.Phi());
	  hPt->Fill(p.Perp());
	  hEta->Fill(p.PseudoRapidity());
	  if(fabs(p.PseudoRapidity())<0.5)hSelPt->Fill(p.Perp());
	  //end of the filling
     }
  }
  hEffPt->Divide(hSelPt,hSelPtMc,1,1,"B");

  if (nEvents > 1) chain -> Finish() ;

  if(tags_output!=NULL) tags_output -> Write() ;
  if(tags_output!=NULL) tags_output -> Close() ;
  //flush(tags_output);
  delete tags_output;

  // Cleanup
  delete chain ;
}
