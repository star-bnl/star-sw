//Xianglei Zhu 
//Skeleton embedding MuDST analysis macro with StMuDSTMaker 
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
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StBTofHeader.h"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#endif

void makeMuDstQA(TString InputFileList, Int_t nFiles = 1, Int_t nEvents = 0, TString OutputFile = "test.histo.root" );

void makeMuDstQA(TString InputFileList, Int_t nFiles, Int_t nEvents, TString OutputFile ) 
{
 
  // Load libraries for CINT mode
#ifdef __CINT__
  gROOT   -> Macro("loadMuDst.C");
#endif

  // List of member links in the chain
  StChain*                    chain  =  new StChain ;

  StMuDstMaker*          muDstMaker  =  new StMuDstMaker(0,0,"",InputFileList,"MuDst",nFiles) ;

  // ---------------- modify here according to your QA purpose --------------------------
  // Turn off everything but Primary tracks in order to speed up the analysis and eliminate IO
  muDstMaker -> SetStatus("*",0) ;               // Turn off all branches
  muDstMaker -> SetStatus("MuEvent",1) ;         // Turn on the Event data (esp. Event number)
  muDstMaker -> SetStatus("PrimaryVertices",1) ;    // Turn on the primary track data
  muDstMaker -> SetStatus("PrimaryTracks",1) ;    // Turn on the primary track data
  muDstMaker -> SetStatus("GlobalTracks",1) ;    // Turn on the global track data
  muDstMaker -> SetStatus("CovGlobTrack",1);   // to fix the refmult in Run14!!!
  muDstMaker -> SetStatus("MCAll",1) ;          // Turn on the McVertex/McTrack data
  muDstMaker -> SetStatus("BTofHeader",1) ;    // Turn on the btof data
  muDstMaker -> SetDebug(0) ;                    // Turn off Debug information

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

     StMuDst* mMuDst = muDstMaker->muDst();
     if(!mMuDst) {
	  LOG_WARN << " No MuDst " << endm; continue;
     }

     StMuEvent* mMuEvent = mMuDst->event();
     if(!mMuEvent) {
	  LOG_WARN << " No MuEvent " << endm; continue;
     }

     //-----------------------------------------------------------------------------
     //vertex selection
     int const originalVertexId = mMuDst->currentVertexIndex();
     StMuPrimaryVertex* selectedVertex = nullptr;
     // choose the default vertex, i.e. the first vertex
     mMuDst->setVertexIndex(0);
     selectedVertex = mMuDst->primaryVertex();
     // fall back to default vertex if no vertex is selected in the algorithm above.
     // should skip this event in the event cuts below.
     if ( ! selectedVertex ){
	  LOG_INFO << "Vertex is not valid" << endm;
	  //cout<<originalVertexId<<endl;
	  mMuDst->setVertexIndex(originalVertexId);
     }
     //end of vertex selection
     //------------------------------------------------------------------------------

     //vertex is not selected
     if ( ! selectedVertex ) continue;
     //trigger
     if ( ! mMuEvent->triggerIdCollection().nominal().isTrigger(610001) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(610011) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(610021) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(610031) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(610041) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(610051) ) continue ;
     //Vz
     if ( fabs(mMuEvent->primaryVertexPosition().z()) > 75.0 ) continue ;
     //Vr
     if ( mMuEvent->primaryVertexPosition().perp() > 2.0 ) continue ;
     
     //fill MC histograms
     //The MC arrays in MuDST
     TClonesArray *MuMcVertices   = mMuDst->mcArray(0);
     Int_t NoMuMcVertices = MuMcVertices->GetEntriesFast();
     TClonesArray *MuMcTracks     = mMuDst->mcArray(1); 
     Int_t NoMuMcTracks = MuMcTracks->GetEntriesFast();
     LOG_INFO <<"# of MC tracks = "<< NoMuMcTracks <<" # of MC vertices = "<< NoMuMcVertices << endm;
     if (! NoMuMcVertices || ! NoMuMcTracks) {
	  LOG_WARN << "Ev. " << i  << " has no MC information ==> skip it" << endm;
	  continue;
     }
     Int_t nMc = 0;

     // Loop for MC tracks
     for(Int_t itrk=0; itrk<NoMuMcTracks; itrk++){
	  StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(itrk);
	  if (! mcTrack) continue;

	  // Select only Triggered Mc Vertex, i.e. the MC track should originate from PV (IdVx=1)
	  Int_t IdVx = mcTrack->IdVx();
	  if (IdVx != 1) continue;

	  const int Gid = mcTrack->GePid();

	  nMc++;  // # of MC tracks
	  if(Gid==11){//k+
		hPtMc->Fill(mcTrack->Pxyz().perp());
		hPhiMc->Fill(mcTrack->Pxyz().phi());
		hEtaMc->Fill(mcTrack->Pxyz().pseudoRapidity());
		if(fabs(mcTrack->Pxyz().pseudoRapidity())<0.5)hSelPtMc->Fill(mcTrack->Pxyz().perp());
	  }
	  else {
	     LOG_WARN << "Gid: "<<Gid<<" in Ev. "<<i<<endm;
	  }
     }

     //fill Event QA histograms
     TObjArray* tracks = muDstMaker->muDst()->primaryTracks() ;
     TObjArrayIter GetTracks(tracks) ;
     StMuTrack* ptrack ; 
     while ( ( ptrack = (StMuTrack*)GetTracks.Next() ) )
     {
	  if (ptrack->idTruth() <= 0 || ptrack->idTruth() > NoMuMcTracks) {
	     //cout << "Illegal idTruth " << ptrack->idTruth() << " The track is ignored" << endl;
	     continue;
	  }
	  StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(ptrack->idTruth()-1);
	  if (!mcTrack) {
	     LOG_WARN << "Inconsistency in mcArray(1), ignored" << endm;
	     continue;
	  }
	  if (mcTrack->Id() != ptrack->idTruth()) {
	     LOG_WARN << "Mismatched idTruth " << ptrack->idTruth() << " and mcTrack Id " <<  mcTrack->Id() 
		  << " this track is ignored" <<  endm;
	  }
	  Int_t idMcVx = mcTrack->IdVx();
	  while (idMcVx != 1) {
	     StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(idMcVx-1);
	     Int_t idMcTrack = mcVertex->IdParTrk();
	     if (! idMcTrack) break;
	     StMuMcTrack *mcTrackP = (StMuMcTrack *) MuMcTracks->UncheckedAt(idMcTrack-1);
	     idMcVx = mcTrackP->IdVx();
	     if (! idMcVx) break;
	  }
	  if (idMcVx != 1) continue; //this MC track is not eventually originated from PV

	  if(mcTrack->GePid() != 11) continue;
	  if(mcTrack->IdVx() != 1) {
	     LOG_WARN<<"mc track may not directly originate from PV!"<<endm;
	  }
	  if(ptrack->qaTruth()<50.) continue;

	  if(ptrack->nHits()<=15)continue;
	  if(ptrack->flag()<=0)continue;
	  if(abs(ptrack->charge())!=1) continue;

	  StThreeVectorF p = ptrack->p();
	  hPhi->Fill(p.phi());
	  hPt->Fill(p.perp());
	  hEta->Fill(p.pseudoRapidity());
	  if(fabs(p.pseudoRapidity())<0.5)hSelPt->Fill(p.perp());
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
