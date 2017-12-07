//Xianglei Zhu, created on May 26, 2009
//General MuDST QA macro with StMuDSTMaker 
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
#include "StChain.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StBTofHeader.h"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#endif

void makeMuDstQA(TString InputFileList, Int_t nFiles = 1, Int_t nEvents = 0, TString OutputDir = "output/" );

void makeMuDstQA(TString InputFileList, Int_t nFiles, Int_t nEvents, TString OutputDir ) 
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
  muDstMaker -> SetStatus("BTofHeader",1) ;    // Turn on the btof data
  muDstMaker -> SetDebug(0) ;                    // Turn off Debug information

  if ( nEvents == 0 )  nEvents = 10000000 ;       // Take all events in nFiles if nEvents = 0

  // ---------------- modify here according to your QA purpose --------------------------
  //book histograms or trees if you need
  TString oFile(muDstMaker->GetFile());
  TString oChopFile;
  int fileBeginIndex = oFile.Index("st_",0);
  oFile.Remove(0,fileBeginIndex);
  short indx1 = oFile.First('.');
  short indx2 = oFile.Last('.');
  if (indx1!=indx2) oFile.Remove(indx1+1,(indx2-indx1));
  oChopFile=oFile;
  oFile.Insert(indx1+1,"moretags.");
  oFile.Prepend(OutputDir);
  oChopFile.Insert(indx1+1,"chopper.");
  oChopFile.ReplaceAll("root","txt");
  oChopFile.Prepend(OutputDir);

  ofstream chop_output(oChopFile);

  TFile *tags_output = new TFile( oFile, "recreate" ) ;
  tags_output->cd();

  //TH1F *hPhi = new TH1F("hPhi","Phi of proton",200,-TMath::Pi(),TMath::Pi());
  //TH2F *hPhiFirstZ = new TH2F("hPhiFirstZ","Phi vs. FirstZ",200,-150,150,200,-TMath::Pi(),TMath::Pi()); 

  //Prepare the output tree
  Int_t mRunId, mEvtId;
  Int_t mnRefMult, mngRefMult, mnTofMatch;
  Float_t mVX, mVY, mVZ;
  Float_t mVpdVz;
  Float_t mPVRank;
  TTree *mMoreTagsTree = new TTree("MoreTags","MoreTags");
  mMoreTagsTree->Branch("RunId",&mRunId,"RunId/I");
  mMoreTagsTree->Branch("EvtId",&mEvtId,"EvtId/I");
  mMoreTagsTree->Branch("nRefMult",&mnRefMult,"nRefMult/I");
  mMoreTagsTree->Branch("ngRefMult",&mngRefMult,"ngRefMult/I");
  mMoreTagsTree->Branch("nTofMatch",&mnTofMatch,"nTofMatch/I");
  mMoreTagsTree->Branch("VX",&mVX,"VX/F");
  mMoreTagsTree->Branch("VY",&mVY,"VY/F");
  mMoreTagsTree->Branch("VZ",&mVZ,"VZ/F");
  mMoreTagsTree->Branch("VpdVz",&mVpdVz,"VpdVz/F");
  mMoreTagsTree->Branch("PVRank",&mPVRank,"PVRank/F");
  mMoreTagsTree->SetAutoSave(10000000);

  
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
     //let's do the QA here...
     //start with event cutting...
     //cout<<"In event #. "<<i-1<<" Maker status "<<istat<<endl;

     StMuDst* mMuDst = muDstMaker->muDst();
     if(!mMuDst) {
	  LOG_WARN << " No MuDst " << endm; continue;
     }

     StMuEvent* mMuEvent = mMuDst->event();
     if(!mMuEvent) {
	  LOG_WARN << " No MuEvent " << endm; continue;
     }

     //vzVpd
     StBTofHeader const* mBTofHeader = mMuDst->btofHeader();
     Float_t vzVpd=-999;
     if (mBTofHeader) vzVpd = mBTofHeader->vpdVz();

/*
     //Run14 vertex selection
     //////////////////////////////////////
     // select the right vertex using VPD
     /////////////////////////////////////
     for(unsigned int i=0;i<mMuDst->numberOfPrimaryVertices();i++) {
	  StMuPrimaryVertex *vtx = mMuDst->primaryVertex(i);
	  if(!vtx) continue;
	  Float_t vz = vtx->position().z();
	  if(fabs(vzVpd)<100 && fabs(vzVpd-vz)<3.) {
	     mMuDst->setVertexIndex(i);
	     break;
	  }
     }
     /////////////////////////////////////
*/

/*
     //Run16 vertex selection
     ////////////////////////////////////////////////////////////////
     if (fabs(vzVpd) < 200)
     {
	  for (unsigned int iVtx = 0; iVtx < mMuDst->numberOfPrimaryVertices(); ++iVtx)
	  {
	     StMuPrimaryVertex* vtx = mMuDst->primaryVertex(iVtx);
	     if (!vtx) continue;

	     if (fabs(vzVpd - vtx->position().z()) < 3.)
	     {
		  mMuDst->setVertexIndex(iVtx);
		  break;
	     }
	  }
     }
     ////////////////////////////////////////////////////////////////
*/

     mRunId = mMuEvent->runNumber();
     mEvtId = mMuEvent->eventNumber();
     mnRefMult = mMuEvent->refMult();

     Int_t nTofMatPrTrack = 0;
     TObjArray* prtracks = muDstMaker->muDst()->primaryTracks() ;    // Create a TObject array containing the global tracks  
     TObjArrayIter GetPrTracks(prtracks) ;                              // Create an iterator to step through the tracks  
     StMuTrack* prtrack ;                                             // Pointer to a track
     while ( ( prtrack = (StMuTrack*)GetPrTracks.Next() ) )             // Main loop for Iterating over tracks
     {
	  if(prtrack->btofPidTraits().matchFlag()) nTofMatPrTrack ++;
     }
     mnTofMatch = nTofMatPrTrack;

     mVX = mMuEvent->primaryVertexPosition().x();
     mVY = mMuEvent->primaryVertexPosition().y();
     mVZ = mMuEvent->primaryVertexPosition().z();

     mVpdVz = vzVpd;
     if(mMuDst->primaryVertex())mPVRank = mMuDst->primaryVertex()->ranking();
     else mPVRank = -1e9;

     Int_t nGlTrack = 0;
     TObjArray* gltracks = muDstMaker->muDst()->globalTracks() ;    // Create a TObject array containing the global tracks  
     TObjArrayIter GetGlTracks(gltracks) ;                              // Create an iterator to step through the tracks  
     StMuTrack* gltrack ;                                             // Pointer to a track
     while ( ( gltrack = (StMuTrack*)GetGlTracks.Next() ) )             // Main loop for Iterating over tracks
     {
	  if(fabs(gltrack->eta())>=0.5)continue;
	  if(gltrack->nHitsFit()<10)continue;
	  if(gltrack->dca().mag()>=3.0)continue;
	  nGlTrack++ ;
     }
     mngRefMult = nGlTrack;

     mMoreTagsTree->Fill();

     //Event info (for debug)
     //cout<<"Run#: "<<mMuEvent->runNumber()<<endl;
     //cout<<"Evt#: "<<mMuEvent->eventNumber()<<endl;
     //cout<<muDstMaker->muDst()->currentVertexIndex()<<endl;
     //cout<<"refmult: "<<mMuEvent->refMult()<<endl;

     //Event cuts (NO EVENT CUTS TILL HERE!)
     //trigger
     if ( ! mMuEvent->triggerIdCollection().nominal().isTrigger(410008) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(410005) ) continue;
     //Vz
     if ( fabs(mMuEvent->primaryVertexPosition().z()) > 30.0 ) continue ;
     //Vr
     //if ( mMuEvent->primaryVertexPosition().perp() > 100.0 ) continue ;
     //VF failed (for some old dataset)
     //if ( fabs(mMuEvent->primaryVertexPosition().x()) < 1e-5 && fabs(mMuEvent->primaryVertexPosition().y()) < 1e-5 && fabs(mMuEvent->primaryVertexPosition().z()) < 1e-5 ) continue;

     chop_output<<mRunId<<'\t'<<mEvtId<<endl;
     
     /*
     //fill Event QA histograms
     TObjArray* tracks = muDstMaker->muDst()->primaryTracks() ;
     TObjArrayIter GetTracks(tracks) ;
     StMuTrack* gtrack ; 
     while ( ( gtrack = (StMuTrack*)GetTracks.Next() ) )
     {
	  //const StMuTrack * gtrack = track->globalTrack();
	  if(gtrack->nHits()<=15)continue;
	  if(gtrack->flag()<=0)continue;
	  if(abs(gtrack->charge())!=1) continue;
	  if(gtrack->pt()>0.5) continue;
	  if(fabs(gtrack->nSigmaProton())>2)continue;
	  hPhi->Fill(gtrack->phi());
	  hPhiFirstZ->Fill(gtrack->firstPoint().z(),gtrack->phi());
     }
     //end of the filling
     */
  }

  if (nEvents > 1) chain -> Finish() ;

  if(tags_output!=NULL) tags_output -> Write() ;
  if(tags_output!=NULL) tags_output -> Close() ;
  //flush(tags_output);
  delete tags_output;

  chop_output.close();
  // Cleanup
  delete chain ;
}
