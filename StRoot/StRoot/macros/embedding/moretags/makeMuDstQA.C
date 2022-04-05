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
#include "TDatime.h"
#include "StarRoot/TUnixTime.h"
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
  Int_t mnRefMult, mnRefMult2, mnRefMult3, mnRefMult4, mngRefMult, mnTofMatch;
  Double_t mVX, mVY, mVZ;
  Double_t mVXsigma, mVYsigma, mVZsigma;
  Float_t mVpdVz;
  Float_t mPVRank;
  Double_t mEvtTime, mProdTime, mmagField;
  TTree *mMoreTagsTree = new TTree("MoreTags","MoreTags");
  mMoreTagsTree->Branch("RunId",&mRunId,"RunId/I");
  mMoreTagsTree->Branch("EvtId",&mEvtId,"EvtId/I");
  mMoreTagsTree->Branch("EvtTime",&mEvtTime,"EvtTime/D");
  mMoreTagsTree->Branch("ProdTime",&mProdTime,"ProdTime/D");
  mMoreTagsTree->Branch("magField",&mmagField,"magField/D");
  mMoreTagsTree->Branch("nRefMult",&mnRefMult,"nRefMult/I");
  mMoreTagsTree->Branch("nRefMult2",&mnRefMult2,"nRefMult2/I");
  mMoreTagsTree->Branch("nRefMult3",&mnRefMult3,"nRefMult3/I");
  mMoreTagsTree->Branch("nRefMult4",&mnRefMult4,"nRefMult4/I");
  mMoreTagsTree->Branch("ngRefMult",&mngRefMult,"ngRefMult/I");
  mMoreTagsTree->Branch("nTofMatch",&mnTofMatch,"nTofMatch/I");
  mMoreTagsTree->Branch("VX",&mVX,"VX/D");
  mMoreTagsTree->Branch("VY",&mVY,"VY/D");
  mMoreTagsTree->Branch("VZ",&mVZ,"VZ/D");
  mMoreTagsTree->Branch("VXsigma",&mVXsigma,"VXsigma/D");
  mMoreTagsTree->Branch("VYsigma",&mVYsigma,"VYsigma/D");
  mMoreTagsTree->Branch("VZsigma",&mVZsigma,"VZsigma/D");
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

     //-----------------------------------------------------------------------------
     //vertex selection in StPicoDstMaker
     enum PicoVtxMode {NotSet=0, Default=1, Vpd=2, VpdOrDefault=3};
     PicoVtxMode mVtxMode;

     //MUST assign this line!!!
     mVtxMode = VpdOrDefault;
     const double mTpcVpdVzDiffCut = 3;

     int const originalVertexId = mMuDst->currentVertexIndex();

     StMuPrimaryVertex* selectedVertex = nullptr;

     if (mVtxMode == Default) {
	  // choose the default vertex, i.e. the first vertex
	  mMuDst->setVertexIndex(0);
	  selectedVertex = mMuDst->primaryVertex();
     }
     else if (mVtxMode == Vpd || mVtxMode == VpdOrDefault) {

	  if(mVtxMode == VpdOrDefault) {
	     mMuDst->setVertexIndex(0);
	     selectedVertex = mMuDst->primaryVertex();
	  }

	  //StBTofHeader const* mBTofHeader = mMuDst->btofHeader();

	  if (mBTofHeader && fabs(mBTofHeader->vpdVz()) < 200) {
	     float vzVPD = mBTofHeader->vpdVz();

	     for (unsigned int iVtx = 0; iVtx < mMuDst->numberOfPrimaryVertices(); ++iVtx) {
		  StMuPrimaryVertex* vtx = mMuDst->primaryVertex(iVtx);
		  if (!vtx) continue;

		  if (fabs(vzVPD - vtx->position().z()) < mTpcVpdVzDiffCut) {
		     mMuDst->setVertexIndex(iVtx);
		     selectedVertex = mMuDst->primaryVertex();
		     break;
		  } //if (fabs(vzVPD - vtx->position().z()) < mTpcVpdVzDiffCut)
	     } //for (unsigned int iVtx = 0; iVtx < mMuDst->numberOfPrimaryVertices(); ++iVtx)
	  } //if (mBTofHeader && fabs(mBTofHeader->vpdVz()) < 200)
     } //else if (mVtxMode == Vpd || mVtxMode == VpdOrDefault)
     else { // default case
	  LOG_ERROR << "Pico Vtx Mode not set!" << endm;
     }

     // fall back to default vertex if no vertex is selected in the algorithm above.
     // should skip this event in the event cuts below.
     if ( ! selectedVertex ){
	  LOG_INFO << "Vertex is not valid" << endm;
	  //cout<<originalVertexId<<endl;
	  mMuDst->setVertexIndex(originalVertexId);
     }
     //end of vertex selection
     //------------------------------------------------------------------------------

     mRunId = mMuEvent->runNumber();
     mEvtId = mMuEvent->eventNumber();

     //add eventtime and prodtime to moretags.root, for HFT embedding
     //cout<<mRunId<<" "<<mEvtId<<" "<<mMuEvent->eventInfo().time()<<" "<<mMuEvent->runInfo().productionTime()<<endl;
     TDatime EventTime, ProdTime;
     TUnixTime unixTime(mMuEvent->eventInfo().time());
     Int_t dat=0,tim=0;
     unixTime.GetGTime(dat,tim);
     EventTime.Set(dat,tim);
     unixTime.SetUTime(mMuEvent->runInfo().productionTime());
     unixTime.GetGTime(dat,tim);
     ProdTime.Set(dat,tim);
     mEvtTime = EventTime.GetDate() + EventTime.GetTime()/1000000.;
     mProdTime= ProdTime.GetDate() +  ProdTime.GetTime()/1000000. ;
     //cout<<EventTime.GetDate() + EventTime.GetTime()/1000000.<<" "<<ProdTime.GetDate() +  ProdTime.GetTime()/1000000. <<endl;

     mmagField = mMuEvent->runInfo().magneticField();

     mnRefMult = mMuEvent->refMult();

     Int_t nTofMatPrTrack = 0;
     Int_t nprTrack2 = 0;
     Int_t nprTrack3 = 0;
     Int_t nprTrack4 = 0;
     TObjArray* prtracks = muDstMaker->muDst()->primaryTracks() ;    // Create a TObject array containing the global tracks  
     TObjArrayIter GetPrTracks(prtracks) ;                              // Create an iterator to step through the tracks  
     StMuTrack* prtrack ;                                             // Pointer to a track
     while ( ( prtrack = (StMuTrack*)GetPrTracks.Next() ) )             // Main loop for Iterating over tracks
     {
	  if(prtrack->btofPidTraits().matchFlag()) nTofMatPrTrack ++;

	  if (prtrack->flag() < 0 || fabs(prtrack->momentum().mag()) < 1.e-10
		  || prtrack->dca().mag() > 3 || fabs(prtrack->momentum().pseudoRapidity()) > 1) continue;
	  double const eta = prtrack->momentum().pseudoRapidity() ;
	  double const beta = prtrack->btofPidTraits().beta();
	  double const mass2 = beta <= 1.e-5 ? -999. : prtrack->momentum().mag2() * (std::pow(1. / beta, 2) - 1);
	  if (prtrack->nHitsFit(kTpcId) >= 10) {
	     // refMult2 definition in PicoEvent: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 10 && abs(eta) > 0.5 && abs(eta) < 1
	     if (fabs(eta) > 0.5) nprTrack2 += 1;
	     // refMult3 definition in PicoEvent: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 10 && abs(eta) < 1 && Exclude protons
	     if (prtrack->nSigmaProton() < -3. && mass2 < 0.4)  nprTrack3 += 1;
	  }
	  if (prtrack->nHitsFit(kTpcId) >= 15) {
	     // refMult4 definition in PicoEvent: pt> 0.1 && abs(dca) < 3 && nHitsTpc >= 15 && abs(eta) < 1 && Exclude kaons
	     if ((mass2 <= -990. && fabs(prtrack->nSigmaKaon()) > 3) || // tof is not available
		     (mass2 >  -990. && (mass2 > 0.6 || mass2 < 0.1)))    // tof is available
		  nprTrack4 += 1;
	  }
     }
     mnTofMatch = nTofMatPrTrack;
     mnRefMult2 = nprTrack2;
     mnRefMult3 = nprTrack3;
     mnRefMult4 = nprTrack4;

     mVX = mMuEvent->primaryVertexPosition().x();
     mVY = mMuEvent->primaryVertexPosition().y();
     mVZ = mMuEvent->primaryVertexPosition().z();
     mVXsigma = mMuEvent->primaryVertexErrors().x();
     mVYsigma = mMuEvent->primaryVertexErrors().y();
     mVZsigma = mMuEvent->primaryVertexErrors().z();

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

     //Comment out this line for HFT embedding!
     mMoreTagsTree->Fill();

     //Event info (for debug)
     //cout<<"Run#: "<<mMuEvent->runNumber()<<endl;
     //cout<<"Evt#: "<<mMuEvent->eventNumber()<<endl;
     //cout<<muDstMaker->muDst()->currentVertexIndex()<<endl;
     //cout<<"refmult: "<<mMuEvent->refMult()<<endl;

     //Event cuts (NO EVENT CUTS TILL HERE!)
     //vertex is not selected (only for PicoVtxMode::Vpd)
     if ( ! selectedVertex ) continue;
     //trigger
     if ( ! mMuEvent->triggerIdCollection().nominal().isTrigger(520001) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(520011) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(520021) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(520031) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(520041) && ! mMuEvent->triggerIdCollection().nominal().isTrigger(520051) ) continue ;
     //Vz
     if ( fabs(mMuEvent->primaryVertexPosition().z()) > 6.0 ) continue ;
     if ( fabs(mMuEvent->primaryVertexPosition().z() - vzVpd) > 3.0 ) continue ;
     //Vr
     //if ( mMuEvent->primaryVertexPosition().perp() > 2.0 ) continue ;
     //pileup cut
     //if ( mnTofMatch <= 0.46*mnRefMult - 10 ) continue ;
     //VF failed (for some old dataset)
     //if ( fabs(mMuEvent->primaryVertexPosition().x()) < 1e-5 && fabs(mMuEvent->primaryVertexPosition().y()) < 1e-5 && fabs(mMuEvent->primaryVertexPosition().z()) < 1e-5 ) continue;

     chop_output<<mRunId<<'\t'<<mEvtId<<endl;
     //for HFT ONLY! PrepEmbd will not be used, event cuts are done here!
     //mMoreTagsTree->Fill();
     
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
