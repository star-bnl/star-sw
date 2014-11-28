//
//
// author: struck
//
// read rare tree and fill histos
#include "StReadRare.h"
#include "StRareEvent.h"
#include "StRareTrack.h"
#include "StL3RareTrack.h"
#include "StEventTypes.h"

ClassImp(StReadRare)


StReadRare::StReadRare()
{
  mNEvents = 1000000;

  // L3 trigger cut parameters
  mCutFraction = 0.8;
  mMinHits = 27;
  mMaxDCA = 5;
  mMinP = 0.5;
  mMaxDedx = 60;

  // Offline cut parameters
  mMinHitsOffline = 29;
  mDEdxnHitsRatio = 0.3;

  //reset chain
  mChain = 0;

  // histo file
  mHistoFile = new TString("/direct/star+data01/pwg/spectra/struck/2001/RarePlots.root");
}


StReadRare::StReadRare(Int_t nEvents, Char_t* histoFileName)
{
  mNEvents = nEvents;

  // L3 trigger cut parameters
  mCutFraction = 0.8;
  mMinHits = 27;
  mMaxDCA = 5;
  mMinP = 0.5;
  mMaxDedx = 60;

  // Offline cut parameters
  mMinHitsOffline = 29;
  mDEdxnHitsRatio = 0.3;

  //reset chain
  mChain = 0;

  // histo file
  mHistoFile = new TString(histoFileName);
}


void StReadRare::Init()
{
  //   Histograms
  hOffP = new TH1F("hOffP", "offline p", 100, 0, 10);
  hL3P = new TH1F("hL3P", "L3 p", 100, 0, 10);

  hL3Dca = new TH1F("hL3Dca", "dca l3 tracks", 400, 0, 20);

  hOffdEdx = new TH2F("hOffdEdx", "dedx offline tracks", 1000, 0, 2.5, 1200, 0, 50);
  hL3dEdx = new TH2F("hL3dEdx", "dedx l3 tracks", 1000, 0, 2.5, 1200, 0, 50);

  hOffZ = new TH1F("hOffZ", "Z=ln(I_m/I_expect(anti-He3)), offline", 60, -3, 3);
  hL3Z = new TH1F("hL3Z", "Z=ln(I_m/I_expect(anti-He3)), L3", 60, -3, 3);

  hCandidateMatch = new TH1F("hCandidateMatch", "0:offline-L3 match, -1:offline only, +1:L3 only", 10, -2.5, 2.5);
  hOffCandidatedEdx = new TH2F("hOffCandidatedEdx", "dedx offline candidate tracks", 800, 0, 5, 1200, 0, 60);
  hOffCandidateZ = new TH1F("hOffCandidateZ", "Z=ln(I_m/I_expect(anti-He3)), offline candidate", 60, -3, 3);
  hL3CandidatedEdx = new TH2F("hL3CandidatedEdx", "dedx L3 candidate tracks", 800, 0, 5, 1200, 0, 60);
  hL3CandidatedEdxMatch = new TH2F("hL3CandidatedEdxMatch", "dedx L3 candidate tracks, match to offline", 800, 0, 5, 1200, 0, 60);
  hL3CandidatedEdxTriggeredMatch = new TH2F("hL3CandidatedEdxTriggeredMatch", "dedx L3 candidate tracks, match to offline, triggered", 800, 0, 5, 1200, 0, 60);
  hL3CandidateDca = new TH1F("hL3CandidateDca", "dca l3 candidate tracks", 400, 0, 10);
  hL3CandidateZ = new TH1F("hL3CandidateZ", "Z=ln(I_m/I_expect(anti-He3)), l3 candidate tracks", 60, -3, 3);

  
  // generate chain
  mChain = new TChain("RareTree");
  mChain->Add("/direct/star+data02/scratch/struck/test/new291/st_phys*.root");
  mChain->Add("/direct/star+data02/scratch/struck/test/new292/st_phys*.root");
  mChain->Add("/direct/star+data02/scratch/struck/test/new294/st_phys*.root");
  //mChain->Add("data/new291/st_physics_2291023_raw_0012.rareTree.root");
  //mChain->Add("data/new291/st_physics_2291032_raw_0012.rareTree.root");
  //mChain->Add("data/new292/st_physics_2292004_raw_0012.rareTree.root");
  //mChain->Add("/direct/star+data01/pwg/spectra/struck/2001/RareEventTest.root");
  //mChain->Add("RareEvent.root");

  // set number of events
  mNEvents = (Int_t) mChain->GetEntries();
  printf("Total # events: %i\n", mNEvents);
}


Int_t StReadRare::Run()
{
   StRareEvent *event = new StRareEvent();   //we create the event object once outside the loop
   mChain->SetBranchAddress("StRareEvent", &event);
   printf("StReadRare::Start Run.\nTotal # events: %i\n", mNEvents);

   for (int i=0; i<mNEvents; i++) {
         printf("Event:%d\n",i);
	 mChain->GetEvent(i);
	 //printf("  nTracks: %i;  L3: %i\n",
	 //	event->numberOfGoodPrimaryTracks(),
	 //	event->numberOfL3Tracks());

	 if (event->l3Flag()==kTRUE) {
	       printf("  event TRIGGERED!!\n");
	 }

	 int candidateIndex = 0;
	 bool candidateEvent = kFALSE;

	 // offline tracks
	 for (int j=0; j<event->numberOfTracks(); j++) {
	       StRareTrack *track = (StRareTrack* )event->getTracks()->At(j);
	       float p = track->p();
	       float dedx = track->dedx();
	       hOffP->Fill(p);
	       if (dedx>0 
		   && (track->npntfit()>=mMinHitsOffline)
		   && track->chargesign()<0
		   && (((float)track->ndedx())/((float)track->npntfit())>=mDEdxnHitsRatio)) {
		     // dE/dx spectrum, all tracks
		     hOffdEdx->Fill(p, dedx);
		     // Z, all negative tracks
		     float ionExpected = 2 * 4.598 * (1 + 7.89177 / (4 * p*p));
		     hOffZ->Fill(TMath::Log(dedx/ionExpected));
		     // candidate tracks
		     if (dedx>(mCutFraction * ionExpected)) {
		           hOffCandidatedEdx->Fill(p, dedx);
			   hOffCandidateZ->Fill(TMath::Log(dedx/ionExpected));
			   candidateIndex--;
			   candidateEvent = kTRUE;
		     }
	       }
	 } // offline tracks

	 // L3 tracks
	 for (int j=0; j<event->numberOfL3Tracks(); j++) {
	       StL3RareTrack *l3track = (StL3RareTrack* )event->getL3Tracks()->At(j);
	       float p = l3track->p();
	       float dedx = l3track->dedx();
	       hL3P->Fill(p);
	       hL3Dca->Fill(l3track->dca2d());
	       if (dedx>0
		   && (l3track->npntfit()>=mMinHits)
		   && (l3track->chargesign()<0)
		   && (l3track->dca2d()<mMaxDCA)) {
		     // dE/dx spectrum, all tracks
		     hL3dEdx->Fill(p, dedx);
		     // Z, all negative tracks
		     float ionExpected = 2 * 4.598 * (1 + 7.89177 / (4 * p*p));
		     hL3Z->Fill(TMath::Log(dedx/ionExpected));
		     // candidate tracks
		     if ((p>mMinP)
			 && (dedx<mMaxDedx)
			 && (dedx>(mCutFraction*ionExpected))) {
		           hL3CandidatedEdx->Fill(p, dedx);
			   hL3CandidateZ->Fill(TMath::Log(dedx/ionExpected));
			   // match to offline?
			   if (candidateEvent==kTRUE) {
			         hL3CandidatedEdxMatch->Fill(p, dedx);
				 // and triggered?
				 if (event->l3Flag()==kTRUE)
				       hL3CandidatedEdxTriggeredMatch->Fill(p, dedx);  
			   }
			   candidateIndex++;
			   candidateEvent = kTRUE;
			   hL3CandidateDca->Fill(l3track->dca2d());
		     }
	       }
	 } // l3 tracks

	 // fill 'candidate-match' histogram
	 if (candidateEvent==kTRUE)
	       hCandidateMatch->Fill(candidateIndex);

      } // event loop
      return 0;
}


Int_t StReadRare::Finish()
{
   TFile *rFile = new TFile(mHistoFile->Data(),"RECREATE");
   hOffP->Write();
   hL3P->Write();
   hL3Dca->Write();
   hOffdEdx->Write();
   hL3dEdx->Write();
   hOffZ->Write();
   hL3Z->Write();
   hCandidateMatch->Write();
   hOffCandidatedEdx->Write();
   hOffCandidateZ->Write();
   hL3CandidatedEdx->Write();
   hL3CandidatedEdxMatch->Write();
   hL3CandidatedEdxTriggeredMatch->Write();
   hL3CandidateDca->Write();
   hL3CandidateZ->Write();
   rFile->Close();

   return 0;
}
