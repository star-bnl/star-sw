// author: c.struck
//
// readL3Events.C
//
// exapmle macro for L3 tree of StEvent
// ====================================
//
//
// $Id: readStL3Trigger.C,v 1.1 2002/02/13 22:37:39 struck Exp $
//
// $Log: readStL3Trigger.C,v $
// Revision 1.1  2002/02/13 22:37:39  struck
// new version of readStEvent.C ==>> readStL3Trigger.C
//
                


#include "iostream.h"

// histos
TH1F *hnHitsOffline = new TH1F("hnHitsOffline", "nHits, global tracks", 42, 3.5, 45.5);
TH2F *hdEdx = new TH2F("hdEdx", "dEdx vs p", 1000, 0, 2.5, 1200, 0, 30);

TH1F *hpt = new TH1F("hpt", "pt", 200, 0, 7);

// track ntuple
TNtuple *trackNtuple = new TNtuple("trackNtuple", "l3t tracks",
				   "px:py:pz:r0:phi0:z0:q:nHits:dedx:nDedx:chi2xy:chi2sz");

// prototypes

// void readL3Events(Int_t nEvents, const Char_t ** fileList);
// void readL3Events(Int_t startEvent, Int_t nEvents, const Char_t **fileList);
// void readL3Events(Int_t startEvent, Int_t nEvents, const Char_t *file);
// void readL3Events(Int_t nEvents, const Char_t *file);


// ------------------ Here is the actual method -----------------------------------------
void readL3Events(Int_t nEvents, const Char_t **fileList)
{

  cout <<  endl << endl <<" readL3Events -  input # events = " << nEvents << endl;
  Int_t ilist=0;
  while (fileList[ilist]) {
        cout << " readL3Events -  input fileList = " << fileList[ilist] << endl;
	ilist++;
  }

  gSystem->Load("St_base");
  gSystem->Load("StarRoot");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("libglobal_Tables");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");

  // histos
  // TH1F *hnHits = new TH1F("hnHits", "nHits, global tracks", 42, 3.5, 45.5);
  // TH2F *hdEdx = new TH2F("hdEdx", "dEdx vs p", 1000, 0, 2.5, 1200, 0, 30);

  // create chain
  chain = new StChain("MyChain");

  // handle file list
  StFileI *setFiles =0;
  if (fileList) {
    setFiles= new StFile(fileList);
  }

  // connect to the .event.root file
  StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");	//deactivate all branches
  //StIOMaker *IOMk = new StIOMaker("IO","r",
  //                   "/star/rcf/pwg/spectra/struck/He3Events/st_physics_1231015_raw_0008.event.tpc_tracks.root",
  //                   "bfcTree");
  IOMk->SetBranch("eventBranch",0,"r");
  //IOMk->SetIOMode("r");


  // Initialize chain
  Int_t iInit = chain->Init();

  int istat = 0;
  int iEvent = 1;

  // Event loop
  while (iEvent<(nEvents+1) && istat!=2) { 

        // reset chain for upcoming event
        chain->Clear();

	// call Make only for StIOMaker
	istat = chain->Make(iEvent);
	cout << "\n\n====================== ";
	cout <<"Working on Event number " << (iEvent) << endl; 

	// get StEvent
	myEvent = (StEvent*) chain->GetInputDS("StEvent");
	if (!myEvent) {
	      cout <<"No StEvent found.\n";
	      break;
	}

	// get L0 trigger word
	StL0Trigger* myL0Trigger = myEvent->l0Trigger();
	if (!myL0Trigger) {
	      cout <<"No l0 trigger found.\n";
	      //break;
	}
	else printf("Trigger Word: 0x%x\n", myL0Trigger->triggerWord());

	// get L3 data
	myL3Trigger = (StL3Trigger*) myEvent->l3Trigger();
	if (!myL3Trigger) {
	      cout <<"No l3 found inside StEvent.\n";
	      iEvent++;
	      continue;
	}

	// get L3 event summary
	const StL3EventSummary* myL3EventSummary = myL3Trigger->l3EventSummary();
	if (!myL3EventSummary) {
	      cout << "No l3 event summary found." << endl;
	      continue;
	}

	// print summary info
	cout << "Global Counters:  #processed = " << myL3EventSummary->numberOfProcessedEvents()
	     << ",  #reconstructed = " << myL3EventSummary->numberOfReconstructedEvents()
	     << ",  #tracks = " << myL3EventSummary->numberOfTracks()
	     << endl;

	printf("L0 trigger word: %x\n", myL3EventSummary->l0TriggerWord());

	// check unbiased
	if (myL3EventSummary->unbiasedTrigger()==true)
	      cout << "unbiased Trigger: YES" << endl;
	else
	      cout << "unbiased Trigger: NO" << endl;

	// print algorithm info for all algorithms
	unsigned int nAlgorithms = myL3EventSummary->numberOfAlgorithms();
	cout << "Number of L3 algorithms for this run: " << nAlgorithms << endl;
	const StSPtrVecL3AlgorithmInfo& myL3AlgInfo = myL3EventSummary->algorithms(); 
	for (int i=0; i<nAlgorithms; i++) {
	      int algId = myL3AlgInfo[i]->id();
	      const int nProcessed = myL3AlgInfo[i]->numberOfProcessedEvents();
	      const int nAccept = myL3AlgInfo[i]->numberOfAcceptedEvents();
	      const int nBuild = myL3AlgInfo[i]->numberOfBuildEvents();
	      if (myL3AlgInfo[i]->build()) cout << "**";
	      cout << " alg id " << algId
		   << ":\t #proc " << nProcessed
		   << "\t #accept " << nAccept
		   << "\t #build " << nBuild 
		   << "\t preScale " << myL3AlgInfo[i]->preScale()
		   << "\t postScale " << myL3AlgInfo[i]->postScale()
		   << "\t intPara ";
	      for (int j=0; j<myL3AlgInfo[i]->intParameterSize(); j++) {
		    cout << myL3AlgInfo[i]->intParameter(j) << " ";
	      }
	      cout << "\t floatPara ";
	      for (int k=0; k<myL3AlgInfo[i]->floatParameterSize(); k++) {
		    cout << myL3AlgInfo[i]->floatParameter(k) << " ";
	      }
	      cout << endl;
	}

	// print triggered algorithms
	const StPtrVecL3AlgorithmInfo& myL3TriggerAlgInfo = myL3EventSummary->algorithmsAcceptingEvent();
	cout << "Number of L3 algorithms which triggered this event: "
	     << myL3TriggerAlgInfo->size() << endl;
	cout << "triggered algorithms: ";
	for (int i=0; i<myL3TriggerAlgInfo->size(); i++) {
	      cout << myL3TriggerAlgInfo[i]->id() << "  ";
	}
	cout << endl;

	// get L3 vertex
	if (myL3Trigger->primaryVertex()) {
	      cout << "L3 Vertex = ( " << myL3Trigger->primaryVertex()->position()->x()
		   << ", " << myL3Trigger->primaryVertex()->position()->y()
		   << ", " << myL3Trigger->primaryVertex()->position()->z()
		   << " )" << endl;
	}

	// Loop over tracks
	if (false) {
	      StSPtrVecTrackNode& mtracknodes = (StSPtrVecTrackNode&) myL3Trigger->trackNodes();
	      cout << " nubmer of tracks " << mtracknodes->size() << endl;
	      for (Int_t i=0; i<mtracknodes->size(); i++) {

		    Int_t id = i;
		    Float_t px = mtracknodes[i]->track(0)->geometry()->momentum()->x();
		    Float_t py = mtracknodes[i]->track(0)->geometry()->momentum()->y();
		    Float_t pz = mtracknodes[i]->track(0)->geometry()->momentum()->z();

		    Short_t nHits = mtracknodes[i]->track(0)->detectorInfo()->numberOfPoints();
		    Short_t q = mtracknodes[i]->track(0)->geometry()->charge();
		    Float_t psi = mtracknodes[i]->track(0)->geometry()->psi();
		    Float_t tanl = tan(mtracknodes[i]->track(0)->geometry()->dipAngle());
		    Float_t curv = mtracknodes[i]->track(0)->geometry()->curvature();

		    Float_t r0 = mtracknodes[i]->track(0)->geometry()->origin()->perp();
		    Float_t phi0 = mtracknodes[i]->track(0)->geometry()->origin()->phi();
		    Float_t z0 = mtracknodes[i]->track(0)->geometry()->origin()->z();

		    Float_t dedx = 0;
		    Short_t nDedx = 0;

		    Float_t chi2xy = (Float_t) mtracknodes[i]->track(0)->fitTraits()->chi2(0) / nHits;
		    Float_t chi2sz = (Float_t) mtracknodes[i]->track(0)->fitTraits()->chi2(1) / nHits;

		    StSPtrVecTrackPidTraits &traits = mtracknodes[i]->track(0)->pidTraits();
		    StDedxPidTraits *pid;
		    if (traits.size()) {
		          pid = dynamic_cast<StDedxPidTraits*>(traits[0]);
			  if (pid) {
			        dedx = pid->mean();
				nDedx = pid->numberOfPoints();
			  }
		    }

		    trackNtuple->Fill(px, py, pz, r0, phi0, z0,
				      (Float_t) q, (Float_t) nHits, dedx, (Float_t) nDedx, chi2xy, chi2sz);

		    Float_t pt = sqrt(px*px +py*py);

	      
		    hnHitsOffline->Fill(nHits);

		    hpt->Fill(pt);


		    if (nHits>22 && dedx>0) {
		          hdEdx->Fill(mtracknodes[i]->track(0)->geometry()->momentum()->mag(), dedx);
		    }


		    if (i<5 || i%1000==0) {
		      cout << "L3 Track : " << i << "\t";
		      cout << "px : " << Double_t (mtracknodes[i]->track(0)->geometry()->momentum()->x()) << "\t";
		      cout << "py : " << Double_t (mtracknodes[i]->track(0)->geometry()->momentum()->y()) << "\t";
		      cout << "pz : " << Double_t ( mtracknodes[i]->track(0)->geometry()->momentum()->z()) << "\t";
// 	      cout << "x : " << mtracknodes[i]->track(0)->geometry()->origin()->x() << "\t";
// 	      cout << "y : " << mtracknodes[i]->track(0)->geometry()->origin()->y() << "\t";
// 	      cout << "z : " << mtracknodes[i]->track(0)->geometry()->origin()->z() << "\n";
// 	      cout << "chi^2_xy : " << mtracknodes[i]->track(0)->fitTraits()->chi2(0);
// 	      cout << "    chi^2_sz : " << mtracknodes[i]->track(0)->fitTraits()->chi2(1) << "\n";
		      cout << "dca : " << mtracknodes[i]->track(0)->impactParameter() << "\t";
		      cout << "dedx : " << dedx << "\n";
		    }
	      } // for (i<mtracknodes)
	}

	// check hits
// 	int totalNHits = 0;
// 	StTpcHitCollection*  myTpcHitColl;

// 	myTpcHitColl = ml3trigger->tpcHitCollection();

// 	for (iSec=0; iSec<24; iSec++) {
// 	      cout << " sector: " << iSec 
// 		   << "--> #Hits: " << myTpcHitColl->sector(iSec)->numberOfHits()
// 		   << endl;
// 	      totalNHits += myTpcHitColl->sector(iSec)->numberOfHits();
// 	}
// 	cout << " total #hits: " << totalNHits << endl;

	iEvent++;

  } // while (iEvent<nEvents && istat!=2)

//   TFile *f = new TFile("L3NTuple.root","recreate");
//   trackNtuple->Write();
//   f->Close();


}


//--------------------------------------------------------------------------

void readL3Events(const Int_t nEvents, const Char_t *file)
{
  cout << "*file = " << file << endl;
  const char *fileListQQ[]={0,0};
  fileListQQ[0]=file;
  readL3Events(nEvents, fileListQQ);
}
