#include <vector>

#include "TTree.h"
#include "TFile.h"
#include "TChain.h"

#include "StarClassLibrary/StThreeVectorF.hh"
#include "StarClassLibrary/StLorentzVectorF.hh"
#include "StPicoDstMaker/StPicoDst.h"
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoDstMaker/StPicoEvent.h"
#include "StPicoDstMaker/StPicoTrack.h"
#include "StPicoDstMaker/StPicoBTofPidTraits.h"
#include "StPicoPrescales/StPicoPrescales.h"

#include "StHFCuts.h"
#include "StHFHists.h"
#include "StPicoHFEvent.h"
#include "StPicoHFMaker.h"
#include "StHFPair.h"
#include "StHFTriplet.h"

ClassImp(StPicoHFMaker)

// _________________________________________________________
StPicoHFMaker::StPicoHFMaker(char const* name, StPicoDstMaker* picoMaker, 
			     char const* outputBaseFileName,  char const* inputHFListHFtree = "") :
  StMaker(name), mPicoDst(NULL), mHFCuts(NULL), mHFHists(NULL), mPicoHFEvent(NULL), mBField(0.), mOutList(NULL),
  mDecayMode(StPicoHFEvent::kTwoParticleDecay), mMakerMode(StPicoHFMaker::kAnalyze), mMcMode(false),
  mOutputTreeName("picoHFtree"), mOutputFileBaseName(outputBaseFileName), mInputFileName(inputHFListHFtree),
  mPicoDstMaker(picoMaker), mPicoEvent(NULL), mTree(NULL), mHFChain(NULL), mEventCounter(0), 
  mOutputFileTree(NULL), mOutputFileList(NULL) {
  // -- constructor
}

// _________________________________________________________
StPicoHFMaker::~StPicoHFMaker() {
   // -- destructor 
  
  if (mHFCuts)
    delete mHFCuts;
  mHFCuts = NULL;

  /* mTree is owned by mOutputFile directory, it will be destructed once
   * the file is closed in ::Finish() */
}

// _________________________________________________________
Int_t StPicoHFMaker::Init() {
  // -- Inhertited from StMaker 
  //    NOT TO BE OVERWRITTEN by daughter class
  //    daughter class should implement InitHF()

  // -- check for cut class
  if (!mHFCuts)
    mHFCuts = new StHFCuts;
  mHFCuts->init();

  // -- create HF event - using the proper decay mode to initialize
  mPicoHFEvent = new StPicoHFEvent(mDecayMode);
 
  // -- READ ------------------------------------
  if (mMakerMode == StPicoHFMaker::kRead) {

    if (!mHFChain) {
      mHFChain = new TChain("T");
      std::ifstream listOfFiles(mInputFileName.Data());
      if (listOfFiles.is_open()) {
	std::string file;
	while (getline(listOfFiles, file)) {
	  LOG_INFO << " StPicoHFMaker - Adding :" << file << endm;
	  mHFChain->Add(file.c_str());
	}
      }
      else { 
	LOG_ERROR << " StPicoHFMaker - Could not open list of files. ABORT!" << endm;
	return kStErr;
      }
    } // if (mMakerMode == StPicoHFMaker::kRead) {

    mHFChain->GetBranch("hfEvent")->SetAutoDelete(kFALSE);
    mHFChain->SetBranchAddress("hfEvent", &mPicoHFEvent);
  }
  
  // -- file which holds list of histograms
  mOutputFileList = new TFile(Form("%s.%s.root", mOutputFileBaseName.Data(), GetName()), "RECREATE");
  mOutputFileList->SetCompressionLevel(1);

  if (mMakerMode == StPicoHFMaker::kWrite) {
    mOutputFileTree = new TFile(Form("%s.%s.root", mOutputFileBaseName.Data(), mOutputTreeName.Data()), "RECREATE");
    mOutputFileTree->SetCompressionLevel(1);
    mOutputFileTree->cd();

    // -- create OutputTree
    int BufSize = (int)pow(2., 16.);
    int Split = 1;
    if (!mTree) 
      mTree = new TTree("T", "T", BufSize);
    mTree->SetAutoSave(1000000); // autosave every 1 Mbytes
    mTree->Branch("hfEvent", "StPicoHFEvent", &mPicoHFEvent, BufSize, Split);
  } // if (mMakerMode == StPicoHFMaker::kWrite) {

  // -- disable automatic adding of objects to file
  bool oldStatus = TH1::AddDirectoryStatus();
  TH1::AddDirectory(false);

  // -- add list which holds all histograms  
  mOutList = new TList();
  mOutList->SetName(GetName());
  mOutList->SetOwner(true);

  // -- create event stat histograms
  initializeEventStats();

  // -- initialize histogram class
  mHFHists = new StHFHists(Form("hfHists_%s",GetName()));
  mHFHists->init(mOutList,mDecayMode);

  // -- call method of daughter class
  InitHF();

  TH1::AddDirectory(oldStatus);

  // -- reset event to be in a defined state
  resetEvent();

  return kStOK;
}

// _________________________________________________________
Int_t StPicoHFMaker::Finish() {
  // -- Inhertited from StMaker 
  //    NOT TO BE OVERWRITTEN by daughter class
  //    daughter class should implement FinishHF()

  if (mMakerMode == StPicoHFMaker::kWrite) {
    mOutputFileTree->cd();
    mOutputFileTree->Write();
    mOutputFileTree->Close();
  }

  mOutputFileList->cd();
  mOutList->Write(mOutList->GetName(), TObject::kSingleKey);
  
  // -- call method of daughter class
  FinishHF();
  
  mOutputFileList->Close();

  return kStOK;
}

// _________________________________________________________
void StPicoHFMaker::resetEvent() {
  // -- reset event

  mIdxPicoPions.clear();
  mIdxPicoKaons.clear();
  mIdxPicoProtons.clear();
  
  mPicoHFEvent->clear("C");
}

// _________________________________________________________
void StPicoHFMaker::Clear(Option_t *opt) {
  // -- Inhertited from StMaker 
  //    NOT TO BE OVERWRITTEN by daughter class
  //    daughter class should implement ClearHF()

  // -- call method of daughter class
  ClearHF();

  resetEvent();
}

// _________________________________________________________
Int_t StPicoHFMaker::Make() {
  // -- Inhertited from StMaker 
  //    NOT TO BE OVERWRITTEN by daughter class
  //    daughter class should implement MakeHF()
  // -- isPion, isKaon, isProton methods are to be 
  //    implemented by daughter class (
  //    -> methods of StHFCuts can and should be used

  if (!mPicoDstMaker) {
    LOG_WARN << " StPicoHFMaker - No PicoDstMaker! Skip! " << endm;
    return kStWarn;
  }

  mPicoDst = mPicoDstMaker->picoDst();
  if (!mPicoDst) {
    LOG_WARN << " StPicoHFMaker - No PicoDst! Skip! " << endm;
    return kStWarn;
  }
  
  // -- read in HF tree
  if (mMakerMode == StPicoHFMaker::kRead) {
    mHFChain->GetEntry(mEventCounter++);

    if (mPicoHFEvent->runId() != mPicoDst->event()->runId() || mPicoHFEvent->eventId() != mPicoDst->event()->eventId()) {
      LOG_ERROR <<" StPicoHFMaker - !!!!!!!!!!!! ATTENTION !!!!!!!!!!!!!"<<endm;
      LOG_ERROR <<" StPicoHFMaker - SOMETHING TERRIBLE JUST HAPPENED. StPicoEvent and StPicoHFEvent are not in sync."<<endm;
      exit(1);
    }
  } // if (mMakerMode == StPicoHFMaker::kRead) {
  
  Int_t iReturn = kStOK;

  // cout << " nTracks   " <<  mPicoDst->numberOfTracks() << endl;
  // if (mMcMode) {
  //   cout << " nMcTracks " <<  mPicoDst->numberOfMcTracks() << endl;
  // }
  
  if (setupEvent()) {
    UInt_t nTracks = mPicoDst->numberOfTracks();

    // -- Fill vectors of particle types
    if (mMakerMode == StPicoHFMaker::kWrite || mMakerMode == StPicoHFMaker::kAnalyze) {
      for (unsigned short iTrack = 0; iTrack < nTracks; ++iTrack) {
	StPicoTrack* trk = mPicoDst->track(iTrack);

	if (!trk || !mHFCuts->isGoodTrack(trk)) continue;

	if (isPion(trk))   mIdxPicoPions.push_back(iTrack);   // isPion method to be implemented by daughter class
	if (isKaon(trk))   mIdxPicoKaons.push_back(iTrack);   // isKaon method to be implemented by daughter class
	if (isProton(trk)) mIdxPicoProtons.push_back(iTrack); // isProton method to be implemented by daughter class
      
      } // .. end tracks loop
    } // if (mMakerMode == StPicoHFMaker::kWrite || mMakerMode == StPicoHFMaker::kAnalyze) {

    // -- call method of daughter class
    iReturn = MakeHF();

    // -- fill basic event histograms - for good events
    mHFHists->fillGoodEventHists(*mPicoEvent, *mPicoHFEvent);

  } // if (setupEvent()) {
  
  // -- save information about all events, good or bad
  if (mMakerMode == StPicoHFMaker::kWrite)
    mTree->Fill();
  
  // -- fill basic event histograms - for all events
  mHFHists->fillEventHists(*mPicoEvent, *mPicoHFEvent);

  // -- reset event to be in a defined state
  resetEvent();
  
  return (kStOK && iReturn);
}

// _________________________________________________________
void StPicoHFMaker::createTertiaryK0Shorts() {
  // -- Create candidate for tertiary K0shorts

  for (unsigned short idxPion1 = 0; idxPion1 < mIdxPicoPions.size(); ++idxPion1) {
    StPicoTrack const * pion1 = mPicoDst->track(mIdxPicoPions[idxPion1]);

    for (unsigned short idxPion2 = idxPion1+1 ; idxPion2 < mIdxPicoPions.size(); ++idxPion2) {
      StPicoTrack const * pion2 = mPicoDst->track(mIdxPicoPions[idxPion2]);      

      if (mIdxPicoPions[idxPion1] == mIdxPicoPions[idxPion2]) 
	continue;

      StHFPair candidateK0Short(pion1, pion2, 
				mHFCuts->getHypotheticalMass(StHFCuts::kPion), mHFCuts->getHypotheticalMass(StHFCuts::kPion),
				mIdxPicoPions[idxPion1], mIdxPicoPions[idxPion2], 
				mPrimVtx, mBField, false);

      if (!mHFCuts->isGoodTertiaryVertexPair(candidateK0Short)) 
	continue;

      mPicoHFEvent->addHFTertiaryVertexPair(&candidateK0Short);

      // -- fill tertiary pair histograms
      mHFHists->fillTertiaryPairHists(&candidateK0Short, kTRUE);
    }
  }
}

// _________________________________________________________
void StPicoHFMaker::createTertiaryLambdas() {
  // -- Create candidate for tertiary Lambdas

  for (unsigned short idxProton = 0; idxProton < mIdxPicoProtons.size(); ++idxProton) {
    StPicoTrack const * proton = mPicoDst->track(mIdxPicoProtons[idxProton]);

    for (unsigned short idxPion = 0 ; idxPion < mIdxPicoPions.size(); ++idxPion) {
      StPicoTrack const * pion = mPicoDst->track(mIdxPicoPions[idxPion]);      

      if (mIdxPicoPions[idxProton] == mIdxPicoPions[idxPion]) 
	continue;

      StHFPair lambda(proton, pion, 
		      mHFCuts->getHypotheticalMass(StHFCuts::kProton), mHFCuts->getHypotheticalMass(StHFCuts::kPion),
		      mIdxPicoProtons[idxProton], mIdxPicoPions[idxPion], 
		      mPrimVtx, mBField, false);

      if (!mHFCuts->isGoodTertiaryVertexPair(lambda)) 
	continue;

      mPicoHFEvent->addHFTertiaryVertexPair(&lambda);

      // -- fill tertiary pair histograms
      mHFHists->fillTertiaryPairHists(&lambda, kTRUE);
    }
  }
}

// _________________________________________________________
bool StPicoHFMaker::setupEvent() {
  // -- fill members from pico event, check for good eventa and fill event statistics

  mPicoEvent = mPicoDst->event();
  mPicoHFEvent->addPicoEvent(*mPicoEvent);
  
  mBField = mPicoEvent->bField();
  mPrimVtx = mPicoEvent->primaryVertex();
  
  int aEventStat[mHFCuts->eventStatMax()];
  
  bool bResult = mHFCuts->isGoodEvent(mPicoDst, aEventStat);

  // -- fill event statistics histograms
  fillEventStats(aEventStat);

  return bResult;
}

// _________________________________________________________
void StPicoHFMaker::initializeEventStats() {
  // -- Initialize event statistics histograms
  
  const char *aEventCutNames[]   = {"all", "good run", "trigger", "#it{v}_{z}", "#it{v}_{z}-#it{v}^{VPD}_{z}", "accepted"};

  mOutList->Add(new TH1F("hEventStat0","Event cut statistics 0;Event Cuts;Events", mHFCuts->eventStatMax(), -0.5, mHFCuts->eventStatMax()-0.5));
  TH1F *hEventStat0 = static_cast<TH1F*>(mOutList->Last());

  mOutList->Add(new TH1F("hEventStat1","Event cut statistics 1;Event Cuts;Events", mHFCuts->eventStatMax(), -0.5, mHFCuts->eventStatMax()-0.5));
  TH1F *hEventStat1 = static_cast<TH1F*>(mOutList->Last());

  for (unsigned int ii = 0; ii < mHFCuts->eventStatMax(); ii++) {
    hEventStat0->GetXaxis()->SetBinLabel(ii+1, aEventCutNames[ii]);
    hEventStat1->GetXaxis()->SetBinLabel(ii+1, aEventCutNames[ii]);
  }

  //  hEventStat0->GetXaxis()->SetBinLabel(fHEventStatMax, Form("Centrality [0-%s]%%", aCentralityMaxNames[9-1]));
  //  hEventStat1->GetXaxis()->SetBinLabel(fHEventStatMax, Form("Centrality [0-%s]%%", aCentralityMaxNames[9-1]));
}

//________________________________________________________________________
void StPicoHFMaker::fillEventStats(int *aEventStat) {
  // -- Fill event statistics 

  TH1F *hEventStat0 = static_cast<TH1F*>(mOutList->FindObject("hEventStat0"));
  TH1F *hEventStat1 = static_cast<TH1F*>(mOutList->FindObject("hEventStat1"));

  for (unsigned int idx = 0; idx < mHFCuts->eventStatMax() ; ++idx) {
    if (!aEventStat[idx])
      hEventStat0->Fill(idx);
  }
  
  for (unsigned int idx = 0; idx < mHFCuts->eventStatMax(); ++idx) {
    if (aEventStat[idx])
      break;
    hEventStat1->Fill(idx);
  }
}
