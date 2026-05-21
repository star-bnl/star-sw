//usr/bin/env root4star -l root -l -q  $0; exit $?
//usr/bin/env root4star -l -b -q $0'("'${1:-st_physics_23055058_raw_1500001.MuDst.root}'",'${2:-100}')'; exit $?
// that is a valid shebang to run script as executable, but with only one arg

#include <typeinfo.h>

// Fast fwd tracking without DB
// bool runDb = false;
// bool runFttChain = true;
// bool runFcsChain = false; 
// bool runFwdChain = true;
// bool refillMuDst = false;
// bool runFwdQa = false;
// bool runFitQa = false;
// bool runPico = true;

// For EPD QA only
// bool runDb = false;
// bool runFttChain = false;
// bool runFcsChain = true;
// bool runFwdChain = false;
// bool refillMuDst = false;
// bool runFwdQa = false;
// bool runFitQa = true;

// Tracking without FCS (but with DB)
// bool runDb = true;
// bool runFttChain = true;
// bool runFcsChain = true; 
// bool runFwdChain = true;
// bool refillMuDst = false;
// bool runFwdQa = false;
// bool runFitQa = false;
// bool runPico = true;


// Memory Baseline
bool runDb       = true;
bool runFttChain = true;
bool runFcsChain = true;
bool runFwdChain = true;
bool refillMuDst = false;
bool runFwdQa    = false;
bool runFitQa    = false;
bool runPico     = true;

#include "StMemStat.h"


void loadLibs();
void fwd_afterburner( 	const Char_t * fileList = "st_physics_23037002_raw_1000064.MuDst.root",
						size_t nEvents = 1500,
						size_t nSkip   = 0 ){
	cout << "FileList: " << fileList << endl;
	cout << "nEvents: "  << nEvents  << endl;
	cout << "nSkip: "    << nSkip    << endl;

	// First load some shared libraries we need
	loadLibs();

	// create the chain
	StChain *chain  = new StChain("StChain");

	const char* inMuDstFile = fileList;
	// create the StMuDstMaker
	StMuDstMaker *muDstMaker = new StMuDstMaker(  	0,
													0,
													"",
													inMuDstFile,
													"MuDst.root",
													1
												);
	TChain& muDstChain = *muDstMaker.chain();
    printf( "MuDst file has %d events available in tree\n", muDstChain.GetEntries());
    muDstChain.SetCacheSize(0);   // disable input TTreeCache: avoids ~128 MB-class per-event VSize jumps
    printf( "Input TTreeCache size set to 0 (disabled)\n");
	
	/*******************************************************************************************/
	// Initialize the database
		if (runDb){
			cout << endl << "============  Data Base =========" << endl;
			St_db_Maker *dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
			dbMk->SetDateTime(20220225, 0);
			// things will run fine without a timestamp set, but FCS DB will give bad values ...
		}
	/*******************************************************************************************/
	

	/*******************************************************************************************/
	// Create the StMuDst2StEventMaker
    StMuDst2StEventMaker * mu2ev = new StMuDst2StEventMaker();
	mu2ev->setActive(true);
	/*******************************************************************************************/

	/*******************************************************************************************/
	// Setup Fcs Database if needed
	if ( (runFcsChain && runDb) || runFitQa){
		StFcsDbMaker * fcsDb = new StFcsDbMaker();
		chain->AddMaker(fcsDb);
		// fcsDb->SetDebug();
	}
	/*******************************************************************************************/
	

	/*******************************************************************************************/
	// FTT chain
	if (runFttChain){
		gSystem->Load("libStFttDbMaker.so");
		gSystem->Load("libStFttHitCalibMaker.so");
		gSystem->Load("libStFttClusterMaker.so");
		gSystem->Load("libStFttClusterPointMaker.so");
		StFttDbMaker * fttDbMk = new StFttDbMaker();
		chain->AddMaker(fttDbMk);
		StFttHitCalibMaker * ftthcm = new StFttHitCalibMaker();
		StFttClusterMaker * fttclu = new StFttClusterMaker();
		fttclu->SetTimeCut(1, -40, 40);
		StFttClusterPointMaker *fttCP = new StFttClusterPointMaker();
		// StFttPointMaker * fttpoint = new StFttPointMaker();
	}
	/*******************************************************************************************/

	/*******************************************************************************************/
    // FCS Chain
	if (runFcsChain){
		gSystem->Load("libStFcsWaveformFitMaker.so");
		gSystem->Load("libStFcsClusterMaker.so");
		
		StFcsWaveformFitMaker *fcsWFF = new StFcsWaveformFitMaker();
		// This should only be used for simulated data, for real data this done in the database
		// fcsWFF->setEnergySelect(0);
		StFcsClusterMaker *fcsclu = new StFcsClusterMaker();
	}
	/*******************************************************************************************/

	/*******************************************************************************************/
	// FwdTrackMaker Chain
	StFwdTrackMaker *fwdTrack = NULL;
	if (runFwdChain){
		// FwdTrackMaker
		fwdTrack = new StFwdTrackMaker();
		fwdTrack->SetDebug(1);
		fwdTrack->setGeoCache( "fGeom.root" );
		fwdTrack->setSeedFindingWithFst();
		fwdTrack->setTrackRefit( true );

		// Fitter Options
		fwdTrack->setFitDebugLvl( 0 );
		fwdTrack->setFitMinIterations( 40 );
		fwdTrack->setFitMaxIterations( 100 );
		
		// fwdTrack->setDeltaPval( 1e-9 );
		// fwdTrack->setRelChi2Change( 1e-9 );

		// fwdTrack->setSeedFindingOff();
		// fwdTrack->setTrackFittingOff();
		fwdTrack->setFstHitSource( 2 /* = MUDST */);
		fwdTrack->setFttHitSource( 1 /* = STEVENT */);


		// fwdTrack->setConfigKeyValue("TrackFitter:doBeamlineTrackFitting", false);
        // fwdTrack->setConfigKeyValue("TrackFitter:doPrimaryTrackFitting", false);
        // fwdTrack->setConfigKeyValue("TrackFitter:doSecondaryTrackFitting", false);
        // skip finding fwd vertices
	}



		if (runFcsChain){
			// FwdTrack and FcsCluster assciation
			gSystem->Load("StFcsTrackMatchMaker");
			StFcsTrackMatchMaker *match = new StFcsTrackMatchMaker();
			match->setMaxDistance(6,10);
			match->setFileName("fcstrk.root");
		}

		
		
		if (runFwdQa){
			StFwdQAMaker *fwdQA = new StFwdQAMaker();
			fwdQA->SetDebug(2);
			TString fwdqaname( gSystem->BaseName(inMuDstFile) );
			fwdqaname.ReplaceAll(".MuDst.root", ".FwdTree.root");
			cout << fwdqaname.Data() << endl;
			fwdQA->setTreeFilename(fwdqaname);

			gSystem->Load("StFwdUtils.so");
			StFwdAnalysisMaker * fwdAna = new StFwdAnalysisMaker();
			fwdAna->setMuDstInput();
		}


	// The PicoDst
	if (runPico){
		gSystem->Load("libStPicoEvent");
		gSystem->Load("libStPicoDstMaker");
		StPicoDstMaker *picoMk = (StMaker*) (new StPicoDstMaker(StPicoDstMaker::IoWrite, inMuDstFile, "picoDst"));
		cout << "picoMk = " << picoMk << endl;
		picoMk->setVtxMode(StPicoDstMaker::Vtxless);
	}

	if ( runFitQa && runFwdChain){
		StFwdFitQAMaker *fwdFitQA = new StFwdFitQAMaker();
		fwdFitQA->SetDebug();
		TString fitqaoutname(gSystem->BaseName(inMuDstFile));
		fitqaoutname.ReplaceAll(".MuDst.root", ".FwdFitQA.root");
		fwdFitQA->setOutputFilename( fitqaoutname );
	}
	/*******************************************************************************************/

	// gMessMgr->MemoryOff();

	/*******************************************************************************************/
	// Initialize chain
	chain->SetDebug(1);
	Int_t iInit = chain->Init();
	chain->SetDebug(1);
	cout << "CHAIN INIT DONE? (good==0): " << iInit << endl;
	// ensure that the chain initializes

	if ( iInit )
		chain->Fatal(iInit,"on init");
	
	// print the chain status
	chain->PrintInfo();

	StMemStat stmem;
	stmem.PrintMem("BEFORE Event Loop");
	/*******************************************************************************************/
    // OPTIONAL: skip first nSkip events (advance the input MuDst without running any
    // downstream maker work, so per-event accumulation from those events does NOT happen).
    // Use this to test whether a crash is event-content driven (always at the same input
    // event #) or accumulation driven (always after N processed events).
    /*******************************************************************************************/
	size_t totalEntries = muDstChain.GetEntries();
	if (nSkip >= totalEntries) {
		cout << "ERROR: nSkip (" << nSkip << ") >= total entries (" << totalEntries << "). Nothing to process." << endl;
		return;
	}
	if (nSkip > 0) {
		cout << "Skipping first " << nSkip << " events (advancing input only, no chain processing)..." << endl;
		for (size_t s = 0; s < nSkip; s++) {
			chain->Clear();
			if (kStOK != muDstMaker->Make()) {
				cout << "ERROR: muDstMaker->Make() failed during skip at s=" << s << endl;
				break;
			}
		}
		stmem.PrintMem(TString::Format("After Skip of %zu events:", nSkip).Data());
		cout << "Skip complete. Main loop will now process input events starting at #" << nSkip << endl;
	}

	/*******************************************************************************************/
    // MAIN EVENT LOOP
    /*******************************************************************************************/
	size_t availableAfterSkip = totalEntries - nSkip;
	size_t nEntries = availableAfterSkip;
	if (nEvents > 0 && nEvents < availableAfterSkip) {
		nEntries = nEvents;
		cout << "Limiting to " << nEntries << " events (input #" << nSkip
		     << " .. #" << (nSkip + nEntries - 1) << ")." << endl;
	}
	double initialUsedHeap = stmem.Used();
	size_t numProcessed = 0;
	for (int i = 0; i < nEntries; i++) {
		size_t inputEv = nSkip + i;   // absolute input event number
		printf("Processing event %d of %d (input #%zu)\n", i, nEntries, inputEv);
		if (i > 0) // skip first event to make it consistent
			stmem.Start();
		chain->Clear();
		if (fwdTrack)
			fwdTrack->SetDebug(1);

        if (kStOK != chain->Make())
            break;

		if (refillMuDst){
			StEvent *mStEvent = static_cast<StEvent *>(muDstMaker->GetInputDS("StEvent"));
			// muDstMaker->fillFwdTrack( mStEvent);
			fwdQA->Make();
		}
		stmem.PrintMem(TString::Format("After Event %zu:", inputEv).Data());
		if (i > 0)
			stmem.Stop();
		// MipMaker->Make();
		// picoMk->Make();
        cout << "EVENT #" << i << " (input #" << inputEv << ") COMPLETED" << endl;
		double currentUsedHeap = stmem.Used();
		double deltaUsed = currentUsedHeap - initialUsedHeap;
		cout << "Memory used after event #" << inputEv << ": " << currentUsedHeap << " MB (delta: " << deltaUsed << " MB)" << endl;

    }
	stmem.PrintMem("After Event Loop");
	stmem.Summary();
	/*******************************************************************************************/

	// Chain Finish
	// if (nEntries > 1) {
	// 	cout << "FINISH up" << endl;
	// 	chain->Finish();
	// }

	// delete chain;
}



void loadLibs(){	
	gSystem->Load("libStarClassLibrary.so");
	gSystem->Load("libStarRoot.so");
	gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
	loadSharedLibraries();
	
	gSystem->Load("StarMagField");
	gSystem->Load("StMagF");
	gSystem->Load("StDetectorDbMaker");
	gSystem->Load("StTpcDb");
	gSystem->Load("StDaqLib");
	gSystem->Load("StDbBroker");
	gSystem->Load("StDbUtilities");
	gSystem->Load("St_db_Maker");

	gSystem->Load("StEvent");
	gSystem->Load("StEventMaker");
	gSystem->Load("StarMagField");
 
	gSystem->Load("libGeom");
	gSystem->Load("St_g2t");
	
	// Added for Run16 And beyond
	gSystem->Load("libGeom.so");
	
	gSystem->Load("St_base.so");
	gSystem->Load("StUtilities.so");
	gSystem->Load("libPhysics.so");
	gSystem->Load("StarAgmlUtil.so");
	gSystem->Load("StarAgmlLib.so");
	gSystem->Load("libStarGeometry.so");
	gSystem->Load("libGeometry.so");
	
	gSystem->Load("xgeometry");
 
	gSystem->Load("St_geant_Maker");


	// needed since I use the StMuTrack
	gSystem->Load("StarClassLibrary");
	gSystem->Load("StStrangeMuDstMaker");
	gSystem->Load("StMuDSTMaker");
	gSystem->Load("StBTofCalibMaker");
	gSystem->Load("StVpdCalibMaker");
	gSystem->Load("StBTofMatchMaker");
	gSystem->Load("StFcsDbMaker");	

	/*******************************************************************************************/
	// loading libraries
	gSystem->Load("StFcsDbMaker");
	gSystem->Load( "StFttDbMaker" );
	gSystem->Load( "StFttHitCalibMaker" );
	gSystem->Load( "StFttClusterMaker" );
	gSystem->Load( "StFttClusterPointMaker" );
	gSystem->Load( "StFttPointMaker" );
    gSystem->Load("libStarGeneratorUtil.so");
    gSystem->Load("libgenfit2");
    gSystem->Load("libKiTrack");
    gSystem->Load("libXMLIO.so");
    gSystem->Load( "StFwdTrackMaker.so" );
	gSystem->Load( "StFwdUtils.so" );
    gSystem->Load("libStEpdUtil.so");

	gSystem->Load("StStarLogger.so");

	/*******************************************************************************************/


}
