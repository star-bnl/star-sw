// RunStiMaker.C
// M.L. Miller
//  5/00

class StChain;
StChain *chain=0;

void RunStiMaker(Int_t nevents=1,
		 bool simulated=true, //sim or data?
		 //bool doFit=true, // true->fit track only
		 bool doFit=false, // false->find track only
		 //const char* outfile = "/star/rcf/pwg/spectra/mmiller/Evaluation.root",
		 const char* outfile = "Evaluation.root",
		 bool doProfile=false, // produce profiling output? 
		 //const char* MainFile="/a1/pruneau/data/EvalData/MCNtuple/muon_100_neg.event.root")
		 
  //This file points to 30 events of 10 neg muons w/ pt=.9
  const char* MainFile="/star/data22/ITTF/data/simple_geant/DEV_10_8_01/muon_10_neg.event.root")
    //const char* MainFile="/star/data13/reco/dev/2002/01/*3007007*.event.root")
		 //const char* MainFile="/star/data22/ITTF/data/StarNightlyTest/Fri/year_2001/pp_minbias/pds0200_04_12812evts.event.root")
		 //const char* MainFile="/star/data22/ITTF/data/StarNightlyTest/Fri/year_2001/hc_highdensity/hc_highdensity.16_evts.event.root")
		 //const char* MainFile="/star/data22/ITTF/data/StarNightlyTest/Fri/year_2001/hc_standard/hc_standard.40_evts.event.root")
		 //const char* MainFile="/direct/star+rcf/test/dev/trs_redhat61/Wed/year_2001/hc_lowdensity/hc_lowdensity.400_evts.event.root")
    //const char* MainFile="/star/data22/ITTF/data/simple_geant/DEV_10_8_01/*.event.root")
    //This file points to 5 muons /event
    //const char* MainFile="/star/data22/ITTF/EvalData/MCNtuple/muon_100_neg.event.root")
{
    //bool optimized = true;
    bool optimized = false;
    
    // Dynamically link needed shared libs
    cout <<"Loading St_base"<<endl;
    gSystem->Load("St_base");
    
    cout <<"Loading StChain"<<endl;
    gSystem->Load("StChain");
    
    cout <<"Loading St_Tables"<<endl;
    gSystem->Load("St_Tables");
    
    cout <<"Loading StUtilities"<<endl;
    gSystem->Load("StUtilities");
    
    cout <<"Loading StIOMaker"<<endl;
    gSystem->Load("StIOMaker");
    
    cout <<"Loading StarClassLibrary"<<endl;
    gSystem->Load("StarClassLibrary");
    
    cout <<"Loading DataBase"<<endl;
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTpcDb");
    gSystem->Load("StSvtClassLibrary");
    gSystem->Load("StSvtDbMaker");
    
    cout <<"Loading StEvent"<<endl;
    gSystem->Load("StEvent");

    cout <<"Loading StDetectorDbMaker"<<endl;
    gSystem->Load("StDetectorDbMaker");
    
    cout <<"Loading StEventMaker"<<endl;
    gSystem->Load("StEventMaker");

    cout <<"Loading StEmcUtil"<<endl;
    gSystem->Load("StEmcUtil");
    
    cout <<"Loading StMcEvent"<<endl;
    gSystem->Load("StMcEvent");

    cout <<"Loading StMcEventMaker"<<endl;
    gSystem->Load("StMcEventMaker");

    cout <<"Loading AssociationMaker"<<endl;
    gSystem->Load("StAssociationMaker");

    if (optimized) {
	cout <<"Loading Optimized Sti"<<endl;
	gSystem->Load(".i386_redhat61/LIB/Sti.so");
	
	cout <<"Loading Optimized StiGui"<<endl;
	gSystem->Load(".i386_redhat61/LIB/StiGui");
	
	cout <<"Loading Optimized StiEvaluator"<<endl;
	gSystem->Load(".i386_redhat61/LIB/StiEvaluator");
	
	cout <<"Loading Optimized StiMaker"<<endl;
	gSystem->Load(".i386_redhat61/LIB/StiMaker");
    }
    else {
	cout <<"Loading Sti"<<endl;
	gSystem->Load("Sti");
	
	cout <<"Loading StiGui"<<endl;
	gSystem->Load("StiGui");
		
	cout <<"Loading StiEvaluator"<<endl;
	gSystem->Load("StiEvaluator");
	
	cout <<"Loading StiMaker"<<endl;
	gSystem->Load("StiMaker");
    }
    
    //-----------------
    // start profiling
    if(doProfile){
        // this variable tells jprof that it should start profiling when loaded
        // and how often it should record info
        gSystem->Setenv("JPROF_FLAGS", "JP_START JP_PERIOD=0.001001");

        // this starts the profiling
        gSystem->Load("Jprof");
    }
    // create a new instance of the chain
    chain = new StChain("StChain"); 
    chain->SetDebug();
    
    // add makers to the chain
    
    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    ioMaker->SetDebug();
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");            //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");  //activate geant Branch
    ioMaker->SetBranch("dstBranch",0,"r");    //activate Event Branch
    ioMaker->SetBranch("runcoBranch",0,"r");  //activate runco Branch
    
    // Database:

    // This is the real SQL database
    const char* calibDB = "MySQL:StarDb";
    // This is the filesystem database which we need for tsspars, needed
    // by TpcDb
    const char* paramsDB = "$STAR/StarDb";
    // this maker must be called "db" or StSvtDbMaker will crash
    St_db_Maker* calibMk = new St_db_Maker("db",calibDB,paramsDB);
    calibMk->SetDateTime(20010801,000000);
    //calibMk->SetDateTime("year_2b");
    calibMk->SetDebug();
    
    //Tpc Database access
    StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");

    //Svt Database access
    StSvtDbMaker *svtDbMk = new StSvtDbMaker("svtDb");

    //StEventMaker
    StEventMaker*       eventReader   = new StEventMaker("events","title");
    eventReader->doPrintEventInfo = 0;
	  
    //StMcEventMaker
    StMcEventMaker* mcEventReader = 0;
    //Association
    StAssociationMaker* assocMaker = 0;
    
    if (simulated) {
	mcEventReader = new StMcEventMaker();
	assocMaker = new StAssociationMaker();
    }
    
    //StiMaker
    StiMaker* anaMk = StiMaker::instance();

    //StiIOBroker
    StiRootIOBroker* stiIO = new StiRootIOBroker();

    stiIO->setTPHFMinPadrow(1);
    stiIO->setTPHFMaxPadrow(45);
    stiIO->setETSFLowerBound(5);
    stiIO->setETSFMaxHits(6);

    stiIO->setDoTrackFit(doFit);

    //Set Kalman Track Finder (KTF) run-time values:
    stiIO->setKTFMcsCalculated(false);
    stiIO->setKTFElossCalculated(false);
    stiIO->setKTFMaxChi2ForSelection(50);
    stiIO->setKTFBField(.5); //Tesla
    stiIO->setKTFMassHypothesis(.1395); //GeV
    stiIO->setKTFMinContiguousHitCount(2);
    stiIO->setKTFMaxNullCount(40);
    stiIO->setKTFMaxContiguousNullCount(25);
    stiIO->setKTFMinSearchRadius(.5); //cm
    stiIO->setKTFMaxSearchRadius(4.); //cm
    stiIO->setKTFSearchWindowScale(5.); //cm
	
    //Set Local Track Seed Finder (LTSF) run-time values
    stiIO->setLTSFZWindow(5.);
    stiIO->setLTSFYWindow(2.);
    stiIO->setLTSFSeedLength(2);

    stiIO->setLTSFDoHelixFit(true);
    stiIO->setLTSFExtrapYWindow(1.);
    stiIO->setLTSFExtrapZWindow(2.);
    stiIO->setLTSFExtrapMaxSkipped(2);
    stiIO->setLTSFExtrapMinLength(4);
    stiIO->setLTSFExtrapMaxLength(5);
    stiIO->setLTSFUseVertex(true);

    stiIO->setLTMDeltaR(1.); //10% in r
    
    //Add sectors:
    for (unsigned int sector=1; sector<=12; ++sector) {
	stiIO->addLTSFSector(sector);
    }
    //Add padrows;
    //for (unsigned int padrow=1; padrow<=45; ++padrow) {
    for (unsigned int padrow=6; padrow<=45; padrow+=1) {
	stiIO->addLTSFPadrow(padrow);
    }
    
    //This line has to match the corresponding enumeration in StiIOBroker.h
    enum SeedFinderType {kUndefined=0, kComposite=1, kEvaluable=2};
    //stiIO->setSeedFinderType(kEvaluable);
    stiIO->setSeedFinderType(kComposite);

    stiIO->setSimulated(simulated);
    anaMk->setEvaluationFileName(outfile);
    
    if (simulated) {
	anaMk->setMcEventMaker(mcEventReader);
	anaMk->setAssociationMaker(assocMaker);
    }
    
    // now execute the chain member functions
    chain->PrintInfo();

    //Make Control Window if not batch
    MainFrame* sti=0;
    StiGuiIOBroker* guiIO=0;

    if (gROOT->IsBatch()==false) {
	
	cout <<"No batch option detected.  Run Integrated Tracker in Gui Mode."<<endl;
	
	sti = new MainFrame(gClient->GetRoot(), 400, 220);
	
	sti->setStChain(chain);
	sti->setIoMaker(ioMaker);
	
	//we're in batch mode
	stiIO->setUseGui(true);
	
	//Maker io gateway
	guiIO = StiGuiIOBroker::instance();
	
	//Values for hits not assigned to tracks
	guiIO->setUnMarkedHitSize(.3);
	guiIO->setUnMarkedHitColor(4);
	guiIO->setUnMarkedHitStyle(8);
	guiIO->setUpdateEachTrack(false);
	
	//Values for hits assigned to tracks
	guiIO->setMarkedHitSize(.3);
	guiIO->setMarkedHitColor(2);
	guiIO->setMarkedHitStyle(3);
    }
    else {
	cout <<"Batch option detector.  Run Integrated Tracker in non-Gui Mode."<<endl;
	stiIO->setUseGui(false);
    }
    
    cout <<"Calling Init() Methods "<<endl;
    chain->Init();
    
    cout <<"Starting Event Loop"<<endl;
    
    int istat=0,iev=1;
    
    chain->InitRun(0);
    
 EventLoop: if (iev<=nevents && !istat) {
     chain->Clear();
     cout << "---------------------- Processing Event : " << iev << endl;
     istat = chain->Make(iev);
     if (istat) {
	 cout << "Last Event Processed. Status = " << istat << endl;
     }
     iev++; goto EventLoop;
 }
    
    
    return;
}

