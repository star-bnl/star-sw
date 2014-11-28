//
// $ID:$
//
//  A cut down version of RunStiMaker to just write out event.root files.
//
//  A. Rose, M. Calderon
// $Log: WriteStiEvents.C,v $
// Revision 1.2  2002/09/15 15:54:35  andrewar
// Added bool simulated to argument list; true = simulated data,
// false= real data. This fixes the timestamp issue where we explicitly
// try to set a timestamp, even if one exists - which it does for real
// data.
//
// Revision 1.1  2002/06/07 19:26:57  andrewar
// First commit. Set to do nothing but track and write out.
// Data file defaults to public event file in ITTF area.
//
// 

void WriteStiEvents(Int_t nevents=1,
	            const Char_t *Mainfile = "/star/data22/ITTF/EvalData/MCFiles/auau200/rcf0183_12_300evts.geant.root",
		    const Char_t *outfile= "test.event.root",
		    bool simulated = true)
{

    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    gSystem->Load("St_Tables");
    gSystem->Load("StarClassLibrary");

    gSystem->Load("StIOMaker");
    gSystem->Load("StEvent");
    gSystem->Load("StEmcUtil"); 
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StAssociationMaker");

    gSystem->Load("St_db_Maker");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StSvtClassLibrary");
    gSystem->Load("StSvtDbMaker");

    gSystem->Load("Sti");
    gSystem->Load("libGui");
    gSystem->Load("StiGui");
//     gSystem->Load("StiEvaluator");
    gSystem->Load("StiMaker");

    chain  = new StChain("StChain");
    StIOMaker *IOMk = new StIOMaker("IO","r",Mainfile); 
    IOMk->SetBranch("*",0,"0");	//deactivate all branches
    //IOMk->SetBranch("dstBranch",0,"r");
    //IOMk->SetBranch("runcoBranch",0,"r");
    IOMk->SetBranch("eventBranch",0,"r");
    IOMk->SetBranch("geantBranch",0,"r");

    dbaseMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb");
    if (simulated) dbaseMk-> SetDateTime(20010801,000000);  
    tpcDbMk  = new StTpcDbMaker("tpcDb");
    svtDbMk  = new StSvtDbMaker("svtDb");
    detDbMk = new StDetectorDbMaker("detDb");


    bool doFit = false;
    bool optimized = false;

    StiMaker* anaMk = StiMaker::instance();
    StiRootIOBroker* stiIO = anaMk->getIOBroker();//StiRootIOBroker::instance();
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

    //Set up the track filter (this mas to macth the correspoinding enumeration in StiIOBroker.h)
    enum FilterType {kPtFilter=0, kEtaFilter=1, kChi2Filter=2, kNptsFilter=3, kNFitPtsFilter=4,
		     kNGapsFilter=5, kFitPointRatioFilter=6, kPrimaryDcaFilter=7};
    
    stiIO->addFilterType(kPtFilter);
    stiIO->setFilterPtMin(.1); //GeV
    stiIO->setFilterPtMax(50.); //GeV

    stiIO->addFilterType(kEtaFilter);
    stiIO->setFilterEtaMin(-2.);
    stiIO->setFilterEtaMax(2.);

    stiIO->addFilterType(kChi2Filter);
    stiIO->setFilterChi2Max(10.);

    stiIO->addFilterType(kNptsFilter);
    stiIO->setFilterNptsMin(8);

    stiIO->addFilterType(kNFitPtsFilter);
    stiIO->setFilterNFitPtsMin(5);

    stiIO->addFilterType(kNGapsFilter);
    stiIO->setFilterNGapsMax(20);

    stiIO->addFilterType(kPrimaryDcaFilter);
    stiIO->setFilterPrimaryDcaMax(100.);
        
    if (!simulated) stiIO->setSimulated(false);

    if (simulated){
       StMcEventMaker* mcEventReader = mcEventReader = new StMcEventMaker();
       mcEventReader->doUseFtpc = kFALSE;
       mcEventReader->doUseRich = kFALSE;
       mcEventReader->doUseBemc = kFALSE;
       mcEventReader->doUseBsmd = kFALSE;
    }
    cout << "!!!! doEvents: will write out .event.root file !!" << endl << endl;
    StTreeMaker *outMk = new StTreeMaker("EvOut","","bfcTree");
    outMk->SetIOMode("w");
    outMk->SetBranch("eventBranch",outfile,"w");
    outMk->IntoBranch("eventBranch","StEvent");

    if (simulated) StAssociationMaker*	assocMakerIt = new StAssociationMaker();
    if (simulated) assocMakerIt->useInTracker();
    //assocMakerIt->SetDebug();

    chain->PrintInfo();

    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->InitRun(0);
    
    //
    // Event loop
    //
    int istat=0,i=1;
 EventLoop: if (i <= nevents && istat!=2) {

     cout << endl << "============================ Event " << i
	  << " start ============================" << endl;

     chain->Clear();
     istat = chain->Make(i);

     if (istat==2) 
         {cout << "Last  event processed. Status = " << istat << endl;}
     if (istat==3) 
         {cout << "Error event processed. Status = " << istat << endl;}

     
     
     i++;
     goto EventLoop;
 }
    i--;
    cout << endl << "============================ Event " << i
	 << " finish ============================" << endl;

    return;
}
