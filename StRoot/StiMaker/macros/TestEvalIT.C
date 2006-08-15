//
// $Id: TestEvalIT.C,v 1.15 2006/08/15 21:42:27 jeromel Exp $
//
//
// $Log: TestEvalIT.C,v $
// Revision 1.15  2006/08/15 21:42:27  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.14  2002/10/04 01:55:03  pruneau
// DefaultToolkit now uses the StiHitLoader scheme rahter than the StiHitFiller.
//
// Revision 1.13  2002/09/27 19:19:28  mmiller
// Hid some necessary calls behind if (simulated) to restore proper handling of both mc and data.
//
// Revision 1.12  2002/08/28 17:14:28  pruneau
// Simplified the interface of StiKalmanTrackFinder and the calls
// required in StiMaker.
//
// Revision 1.11  2002/08/23 18:16:56  pruneau
// Added StiSimpleTrackFilter to StiMaker to enable simple and
// fast track finding diagnostics.
//
// Revision 1.10  2002/08/22 21:46:14  pruneau
// Made a fix to StiStEventFiller to remove calls to StHelix and StPhysicalHelix.
// Currently there is one instance of StHelix used a calculation broker to
// get helix parameters such as the distance of closest approach to the main
// vertex.
//
// Revision 1.9  2002/08/19 19:33:19  pruneau
// eliminated cout when unnecessary, made helix member of the EventFiller
//
// Revision 1.8  2002/06/28 23:33:46  calderon
// Changes to work with bug fixes and new conventions in StMiniMcMaker
// Output file names will begin with "EvalItTest" and the rest of the input
// filename will be appended to this prefix.
//
// Revision 1.7  2002/06/25 21:44:40  pruneau
// *** empty log message ***
//
// Revision 1.6  2002/06/25 15:09:16  pruneau
// *** empty log message ***
//
// Revision 1.5  2002/06/21 22:06:46  andrewar
// Add command line flag for Sti track/ Tpt track association: associateStiTrack
// (=1 for StiTracks)....A. Rose, 6.21.2002
//
// Revision 1.4  2002/06/18 20:25:12  pruneau
// Changed TestEvalIT.C to have nevent==1 imply GUI mode
//
// Revision 1.3  2002/06/05 20:45:27  calderon
// -Make sure that StMcEventMaker and StAssociationMaker are loaded, the last
// commit had these commented out for some reason (?)
// -Remove any extraneous reference to the (private code) StItTestMaker to
// avoid confusion.
// -Make sure all branches are deactivated in the IO maker first, then turn
// on only the ones we need, event, geant, etc.
//
// Revision 1.2  2002/06/04 18:13:26  andrewar
//
// ----------------------------------------------------------------------
//
// Committing in StRoot/StiMaker/macros
//
//  Modified Files:
//  	TestEvalIT.C
//
//
// 	Corrected TestEvalIT.C to work with current
// 	repository software.
//
// 	A. Rose, M. Calderon
//
//
//  ----------------------------------------------------------------------
//
// Revision 1.1  2002/05/29 19:18:23  calderon
// First version to test primary tracks from IT.
// This writes the StMiniMcEvent tree from Bum and Manuel,
// later this can be switched to something else.
//
//
#include <iostream>
#include <stdexcept>
Bool_t   doProfile = false;
Int_t    usePath = 0;
Int_t    nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
class    StChain;
StChain  *chain=0;
class StEventDisplayMaker;
StEventDisplayMaker *dsMaker = 0;
TBrowser *b=0;

const char *dstFile = 0;
const char *xdfFile = 0;
const char *mdcFile = 0;
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

void Help()
{
    cout << "Usage: TestEvalIT.C(firstEvtIndex,nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
    cout << "       TestEvalIT.C(firstEvtIndex,nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
    cout << "       TestEvalIT.C(firstEvtIndex,nevents,\"some_directory\",\"*.dst.root\")" << endl;	
}

void loadLibrairies(bool doProfile)
{	
    cout<<"Loading modules:"<<endl;
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StBFChain"); 
    gSystem->Load("St_Tables");
	
    gSystem->Load("libgen_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("geometry");
    gSystem->Load("St_g2t");
    gSystem->Load("St_geant_Maker");
    gSystem->Load("StIOMaker");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("StSvtDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StTpcDb");
    gSystem->Load("StEvent");
    gSystem->Load("StEventMaker");
    gSystem->Load("StEmcUtil"); 
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StDAQMaker");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StSvtClassLibrary");
    gSystem->Load("StSvtDaqMaker");
    gSystem->Load("StSvtSimulationMaker");
    gSystem->Load("StSvtCalibMaker");
    gSystem->Load("StSvtSeqAdjMaker");
    //gSystem->Load("StSvtEvalMaker");
    gSystem->Load("StSvtClusterMaker");
    
    cout <<"/Sti" << endl;                 gSystem->Load("Sti");
    cout <<"/StiGui"<< endl;               gSystem->Load("StiGui");
    cout <<"/StiEvaluator"<< endl;         gSystem->Load("StiEvaluator");
    cout <<"/libGui"<< endl;               gSystem->Load("libGui");
    cout <<"/StiMaker"<< endl;             gSystem->Load("StiMaker");
    cout <<"/StMiniMcEvent"<< endl;        gSystem->Load("StMiniMcEvent");
    cout <<"/StMiniMcMaker"<< endl;        gSystem->Load("StMiniMcMaker");
    
    if(doProfile)
	{
	    cout <<"/Jprof";
	    gSystem->Setenv("JPROF_FLAGS", "JP_START JP_PERIOD=0.001");
	    gSystem->Load("libJprof");
	}
    cout <<"\nDone.";
}

void TestEvalIT(Int_t firstEvtIndex=0,
		Int_t nevents=1,
		//const Char_t* path ="/star/data11/reco/ppMinBias/ReversedFullField/P02gh2/2002/009/", //pp-data
		//const Char_t* file = "st_physics_3009012_raw_0001.event.root",
		const Char_t* path = "/star/data22/ITTF/EvalData/MCFiles/auau200/", //mc test
		const Char_t* file="rcf0183_12_300evts.geant.root",
		const Char_t *qaflag = "off",
		const Int_t  wrStEOut = 0,
		const int    associateStiTrack=1,
		const char* filePrefix = "rcf")
{
    if (nevents==-1) 
	{
	    Help(); 
	    return;
	}
    const char *fileListQQ[]={0,0};
    if (strncmp(path,"GC",2)==0) 
	fileListQQ=0;
    else if (path[0]=='-') 
	fileListQQ[0]=file;
    else if (!file[0]) 
	fileListQQ[0]=path;
    else 
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    TestEvalIT(firstEvtIndex, nevents,fileListQQ,qaflag,wrStEOut,associateStiTrack,filePrefix);
}


// ------------------ Here is the actual method -----------------------------------------

void TestEvalIT(Int_t firstEvtIndex, 
		Int_t nevents, 
		const Char_t **fileList, 
		const Char_t *qaflag, 
		const Int_t   wrStEOut,
		const int     associateStiTrack,
		const char* filePrefix)
{
    Int_t theRunNumber=0;
    bool simulated = true;
    bool doFit = false;
    bool optimized = false;
    bool doProfile = false;
    char* outfile = "Evaluation.root";
  
    cout <<  endl << endl <<" TestEvalIT -  input # events = " << nevents << endl;
    Int_t ilist=0;
    while(fileList[ilist]){ 
	cout << " TestEvalIT -  input fileList = " << fileList[ilist] << endl;
	ilist++; 
    }
    cout << " TestEvalIT -  input qaflag   = " << qaflag << endl;
    cout << " TestEvalIT -  input wrStEOut = " << wrStEOut << endl;
    cout << " TestEvalIT -  file Prefix    = " << filePrefix << endl << endl;
  
    // String Manipulation to create the output name
    TString fileName = "EvalItTest";
    TString MainFile = fileList[0];
    int fileBeginIndex = MainFile.Index(filePrefix,0);
    MainFile.Remove(0,fileBeginIndex);
    fileName.Append(MainFile);
    cout << "Output MiniMcTree : " << fileName << endl;
  
    loadLibrairies(doProfile);
  
    // Handling depends on whether file is a ROOT file or XDF file
    //
    chain  = new StChain("StChain");
  
    StFileI *setFiles =0;
    if (fileList) {	//Normal case
	setFiles= new StFile(fileList);
    } else        {	//Grand Challenge
	gSystem->Load("StChallenger");
	setFiles = StChallenger::Challenge();
	setFiles->SetDebug();
	const char *Argv[]= {
	    "-s","daq",                           // list of components needed
	    "-q","mRunNumber=1228023",   // example of user query
	    "-c","/afs/rhic.bnl.gov/star/incoming/GCA/daq/stacs.rc"  // pointer to GC servers for daq
	};
	Int_t Argc=sizeof(Argv)/4;
	setFiles->Init(Argc,Argv);
    }
  
    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles); //StMiniMcMaker wants this name
    IOMk->SetBranch("*",0,"0");	//deactivate all branches
    //IOMk->SetBranch("dstBranch",0,"r");
    //IOMk->SetBranch("runcoBranch",0,"r");
    IOMk->SetBranch("eventBranch",0,"r");
    if (simulated) { //addded by MLM
	IOMk->SetBranch("geantBranch",0,"r");
    }
    IOMk->SetDebug();
  
    St_geant_Maker       *geantMk  = new St_geant_Maker("geant");
    geantMk->SetActive(kFALSE);
  
    // Maker to read events from file or database into StEvent
    //StEventMaker *readerMaker =  new StEventMaker("events","title");
  
    // DB maker
    // dbaseMk = new St_db_Maker("svt","$PWD/svtcvs/StarDb");
    //dbaseMk = new St_db_Maker("db","StarDb");
    dbaseMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb");
    //dbaseMk->SetDateTime("year_1h");
    //dbaseMk->SetDateTime("year_2a");
    if (simulated) dbaseMk-> SetDateTime(20010801,000000);  
  
    // TPC Db maker
    tpcDbMk  = new StTpcDbMaker("tpcDb");
  
    // SVT Db maker
    svtDbMk  = new StSvtDbMaker("svtDb");
  
    // Detector Db Maker (TPC rdos, etc)
    detDbMk = new StDetectorDbMaker("detDb");
  
    //StMcEventMaker
    StMcEventMaker* mcEventReader = 0;
    //Association
    StAssociationMaker* assocMaker = 0;
  
    if (simulated) 
	{
	    mcEventReader = new StMcEventMaker();
	    assocMaker = new StAssociationMaker("EgrStAssociationMaker");
	}
    //StiMaker
    StiMaker* anaMk = StiMaker::instance();
  
    //StiIOBroker
    cout <<"\n --- Setup StiIOBroker ---\n"<<endl;
    StiRootIOBroker* stiIO = anaMk->getIOBroker();//StiRootIOBroker::instance();
    stiIO->setTPHFMinPadrow(1);
    stiIO->setTPHFMaxPadrow(45);
    stiIO->setETSFLowerBound(5);
    stiIO->setETSFMaxHits(6);
    stiIO->setDoTrackFit(doFit);
    //Set Kalman Track Finder (KTF) run-time values:
    //stiIO->setKTFMcsCalculated(true);
    //stiIO->setKTFElossCalculated(true);
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
    //stiIO->setLTSFZWindow(5.);
    //stiIO->setLTSFYWindow(2.);
    stiIO->setLTSFZWindow(10.);
    stiIO->setLTSFYWindow(4.);
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
  
    //Set up the track filter (this mas to macth the correspoinding enumeration in StiIOBroker.h)
    enum FilterType {kPtFilter=0, kEtaFilter=1, kChi2Filter=2, kNptsFilter=3, kNFitPtsFilter=4,
		     kNGapsFilter=5, kFitPointRatioFilter=6, kPrimaryDcaFilter=7};
  
    //stiIO->setSeedFinderType(kEvaluable);
    stiIO->setSeedFinderType(kComposite);
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
    //stiIO->addFilterType(kFitPointRatioFilter);
    //stiIO->setFilterFitPointRatioMin(.5);
    stiIO->addFilterType(kPrimaryDcaFilter);
    stiIO->setFilterPrimaryDcaMax(100.);
    stiIO->setSimulated(simulated);
    anaMk->setEvaluationFileName(outfile);
    if (simulated) 
	{
	    anaMk->setMcEventMaker(mcEventReader);
	    anaMk->setAssociationMaker(assocMaker);
	}
  
    //Make Control Window if not batch
    MainFrame* sti=0;
    StiGuiIOBroker* guiIO=0;
  
    if (gROOT->IsBatch()||nevents>1)
	{
	    cout <<"Batch mode selected. Run Integrated Tracker in non-Gui Mode."<<endl;
	    stiIO->setUseGui(false);
	}
    else
	{
	    cout <<"Interactive mode selected.  Run Integrated Tracker in Gui Mode."<<endl;
	    sti = new MainFrame(gClient->GetRoot(), 400, 220);
	    sti->setStChain(chain);
	    sti->setIoMaker(IOMk);
	    stiIO->setUseGui(true);
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
    cout <<"\n --- Done setting up StiIOBroker --- \n"<<endl;
  
    // WriteOut StEvent
    if (wrStEOut) {
	cout << "!!!! doEvents: will write out .event.root file !!" << endl << endl;
	StTreeMaker *outMk = new StTreeMaker("EvOut","","bfcTree");
	outMk->SetIOMode("w");
	outMk->SetBranch("eventBranch","test.event.root","w");
	outMk->IntoBranch("eventBranch","StEvent");
    }

    StMiniMcMaker* minimcMaker = 0;
    if (simulated) {
	assocMakerIt = new StAssociationMaker();
	if(associateStiTrack) assocMakerIt->useInTracker();
	assocMakerIt->SetDebug();

	minimcMaker = new StMiniMcMaker;
	minimcMaker->setDebug();
	minimcMaker->setOutDir("./");
	minimcMaker->setFileName(fileName);
    }
    //} // mlm (9/27/02 (these were after the simulated catch?)
    //StMiniMcMaker* minimcMaker = new StMiniMcMaker;
    //minimcMaker->setDebug();
    //minimcMaker->setOutDir("./");
    //minimcMaker->setFileName(fileName);
  
    // Initialize chain
    chain->PrintInfo();
    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->InitRun(theRunNumber);
  
    // Event loop
    int i=firstEvtIndex;
    int iLast=firstEvtIndex+nevents;
    int istat;
    while (i < iLast && istat!=2) 
	{
	    cout << "============= Event " <<i<< " started =============" << endl;
	    chain->Clear();
	    try
	      {
		istat =chain->Make(i); 
		switch(istat)
		  {
		  case 2: cout << "Event completed with Status = 2" << endl; break;
		  case 3: cout << "Event completed with error Status = 3" << endl; break;
		  }
		i++;
	      }
	    catch (runtime_error & rte)
	      {
		cout << "TestEvalIT.C - ERROR -" << rte.what() << endl;
	      }
	    catch (exception & eee)
	      {
		cout << "TestEvalIT.C - ERROR -" << eee.what() << endl;
	      }
	    catch (...)
	      {
		cout << " TestEvalIT.C - ERROR - Unknown exception"<<endl;
	      }
	}
    //chain->Finish();
    cout << "\n=============== Event "<<i<< " finished ============" << endl;
}

