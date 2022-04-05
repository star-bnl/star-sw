#include <iostream>

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
    cout << "Usage: doEvents.C(nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
    cout << "       doEvents.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
    cout << "       doEvents.C(nevents,\"some_directory\",\"*.dst.root\")" << endl;	
}


void RunStiMaker(Int_t, const Char_t **, const Char_t *qaflag = "");
//const char* MainFile="/star/data22/ITTF/data/simple_geant/DEV_10_8_01/muon_10_neg.event.root")

void RunStiMaker(Int_t nevents=1,
		 //const Char_t *path="/star/data13/reco/dev/2002/01/",
		 const Char_t *path = "/star/data22/ITTF/data/simple_geant/DEV_10_8_01/",
		 
		 //const Char_t *file="*3007007*.event.root",
		 const Char_t *file= "muon_10_neg.event.root",
		 
		 const Char_t *qaflag = "off",
		 const Int_t wrStEOut = 0); //Set this to '1' to write out new StEvents,

// ------------------ Here is the actual method -----------------------------------------
void RunStiMaker(Int_t nevents, const Char_t **fileList, const Char_t *qaflag, const Int_t wrStEOut)
{
    Int_t theRunNumber=0;
    bool simulated = true;
    bool doFit = false;
    bool optimized = false;
    char* outfile = "Evaluation.root";
    
    cout <<  endl << endl <<" doEvents -  input # events = " << nevents << endl;
    Int_t ilist=0;
    while(fileList[ilist]){ 
	cout << " doEvents -  input fileList = " << fileList[ilist] << endl;
	ilist++; 
    }
    cout << " doEvents -  input qaflag   = " << qaflag << endl;
    cout << " doEvents -  input wrStEOut = " << wrStEOut << endl << endl << endl;
 
    //
    // First load some shared libraries we need
    //

    gSystem->Load("St_base");
    gSystem->Load("StUtilities");
    gSystem->Load("StChain");
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

    gSystem->Load("StSvtClassLibrary");
    gSystem->Load("StSvtDaqMaker");
    gSystem->Load("StSvtSimulationMaker");
    gSystem->Load("StSvtCalibMaker");
    gSystem->Load("StSvtSeqAdjMaker");
    //gSystem->Load("StSvtEvalMaker");
    gSystem->Load("StSvtClusterMaker");

    //cout <<"Loading StMcEventMaker"<<endl;
    //gSystem->Load("StMcEventMaker");

    //cout <<"Loading AssociationMaker"<<endl;
    //gSystem->Load("StAssociationMaker");

    cout <<"Loading Sti"<<endl;
    gSystem->Load("Sti");
    
    cout <<"Loading StiGui"<<endl;
    gSystem->Load("StiGui");
    
    cout <<"Loading StiEvaluator"<<endl;
    gSystem->Load("StiEvaluator");

    cout <<"Loading libGui"<<endl;
    gSystem->Load("libGui");
    
    cout <<"Loading StiMaker"<<endl;
    gSystem->Load("StiMaker");

    cout <<"Loading StItTestMaker"<<endl;
    gSystem->Load("StItTestMaker");
    //
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

    StIOMaker *IOMk = new StIOMaker("inputStream","r",setFiles);
    //IOMk->SetBranch("*",0,"0");	//deactivate all branches
    //IOMk->SetBranch("dstBranch",0,"r");
    //IOMk->SetBranch("runcoBranch",0,"r");
    IOMk->SetBranch("eventBranch",0,"r");
    if (simulated) { //addded by MLM
	IOMk->SetBranch("geantBranch",0,"r");
    }
    IOMk->SetDebug();

    St_geant_Maker       *geantMk  = new St_geant_Maker("geant");
    geantMk->SetActive(kFALSE);

    //
    // Maker to read events from file or database into StEvent
    //
    //StEventMaker *readerMaker =  new StEventMaker("events","title");

    //
    // DB maker
    //       
    // dbaseMk = new St_db_Maker("svt","$PWD/svtcvs/StarDb");
    //dbaseMk = new St_db_Maker("db","StarDb");
    dbaseMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb");
    //dbaseMk->SetDateTime("year_1h");
    //dbaseMk->SetDateTime("year_2a");
    dbaseMk-> SetDateTime(20010801,000000);  

    //
    // TPC Db maker
    // 
    tpcDbMk  = new StTpcDbMaker("tpcDb");

    //
    // SVT Db maker
    // 
    svtDbMk  = new StSvtDbMaker("svtDb");

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
  
    cout <<"\n --- Setup StiIOBroker ---\n"<<endl;

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

    //stiIO->addFilterType(kFitPointRatioFilter);
    //stiIO->setFilterFitPointRatioMin(.5);
    
    stiIO->addFilterType(kPrimaryDcaFilter);
    stiIO->setFilterPrimaryDcaMax(100.);
        
    stiIO->setSimulated(simulated);
    anaMk->setEvaluationFileName(outfile);
  
    if (simulated) {
	anaMk->setMcEventMaker(mcEventReader);
	anaMk->setAssociationMaker(assocMaker);
    }
  
    //Make Control Window if not batch
    MainFrame* sti=0;
    StiGuiIOBroker* guiIO=0;
    
    if (gROOT->IsBatch()==false) {
	
	cout <<"No batch option detected.  Run Integrated Tracker in Gui Mode."<<endl;
	
	sti = new MainFrame(gClient->GetRoot(), 400, 220);
	
	sti->setStChain(chain);
	sti->setIoMaker(IOMk);
	
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

    cout <<"\n --- Done setting up StiIOBroker --- \n"<<endl;
    
    // WriteOut StEvent
    if (wrStEOut) {
	cout << "!!!! doEvents: will write out .event.root file !!" << endl << endl;
	StIOMaker *outMk = new StIOMaker("EvOut","w","test.event.root","bfcTree");
	//        outMk->SetBranch("eventBranch","test.event.root","w");
	outMk->IntoBranch("evtselBranch","StEvent");
	IOMk->SetNotify("CloseFile",outMk);
	IOMk->SetNotify("OpenFile" ,outMk);
    }


    //
    // test the Ittf tracks in StEvent
    //
    StItTestMaker* itTest = new StItTestMaker("StItTestMaker");
    
    /*
      dbaseMk->Init();
      svtDbMk->setSvtDb_Reader();
      dbaseMk->Make();
      svtDbMk->readSvtConfig();
      svtDbMk->readSvtGeometry();
    */
    
    //
    // Initialize chain
    //

    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    //chain->PrintInfo();
    chain->InitRun(theRunNumber);

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

    //chain->Finish();

    i--;
    cout << endl << "============================ Event " << i
	 << " finish ============================" << endl;

}

//--------------------------------------------------------------------------

void RunStiMaker(const Int_t nevents, const Char_t *path, const Char_t *file,
		      const Char_t *qaflag, const Int_t wrStEOut)
{
    if (nevents==-1) { Help(); return;}

    const char *fileListQQ[]={0,0};
    if (strncmp(path,"GC",2)==0) {
	fileListQQ=0;
    } else if (path[0]=='-') {
	fileListQQ[0]=file;
    } else if (!file[0]) {
	fileListQQ[0]=path;
    } else {
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    }

    /*
      const char *fileListQQ[3];
      fileListQQ[0] = "/star/svt/online/jundata/st_pedestal_2165026_raw_0001.daq";
      fileListQQ[1] = "/star/svt/online/jundata/st_pedestal_2165027_raw_0001.daq";
      fileListQQ[2] = NULL;                                                      
    */

    RunStiMaker(nevents,fileListQQ,qaflag,wrStEOut);
}


