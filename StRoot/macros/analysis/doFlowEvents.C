///////////////////////////////////////////////////////////////////////////////
//
// $Id: doFlowEvents.C,v 1.14 2000/06/05 15:22:19 posk Exp $
//
// Description: 
// Chain to read events from files into StFlowEvent and analyze.
// what it does: reads .dst.root, .xdf, flowevent.root, flownanoevent.root files 
//          to fill StFlowEvent
//
// Environment:
// Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Ways to run:
// If you specify a path, all DST files below that path will be
// found, and 'nevents' events will be analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file' ends in '.xdf', XDF DSTs are searched for.
// If 'file' ends in '.dst.root', ROOT DSTs are searched for.
// If 'file' ends in 'event.root' a StEvent file is used.
// If 'file' ends in 'flowevent.root' a StFlowEvent file is used.
// If 'file' ends in 'flownanoevent.root' a StFlowNanoEvent file is used.
// If 'file' ends in 'flowpicoevent.root' a StFlowPicoEvent file is used.
//
//  inputs:
//      nevents = # events to process
//      path = a. directory you want files from
//             b. "-" to get just the one file you want
//      file = a. file names in directory (takes all files)
//             b. the 1 particular full file name (with directory) you want
//      qaflag = "off"  - doesn't do anything now
//      wrStEOut = flag to turn on=1, off=0 writing of output test.event.root
//                 file --- set to off by default 
//
// Usage: 
// doFlowEvents.C(nevents, "-", "some_directory/some_dst_file.xdf")
// doFlowEvents.C(nevents, "-", "some_directory/some_dst_file.root")
// doFlowEvents.C(nevents, "some_directory", "*.dst.root/*.event.root")	
// doFlowEvents.C(nevents)	
// doFlowEvents.C()                // 2 events	
//
// Author List: Torre Wenaus, BNL  2/99
//              Victor Perevoztchikov
//              Art Poskanzer
//  
///////////////////////////////////////////////////////////////////////////////
//
// $Log: doFlowEvents.C,v $
// Revision 1.14  2000/06/05 15:22:19  posk
// Fixed typo in EOF recognition.
//
//
// Revision 1.12  2000/05/18 18:12:06  posk
// Updated to doEvents.C
//
// Revision 1.11  2000/05/17 16:20:59  kathy
// add some print stmts and also run some IOMaker methods in order to get default input files that are softlinks to other files working correctly
//
// Revision 1.10  2000/05/16 20:57:31  posk
// Voloshin's flownanoevent.root added.
//
// Revision 1.9  2000/05/11 00:22:28  posk
// Can read StEvent files which have extention .event.root .
//
// Revision 1.8  2000/05/09 19:38:22  kathy
// update to use standard default input files and only process few events by default - to make it easy to run in automatic macro testing script
//
// Revision 1.7  2000/04/24 20:25:45  posk
// Added doEvents.C updates.
//
// Revision 1.6  2000/04/13 21:46:34  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.5  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.4  2000/03/28 23:26:46  posk
// Allow multiple instances of the AnalysisMaker.
//
// Revision 1.3  2000/03/15 23:33:54  posk
// Added StFlowSelection.
//
// Revision 1.2  2000/03/07 17:51:23  snelling
// Added switch for Nano DST
//
// Revision 1.1  2000/03/02 23:49:11  posk
// Version of doEvents.C for flow analysis which can set cut parameters.
//
//
///////////////////////////////////////////////////////////////////////////////

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
const char *fileList[] = {dstFile, xdfFile, 0};

void doFlowEvents(Int_t, const Char_t **, const char *qaflag = "",
		  const Int_t wrStEOut = 0);
void doFlowEvents(Int_t, const Char_t *, const Char_t *, 
		  const char *qaflag = "off", const Int_t wrStEOut = 0);
void doFlowEvents(Int_t nevents = 2);


// ------------------ Here is the actual method -----------------------------------------
void doFlowEvents(Int_t nevents, const Char_t **fileList, const char *qaflag,
		  const Int_t wrStEOut)
{
  cout <<  endl << endl <<" doFlowEvents -  input # events = " << nevents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
      cout << " doFlowEvents -  input fileList = " << fileList[ilist] << endl;
      ilist++; 
    }
  cout << " doFlowEvents -  input qaflag   = " << qaflag << endl;
  cout << " doFlowEvents -  input wrStEOut = " << wrStEOut << endl << endl << endl;
 
  //
  // First load some shared libraries we need
  // Do it in this order
  //
  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");

  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StMagF");

  gSystem->Load("StFlowMaker");
  gSystem->Load("StFlowTagMaker");
  gSystem->Load("StFlowAnalysisMaker");
  
  // Make a chain with a file list
  chain  = new StChain("StChain");
  //chain->SetDebug();
  
  StFileI *setFiles =0;
  if (fileList) {	//Normal case
    setFiles= new StFile(fileList);
  } else        {	//Grand Challenge
    gSystem->Load("StChallenger");
    setFiles = StChallenger::Challenge();
    setFiles->SetDebug();
    Int_t Argc=4;
    const char *Argv[4]= {
      "-s","dst;hist;runco",
      "-q","-5<=qxa_3<0.3 && 22>qxc_1>18"
    };
    setFiles->Init(Argc,Argv);
  }
  
  if (strstr(fileList[0], "event.root")==0) {
    // Read raw events and make StEvent
    gSystem->Load("StEventMaker");
    StIOMaker *IOMk = new StIOMaker("IO", "r", setFiles, "bfcTree");
    IOMk->SetIOMode("r");
    IOMk->SetBranch("*", 0, "0");                 //deactivate all branches
    IOMk->SetBranch("dstBranch", 0, "r");
    IOMk->SetBranch("runcoBranch", 0, "r");
    //IOMk->SetDebug();
    StEventMaker *readerMaker =  new StEventMaker("events", "title");
    StFlowMaker* flowMaker = new StFlowMaker();
    if (wrStEOut) {
      // Write out StEvent
      cout << "doFlowEvents - will write out .event.root file" << endl << endl;
      StTreeMaker *outMk = new StTreeMaker("EvOut", "", "bfcTree");
      outMk->SetIOMode("w");
      outMk->SetBranch("eventBranch", "test.event.root", "w");
      outMk->IntoBranch("eventBranch", "StEvent");
    }
    
  } else if (strstr(fileList[0], "nano")==0 && strstr(fileList[0], "pico")==0) {
    // Read StEvent (or StFlowEvent) files
    StIOMaker *IOMk = new StIOMaker("IO", "r", setFiles, "bfcTree");
    IOMk->SetIOMode("r");
    IOMk->SetBranch("*", 0, "0");                 //deactivate all branches
    IOMk->SetBranch("eventBranch", 0, "r");
    IOMk->SetBranch("runcoBranch", 0, "r");
    //IOMk->SetDebug();
    StFlowMaker* flowMaker = new StFlowMaker();
    
  } else if (strstr(fileList[0], "nano")!=0) { 
    //Read nano-DST
    StFlowMaker* flowMaker = new StFlowMaker();
    flowMaker->NanoEventRead(kTRUE);
    flowMaker->SetNanoEventFileName(fileList[0]); 

  } else {
    //Read pico-DST
    StFlowMaker* flowMaker = new StFlowMaker();
    flowMaker->PicoEventRead(kTRUE);
    flowMaker->SetPicoEventFileName(fileList[0]); 
  }
  
  //////////////
  // Flow Makers
  //   Use of the TagMaker is optional.
  //   The AnalysisMaker may be used with a selection object.
  //   If you instantiate more than one AnalysisMaker,
  //      make sure each has a different selection object number
  //      and that you do not instantiate the TagMaker.
  //   If you want to read more than one PhiWeight file, instantiate multiple
  //      FlowMakers with the corresponding selection objects.
  //
  StFlowTagMaker* flowTagMaker = new StFlowTagMaker();
  //StFlowAnalysisMaker* flowAnalysisMaker = new StFlowAnalysisMaker();
  
  // Make Selection objects and instantiate Makers
  //StFlowSelection flowSelect;
  //StFlowSelection flowSelect1;
  //flowSelect1->SetNumber(1);
  //flowSelect->SetCentrality(0);
  //flowSelect1->SetPid("pi"); // pi+, pi-, pi, or proton
  //flowSelect1->SetPidPart("pi"); // pi+, pi-, pi, or proton
  //char makerName[30];
  //sprintf(makerName, "Flow%s", flowSelect->Number());
  //StFlowMaker* flowMaker = new StFlowMaker(makerName, flowSelect);
  //sprintf(makerName, "FlowAnalysis%s", flowSelect->Number());
  //StFlowAnalysisMaker* flowAnalysisMaker = new StFlowAnalysisMaker(makerName, flowSelect);
  //sprintf(makerName, "Flow%s", flowSelect1->Number());
  //StFlowMaker* flowMaker1 = new StFlowMaker(makerName, flowSelect1);
  //sprintf(makerName, "FlowAnalysis%s", flowSelect1->Number());
  //StFlowAnalysisMaker* flowAnalysisMaker1 = new StFlowAnalysisMaker(makerName, flowSelect1);
  
  //
  // Set write flages and file names
  //
  //flowMaker->NanoEventWrite(kTRUE);
  //flowMaker->SetNanoEventFileName("testnanoevent.root"); 
  //flowMaker->PicoEventWrite(kTRUE);
  //flowMaker->SetPicoEventFileName("/data06/posk/flow1picoevent.root"); 
  //flowMaker->FlowEventWrite(kTRUE);

  //flowMaker->FlowEventRead(kTRUE);
  
  // Set Debug status
  //flowMaker->SetDebug();
  //flowTagMaker->SetDebug();
  //flowAnalysisMaker->SetDebug();

  //
  // Initialize chain
  //
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit, "on init");
  chain->PrintInfo();
  
  //
  // Set the parameters
  //
  // Set the event cuts
  //StFlowCutEvent::SetMult(100, 10000);
  //StFlowCutEvent::SetVertexX(0., 0.);
  //StFlowCutEvent::SetVertexY(0., 0.);
  //StFlowCutEvent::SetVertexZ(-10., 10.);
  //StFlowCutEvent::SetEtaSym(-0.05, 0.05);
  
  // Set the track cuts
  //StFlowCutTrack::SetFitPts(15, 200);
  //StFlowCutTrack::SetFitOverMaxPts(0, 0);
  
  // Set the event plane selections
  //StFlowEvent::SetEtaCut(0.5, 1., 0, 0); // selection 1, harmonic 1
  //StFlowEvent::SetPtCut(0.2, 1., 0, 0);
  
  // Set the PID windows
  //StFlowEvent::SetPiPlusCut(-2., 2.);
  //StFlowEvent::SetPiMinusCut(-2., 2.);
  //StFlowEvent::SetProtonCut(-2., 2.);
  
  //
  // Event loop
  //
  int istat=0, i=1;
 EventLoop: if (i <= nevents && istat!=2) {
   cout << "============================ Event " << i
	<< " start ============================" << endl;
   chain->Clear();
   istat = chain->Make(i);
   if (istat==2) {cout << "Last  event processed. Status = " << istat << endl;}
   if (istat==3) {cout << "Error event processed. Status = " << istat << endl;}
   i++;

   //chain->Finish();

   goto EventLoop;
 }
  
  i--;
  cout << "============================ Event " << i
       << " finish ============================" << endl;
  if (nevents > 1) {
    chain->Clear();
    chain->Finish();
  }
  else {
    if (!b) {
      b = new TBrowser;
    }
  }
}

void doFlowEvents(const Int_t nevents, const Char_t *path, const Char_t *file, const char *qaflag, const Int_t wrStEOut)
{
  const char *fileListQQ[] = {0,0};
  if (strncmp(path, "GC", 2)==0) {
    fileListQQ = 0;
  } else if (path[0]=='-') {
    fileListQQ[0] = file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }

  doFlowEvents(nevents, fileListQQ, qaflag, wrStEOut);
}

void doFlowEvents(const Int_t nevents)
{
  // Commit to cvs with these defaults:
  const Char_t *filePath="-";
  const Char_t *fileExt="/afs/rhic/star/data/samples/gstar.dst.root";
  
  // BNL
  //Char_t* filePath="/star/rcf/data03/reco/auau200/mevsim/vanilla/flow/year_1h/hadronic_on/tfs_6/";
  //Char_t* fileExt="*.dst.root";
  
  // Both
  //Char_t* filePath="/afs/rhic/star/ebye/flow/";
  //Char_t* fileExt="test.event.root";
  
  //Char_t* filePath="/afs/rhic/star/ebye/flow/fixed10/";
  //Char_t* filePath="/afs/rhic/star/ebye/flow/random10/";
  //Char_t* fileExt="*.xdf";
  
  //Char_t* filePath="./";
  //Char_t* fileExt="*.event.root";
  
  //Char_t* filePath="./";
  //Char_t* fileExt="flownanoevent.root";
  
  //Char_t* filePath="/data06/posk/";
  //Char_t* fileExt="flowpicoevent.root";
  
  // LBNL
  //Char_t* filePath="/data06/snelling/flow/";
  //Char_t* fileExt="*.dst.root";

  doFlowEvents(nevents, filePath, fileExt);
}
