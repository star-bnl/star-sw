///////////////////////////////////////////////////////////////////////////////
//
// $Id: doFlowEvents.C,v 1.41 2002/06/10 22:56:40 posk Exp $
//
// Description: 
// Chain to read events from files into StFlowEvent and analyze.
// It reads dst.root, picoevent.root, or MuDst.root files to fill StFlowEvent
//
// Environment:
// Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Ways to run:
// If you specify a path, all DST files below that path will be
// found, and 'nevents' events will be analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file' ends in '.dst.root', ROOT DSTs are searched for.
// If 'file' ends in '.event.root' a StEvent file is used.
// If 'file' ends in 'flowpicoevent.root' a StFlowPicoEvent file is used.
// If 'file' ends in 'MuDst.root' a StMuDST file is used.
//
//  inputs:
//      nevents = # events to process
//      path = a. directory you want files from
//             b. "-" to get just the one file you want
//      file = a. file names in directory (takes all files)
//             b. the one particular full file name (with directory) you want
//      qaflag = "off"  - doesn't do anything now
//
// Usage: 
// doFlowEvents.C(nevents, "-", "some_directory/some_dst_file.root")
// doFlowEvents.C(nevents, "some_directory", "*.root")	
// doFlowEvents.C(nevents)	
// doFlowEvents.C()                // 2 events
//
// Parameters, RunType and OutPicoDir, may be passed from the calling LSF shell script
//   (see pdsf:: ~posk/doFlowEvents.csh):
//        root4star -b << eof >& $LOG
//        Int_t RunType = $runNo ;
//        Char_t* OutPicoDir = $outPicoDir ;
//        .L $doFile
//        doFlowEvents.C
//        .q
//eof
//
// Author List: Torre Wenaus, BNL  2/99
//              Victor Perevoztchikov
//              Art Poskanzer
//              Raimond Snellings
//              Kirill Filimonov
//  
///////////////////////////////////////////////////////////////////////////////
#include <iostream.h> // needed on Solaris
class    StChain;
StChain  *chain = 0;
TBrowser *b = 0;
Int_t    RunType;
Char_t*  OutPicoDir;

const char *dstFile = 0;
const char *fileList[] = {dstFile, 0};

//--------- Prototypes -----------
void doFlowEvents(Int_t, const Char_t **, const char *qaflag = "");
void doFlowEvents(Int_t, const Char_t *, const Char_t *, 
		  const char *qaflag = "off");
void doFlowEvents(Int_t nevents = 2);

// -------- Here is the actual method ----------
void doFlowEvents(Int_t nevents, const Char_t **fileList, const char *qaflag)
{
  cout <<  endl << endl <<" doFlowEvents -  input # events = " << nevents << endl;
  Int_t ilist = 0;
  while (fileList[ilist]){ 
      cout << " doFlowEvents -  input fileList = " << fileList[ilist] << endl;
      ilist++; 
    }
  cout << " doFlowEvents -  input qaflag   = " << qaflag << endl << endl << endl;
 
  //
  // First load some shared libraries we need
  // (Do it in this order)
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
  gSystem->Load("StEventUtilities");
  gSystem->Load("StMagF");

  gSystem->Load("StFlowMaker");
  gSystem->Load("StFlowTagMaker");
  gSystem->Load("StFlowAnalysisMaker");

  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  
  // Make a chain with a file list
  chain  = new StChain("StChain");
  //chain->SetDebug();
  StFileI *setFiles = 0;
  if (fileList) {	//Normal case
    setFiles = new StFile(fileList);
  } else        {	//Grand Challenge
    gSystem->Load("StChallenger");
    setFiles = StChallenger::Challenge();
    setFiles->SetDebug();
    const char *Argv[]= {
      "-s","dst runco",                               // list of components needed
      "-q","n_trk_tpc[0]>1000 && n_trk_tpc[1]>1000",  // example of user query
      "-c","/afs/rhic/star/incoming/GCA/daq/stacs.rc" // GC servers for daq
    };
    Int_t Argc=sizeof(Argv)/4;
    setFiles->Init(Argc,Argv);
  }
  
  //
  // Make Selection objects and instantiate FlowMaker
  //
  char makerName[30];
//   StFlowSelection flowSelect;
  // particles:h+, h-, pi+, pi-, pi, k+, k-, k, e-, e+, e, pr-, pr+, pr, d+, d-, and d
//   flowSelect->SetPidPart("h+");               // for parts. wrt plane
//   flowSelect->SetPtPart(0., 8.);              // for parts. wrt plane
//   flowSelect->SetPtBinsPart(256);             // for parts. wrt plane
//   flowSelect->SetPPart(0.15, 5.);             // for parts. wrt plane
//   flowSelect->SetEtaPart(0., 0.);             // for parts. wrt plane
//   flowSelect->SetFitPtsPart(20, 50);          // for parts. wrt plane
//   flowSelect->SetFitOverMaxPtsPart(0.52, 1.); // for parts. wrt plane
//   flowSelect->SetChiSqPart(0.1, 1.3);         // for parts. wrt plane
//   flowSelect->SetDcaGlobalPart(0., 0.);       // for parts. wrt plane
//   flowSelect->SetYPart(-0.5, 0.5);            // for parts. wrt plane

  // uncomment next line if you make a selection object
//   sprintf(makerName, "Flow");

  if (strstr(fileList[0], "MuDst.root")) {
    //Read mu-DST
    //cout << "##### doFlowEvents: MuDST file" << endl;
    if (makerName[0]=='\0') {
      StFlowMaker* flowMaker = new StFlowMaker();
    } else {
      StFlowMaker* flowMaker = new StFlowMaker(makerName, flowSelect);
    }
    flowMaker->MuEventRead(kTRUE);
    flowMaker->SetMuEventFileName(setFiles);
    
  } else if (strstr(fileList[0], "picoevent.root")) {
    //Read pico-DST
    //cout << "##### doFlowEvents: pico file" << endl;
    if (makerName[0]=='\0') {
      StFlowMaker* flowMaker = new StFlowMaker();
    } else {
      StFlowMaker* flowMaker = new StFlowMaker(makerName, flowSelect);
    }
    flowMaker->PicoEventRead(kTRUE);
    flowMaker->SetPicoEventFileName(setFiles);
    
  } else if (strstr(fileList[0], ".dst.root") ||
	     strstr(fileList[0], ".event.root")) {
    // Read raw events and make StEvents or read StEvents
    TString mainBranch;
    mainBranch = fileList[0];
    mainBranch.ReplaceAll(".root","");
    int idot = strrchr((char*)mainBranch,'.') - mainBranch.Data();
    mainBranch.Replace(0,idot+1,"");
    mainBranch+="Branch";
    printf("*** mainBranch=%s ***\n",mainBranch.Data());
    
    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
    IOMk->SetIOMode("r");
    IOMk->SetBranch("*",0,"0");	//deactivate all branches
    if(!mainBranch.IsNull())	IOMk->SetBranch(mainBranch,0,"r");  
    IOMk->SetBranch("dstBranch",0,"r");
    IOMk->SetBranch("runcoBranch",0,"r");
    //IOMk->SetDebug();
    
    // Maker to read events from file or database into StEvent
    if (!mainBranch.Contains("eventBranch")) {
      gSystem->Load("StEventMaker");
      StEventMaker *readerMaker =  new StEventMaker("events","title");
    }
    
    if (makerName[0]=='\0') {
      StFlowMaker* flowMaker = new StFlowMaker();
    } else {
      StFlowMaker* flowMaker = new StFlowMaker(makerName, flowSelect);
    }
    
  } else {
    cout << "##### doFlowEvents:  unknown file name = " << fileList[0] << endl;
  }
  
  //////////////
  // Flow Makers
  //   Use of the TagMaker is optional.
  //   The AnalysisMaker, CumulantMaker, and ScalarProdMaker may be used with
  //   a selection object.
  bool tagMaker = kFALSE;
  //bool tagMaker = kTRUE;
  //bool anaMaker = kFALSE;
  bool anaMaker = kTRUE;
  bool cumMaker = kFALSE;
  //bool cumMaker = kTRUE;
  bool spMaker = kFALSE;
  //bool spMaker = kTRUE;

  if (tagMaker) StFlowTagMaker* flowTagMaker = new StFlowTagMaker();

  if (makerName[0]=='\0') { // blank if there is no selection object
    if (anaMaker) StFlowAnalysisMaker*  flowAnalysisMaker = new StFlowAnalysisMaker();
    if (cumMaker) StFlowCumulantMaker*  flowCumulantMaker = new StFlowCumulantMaker();
    if (spMaker) StFlowScalarProdMaker* flowScalarProdMaker = new StFlowScalarProdMaker();
  } else {
    if (anaMaker) {
      sprintf(makerName, "FlowAnalysis");
      StFlowAnalysisMaker* flowAnalysisMaker = new 
	StFlowAnalysisMaker(makerName, flowSelect);
    }
    if (cumMaker) {
      sprintf(makerName, "FlowCumulant");
      StFlowCumulantMaker* flowCumulantMaker = new 
	StFlowCumulantMaker(makerName, flowSelect);
    }
    if (spMaker) {
      sprintf(makerName, "FlowScalarProd");
      StFlowScalarProdMaker* flowScalarProdMaker = new 
	StFlowScalarProdMaker(makerName, flowSelect);
    }
  }

  // Set write flages and file names
//  flowMaker->PicoEventWrite(kTRUE);
//  flowMaker->SetPicoEventDir("./");
//  flowMaker->SetPicoEventDir(*OutPicoDir);
  
  // Set Debug status
//  flowMaker->SetDebug();
//  flowTagMaker->SetDebug();
//  flowAnalysisMaker->SetDebug();
//  flowCumulantMaker->SetDebug();
//  flowScalarProdMaker->SetDebug();

  //
  // Initialize chain
  //
  Int_t iInit = chain->Init();
  chain->PrintInfo();
  if (iInit) {
    chain->Fatal(iInit, "on init");
    goto END;
  }

  //
  // Set the parameters
  //

  // Get centrality from RunType
  if (RunType) {
    Int_t centrality = RunType % 10 ;
    StFlowCutEvent::SetCent(centrality, centrality);
  }

  // Set the event cuts
//   StFlowCutEvent::SetCent(3, 3);
//   StFlowCutEvent::SetMult(0, 0);
//   StFlowCutEvent::SetVertexX(0., 0.);
//   StFlowCutEvent::SetVertexY(0., 0.);
//   StFlowCutEvent::SetVertexZ(0., 0.);
//   StFlowCutEvent::SetEtaSym(0., 0.);
//   StFlowCutEvent::SetTrigger(0.);
  
  // Set the track cuts
//   StFlowCutTrack::SetFitPtsTpc(0, 0);
//   StFlowCutTrack::SetFitPtsFtpc(0, 0);
//   StFlowCutTrack::SetFitOverMaxPts(0., 0.);
//   StFlowCutTrack::SetChiSqTpc(0., 0.);
//   StFlowCutTrack::SetChiSqFtpc(0., 0.);
//   StFlowCutTrack::SetDcaFtpc(0., 0.);
//   StFlowCutTrack::SetPtTpc(0., 0.);
//   StFlowCutTrack::SetPtFtpc(0., 0.);
//   StFlowCutTrack::SetEtaTpc(0., 0.);
//   StFlowCutTrack::SetEtaFtpc(0., 0., 0., 0.);
//   StFlowCutTrack::SetChgTpc(0, 1); // positive tracks

  // Set the event plane selections
//   StFlowEvent::SetEtaTpcCut(0.05, 1., 0, 0);  // harmonic 1, selection 1
//   StFlowEvent::SetEtaTpcCut(0.05, 1., 1, 0);  // harmonic 2, selection 1
//   StFlowEvent::SetEtaTpcCut(0.05, 1., 2, 0);  // harmonic 3, selection 1
//   StFlowEvent::SetEtaTpcCut(0.05, 1., 1, 1);  // harmonic 2, selection 2
//   StFlowEvent::SetEtaFtpcCut(0.05, 1., 1, 1); // harmonic 2, selection 2
//   StFlowEvent::SetPtTpcCut(0.0, 1., 1, 1);    // harmonic 2, selection 2
//   StFlowEvent::SetPtFtpcCut(0.0, 1., 1, 1);   // harmonic 2, selection 2
//   StFlowEvent::SetDcaGlobalCut(0., 1.);       // for event plane

  // particles:h+, h-, pi+, pi-, pi, k+, k-, k, e-, e+, e, pr-, pr+, pr, d+, d-, and d
//   StFlowEvent::SetPid("h-");                 // for event plane

  // Make Eta subevents
//  StFlowEvent::SetEtaSubs();

  // Enable weights in the event plane calcualtion
//   StFlowEvent::SetPtWgt(kFALSE);
//   StFlowEvent::SetEtaWgt(kFALSE);

  // Use Aihong's probability PID method
//   StFlowEvent::SetProbPid();

  // Have the CumulantMaker use the old method
//   StFlowCumulantMaker::SetOldMethod(kTRUE); 

  // Set the PID deviant windows
//   StFlowEvent::SetPiPlusCut(-3., 3.);
//   StFlowEvent::SetPiMinusCut(-3., 3.);
//   StFlowEvent::SetProtonCut(-3., 3.);
//   StFlowEvent::SetAntiProtonCut(-3., 3.);
//   StFlowEvent::SetKPlusCut(-3., 3.);
//   StFlowEvent::SetKMinusCut(-3., 3.);
//   StFlowEvent::SetDeuteronCut(-3., 3.);
//   StFlowEvent::SetAntiDeuteronCut(-3., 3.);
//   StFlowEvent::SetElectronCut(-3., 3.);
//   StFlowEvent::SetPositronCut(-3., 3.);
  
  //
  // Event loop
  //
  int istat = 0, i = 1;
 EventLoop: if (i <= nevents && istat != 2) {
   
   cout << endl << "============================ Event " << i
	<< " start ============================" << endl;
   
   chain->Clear();
   istat = chain->Make(i);
   if (istat == 2) 
     {cout << "Last  event processed. Status = " << istat << endl;}
   if (istat == 3) 
     {cout << "Error event processed. Status = " << istat << endl;}
   
   //   gObjectTable->Print();
   i++;
   goto EventLoop;
 }
  
  i--;
  cout << endl << "============================ Event " << i
       << " finish ============================" << endl;

  //
  // Chain Finish
  //
  if (nevents > 1) {
    //chain->Clear();
    chain->Finish();
    delete chain;
  }
  else {
    if (!b) {
      b = new TBrowser;
    }
  }

  // Move the flow.cumulant.root and flow.scalar.root files into the 
  // flow.hist.root file.
  if (cumMaker) {
    TFile cumFile("flow.cumulant.root", "READ");
    if (cumFile->IsOpen()) { 
      cumFile.ReadAll();
    } else {
      cout << "### Can't find file flow.cumulant.root" << endl;
    }
  }
  if (spMaker) {
    TFile spFile("flow.scalar.root", "READ");
    if (spFile->IsOpen()) { 
      spFile.ReadAll();
    } else {
      cout << "### Can't find file flow.scalar.root" << endl;
    }
  }
  if (anaMaker) {
    TFile anaFile("flow.hist.root", "UPDATE");
  } else {
    TFile anaFile("flow.hist.root", "RECREATE");
  }
  if (anaFile->IsOpen()) {
    if (cumMaker) cumFile.GetList()->Write();
    if (spMaker)   spFile.GetList()->Write();
    //anaFile->ls();
    anaFile.Close();    
  } else {
    cout << "### Can't find file flow.hist.root" << endl;
  }

END:
}

// ----------- This concatenates the path and the file name ---------------------
void doFlowEvents(const Int_t nevents, const Char_t *path, const Char_t *file, 
		  const char *qaflag)
{
  const char *fileListQQ[] = {0,0};
  if (strncmp(path, "GC", 2) == 0) {
    fileListQQ = 0;
  } else if (path[0] == '-') {
    fileListQQ[0] = file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }

  doFlowEvents(nevents, fileListQQ, qaflag);
}

// ----------- This sets default path and file names ---------------------------
void doFlowEvents(const Int_t nevents)
{
  // Commit to cvs with these defaults:
   const Char_t *filePath="-";
   const Char_t *fileExt="/afs/rhic/star/data/samples/gstar.dst.root";

//  Char_t* filePath="./";
//  Char_t* fileExt="*.event.root";
//  Char_t* fileExt="*.flowpicoevent.root";
//  Char_t* fileExt="*.MuDst.root";

// PDSF pico files
  // 200 GeV
  
  // P02gc
//   Char_t* filePath="/auto/stardata/starspec2/flow_pDST_production/reco/ProductionMinBias/FullField/P02gc/2001/";
//   if (nevents < 250) {
//     Char_t* fileExt="2269018/st_physics_2269018_raw_0001.flowpicoevent.root";
//   } else {
//     Char_t* fileExt="*/*.flowpicoevent.root";
//   }

  // P02gd
//   Char_t* filePath="/auto/stardata/starspec3/flow_pDST_production/reco/ProductionMinBias/ReversedFullField/P02gd/2001/2258044/";
//   if (nevents < 250) {
//     Char_t* fileExt="st_physics_2258044_raw_0026.flowpicoevent.root";
//    } else {
//      Char_t* fileExt="*.flowpicoevent.root";
//    }

  // 130 GeV
//   Char_t* filePath="/auto/pdsfdv10/starprod/DST/kirll_flow_pDST_minbias/";
//   if (nevents < 250) {
//     Char_t* fileExt="st_physics_1239006_raw_0017.event.root.flowpicoevent.root";
//   } else {
//     Char_t* fileExt="*.flowpicoevent.root";
//   }

  // 22 GeV
//   Char_t* filePath="/auto/stardata/starspec3/flow_pDST_production/reco/minBias22GeVZDC/ReversedHalfField/P02ge/2001/2329085";
//   if (nevents < 250) {
//     Char_t* fileExt="st_physics_2329085_raw_0001.flowpicoevent.root";
//   } else {
//     Char_t* fileExt="*.flowpicoevent.root";
//   }

  // muDST files
  // 200 GeV
//   Char_t* filePath="/beta/starprod/MuDST/P02gc/Common/ProductionMinBias/RevFullField/";
//   if (nevents < 250) {
//     Char_t* fileExt="st_physics_2269002_raw_0177.MuDst.root"; 
//   } else {
//     Char_t* fileExt="st_physics_2269002_raw_*.MuDst.root";
//   }


  // event.root files
  // Char_t* filePath="/beta/starprod/kirill/";
  // Char_t* fileExt="st_physics_2269002_raw_0177.event.root"; 

  doFlowEvents(nevents, filePath, fileExt);
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: doFlowEvents.C,v $
// Revision 1.41  2002/06/10 22:56:40  posk
// pt and eta weighting now default.
//
// Revision 1.40  2002/06/07 17:29:43  kirill
// Introduced MuDst reader
//
// Revision 1.39  2002/02/13 22:37:09  posk
// Added SetEtaWeight(bool) command.
//
// Revision 1.38  2002/02/05 17:00:37  posk
// Added commands for SetPtBinsPart and h+/h-
//
// Revision 1.37  2002/01/30 13:05:13  oldi
// Trigger cut implemented.
//
// Revision 1.36  2002/01/15 17:36:34  posk
// Can instantiate StFlowScalarProdMaker.
//
// Revision 1.35  2001/12/18 19:32:02  posk
// "proton" and "antiproton" replaced with "pr+" and "pr-".
//
// Revision 1.34  2001/12/11 22:14:33  posk
// Combined histogram output files from the CumulantMaker and the AnalysisMaker.
//
// Revision 1.33  2001/11/09 21:44:34  posk
// Added StFlowCumulantMaker.
//
// Revision 1.32  2001/06/07 20:12:12  posk
// Added global dca cut for event plane particles.
// Changed SePtWgt() to SetPtWgt(Bool_t).
//
// Revision 1.31  2001/05/22 19:58:42  posk
// Can take centrality from the shell script.
// Removed multiple instances feature.
//
// Revision 1.30  2000/12/12 18:49:18  posk
// Moved log comments to the end of the file.
//
// Revision 1.29  2000/12/08 17:04:36  oldi
// Phi weights for both FTPCs included.
//
// Revision 1.27  2000/11/15 14:41:51  posk
// Protected against running Finish() twice.
//
// Revision 1.26  2000/11/13 01:32:35  snelling
// load StEventUtilities
//
// Revision 1.25  2000/11/09 17:39:14  snelling
// Added switch for probability pid
//
// Revision 1.24  2000/09/16 22:21:15  snelling
// Added lines to set selection on P and global DCA
//
// Revision 1.23  2000/09/15 22:54:44  posk
// Added Pt weighting for event plane calculation.
//
// Revision 1.22  2000/09/15 01:22:27  snelling
// Added the new selection options to the macro
//
// Revision 1.21  2000/09/05 16:29:43  snelling
// Added cuts for new particles
//
// Revision 1.18  2000/08/28 16:15:50  snelling
// Added Pt and Eta cuts to macro
//
// Revision 1.17  2000/08/26 21:39:51  snelling
// Modified IO for multiple pico events
//
// Revision 1.15  2000/06/30 14:57:34  posk
// Updated to latest doEvents.C .
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
///////////////////////////////////////////////////////////////////////////////
