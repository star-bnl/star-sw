///////////////////////////////////////////////////////////////////////////////
//
// $Id: doFlowEvents.C,v 1.7 2011/07/25 15:54:47 posk Exp $
// Put a link to this at /StRoot/macros/analysis/doFlowEvents.C
//
// Description: 
// Chain to read events from files into StFlowEvent and analyze.
// It reads dst.root, event.root, MuDst.root, or picoevent.root
//   files to fill StFlowEvent
//
// Environment:
// Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Ways to run:
// If you specify a path, all DST files below that path will be
// found, and 'nEvents' events will be analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file' ends in '.dst.root', ROOT DSTs are searched for.
// If 'file' ends in '.event.root' a StEvent file is used.
// If 'file' ends in 'MuDst.root' a StMuDST file is used.
// If 'file' ends in 'flowpicoevent.root' a StFlowPicoEvent file is used.
//
//  inputs:
//      nEvents = # events to process
//      path = a. directory you want files from
//             b. "-" to get just the one file you want
//      file = a. file names in directory (takes all files)
//             b. the one particular full file name (with directory) you want
//      firstPass = kTRUE runs StFlowPhiWgtMaker or StFlowReCentMaker only
//
// Usage: 
// doFlowEvents.C(nEvents, "-", "some_directory/some_dst_file.root")
// doFlowEvents.C(nEvents, "some_directory", "*.root")	
// doFlowEvents.C(nEvents, "some_directory/some_dst_file.root")
// doFlowEvents.C(nEvents)	   // default file
// doFlowEvents.C()                // 2 events
//
// Parameters, RunType and OutPicoDir, may be passed from the calling shell script
//   (see pdsf:: ~posk/doFlowSubmit.pl):
//        root4star -b << eof >& $LOG
//        Int_t RunType = $runNo;
//        const Char_t* OutPicoDir = "./$outPicoDir/";
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
//              Markus Oldenburg
//  
///////////////////////////////////////////////////////////////////////////////
class    StChain;
StChain  *chain = 0;
TBrowser *b = 0;
Int_t    RunType;
Char_t*  OutPicoDir;
class    StFileI;
StFileI* setFiles = 0;
TString  mainBranch;

const char *dstFile = 0;
const char *fileList[] = {dstFile, 0};

//--------- Prototypes -----------
void doFlowEvents(Int_t nEvents, const Char_t **fileList, Bool_t firstPass = kFALSE);
void doFlowEvents(Int_t nEvents, const Char_t *path, const Char_t *file, 
		  Bool_t firstPass = kFALSE);
void doFlowEvents(Int_t nEvents, const Char_t *path/file, Bool_t firstPass = kFALSE);
void doFlowEvents(Int_t nEvents = 2, Bool_t firstPass = kFALSE);

// -------- Here is the actual method ----------
void doFlowEvents(Int_t nEvents, const Char_t **fileList, Bool_t firstPass)
{
  cout <<  endl << endl <<" doFlowEvents - input # events = " << nEvents << endl;
  Int_t ilist = 0;
  while (fileList[ilist]) { 
    cout << " doFlowEvents - input fileList = " << fileList[ilist] << endl;
    ilist++; 
  }

  Int_t maxTheta = 5;  // LYZ
  Int_t nSels    = 2;
  Bool_t reCent  = kFALSE;

  if (firstPass) {
    cout << " doFlowEvents - firstPass makers = kTRUE" << endl;
  } else {
    cout << " doFlowEvents - firstPass makers = kFALSE" << endl;
  }
  if (reCent) {
    cout << " doFlowEvents - reCent = kTRUE" << endl;
  } else {
    cout << " doFlowEvents - phiWgt = kTRUE" << endl;
  }
  cout << endl << endl;

  //
  // First load some shared libraries we need
  //
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StFlowMaker");
  gSystem->Load("StFlowAnalysisMaker");
  
  // Make a chain with a file list
  chain  = new StChain("StChain");
  setFiles = new StFile(fileList);
  
  //
  // Make Selection objects
  //
  char makerName[30];
  StFlowSelection flowSelect;
  // particles:h+, h-, pi+, pi-, pi, k+, k-, k, e-, e+, e, pr-, pr+, pr, d+, d-, and d
//   flowSelect.SetPidPart("pr-");               // for parts. wrt plane
//   flowSelect.SetPtPart(0.15, 6.0);            // for parts. wrt plane
//   flowSelect.SetPtPart(0.15, 2.0);            // for parts. wrt plane
//   flowSelect.SetPtBinsPart(60);               // for parts. wrt plane
//   flowSelect.SetPPart(0.15, 5.);              // for parts. wrt plane
//   flowSelect.SetEtaPart(-1.1, 1.1);           // for parts. wrt plane
//   flowSelect.SetFitPtsPart(20, 50);           // for parts. wrt plane
//   flowSelect.SetFitOverMaxPtsPart(0.52, 1.);  // for parts. wrt plane
//   flowSelect.SetChiSqPart(0.1, 1.3);          // for parts. wrt plane
//   flowSelect.SetDcaGlobalPart(0., 2.0);       // for parts. wrt plane
//   flowSelect.SetYPart(-0.5, 0.5);             // for parts. wrt plane

  // Uncomment next line if you make a selection object. Always!
  sprintf(makerName, "Flow");
  
  // Determine the kind of file and instantiate the FlowMaker after the IOMaker
  StIOMaker *IOMk = new StIOMaker("IO", "r", setFiles, "bfcTree");
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");	// deactivate all branches
  if (!mainBranch.IsNull()) { IOMk->SetBranch(mainBranch,0,"r"); }

  if (strstr(fileList[0], "MuDst.root")) {
    // Read mu-DST
    if (makerName[0]=='\0') {
      StFlowMaker* flowMaker = new StFlowMaker();
    } else {
      StFlowMaker* flowMaker = new StFlowMaker(makerName, flowSelect);
    }
    flowMaker->MuEventRead(kTRUE);
    flowMaker->SetMuEventFileName(setFiles);
    
  } else if (strstr(fileList[0], "picoevent.root")) {
    // Read pico-DST
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
    mainBranch = fileList[0];
    mainBranch.ReplaceAll(".root","");
    int idot = strrchr((char*)mainBranch,'.') - mainBranch.Data();
    mainBranch.Replace(0,idot+1,"");
    mainBranch += "Branch";
    printf("*** mainBranch=%s ***\n",mainBranch.Data());
    
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
  //   The AnalysisMaker, CumulantMaker, ScalarProdMaker, DirectCumulantMaker, and LeeYangZerosMaker 
  //   may be used with a selection object.
  Bool_t reCentMaker;
  Bool_t phiWgtMaker;
  Bool_t anaMaker;
  Bool_t cumuMaker;
  Bool_t spMaker;
  Bool_t lyzMaker;
  Bool_t dirCumuMaker;
  if (firstPass) {
    if (reCent) {
      reCentMaker = kTRUE;
      phiWgtMaker = kFALSE;
    } else {
      reCentMaker = kFALSE;
      phiWgtMaker = kTRUE;
    }
    anaMaker    = kFALSE;
    cumuMaker   = kFALSE;
    spMaker     = kFALSE;
    lyzMaker    = kFALSE;
  } else {
    reCentMaker = kFALSE;
    phiWgtMaker = kFALSE;
    anaMaker    = kTRUE;
    cumuMaker   = kFALSE;
    spMaker     = kFALSE;
    lyzMaker    = kFALSE;
    dirCumuMaker= kFALSE;
  }
  Bool_t includeTpcTracks  = kTRUE;
  Bool_t includeFtpcTracks = kTRUE; // must be kTRUE if sel 1 is FTPC EP

// For LYZ and anaMaker
  Float_t ptRange_for_vEta[2] = {0.15, 2.};
  Float_t etaRange_for_vPt[2] = {0., 1.1}; // show only TPC particles in v(pt)
//   Float_t ptRange_for_vEta[2] = {0., 0.}; // integrate over the full pt range
//   Float_t etaRange_for_vPt[2] = {0., 0.}; // integrate over the full eta range
//   Float_t etaRange_for_vPt[2] = {2.5, 4.}; // show only FTPC particles in v(pt)
  
  // Set recentering FALSE except for ana and LYZ makers
  // For LYZ: If recentering is TRUE, pass zero will be run to calculate the recentering
  //   parameters
  // For ana: If recentering is TRUE, first pass will calculate
  //   the recentering parameters and write them to flow.reCentAnaNew.root
  if (anaMaker) {
    if (reCent) {
      flowMaker->SetPhiWgtCalc(kFALSE);
      flowMaker->SetReCentCalc(kTRUE);
    } else {
      flowMaker->SetPhiWgtCalc(kTRUE);
      flowMaker->SetReCentCalc(kFALSE);
    }
  } else if (lyzMaker) {
    flowMaker->SetReCentCalc();
  } else {
    flowMaker->SetPhiWgtCalc();
    flowMaker->SetReCentCalc(kFALSE);
  }

  // To calculate v1{EP1,EP2} use the following switch.
  // Since v1{EP1} doesn't work very well at RHIC energies, v1{EP1,EP2} is set to be 
  // the default.
  // To make full use of it, the cuts for Har1 should allow for FTPC tracks only, 
  // while the cuts for Har2 should use TPC tracks only. This method works for 
  // FTPC eta subevents (SetEtaSubs(kTRUE)) and random subevents (SetEtaSubs(kFALSE)).
  Bool_t v1Ep1Ep2 = kFALSE;

  if (makerName[0]=='\0') { // blank if there is no selection object
    if (anaMaker) {
      StFlowAnalysisMaker*  flowAnalysisMaker = new StFlowAnalysisMaker();
      flowAnalysisMaker->SetHistoRanges(includeFtpcTracks);
      flowAnalysisMaker->SetPtRange_for_vEta(ptRange_for_vEta[0], ptRange_for_vEta[1]);
      flowAnalysisMaker->SetEtaRange_for_vPt(etaRange_for_vPt[0], etaRange_for_vPt[1]);
      flowAnalysisMaker->SetV1Ep1Ep2(v1Ep1Ep2);
    }
    if (cumuMaker) {
      StFlowCumulantMaker*  flowCumulantMaker = new StFlowCumulantMaker();
      flowCumulantMaker->SetHistoRanges(includeFtpcTracks);
    }
    if (spMaker) {
      StFlowScalarProdMaker* flowScalarProdMaker = new StFlowScalarProdMaker();
      flowScalarProdMaker->SetHistoRanges(includeFtpcTracks);
    }
    if (lyzMaker) {
      StFlowLeeYangZerosMaker* flowLeeYangZerosMaker = new StFlowLeeYangZerosMaker();
      flowLeeYangZerosMaker->SetHistoRanges(includeFtpcTracks);
      flowLeeYangZerosMaker->SetPtRange_for_vEta(ptRange_for_vEta[0], ptRange_for_vEta[1]);
      flowLeeYangZerosMaker->SetEtaRange_for_vPt(etaRange_for_vPt[0], etaRange_for_vPt[1]);
    }
    if (dirCumuMaker) {
      StFlowDirectCumulantMaker*  flowDirectCumulantMaker = new StFlowDirectCumulantMaker();
    }
  } else {
    if (anaMaker) {
      sprintf(makerName, "FlowAnalysis");
      StFlowAnalysisMaker* flowAnalysisMaker = new 
	StFlowAnalysisMaker(makerName, flowSelect);
      flowAnalysisMaker->SetHistoRanges(includeFtpcTracks);
      flowAnalysisMaker->SetPtRange_for_vEta(ptRange_for_vEta[0], ptRange_for_vEta[1]);
      flowAnalysisMaker->SetEtaRange_for_vPt(etaRange_for_vPt[0], etaRange_for_vPt[1]);
      flowAnalysisMaker->SetV1Ep1Ep2(v1Ep1Ep2);
    }
    if (cumuMaker) {
      sprintf(makerName, "FlowCumulant");
      StFlowCumulantMaker* flowCumulantMaker = new 
	StFlowCumulantMaker(makerName, flowSelect);
      flowCumulantMaker->SetHistoRanges(includeFtpcTracks);
    }
    if (spMaker) {
      sprintf(makerName, "FlowScalarProd");
      StFlowScalarProdMaker* flowScalarProdMaker = new 
	StFlowScalarProdMaker(makerName, flowSelect);
      flowScalarProdMaker->SetHistoRanges(includeFtpcTracks);
    }
    if (lyzMaker) {
      sprintf(makerName, "FlowLeeYangZeros");
      StFlowLeeYangZerosMaker* flowLeeYangZerosMaker = new 
	StFlowLeeYangZerosMaker(makerName, flowSelect);
      flowLeeYangZerosMaker->SetHistoRanges(includeFtpcTracks);
      flowLeeYangZerosMaker->SetPtRange_for_vEta(ptRange_for_vEta[0], ptRange_for_vEta[1]);
      flowLeeYangZerosMaker->SetEtaRange_for_vPt(etaRange_for_vPt[0], etaRange_for_vPt[1]);
    }
    if (dirCumuMaker) {
      sprintf(makerName, "FlowDirectCumulant");
      StFlowDirectCumulantMaker* flowDirectCumulantMaker = new 
	StFlowDirectCumulantMaker(makerName, flowSelect);
    }
  }
  if (phiWgtMaker) {
    StFlowPhiWgtMaker*  flowPhiWgtMaker = new StFlowPhiWgtMaker();
  }
  if (reCentMaker) {
    StFlowReCentMaker*  flowReCentMaker = new StFlowReCentMaker();
  }

  // Set write flages and file names
//   flowMaker->PicoEventWrite(kTRUE);
//   cout << " doFlowEvents -  OutPicoDir = " << OutPicoDir << endl;
//   flowMaker->SetPicoEventDir(OutPicoDir);
//   flowMaker->SetPicoEventDir("../");
//   flowMaker->SetPicoEventDir("./");

  // Set Debug status
//   IOMk->SetDebug(1);
//   flowMaker->SetDebug();
//   flowAnalysisMaker->SetDebug();
//   flowPhiWgtMaker->SetDebug();
//   flowCumulantMaker->SetDebug();
//   flowScalarProdMaker->SetDebug();
//   flowLeeYangZerosMaker->SetDebug();
//   flowDirectCumulantMaker->SetDebug();
//   chain->SetDebug();
//   StMuDebug::setLevel(0);

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
  // For centrality=0 there is no centrality selection
  if (RunType) {
    Int_t centrality = RunType % 10 ; // last digit
    StFlowCutEvent::SetCent(centrality, centrality);
  }
  
  // Set the event cuts
//   StFlowCutEvent::SetCent(5, 5);
//   StFlowCutEvent::SetMult(0, 0);
//   StFlowCutEvent::SetVertexX(0., 0.);
//   StFlowCutEvent::SetVertexY(0., 0.);
  StFlowCutEvent::SetVertexZ(-30., 30.);
//   StFlowCutEvent::SetEtaSymTpc(0., 0.);
  StFlowCutEvent::SetEtaSymFtpc(0., 0.); // no FTPC eta sym cut
   if (firstPass) { // all centralities
     StFlowCutEvent::SetCent(0, 0);
   }
  
  // Set the track cuts
   StFlowCutTrack::IncludeTpcTracks(includeTpcTracks);
//   StFlowCutTrack::SetFitPtsTpc(0, 0);
//   StFlowCutTrack::SetFitOverMaxPts(0., 0.);
//   StFlowCutTrack::SetChiSqTpc(0., 0.);
//    StFlowCutTrack::SetPtTpc(0.15, 2.); // for integrated cumulant
//    StFlowCutTrack::SetEtaTpc(-1.1, 1.1); // for integrated cumulant
//   StFlowCutTrack::SetChgTpc(0., 0.);
  
   StFlowCutTrack::IncludeFtpcTracks(includeFtpcTracks);
//   StFlowCutTrack::SetFitPtsFtpc(0, 0);
   StFlowCutTrack::SetChiSqFtpc(0., 4.);
   StFlowCutTrack::SetDcaFtpc(0., 2.);
//   StFlowCutTrack::SetDcaGlobalFtpc(0., 0.);
   StFlowCutTrack::SetPtFtpc(0.15, 2.);
//   StFlowCutTrack::SetEtaFtpc(-4.0, -2.5, 2.5, 4.0);
//   StFlowCutTrack::SetChgFtpc(0, 0);

  // Set the event plane selections
   // Harmonic 1 means odd, harmonic 2 means even
   // For selection 1 = FTPC event plane, selection 2 = TPC event plane
   // (Must include FTPC paticles>):
   StFlowEvent::SetEtaTpcCut(9., 10., 0, 0);  // harmonic 1, selection 1, no TPC
   StFlowEvent::SetEtaTpcCut(9., 10., 1, 0);  // harmonic 2, selection 1, no TPC
   StFlowEvent::SetEtaFtpcCut(-10., -9., 9., 10., 0, 1);  // harmonic 1, selection 2, no FTPC
   StFlowEvent::SetEtaFtpcCut(-10., -9., 9., 10., 1, 1);  // harmonic 2, selection 2, no FTPC
  // TPC
//   StFlowEvent::SetEtaTpcCut(0.5, 2., 0, 0);  // harmonic 1, selection 1
//   StFlowEvent::SetEtaTpcCut(0.0, 1., 1, 0);  // harmonic 2, selection 1
//   StFlowEvent::SetPtTpcCut(0.0, 1., 1, 1);   // harmonic 2, selection 2
//   StFlowEvent::SetDcaGlobalTpcCut(0., 1.);   // for event plane
  // FTPC
//   StFlowEvent::SetPtFtpcCut(0., 10., 0, 0);   // harmonic 1, selection 1
//   StFlowEvent::SetPtFtpcCut(0., 10., 1, 0);   // harmonic 2, selection 1
//   StFlowEvent::SetPtFtpcCut(0., 10., 0, 1);   // harmonic 1, selection 2
//   StFlowEvent::SetPtFtpcCut(0., 10., 1, 1);   // harmonic 2, selection 2
//   StFlowEvent::SetDcaGlobalFtpcCut(0., 1.);   // for event plane

  // particles:h+, h-, pi+, pi-, pi, k+, k-, k, e-, e+, e, pr-, pr+, pr, d+, d-, and d
//   StFlowEvent::SetPid("h-");                 // for event plane

    // Make Eta or Random subevents
    // These correlate each particle with the other subevent plane.
    // With neither flag set the standard method is used, which
    // corelates each particle with the event plane from the full event
    // minus the particle of interest, and subevents are made according to eta.
    // Don't set both of these at the same time.
//    StFlowEvent::SetEtaSubs();
//     StFlowEvent::SetRanSubs();
   // With either of these set the higher harmonics are done with respect to the event planes
   // of the higher harmonic. This is not a good idea as the second harmonic full event plane
   // would be better.

  // Disable weights for the event plane and integrated flow for LYZ
    if (reCent && lyzMaker) {
      StFlowEvent::SetPtWgt(kFALSE);
    }
//   StFlowEvent::SetPtWgtSaturation(1.);
    StFlowEvent::SetPtWgt(kTRUE);
    StFlowEvent::SetEtaWgt(kFALSE);

   // In LeeYangZeros do not use mixed harmonics for v1
//    StFlowLeeYangZerosMaker::SetV1Mixed(kFALSE);

  // use ZDCSMD for the event plane
//     StFlowEvent::SetUseZDCSMD(kTRUE);

  // Use Aihong's probability PID method
//     StFlowEvent::SetProbPid();

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
   int istat = 0, iEvt = 1;
EventLoop: if (iEvt <= nEvents && istat != 2) {
     
     cout << "===== Event " << iEvt << " start ===== " << endl;
     chain->Clear();
     istat = chain->Make(iEvt);
     if (istat == 2) 
       {cout << "Last  event processed. Status = " << istat << endl;}
     if (istat == 3) 
       {cout << "Error event processed. Status = " << istat << endl;}
     
     iEvt++;
     goto EventLoop;
   }
  
   iEvt--;
   cout << "============================ Event " << iEvt
	<< " finish ============================" << endl;

   //
   // Chain Finish
   //
   if (nEvents > 1) {
     chain->Finish();
   } else {
     if (!b) { b = new TBrowser; }
   }

   TVectorD* cumulConstants = new TVectorD(30); // temporary fix for a root bug
   TObjString* cumulMethodTag = new TObjString( "cumulNew" );

   // Move the flow.cumulant.root, flow.scalar.root, and flow.LeeYangZeros.root files
   // into the flow.hist.root file.
   if (cumuMaker) {
    TFile cumuFile("flow.cumulant.root", "READ");
    if (cumuFile.IsOpen()) { 
      cumuFile.ReadAll();
      for (int mm=0; mm<30; mm++)  // temporary fix for a root bug
	(*cumulConstants)(mm) =
	  (*((TVectorD* )cumuFile.Get("CumulConstants")))(mm);
    } else {
      cout << "### Can't find file flow.cumulant.root" << endl;
    }
   }
   if (spMaker) {
     TFile spFile("flow.scalar.root", "READ");
     if (spFile.IsOpen()) { 
       spFile.ReadAll();
     } else {
       cout << "### Can't find file flow.scalar.root" << endl;
     }
   }
   if (lyzMaker) {
     // combine the zero, first, and second pass outputs
     TFile lyzFirstPassFile("flow.firstPassLYZ.root", "READ");
     if (lyzFirstPassFile.IsOpen()) { 
       TFile lyzZeroPassFile("flow.reCent.root", "READ");
       if (lyzZeroPassFile.IsOpen()) { 
	 lyzZeroPassFile.ReadAll();
	 TList* zeroPassList = lyzZeroPassFile.GetList();
	 //zeroPassList->ls();
       }
       lyzFirstPassFile.ReadAll();
       TList* firstPassList = lyzFirstPassFile.GetList();
       //firstPassList->ls();
       TString* histTitle; // remove ReG and ImG
       for (int k = 0; k < nSels; k++) {
	 for (int j = 0; j < 2; j++) { // only 2 harmonics in the first pass file
	   for (int Ntheta = 0; Ntheta < maxTheta; Ntheta++) {
	     histTitle = new TString("FlowImGtheta");
	     *histTitle += Ntheta;
	     *histTitle += "_Sel";
	     *histTitle += k+1;
	     *histTitle += "_Har";
	     *histTitle += j+1;
	     hist = firstPassList->FindObject(histTitle->Data());
	     firstPassList->Remove(hist);
	     delete histTitle;
	     histTitle = new TString("FlowReGtheta");
	     *histTitle += Ntheta;
	     *histTitle += "_Sel";
	     *histTitle += k+1;
	     *histTitle += "_Har";
	     *histTitle += j+1;
	     hist = firstPassList->FindObject(histTitle->Data());
	     firstPassList->Remove(hist);
	     delete histTitle;
	   }
	 }
       }
       //firstPassList->ls();
       TFile lyzFile("flow.LeeYangZeros.root", "UPDATE");
       if (lyzFile.IsOpen()) {
	 if (lyzZeroPassFile.IsOpen()) { 
	   zeroPassList->Write();
	 }
	 firstPassList->Write();
	 lyzFile.Close();
       }
     }
     TFile lyzFile("flow.LeeYangZeros.root", "READ");
     if (lyzFile.IsOpen()) {
       lyzFile.ReadAll();
     } else {
       cout << "### Can't find file flow.LeeYangZeros.root" << endl;
     }
   }
   if (anaMaker) {
     TFile anaFile("flow.hist.root", "UPDATE");
   } else {
     TFile anaFile("flow.hist.root", "RECREATE");
   }
   if (anaFile.IsOpen()) {
     if (cumuMaker) {
       cumuFile.GetList()->Write();
       cumulConstants->Write("CumulConstants",TObject::kOverwrite | TObject::kSingleKey);
       cumulMethodTag->Write("CumulMethodTag",TObject::kOverwrite | TObject::kSingleKey);
     }    
     if (spMaker) { spFile.GetList()->Write(); }
     if (lyzMaker) { lyzFile.GetList()->Write(); }
     //anaFile.ls();
     anaFile.Close();    
   } else {
     cout << "### Can't find file flow.hist.root" << endl;
   }
   
END:
}

// ----------- This concatenates the path and the file name ---------------------
void doFlowEvents(Int_t nEvents, const Char_t *path, const Char_t *file, 
		  Bool_t firstPass)
{
  const char *fileListQQ[] = {0,0};
  if (path[0] == '-') {
    fileListQQ[0] = file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  doFlowEvents(nEvents, fileListQQ, firstPass);
}

// ----------- When only a file is specified ---------------------
void doFlowEvents(Int_t nEvents, const char *file, Bool_t firstPass)
{
  printf("*file = %s\n",file);
  const char *fileListQQ[]={0,0};
  fileListQQ[0]=file;
  cout << "Calling (nEvents, fileListQQ, firstPass)" << endl;
  doFlowEvents(nEvents, fileListQQ, firstPass);
}

// ----------- This sets default path and file names ---------------------------
void doFlowEvents(Int_t nEvents, Bool_t firstPass) {

//  Char_t* filePath="./";
//  Char_t* fileExt="*.flowpicoevent.root";
//  Char_t* fileExt="*.event.root";
//  Char_t* fileExt="*.MuDst.root";

  // 200 GeV

  // run 4 P05ic
  // muDST files
//   Char_t* filePath="/eliza9/starprod/reco/productionMinBias/ReversedFullField/P05ic/2004/024/";
//   if (nEvents < 450) {
//     Char_t* fileExt="st_physics_5024001_raw_1010001.MuDst.root";
//     //Char_t* fileExt="st_physics_5024092_raw_1010002.MuDst.root";
//   } else {
//     Char_t* fileExt="*.MuDst.root";
//   }

  // run 7 P07id
  // muDST files
//   Char_t* filePath="/eliza3/starprod/reco/2007ProductionMinBias/FullField/P07id/2007/131/8131027/";
//   //Char_t* filePath="./outDir/muDST/";
//   if (nEvents < 450) {
//     //Char_t* fileExt="st_physics_8102049_raw_1010001.MuDst.root"; // 45 events
//     Char_t* fileExt="st_physics_8131027_raw_1010001.MuDst.root";
//   } else {
//     Char_t* fileExt="*.MuDst.root";
//   }

  // run 7 P08ic
  // muDST files
  Char_t* filePath="/eliza9/starprod/reco/2007ProductionMinBias/FullField/P08ic/2007/125/";
  if (nEvents < 450) {
    Char_t* fileExt="st_physics_8125119_raw_1040090.MuDst.root";
  } else {
    Char_t* fileExt="st_physics_*.MuDst.root";
  }

  // run 10 39 GeV
  // muDST files
//   Char_t* filePath="/eliza17/star/starprod/reco/2010Production/reco/AuAu39_production/ReversedFullField/P10ik/2010/";
//   if (nEvents < 450) {
//     Char_t* fileExt="099/11099061/st_physics_11099061_raw_5030001.MuDst.root";
//   } else {
//     Char_t* fileExt="st_physics_*.MuDst.root";
//   }

  doFlowEvents(nEvents, filePath, fileExt, firstPass);
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: doFlowEvents.C,v $
// Revision 1.7  2011/07/25 15:54:47  posk
// Added correction for non-flatness of event plane.
//
// Revision 1.6  2011/03/10 18:56:28  posk
// Added histogram for laboratory azimuthal distribution of particles.
//
// Revision 1.5  2010/09/30 19:28:15  posk
// Instead of reversing the weight for negative pseudrapidity for odd harmonics,
// it is now done only for the first harmonic.
// Recentering is now done for all harmonics.
//
// Revision 1.4  2010/07/23 21:01:46  posk
// Added a comment about higher harmonics with the subevent method.
//
// Revision 1.3  2010/06/10 16:33:59  posk
// Correction to macro directCumulants_v2.C .
//
// Revision 1.2  2010/03/08 16:54:49  posk
// Added StFlowDirectCumulantMaker written by Dhevan Gangadharan.
//
// Revision 1.65  2009/11/24 19:40:35  posk
// Added reCenter to remove acceptance correlations as an option instead of phiWgt.
// Default selection 1 now calculates the event plane from the FTPCs and selection 2 from the main TPC.
//
// Revision 1.64  2009/07/24 21:00:11  posk
// Removed John Wu's Grid Collector.
//
// Revision 1.61  2006/03/22 22:15:26  posk
// Updated to read the flow.firstPassLYZ.root files.
//
// Revision 1.60  2006/02/22 19:48:08  posk
// Added StFlowLeeYangZerosMaker
// For MuDsts, the IOMaker is now the default
//
// Revision 1.59  2005/08/31 15:03:09  fisyak
// Add dependence StMagF vs StarMagField
//
// Revision 1.58  2005/02/10 18:01:38  posk
// Option for working with the Grid Collector.
//
// Revision 1.57  2004/12/17 16:54:13  aihong
// temporary fix for a root bug on TVectorD
//
// Revision 1.56  2004/11/19 18:05:19  posk
// A bit more like doEvents.C
//
// Revision 1.55  2004/06/23 20:06:02  perev
// const Int_t replaced by Int_t
//
// Revision 1.54  2004/03/11 18:01:56  posk
// Added Random Subs analysis method.
//
// Revision 1.53  2003/12/19 21:23:43  posk
// Changed File->IsOpen() to File.IsOpen().
//
// Revision 1.52  2003/12/12 02:29:40  oldi
// Minor code clean-ups. Some comments added.
//
// Revision 1.51  2003/11/14 20:01:22  oldi
// Implementation of v1{EP1,EP2}. This method is set to be the default for v1 now!
// Minor code clean-ups.
//
// Revision 1.50  2003/09/05 18:01:37  posk
// Updated list of shared libraries.
//
// Revision 1.49  2003/08/26 21:18:12  posk
// update
//
// Revision 1.48  2003/08/06 20:54:26  oldi
// Introduction of possibility to exclude pt ranges for v(eta) and eta regions
// for v(pt) histograms. Default behavior stays the same (all available tracks
// are included in v(pt) and v(eta)).
//
// Revision 1.47  2003/07/30 22:09:18  oldi
// Eta cuts for event plane selection separated for FTPC east and west.
// PtWgtSaturation parameter introduced (default set to 2. -> no change of default behavior).
//
// Revision 1.46  2003/06/27 21:17:18  posk
// Event plane cuts now only odd and even, instead of different for each harmonic.
//
// Revision 1.45  2003/05/16 20:47:33  posk
// Runs only StFlowPhiWgtMaker if called with phiWgtOnly=kTRUE.
//
// Revision 1.44  2003/01/14 14:12:07  oldi
// Possibility to exclude TPC tracks completely (= FTPC only).
//
// Revision 1.43  2003/01/10 16:42:39  oldi
// Several changes to comply with FTPC tracks:
// - Switch to include/exclude FTPC tracks introduced.
//   The same switch changes the range of the eta histograms.
// - Eta symmetry plots for FTPC tracks added and separated from TPC plots.
// - PhiWgts and related histograms for FTPC tracks split in FarEast, East,
//   West, FarWest (depending on vertex.z()).
// - Psi_Diff plots for 2 different selections and the first 2 harmonics added.
// - Cut to exclude mu-events with no primary vertex introduced.
//   (This is possible for UPC events and FTPC tracks.)
// - Global DCA cut for FTPC tracks added.
// - Global DCA cuts for event plane selection separated for TPC and FTPC tracks.
// - Charge cut for FTPC tracks added.
//
// Revision 1.42  2002/07/01 16:11:54  posk
// Removed StFlowTagMaker.
//
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
