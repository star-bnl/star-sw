///////////////////////////////////////////////////////////////////////////////
//
// $Id: doFlowEvents.C,v 1.6 2002/03/23 21:45:52 posk Exp $
//
// Description: 
// Chain to read events from microDST files into StFlowEvent and analyze.
//
// Ways to run:
// If you specify a path, all DST files below that path will be
// found, and 'nevents' events will be analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file' ends in 'muDST.root' a StEbyeEvent file is used.
//
//  inputs:
//      nevents = # events to process
//      path = a. directory you want files from
//             b. "-" to get just the one file you want
//      file = a. file names in directory (takes all files)
//             b. the 1 particular full file name (with directory) you want
//                 file --- set to off by default 
//
// Usage: 
// doFlowEvents.C(nevents, "-", "some_directory/some_file")
// doFlowEvents.C(nevents, "some_directory", "*.root")	
// doFlowEvents.C(nevents)	
// doFlowEvents.C()                // 2 events	
//
// Author List: Torre Wenaus, BNL  2/99
//              Victor Perevoztchikov
//              Art Poskanzer
//  
///////////////////////////////////////////////////////////////////////////////
#include <iostream.h>
class    StChain;
StChain  *chain=0;
TBrowser *b=0;

const char *fileList[] = {0};

// ------------------ Prototypes ------------------------------

void doFlowEvents(Int_t, const Char_t **);
void doFlowEvents(Int_t, const Char_t *, const Char_t *);
void doFlowEvents(Int_t nevents = 2);

// ------------------ Here is the actual method ---------------

void doFlowEvents(Int_t nevents, const Char_t **fileList)
{
  cout <<  endl << endl <<" doFlowEvents -  input # events = " << nevents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
      cout << " doFlowEvents -  input fileList = " << fileList[ilist] << endl;
      ilist++; 
    }
 
  //
  // First load some shared libraries we need
  // Do it in this order
  //
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StarClassLibrary");

  gSystem->Load("StFlowMaker");
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
    const char *Argv[]= {
      "-s","dst runco",                           // list of components needed
      "-q","n_trk_tpc[0]>1000 && n_trk_tpc[1]>1000",   // example of user query
      "-c","/afs/rhic/star/incoming/GCA/daq/stacs.rc"  // GC servers for daq
    };
    Int_t Argc=sizeof(Argv)/4;
    setFiles->Init(Argc,Argv);
  }
  
  //
  // Make Selection objects and instantiate FlowMakers
  //
  char makerName[30];
  StFlowSelection flowSelect;
  // particles:  pi+, pi-, pi, e-, e+, pbar, proton
  flowSelect->SetPidPart("pi");               // for parts. wrt plane
//  flowSelect->SetPtPart(0., 1.);              // for parts. wrt plane
//  flowSelect->SetPPart(0.15, 5.);             // for parts. wrt plane
//  flowSelect->SetEtaPart(0., 0.);             // for parts. wrt plane
//  flowSelect->SetFitPtsPart(20, 50);          // for parts. wrt plane
//  flowSelect->SetFitOverMaxPtsPart(0.52, 1.); // for parts. wrt plane
//  flowSelect->SetChiSqPart(0.1, 1.3);         // for parts. wrt plane
//  flowSelect->SetDcaPart(0., 0.8);            // for parts. wrt plane
//  flowSelect->SetYPart(-0.5, 0.5);            // for parts. wrt plane

  // uncomment next line if you make a selection object
  sprintf(makerName, "Flow");
  if ((strstr(fileList[0], "muDST")!=0)) {
    //Read microDST
    if (makerName[0]=='\0') {
      StFlowMaker* flowMaker = new StFlowMaker();
    } else {
      StFlowMaker* flowMaker = new StFlowMaker(makerName, flowSelect);
    }
    flowMaker->SetMicroEventFileName(setFiles); 
  } else {
    cout<<"##### doFlowEvents: No micro DST"<<endl;
    goto END;
  }
  
  if (makerName[0]=='\0') { // blank if there is no selection object
    StFlowAnalysisMaker* flowAnalysisMaker = new StFlowAnalysisMaker();
  } else {
    sprintf(makerName, "FlowAnalysis");
    StFlowAnalysisMaker* flowAnalysisMaker = new StFlowAnalysisMaker(makerName, flowSelect);
  }

  //
  // Chain Init()
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

  // Set the event cuts
//    StFlowCutEvent::SetCent(3, 3);
//    StFlowCutEvent::SetMult(10, 10000);
//    StFlowCutEvent::SetFinalMult(6, 10000);
//    StFlowCutEvent::SetVertexX(-3.6, 3.6);
//    StFlowCutEvent::SetVertexY(-3.5, 3.5);
//    StFlowCutEvent::SetVertexZ(-579.8, -578.);
//    StFlowCutEvent::SetEtaSym(0., 0.);

  // Set the track cuts
//    StFlowCutTrack::SetFitPtsV1(20, 200);
//    StFlowCutTrack::SetFitPtsV2(20, 200);
//    StFlowCutTrack::SetFitPtsM(30, 200);
//    StFlowCutTrack::SetMaxPtsV1(20, 200);
//    StFlowCutTrack::SetMaxPtsV2(20, 200);
//    StFlowCutTrack::SetMaxPtsM(30, 200);
//    StFlowCutTrack::SetFitOverMaxPts(0.55, 2.);
//    StFlowCutTrack::SetChiSq(0., 10.);
//    StFlowCutTrack::SetDca(0., 2.);
//    StFlowCutTrack::SetPt(0., 2.);
//    StFlowCutTrack::SetEta(0., 0.);
//    StFlowCutTrack::SetBx(-3.,3.);
//    StFlowCutTrack::SetBy(-0.5,0.5);
  
  // Set the event plane selections
//   StFlowEvent::SetYCut(0., 0., 0, 0); // harmonic 1, selection 1
//   StFlowEvent::SetYCut(0., 0., 1, 0); // harmonic 2, selection 1
//   StFlowEvent::SetYCut(0., 0., 2, 0); // harmonic 3, selection 1
//    StFlowEvent::SetYCut(2.5, 4., 0, 1); // harmonic 1, selection 2
//    StFlowEvent::SetYCut(1.8, 3.5, 1, 1); // harmonic 2, selection 2
//    StFlowEvent::SetYCut(2.5, 4., 2, 1); // harmonic 3, selection 2

//   StFlowEvent::SetPtCut(0.05, 1., 0, 0);
//   StFlowEvent::SetPtCut(0.05, 1., 1, 0);
//   StFlowEvent::SetPtCut(0., 2., 2, 0);
//   StFlowEvent::SetPtCut(0.05, 1., 0, 1);
//   StFlowEvent::SetPtCut(0.05, 1., 1, 1);
//   StFlowEvent::SetPtCut(0., 2., 2, 1);

  if (Flow::eBeam == 40) {
    StFlowCutEvent::SetVertexZ(-581.7, -580.5);
//    StFlowEvent::SetYCut(2.5, 4., 0, 1); // harmonic 1, selection 2
//    StFlowEvent::SetYCut(1.8, 3.5, 1, 1); // harmonic 2, selection 2
//    StFlowEvent::SetYCut(2.5, 4., 2, 1); // harmonic 3, selection 2
    StFlowEvent::SetYCut(3., 5., 0, 1); // harmonic 1, selection 2
//    StFlowEvent::SetYCut(1.8, 4., 1, 1); // harmonic 2, selection 2
    StFlowEvent::SetYCut(3., 5., 1, 1); // harmonic 2, selection 2
    StFlowEvent::SetPtCut(0., 0.5, 1, 1);
    StFlowEvent::SetYCut(3., 5., 2, 1); // harmonic 3, selection 2
    StFlowCutEvent::SetEtaSym(0.36, 0.84);
    //Method using only sin correlation terms for bad acceptance
    StFlowEvent::SetSinOnly();
  }

//   StFlowEvent::SetMeanSinCosCut(-0.1, 0.1);

  // Use weights in the event plane calcualtion
  StFlowEvent::SetPtWgt();
  StFlowEvent::SetYWgt();

  // Calculate v2 from the 1st harmonic event plane
//  StFlowAnalysisMaker::SetV21();

  // Make striped subevents
// StFlowEvent::SetStripes(1);  // either 1 or 2

  // Particles for the event plane
  // particles:  pi+, pi-, pi, e-, e+, pbar, proton
// StFlowEvent::SetPid("p");                    // for all charged particles
// StFlowEvent::SetPid("pi-");

  // Use probability PID method
//  StFlowEvent::SetProbPid();
  
  //
  // Event loop
  //
  int istat=0, i=1;
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

  //
  // Chain Finish()
  //
  if (nevents > 1) {
    chain->Clear();
    chain->Finish();
    delete chain;
  }
  else {
    if (!b) {
      b = new TBrowser;
    }
  }

END:
}

// ----------- This concatenates the path and the file name ---------------------
void doFlowEvents(const Int_t nevents, const Char_t *path, const Char_t *file)
{
  const char *fileListQQ[] = {0,0};
  if (strncmp(path, "GC", 2)==0) {
    fileListQQ = 0;
  } else if (path[0]=='-') {
    fileListQQ[0] = file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }

  doFlowEvents(nevents, fileListQQ);
}

// ----------- This sets default path and file names ---------------------------
void doFlowEvents(const Int_t nevents)
{
  // BNL
  //Char_t* filePath="/direct/star+data01/pwg/ebye/awetzler/na49";
  //Char_t* fileExt="*muDST.root";

  //LBL
  //Char_t* filePath="/auto/na49/160GeV/std+";
  Char_t* filePath="/auto/na49/160GeV/std-";
  //Char_t* filePath="/auto/na49/160GeV/std+cen";
  //Char_t* filePath="/auto/na49/40GeV/std+";
  //Char_t* filePath="/auto/na49/40GeV/std-";
  //Char_t* filePath="/auto/na49/40GeV/cen";
  Char_t* fileExt="*muDST.root";

  doFlowEvents(nevents, filePath, fileExt);
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: doFlowEvents.C,v $
// Revision 1.6  2002/03/23 21:45:52  posk
// More 40 GeV compatability.
//
// Revision 1.5  2002/01/16 18:21:35  posk
// Fit q in plot.C. Updated momentum conservation corr. in vProj.C.
//
// Revision 1.4  2001/11/06 18:02:43  posk
// 40 GeV compatability.
//
// Revision 1.3  2001/08/17 22:14:42  posk
// Updated to also do 40 GeV.
//
// Revision 1.2  2001/05/14 23:15:20  posk
// Lower pt uppers for centralities 1 and 2.
//
// Revision 1.1  2001/02/23 00:57:53  posk
// NA49 version of STAR software.
//
// Revision 1.27  2000/11/15 14:41:51  posk
//
///////////////////////////////////////////////////////////////////////////////
