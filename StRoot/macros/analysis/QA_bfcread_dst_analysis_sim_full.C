// $Id: QA_bfcread_dst_analysis_sim_full.C,v 1.1 2000/01/31 21:28:41 kathy Exp $
// $Log: QA_bfcread_dst_analysis_sim_full.C,v $
// Revision 1.1  2000/01/31 21:28:41  kathy
// 2 new macros that read the dst file(s) and run the postdst analysis Makers
//
//
//======================================================================
// owner:  Kathy Turner
// what it does: 
//    - reads  *.dst.root, *.geant.root, & *.runco.root files using StIOMaker
//    - runs 
//         StIOMaker       - to read input files    (name = IO)
//         StEventMaker    - load StEvent data structures (name = events)
//         StAnalysisMaker - sample analysis + QA for StEvent (name = analysis)
//         StEventQAMaker  - QA histograms for StEvent (name = EventQA)
//         StMcEventMaker  - load StMcEvent data structures (name = MCEvent)
//         StAssociationMaker - associate generated & reconstructed variables 
//                              (name = Associations)
//    - draws/prints to ps file  EventQA histograms using StHistUtil methods
//
// For use with simulated data only (... needs the geant.root file)
//=======================================================================
//
// inputs: nevents   - # events to process
//         MainFile  - input *.dst.root or *.dst.xdf file from bfc output
//         psFile    - output postscript filename
//         MakerHistDir - this is the Maker name that you want to get histograms
//                        from - leave as "EventQA" for this macro since this 
//                        macro is setup to run StEventQAMaker!
//         PageTitle - title you want on each output page, default = "" is
//                       MainFile name
//         PrintList - name of subset histogram list that you want printed
//                   - these are defined in StHistUtil, method SetDefaultPrintList
//                   - default = "", prints all histograms in directory
//
// standard Maker names in bfc,doEvents
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
//  http://www.star.bnl.gov/STAR/html/comp_l/train/tut/bfc_maker_names.html
//                                                     doEvents_maker_names.html
//
//======================================================================

class StChain;
StChain *chain;

//TBrowser *brow=0;

void QA_bfcread_dst_analysis_sim_full(
     Int_t nevents=2,
     const Char_t *MainFile=
      "/afs/rhic/star/data/samples/gstar.dst.root",
    const Char_t *psFile="Event_QA_analysis_hist.ps",
    const Char_t *MakerHistDir="EventQA",
    const Char_t *PageTitle="",
    const Char_t *PrintList="")
{
//
  cout << "QA_bfcread_dst_analysis_sim_full.C, num events to process " << 
        nevents  << endl;
  cout << "QA_bfcread_dst_analysis_sim_full.C, input file name       " << 
        MainFile << endl;
  cout << "QA_bfcread_dst_analysis_sim_full.C, output psfile name    " << 
        psFile   << endl;
  cout <<"QA_bfcread_dst_analysis_sim_full.C, Maker directory containing histograms =  " <<
        MakerHistDir   << endl;
  cout << "QA_bfcread_dst_analysis_sim_full.C, hist page title " << 
        PageTitle  << endl;
  cout << "QA_bfcread_dst_analysis_sim_full.C, subset list name of which histograms to draw,print = " <<
        PrintList  << endl;


  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StIOMaker");
  gSystem->Load("StMagF");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");
  gSystem->Load("StAnalysisMaker");
  gSystem->Load("St_QA_Maker");  
  gSystem->Load("StAnalysisUtilities");

  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StAssociationMaker");


//  Setup top part of chain
  chain = new StChain("MyChain");
  chain->SetDebug();
   
// Input File Maker - Mainfile is .dst.root but also
//                 need to set branches "runco" and "geant" on
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
//   also open the runco branch in addition to dst branch (input file)
  IOMk->SetBranch("runcoBranch",0,"r");
  IOMk->SetBranch("geantBranch",0,"r");
  IOMk->SetDebug();

// constructor for other maker (not used in chain)
  StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
  HU->SetPntrToMaker((StMaker *)IOMk);


//  add other makers to chain:
  StEventMaker    *eventMaker    = new StEventMaker("events","title");
  StAnalysisMaker *analysisMaker = new StAnalysisMaker("analysis");
  StEventQAMaker  *EventQA       = new StEventQAMaker("EventQA","StEvent/QA");
  StMcEventMaker     *mcEventReader = new StMcEventMaker; 
  StAssociationMaker *associator    = new StAssociationMaker;

// Define the cuts for the StAssociationMaker
    StMcParameterDB* parameterDB = StMcParameterDB::instance();  
    // TPC
    parameterDB->setXCutTpc(.5); // 5 mm
    parameterDB->setYCutTpc(.5); // 5 mm
    parameterDB->setZCutTpc(.2); // 2 mm
    parameterDB->setReqCommonHitsTpc(3); // Require 3 hits in common for tracks to be associated
    // FTPC
    parameterDB->setRCutFtpc(.3); // 3 mm
    parameterDB->setPhiCutFtpc(5*(3.1415927/180.0)); // 5 degrees
    parameterDB->setReqCommonHitsFtpc(3); // Require 3 hits in common for tracks to be associated
    // SVT
    parameterDB->setXCutSvt(.1); // 1 mm
    parameterDB->setYCutSvt(.1); // 1 mm
    parameterDB->setZCutSvt(.1); // 1 mm
    parameterDB->setReqCommonHitsSvt(1); // Require 1 hits in common for tracks to be associated


// --- now execute chain member functions --> Init
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();
 

// loop over events:
  int iev=0,iret=0, evnum=0;
 EventLoop: if (iev<nevents && !iret) {  // goto loop code
   evnum=iev+1;
   cout <<  " !!! QA_bfcread_dst_analysis_sim_full.C, processing event !!! " << evnum << endl ;
   chain->Clear();
   iret = chain->Make();
   iev++;                                // goto loop code
   goto EventLoop;                       // goto loop code
 }

  cout <<  " !!! QA_bfcread_dst_analysis_sim_full.C, passed chain->Make !!!" << endl ;

// ---- rest of macro is to draw,print histograms ------------------


// method to print out list of histograms - 
//can do this anytime after they're booked
  Int_t NoHist=0;
  NoHist = HU->ListHists(MakerHistDir);
  cout << " !!! QA_bfcread_dst_analysis_sim_full.C, No. of Hist we have == " << NoHist << endl;

// Set the default canvas style to plain (so it won't print out grey!)
  gROOT->SetStyle("Plain");
//    gStyle->SetOptStat(111111);

  HU->SetHistsNamesDraw("*","*");
  HU->SetPostScriptFile(psFile);
  HU->SetZones(2,3);
  HU->SetPaperSize();
  HU->SetDefaultLogXList(MakerHistDir);
  HU->SetDefaultLogYList(MakerHistDir);
    if (PageTitle=="") PageTitle=MainFile;
  HU->SetGlobalTitle(PageTitle);

  HU->SetDefaultPrintList(MakerHistDir,PrintList);

  Int_t numLog = 0;
  numLog = HU->ExamineLogYList();
  cout << " QA_bfcread_dst_analysis_sim_full.C, Number hist to plot with log scale = " << numLog << endl;

  Int_t numPrint = 0;
  numPrint = HU->ExaminePrintList();
  cout << " QA_bfcread_dst_analysis_sim_full.C, Number hist to print = " << numPrint << endl;

//  Now draw the actual histograms to canvas and to ps file
  HU->DrawHists(MakerHistDir);

  chain->Finish();
  cout <<  "QA_bfcread_dst_analysis_sim_full.C, passed chain->Finish" << endl ; 

}


