// $Id: bfcread_event_QAhist.C,v 1.3 2000/06/02 20:26:10 lansdell Exp $
// $Log: bfcread_event_QAhist.C,v $
// Revision 1.3  2000/06/02 20:26:10  lansdell
// added check on Make() return codes
//
// Revision 1.2  2000/05/15 20:24:01  kathy
// correct Log,Id so they get written out
//
//
//======================================================================
// owner:  Curtis Lansdell
// what it does: 
//    - reads in *.event.root
//    - sets event branch
//    - runs StEventQAMaker
//    - draws EventQA histograms and then sends them to a postscript file
//
//=======================================================================
//
// inputs: nevents   - # events to process
//         MainFile  - input *.event.root file from bfc output
//         psFile    - output postscript filename
//         PageTitle - title you want on each output page, default = "" is
//                       MainFile name
//         PrintList - name of subset histogram list that you want printed
//                   - these are defined in StHistUtil, method SetDefaultPrintList
//                   - default = "", prints all histograms in directory
//         MakerHistDir - this is the Maker name that you want to get histograms
//                        from - leave as "QA" for this macro since this 
//                        macro is setup to run St_QA_Maker!
//
// standard Maker names in bfc,doEvents
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
//  http://www.star.bnl.gov/STAR/html/comp_l/train/tut/bfc_maker_names.html
//                                                     doEvents_maker_names.html
//
// Documentation on StEventQAMaker class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/St_QA_Maker/doc/
//
// Documentation on StHistUtil class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/StAnalysisUtilities/doc/
//
//
//======================================================================

class StChain;
StChain *chain;

//TBrowser *brow=0;

void bfcread_event_QAhist(
     Int_t nevents=2,
     const Char_t *MainFile=
     //"/afs/rhic/star/data/samples/gstar.dst.root",
"/star/rcf/test/dev/tfs_redhat61/Tue/year_1h/hc_standard/hc_standard.40_evts.event.root",
    const Char_t *psFile="EventQAhist.ps",
    const Char_t *PageTitle="",
    const Char_t *PrintList="",
    const Char_t *MakerHistDir="EventQA")
{
//
  cout << "bfcread_event_QAhist.C, num events to process " << 
     nevents  << endl;
  cout << "bfcread_event_QAhist.C, input file name       " << 
     MainFile << endl;
  cout << "bfcread_event_QAhist.C, output psfile name    " << 
     psFile   << endl;
  cout << "bfcread_event_QAhist.C, hist page title " << 
     PageTitle  << endl;
  cout << "bfcread_event_QAhist.C, Maker directory containing histograms =  " 
       << MakerHistDir   << endl;
  cout << "bfcread_event_QAhist.C, subset list name of which histograms to draw,print = "
       << PrintList  << endl;

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker");  
  gSystem->Load("StEvent");

//  Setup top part of chain
  chain = new StChain("MyChain");
  chain->SetDebug();
   
// Input File Maker
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetBranch("event",0,"r");
  IOMk->SetDebug();

// constructor for other maker (not used in chain)
  StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
  HU->SetPntrToMaker((StMaker *)IOMk);

//  add other makers to chain:
  StEventQAMaker *EventQA = new StEventQAMaker("EventQA","StEvent/QA");

// --- now execute chain member functions --> Init
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();
 
// method to print out list of histograms - 
//can do this anytime after they're booked
  Int_t NoHist=0;
  NoHist = HU->ListHists(MakerHistDir);
  cout << " !!! bfcread_event_QAhist.C, No. of Hist we have == " << NoHist << endl;

// loop over events:
  int iev=0,iret=0, evnum=0;
 EventLoop: if (iev<nevents && iret!=2) {  // goto loop code
   evnum=iev+1;
   cout <<  " !!! bfcread_event_QAhist.C, processing event !!! " << evnum << endl ;
   chain->Clear();
   switch (iret = chain->Make()) {
     case 0: break;
     case 2: { gMessMgr->Info("Last event from input."); break; }
     case 3: { gMessMgr->Error() << "Event " << evnum << " had error " <<
	       iret << ". Now skipping event."; gMessMgr->Print(); break; }
     default: { gMessMgr->Warning() << "Event " << evnum << " returned status "
	        << iret << ". Continuing."; gMessMgr->Print(); }
   }
   iev++;                                // goto loop code
   goto EventLoop;                       // goto loop code
 }

  cout <<  " !!! bfcread_event_QAhist.C, passed chain->Make !!!" << endl ;

//  brow = new TBrowser("BName","BTitle");   

// the following methods are already set to default values in St_QA_Maker::Init - now write over them

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
  cout << " bfcread_event_QAhist.C, Number hist to plot with log scale = " << numLog << endl;

  Int_t numPrint = 0;
  numPrint = HU->ExaminePrintList();
  cout << " bfcread_event_QAhist.C, Number hist to print = " << numPrint << endl;


//  Now draw the actual histograms to canvas and to ps file
  HU->DrawHists(MakerHistDir);

//  overlay two histograms and print to screen
  Int_t result = HU->Overlay1D(MakerHistDir,"StEQaGtrkRT","StEQaPtrkRT");
  if (result == kStErr)
    cout << " !!! There was an error in Overlay1D !!!" << endl;  

  result = HU->Overlay2D(MakerHistDir,"StEQaGtrkLengthVEtaT","StEQaPtrkLengthVEtaT");
  if (result == kStErr)
    cout << " !!! There was an error in Overlay2D !!!" << endl;  

  //chain->Finish();
  //cout <<  "bfcread_event_QAhist.C, passed chain->Finish" << endl ; 
}
