// $Id: bfcread_dst_EventQAhist.C,v 1.6 1999/11/30 19:23:05 kathy Exp $ 
// $Log: bfcread_dst_EventQAhist.C,v $
// Revision 1.6  1999/11/30 19:23:05  kathy
// changed bfcread_dst*.C so that MakerHist is hardwired in instead of being input; wrote better documentation in bfcread_hist*.C so that it explains where top level directory is set
//
// Revision 1.5  1999/11/29 21:49:22  kathy
// more print statements
//
// Revision 1.4  1999/11/29 21:40:17  kathy
// clean up macros; change name of output files; remove unneccessary lines
//
// Revision 1.3  1999/11/29 20:25:55  kathy
// remove call to method SetDraw - doesn't do anything
//
// Revision 1.2  1999/11/18 19:36:04  kathy
// fix to pick up library in the new area
//
//======================================================================
// owner:  Curtis Lansdell
// what it does: 
//    - reads a *.dst.root OR .dst.xdf file from 99e and up
//    - runs StEventMaker & StEventQAMaker
//    - draws EventQA histograms and then sends them to a postscript file
//
//=======================================================================
//
//
// inputs: nevents   - # events to process
//         MainFile  - *.dst.root file from bfc output
//         psFile    - output postscript filename
//         PageTitle - title you want on each output page, default is
//                       MainFile name
//
//
//======================================================================

class StChain;
StChain *chain;

//TBrowser *brow=0;

void bfcread_dst_EventQAhist(Int_t nevents=10, 
             const char *MainFile="/star/rcf/test/dev/tfs_Solaris/Tue/year_1b/set0352_01_35evts.dst.root",
             const Char_t *psFile="Event_QA_hist.ps",
             const Char_t *PageTitle="")
{
//
  cout << "bfcread_dst_EventQAhist.C, input file name       " << 
     MainFile << endl;
  cout << "bfcread_dst_EventQAhist.C, output psfile name    " << 
     psFile   << endl;
  cout << "bfcread_dst_EventQAhist.C, num events to process " << 
     nevents  << endl;

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker");  

  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");

// force the directory name for histograms since this macro is 
// specifically for running StEventQAMaker
             const Char_t *MakerHist="EventQA";
  cout << "bfcread_dst_EventQAhist.C, directory of Maker name      " << 
     MakerHist<< endl;

//  Setup top part of chain
  chain = new StChain("MyChain");
  chain->SetDebug();
   
// Input File Maker
    StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");


// constructor for other maker (not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
   HU->SetPntrToMaker((StMaker *)IOMk);


//  add other makers to chain:
  StEventMaker *eventMaker = new StEventMaker("events","title");
  StEventQAMaker *EventQA = new StEventQAMaker("EventQA","StEvent/QA");


// --- now execute chain member functions --> Init
    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->PrintInfo();
 
// method to print out list of histograms - 
//can do this anytime after they're booked
  Int_t NoHist=0;
  NoHist = HU->ListHists(MakerHist);
  cout << " !!! bfcread_dst_QAhist.C, No. of Hist we have == " << NoHist << endl;

 
// loop over events:
  int iev=0,iret=0;
 EventLoop: if (iev<nevents && !iret) {  // goto loop code
   cout <<  " !!! bfcread_dst_EventQAhist.C, processing event !!! " << iev << endl ;
   chain->Clear();
   iret = chain->Make();
   iev++;                                // goto loop code
   goto EventLoop;                       // goto loop code
 }

  cout <<  " !!! bfcread_dst_EventQAhist.C, passed chain->Make !!!" << endl ;

//  brow = new TBrowser("BName","BTitle");   

// the following methods are already set to default values in St_QA_Maker::Init - now write over them

// Set the default canvas style to plain (so it won't print out grey!)
    gROOT->SetStyle("Plain");
//    gStyle->SetOptStat(111111);

    HU->SetHistsNamesDraw("*","*");
    HU->SetPostScriptFile(psFile);
    HU->SetZones(2,3);
    HU->SetPaperSize();
    HU->SetDefaultLogYList(MakerHist);
      if (PageTitle=="") PageTitle=MainFile;
    HU->SetGlobalTitle(PageTitle);

  Int_t numLog = 0;
  numLog = HU->ExamineLogYList();
  cout << " bfcread_dst_EventQAhist.C, Number hist to plot with log scale = " << numLog << endl;

  chain->Finish();
  cout <<  "bfcread_dst_EventQAhist.C, passed chain->Finish" << endl ; 

//  Now draw the actual histograms to canvas and to ps file
    HU->DrawHists(MakerHist);
   
}
 














