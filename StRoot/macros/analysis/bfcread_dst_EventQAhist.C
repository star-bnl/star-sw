// $Id: bfcread_dst_EventQAhist.C,v 1.10 2000/01/13 16:55:11 kathy Exp $ 
// $Log: bfcread_dst_EventQAhist.C,v $
// Revision 1.10  2000/01/13 16:55:11  kathy
// updating bfcread_dst*.C macros to use the new methods in StHistUtil which allow printing from a list; also make sure all libraries needed are loaded in the ones running St_QA_Maker; also update documentation
//
// Revision 1.9  2000/01/11 16:31:02  kathy
// change to current input file in Root2XDF.C and bfcread_dst_EventQA*.C; load St_global library in bfcread_dst_QA_outhistfile.C which is now needed when using St_QA_Maker class
//
// Revision 1.8  1999/12/03 20:20:23  kathy
// correct the event number counter in bfcread_dst*.C macros
//
// Revision 1.7  1999/12/01 21:30:11  kathy
// added input TopDirTree to bfcread_hist* macros in order to tell which top level directory hist file has since sometimes its not bfcTree; cleaned up print statements in bfcread_dst*hist.C macros; two new macros bfcread_dst_*QA_outhistfile.C added which read dst file and book and fill histograms and write out a new *.hist.root file, instead of just sending hist to postscript - this new *.hist.root file can then be read into bfcread_hist*.C to look at it --- note that topdirtree is different!
//
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
//         MainFile  - input *.dst.root file from bfc output
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
//======================================================================

class StChain;
StChain *chain;

//TBrowser *brow=0;

void bfcread_dst_EventQAhist(
     Int_t nevents=10, 
     const Char_t *MainFile=
      "/star/rcf/test/dev/tfs_Linux/Mon/year_1b/hc_lowdensity/gstar.dst.root",
    const Char_t *psFile="Event_QA_hist.ps",
    const Char_t *PageTitle="",
    const Char_t *PrintList="",
    const Char_t *MakerHistDir="EventQA")
{
//
  cout << "bfcread_dst_EventQAhist.C, num events to process " << 
     nevents  << endl;
  cout << "bfcread_dst_EventQAhist.C, input file name       " << 
     MainFile << endl;
  cout << "bfcread_dst_EventQAhist.C, output psfile name    " << 
     psFile   << endl;
  cout << "bfcread_dst_EventQAhist.C, hist page title " << 
     PageTitle  << endl;
  cout << "bfcread_dst_QAhist.C, Maker directory containing histograms =  " 
       << MakerHistDir   << endl;
  cout << "bfcread_dst_QAhist.C, subset list name of which histograms to draw,print = "
       << PrintList  << endl;

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker");  

  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");


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
  NoHist = HU->ListHists(MakerHistDir);
  cout << " !!! bfcread_dst_QAhist.C, No. of Hist we have == " << NoHist << endl;

 
// loop over events:
  int iev=0,iret=0, evnum=0;
 EventLoop: if (iev<nevents && !iret) {  // goto loop code
   evnum=iev+1;
   cout <<  " !!! bfcread_dst_EventQAhist.C, processing event !!! " << evnum << endl ;
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
    HU->SetDefaultLogYList(MakerHistDir);
      if (PageTitle=="") PageTitle=MainFile;
    HU->SetGlobalTitle(PageTitle);

    HU->SetDefaultPrintList(MakerHistDir,PrintList);

  Int_t numLog = 0;
  numLog = HU->ExamineLogYList();
  cout << " bfcread_dst_EventQAhist.C, Number hist to plot with log scale = " << numLog << endl;

  Int_t numPrint = 0;
  numPrint = HU->ExaminePrintList();
  cout << " bfcread_dst_EventQAhist.C, Number hist to print = " << numPrint << endl;

  chain->Finish();
  cout <<  "bfcread_dst_EventQAhist.C, passed chain->Finish" << endl ; 

//  Now draw the actual histograms to canvas and to ps file
    HU->DrawHists(MakerHistDir);
   
}
 














