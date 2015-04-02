// $Id: bfcread_event_QAhist.C,v 1.21 2015/04/02 19:54:41 genevb Exp $
// $Log: bfcread_event_QAhist.C,v $
// Revision 1.21  2015/04/02 19:54:41  genevb
// Bichsel now needed for TPC dE/dx QA
//
// Revision 1.20  2013/03/14 17:28:31  genevb
// StTpcDb.so now depends on StEvent.so
//
// Revision 1.19  2011/03/09 22:17:10  genevb
// Update dependence on StEEmcUtil
//
// Revision 1.18  2011/01/19 20:03:03  fisyak
// switch order DbUtil and TpcDb shared libraries loading
//
// Revision 1.17  2010/02/22 20:05:16  genevb
// Using StTpcDbMaker now requires StMagFMaker
//
// Revision 1.16  2008/03/07 19:26:11  genevb
// Ye olde loading of StdetectorDbMaker library change
//
// Revision 1.15  2006/08/15 21:42:37  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.14  2006/05/18 16:38:04  genevb
// Introduce StHistUtil::GetRunYear()
//
// Revision 1.13  2005/08/31 15:03:09  fisyak
// Add dependence StMagF vs StarMagField
//
// Revision 1.12  2005/01/27 05:28:58  genevb
// open runcoBranch
//
// Revision 1.11  2005/01/05 21:57:02  genevb
// More lib loads, evtselBranch, remove date set on calib maker
//
// Revision 1.10  2003/01/28 23:52:14  genevb
// Allow for multiple zones per page
//
// Revision 1.9  2001/07/17 03:22:31  genevb
// Modify TPC DB info for year 2001
//
// Revision 1.8  2001/05/24 20:10:37  lansdell
// changed DB maker SetDateTime option to year_2b
//
// Revision 1.7  2001/05/16 20:53:37  lansdell
// added StMcEvent to chain
//
// Revision 1.6  2001/04/28 21:45:19  genevb
// include libs for EMC
//
// Revision 1.5  2000/07/26 19:53:45  lansdell
// made changes for creating new QA histograms
//
// Revision 1.4  2000/06/13 00:58:59  lansdell
// added libglobal_Tables to resolve crashes
//
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
     //"/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
"/star/rcf/test/dev/trs_redhat61/Tue/year_2001/hc_standard/hc_standard.40_evts.event.root",
    const Char_t *psFile="EventQAhist.ps",
    const Char_t *PageTitle="",
    const Char_t *PrintList="",
    const Char_t *MakerHistDir="EventQA",
    const Int_t ZoneH=2,
    const Int_t ZoneV=3
  )
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
  gSystem->Load("St_Tables");
  gSystem->Load("libglobal_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libgen_Tables");
  gSystem->Load("libgeometry_Tables");

  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StBichsel");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StEvent");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  gSystem->Load("StPmdUtil");
  gSystem->Load("St_QA_Maker");  

//  Setup top part of chain
  chain = new StChain("MyChain");
  chain->SetDebug();
   
// Input File Maker
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetBranch("evtselBranch",0,"r");
  IOMk->SetBranch("event",0,"r");
  IOMk->SetBranch("geantBranch",0,"r");
  IOMk->SetBranch("runcoBranch",0,"r");
  IOMk->SetDebug();

// database stuff
  const char* calibDB = "MySQL:StarDb";
  const char* calibDB2 = "$STAR/StarDb";
  St_db_Maker* calibMk = new St_db_Maker("StarDb",calibDB,calibDB2);
  StMagFMaker* magfMk = new StMagFMaker; // now required for StTpcDbMaker
  //calibMk->SetDateTime("year_2b");
  calibMk->SetDebug();  
  StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");

// constructor for other maker (not used in chain)
  StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
  HU->SetPntrToMaker((StMaker *)IOMk);

//  add other makers to chain:
  StMcEventMaker *mcEvent = new StMcEventMaker;
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
  HU->GetRunYear(MainFile);
  HU->SetPostScriptFile(psFile);
  HU->SetZones(ZoneH,ZoneV);
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
