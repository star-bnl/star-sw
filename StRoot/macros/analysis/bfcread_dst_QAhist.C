// $Id: bfcread_dst_QAhist.C,v 1.34 2000/06/05 17:25:03 lansdell Exp $
// $Log: bfcread_dst_QAhist.C,v $
// Revision 1.34  2000/06/05 17:25:03  lansdell
// StTpcDb no longer loaded
//
// Revision 1.33  2000/06/02 20:25:37  lansdell
// added check on Make() return codes
//
// Revision 1.32  2000/05/09 19:38:04  kathy
// update to use standard default input files and only process few events by default - to make it easy to run in automatic macro testing script
//
// Revision 1.31  2000/04/13 21:46:34  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.30  2000/04/12 15:06:52  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.29  2000/03/20 17:32:55  kathy
// setbranches in all macros so that they will work with softlinks - for StIOMaker
//
// Revision 1.28  2000/03/20 15:43:41  kathy
// add libraries to load in bfcread_dst_QAhist.C so it will work on linux; worked on solaris without adding them - I have no idea why
//
// Revision 1.27  2000/03/17 23:10:06  kathy
// make sure the dst branch is explicitly set in the macros using dst.root files as input - otherwise they don't work properly with soft links
//
// Revision 1.26  2000/02/14 20:30:40  kathy
// removing unneeded macros; updating documentation in bfcread macros
//
// Revision 1.25  2000/02/07 19:46:36  kathy
// add read from geant Branch so that geant table histograms can be filled in St_QA_Maker
//
// Revision 1.24  2000/01/31 21:20:14  kathy
// fixed up printouts - some printed wrong macro name; also moved finish method after drawhists method
//
// Revision 1.23  2000/01/26 19:28:15  kathy
// put in call to method SetDefaultLogXList
//
// Revision 1.22  2000/01/19 16:29:50  kathy
// update macros to use default input files in /afs/rhic/star/data/samples
//
// Revision 1.21  2000/01/18 16:38:05  kathy
// add loading of StUtilities and StAnalysisUtilities so that StHistUtil class can now be picked up from StAnalysisUtilities library
//
// Revision 1.20  2000/01/13 16:55:11  kathy
// updating bfcread_dst*.C macros to use the new methods in StHistUtil which allow printing from a list; also make sure all libraries needed are loaded in the ones running St_QA_Maker; also update documentation
//
// Revision 1.19  2000/01/12 23:16:37  kathy
// add all libraries that are now needed to load for St_QA_Maker; add code for using new print methods - can't yet print from list though....
//
// Revision 1.18  2000/01/10 21:59:17  kathy
// must now load St_global when running St_QA_Maker
//
// Revision 1.17  2000/01/05 22:12:03  kathy
// changed input file to current one
//
// Revision 1.16  1999/12/03 20:20:23  kathy
// correct the event number counter in bfcread_dst*.C macros
//
// Revision 1.15  1999/12/01 21:30:11  kathy
// added input TopDirTree to bfcread_hist* macros in order to tell which top level directory hist file has since sometimes its not bfcTree; cleaned up print statements in bfcread_dst*hist.C macros; two new macros bfcread_dst_*QA_outhistfile.C added which read dst file and book and fill histograms and write out a new *.hist.root file, instead of just sending hist to postscript - this new *.hist.root file can then be read into bfcread_hist*.C to look at it --- note that topdirtree is different!
//
// Revision 1.14  1999/11/30 19:23:05  kathy
// changed bfcread_dst*.C so that MakerHist is hardwired in instead of being input; wrote better documentation in bfcread_hist*.C so that it explains where top level directory is set
//
// Revision 1.13  1999/11/29 21:40:18  kathy
// clean up macros; change name of output files; remove unneccessary lines
//
// Revision 1.12  1999/11/29 21:11:31  kathy
// removed unneccessary commented out lines
//
// Revision 1.11  1999/11/29 20:25:56  kathy
// remove call to method SetDraw - doesn't do anything
//
// Revision 1.10  1999/11/19 20:13:21  kathy
// cleaned up macros to remove uneccessary lines; also added info about new tables to QA* macros
//
// Revision 1.9  1999/11/05 22:50:39  kathy
// now input global title for all output pages in the macro for printing & drawing histograms
//
// Revision 1.8  1999/11/05 16:30:15  kathy
// minor changes to documentation in macro
//
// Revision 1.7  1999/11/03 19:02:54  kathy
// changes to default input files and output file names - needed by perl script for testing
//
// Revision 1.6  1999/09/21 15:07:02  kathy
// change to have notes on input values at top of each macro, also clean up notes on usage and remove the usage of method St_QA_Maker::SetPntrToHistUtil which is not going to be used now that I made St_QA_Maker totally independent of the histogram printing
//
// Revision 1.5  1999/09/20 20:22:56  kathy
// fix to bfcread_dst_QAhist.C to print hist at end
//
// Revision 1.3  1999/09/09 21:18:02  kathy
// changed to use StIOMaker instead of StTreeMaker so that it can read as input both .dst.root and .dst.xdf files
//
// Revision 1.2  1999/08/06 15:08:13  kathy
// removed for loop and put in goto checks - due to CINT problems
//
// Revision 1.1  1999/07/13 00:42:32  kathy
// updated all default input files, removed unneccessary macros, renamed other to make more standard
//
// Revision 1.27  1999/07/13 00:31:30  kathy
// moved DrawDstHistNew.C to DrawDstHist.C
//
// Revision 1.3  1999/07/13 00:29:52  kathy
// updated macros to take out StRootEvent
//
// Revision 1.2  1999/06/11 20:07:39  kathy
// changed default output file name
//
// Revision 1.1  1999/05/21 20:58:50  kathy
// new macro DrawDstHistNew.C - reads 99e DST root files, runs St_QA_Maker, draws histograms and sends to ps file
//
//======================================================================
// owner:  Kathy Turner
// what it does: 
//   (adapted from bfcread.C)
//    - reads a *.dst.root OR .dst.xdf file from 99e and up
//    - sets dst & geant branches (uses both)
//    - runs St_QA_Maker
//    - draws QA histograms and then sends them to a postscript file
//
//=======================================================================
//
// Kathy's notes (9/21/99):
//   - adapted from bfcread.C macro and changed so it could read in
//     .dst.root or .xdf.root file produced from bfc.C 
//   - run maker (St_QA_Maker) 
//   - draws & prints histograms from given input Maker
//
// inputs: nevents - # events to process
//         MainFile - input *.dst.root file from bfc output
//         psFile - output postscript filename
//         PageTitle - title you want on each output page, default = "" is
//                       MainFile name
//         PrintList - name of subset histogram list that you want printed
//                   - these are defined in StHistUtil, method SetDefaultPrintList
//                   - default = "", prints all histograms in directory
//         MakerHistDir - this is the Maker name that you want to get histograms
//                        from - leave as "QA" for this macro since this 
//                        macro is setup to run St_QA_Maker!
//
// standard Maker names in bfc 
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
//  http://www.star.bnl.gov/STAR/html/comp_l/train/tut/bfc_maker_names.html
//
//
//
// Documentation on St_QA_Maker class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/St_QA_Maker/doc/
//
// Documentation on StHistUtil class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/StAnalysisUtilities/doc/
//  
//======================================================================

class StChain;

StChain *chain;

void bfcread_dst_QAhist(
    Int_t nevents=2, 
    const Char_t *MainFile=
      "/afs/rhic/star/data/samples/gstar.dst.root",
    const Char_t *psFile="DSTtable_QA_hist.ps",
    const Char_t *PageTitle="",
    const Char_t *PrintList="",
    const Char_t *MakerHistDir="QA")
{
//
  cout << "bfcread_dst_QAhist.C, num events to process " 
       << nevents  << endl;
  cout << "bfcread_dst_QAhist.C, input file name       " 
       << MainFile << endl;
  cout << "bfcread_dst_QAhist.C, output psfile name    " 
       << psFile   << endl;
  cout << "bfcread_dst_QAhist.C, page title for histograms = " 
       << PageTitle << endl;
  cout << "bfcread_dst_QAhist.C, Maker directory containing histograms =  " 
       << MakerHistDir   << endl;
  cout << "bfcread_dst_QAhist.C, subset list name of which histograms to draw,print = "
       << PrintList  << endl;

  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");

  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StDbUtilities");

  gSystem->Load("St_QA_Maker");
  gSystem->Load("tls");
  gSystem->Load("St_tpc");
  gSystem->Load("St_svt");
  gSystem->Load("St_global");

//  Setup top part of chain
  chain = new StChain("bfc");
  chain->SetDebug();
   
// Input File Maker 
//  - turn geant Branch on - dstBranch already on from input file
    StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
     IOMk->SetDebug();
     IOMk->SetIOMode("r");
     IOMk->SetBranch("*",0,"0");                 //deactivate all branches
     IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch
     IOMk->SetBranch("geantBranch",0,"r"); //activate geant Branch

// constructor for other class  (not a Maker so not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
   HU->SetPntrToMaker((StMaker *)IOMk);

//  add other makers to chain:
  St_QA_Maker  *QA  = new St_QA_Maker;

// --- now execute chain member functions
  chain->Init();
 
// method to print out list of histograms - can do this anytime after they're booked
  Int_t NoHist=0;
  NoHist = HU->ListHists(MakerHistDir);
  cout << " bfcread_dst_QAhist.C, No. of Hist we have == " << NoHist << endl;

// loop over events:
  int iev=0,iret=0, evnum=0;
 EventLoop: if (iev<nevents && iret!=2) {  // goto loop code
   evnum=iev+1;
   cout <<  " bfcread_dst_QAhist.C, processing event !!! " << evnum << endl ;
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

  cout <<  " bfcread_dst_QAhist.C, passed chain->Make !!!" << endl ;

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
  cout << " bfcread_dst_QAhist.C, Number hist to plot with log scale = " << numLog << endl;

  Int_t numPrint = 0;
  numPrint = HU->ExaminePrintList();
  cout << " bfcread_dst_QAhist.C, Number hist to print = " << numPrint << endl;


//  Now draw the actual histograms to canvas and to ps file
    HU->DrawHists(MakerHistDir);

  chain->Finish();
  cout <<  "bfcread_dst_QAhist.C, passed chain->Finish" << endl ;    
}
 














