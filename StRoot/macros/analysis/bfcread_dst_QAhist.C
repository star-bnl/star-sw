// $Id: bfcread_dst_QAhist.C,v 1.16 1999/12/03 20:20:23 kathy Exp $
// $Log: bfcread_dst_QAhist.C,v $
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
//         MainFile - *.dst.root file from bfc output
//         psFile - output postscript filename
//         PageTitle - title you want on each output page, default is
//                       MainFile name
//
// standard Maker names in bfc 
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
// http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/St_QA_Maker/doc/
//
//======================================================================

class StChain;

StChain *chain;

void bfcread_dst_QAhist(
    Int_t nevents=10, 
    const Char_t *MainFile=
      "/star/rcf/test/dev/tfs_Solaris/Fri/year_1b/set0352_01_35evts.dst.root",
    const Char_t *psFile="DSTtable_QA_hist.ps",
    const Char_t *PageTitle="")
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


  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker");

// force the directory name for histograms since this macro is 
// specifically for running St_QA_Maker
  const Char_t *MakerHistDir="QA";
  cout << "bfcread_dst_QAhist.C, directory of Maker name      " << 
     MakerHistDir<< endl;

//  Setup top part of chain
  chain = new StChain("bfc");
  chain->SetDebug();
   
// Input File Maker
    StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");

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
 EventLoop: if (iev<nevents && !iret) {  // goto loop code
   evnum=iev+1;
   cout <<  " bfcread_dst_QAhist.C, processing event !!! " << evnum << endl ;
   chain->Clear();
   iret = chain->Make();
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
    HU->SetDefaultLogYList(MakerHistDir);
      if (PageTitle=="") PageTitle=MainFile;
    HU->SetGlobalTitle(PageTitle);

  Int_t numLog = 0;
  numLog = HU->ExamineLogYList();
  cout << " bfcread_dst_QAhist.C, Number hist to plot with log scale = " << numLog << endl;

  chain->Finish();
  cout <<  "bfcread_dst_QAhist.C, passed chain->Finish" << endl ; 

//  Now draw the actual histograms to canvas and to ps file
    HU->DrawHists(MakerHistDir);
   
}
 














