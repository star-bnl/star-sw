// $Id: bfcread_dst_QAhist.C,v 1.8 1999/11/05 16:30:15 kathy Exp $
// $Log: bfcread_dst_QAhist.C,v $
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
//         MainFile - *.hist.root file from bfc output
//         MakerHist - name of Maker that you want histograms from
//         psFile - output postscript filename
//
// standard Maker names in bfc 
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
// http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/St_QA_Maker/doc/
//
//======================================================================

class StChain;
class St_DataSet;

StChain *chain;
St_DataSet *Event;


void bfcread_dst_QAhist(Int_t nevents=1, 
             const char *MainFile="/disk00000/star/test/new/tfs_Solaris/year_1b/set0352_01_35evts.dst.root",
             const Char_t *MakerHist="QA",
             const Char_t *psFile="QA_hist.ps")
{
//
  cout << "bfcread_dst_QAhist.C, input file name       " << MainFile << endl;
  cout << "bfcread_dst_QAhist.C, input Maker name      " << MakerHist<< endl;
  cout << "bfcread_dst_QAhist.C, output psfile name    " << psFile   << endl;
  cout << "bfcread_dst_QAhist.C, num events to process " << nevents  << endl;

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker");

//  Setup top part of chain
  chain = new StChain("bfc");
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
  St_QA_Maker  *QA  = new St_QA_Maker;

// must set pointer to HistUtil so can use it's methods
// 9/21/99 (KT) actually, this is not needed since I took out
//          StHistUtil method calls from St_QA_Maker 
//   QA->SetPntrToHistUtil(HU);

// --- now execute chain member functions
  chain->Init();
 
// method to print out list of histograms - can do this anytime after they're booked
  Int_t NoHist=0;
  NoHist = HU->ListHists(MakerHist);
  cout << " bfcread_dst_QAhist.C, No. of Hist we have == " << NoHist << endl;

// loop over events:
  int iev=0,iret=0;
  //for (iev=0;iev<nevents; iev++) {     // for loop code
 EventLoop: if (iev<nevents && !iret) {  // goto loop code
   cout <<  " bfcread_dst_QAhist.C, processing event !!! " << iev << endl ;
   chain->Clear();
   iret = chain->Make();
   //if (iret) break;                    // for loop code
   iev++;                                // goto loop code
   goto EventLoop;                       // goto loop code
 }

  cout <<  " bfcread_dst_QAhist.C, passed chain->Make !!!" << endl ;

// the following methods are already set to default values in St_QA_Maker::Init - now write over them
   QA->SetDraw(kTRUE);

// Set the default canvas style to plain (so it won't print out grey!)
    gROOT->SetStyle("Plain");
//    gStyle->SetOptStat(111111);

    HU->SetHistsNamesDraw("*","*");
    HU->SetPostScriptFile(psFile);
    HU->SetZones(2,3);
    HU->SetPaperSize();
    HU->SetDefaultLogYList(MakerHist);

  Int_t numLog = 0;
  numLog = HU->ExamineLogYList();
  cout << " bfcread_dst_QAhist.C, Number hist to plot with log scale = " << numLog << endl;

  chain->Finish();
  cout <<  "bfcread_dst_QAhist.C, passed chain->Finish" << endl ; 

//  Now draw the actual histograms to canvas and to ps file
    HU->DrawHists(MakerHist);
   
}
 














