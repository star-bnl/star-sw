// $Id: bfcread_dst_QAhist.C,v 1.5 1999/09/20 20:22:56 kathy Exp $
// $Log: bfcread_dst_QAhist.C,v $
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
// Kathy's notes (5/13/99):
//     - Victor's example to show how to read in a DST produced from bfc.C and
//       - run another maker (St_QA_Maker) 
//       - look at it using the browser
//
// This example is reading in the "dst" branch of the root file.
// (i.e. the input file is .dst.root)
// If you want to read in a different branch, you must change:
//    - the input file name, e.g. *.bname.root
//    - treeMk->SetBranch("bnameBranch",0,"r");
//    - chain->GetDataSet("bname");
//
// This example has St_QA_Maker put in properly, but it is commented out.
// Just un-comment if you want. 
//
// This example is for debugging/testing purposes and  therefore does
//   not have chain->Finish(); at end
//
//======================================================================

class StChain;
class St_DataSet;

StChain *chain;
St_DataSet *Event;


TBrowser *brow=0;

void bfcread_dst_QAhist(Int_t nevents=1, 
             const char *MainFile="/disk00000/star/test/dev/tfs_Solaris/Wed/year_1b/set0352_01_35evts.dst.root",
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
// -- input any maker pointer but much cast as type StMaker
   HU->SetPntrToMaker((StMaker *)IOMk);

//  add other makers to chain:
  St_QA_Maker  *QA  = new St_QA_Maker;

// must set pointer to HistUtil so can use it's methods
   QA->SetPntrToHistUtil(HU);

// --- now execute chain member functions
  chain->Init();
 
// method to print out list of histograms - can do this anytime after they're booked
  Int_t NoHist=0;
  NoHist = HU->ListHists();
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
 














