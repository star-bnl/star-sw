// $Id: DrawDstHistNew.C,v 1.3 1999/07/13 00:29:52 kathy Exp $
// $Log: DrawDstHistNew.C,v $
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
//    - reads a *.dst.root file from 99e 
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
St_DataSet *Event;
StChain *chain;
TBrowser *brow=0;

void DrawDstHistNew(Int_t nevents=1, 
             const char *MainFile="/afs/rhic/star/data/test/dev/tfs_Solaris/Thu/year_2a/psc0208_01_40evts.dst.root",
             const Char_t *psFile="QA_hist_DrawDstHistNew.ps")
{
//
  cout << "DrawDstHistNew.C, input file name       " << MainFile << endl;
  cout << "DrawDstHistNew.C, output psfile name    " << psFile   << endl;
  cout << "DrawDstHistNew.C, num events to process " << nevents  << endl;

    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StTreeMaker");
    gSystem->Load("St_QA_Maker");
    gSystem->Load("StarClassLibrary");



//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
//  Input Tree
  StTreeMaker *treeMk = new StTreeMaker("treeRead",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();
  treeMk->SetBranch("*",0,"0");  	//deactivate all branches
  treeMk->SetBranch("dstBranch",0,"r");	//activate dstBranch

//  add other makers to chain:
   St_QA_Maker  *QA  = new St_QA_Maker;
  
// --- now execute chain member functions
  chain->Init();
 
// method to print out list of histograms - can do this anytime after they're booked
   Int_t NoHist=0;
   NoHist = QA->ListHists();
   cout << " DrawDstHistNew.C, No. of Hist we have == " << NoHist << endl;

// loop over events:
  for (int iev=0;iev<nevents; iev++)
  {
    cout <<  " DrawDstHistNew.C, processing event !!! " << iev << endl ;
    chain->Clear();
    int iret = chain->Make();
    if (iret) break;
  }

  cout <<  " DrawDstHistNew.C, passed chain->Make !!!" << endl ;

// the following methods are already set to default values in St_QA_Maker::Init - now write over them
    QA->SetDraw(kTRUE);
//    QA->SetHistsNamesDraw(firstHist,lastHist);
    QA->SetPostScriptFile(psFile);
    QA->SetZones();
    QA->SetPaperSize();

    Int_t numLog = 0;
    numLog = QA->ExamineLogYList();
    cout <<" DrawDstHistNew.C, Number hist to plot with log scale = " << numLog << endl;

// Finish method in St_QA_Maker is where the actual DrawHist is done
  chain->Finish();
  cout <<  "DrawDstHistNew.C, passed chain->Finish" << endl ; 

}
 














