// $Id: QA_bfcread_hist_to_ps.C,v 1.3 1999/07/13 00:42:32 kathy Exp $
// $Log: QA_bfcread_hist_to_ps.C,v $
// Revision 1.3  1999/07/13 00:42:32  kathy
// updated all default input files, removed unneccessary macros, renamed other to make more standard
//
// Revision 1.2  1999/07/13 00:29:53  kathy
// updated macros to take out StRootEvent
//
// Revision 1.1  1999/06/25 19:43:47  kathy
// new macros for official QA use
//
//
//======================================================================
// owner:  Kathy Turner
// what it does:  see below
//=======================================================================
// QA_bfcread_hist_to_ps.C 
//
// Kathy's notes (6/25/99):
//   - adapted from bfcread.C macro and changed so it could read in
//     .hist.root file produced from bfc.C in 99f
//   - reads hist file and then draws QA histograms and sends to ps file
//   - will be the official macro called for QA stuff
//======================================================================

class St_DataSet;
St_DataSet *Event;

class StChain;
StChain *chain;

class StTreeMaker;
StTreeMaker *treeMk=0;

//------------------------------------------------------------------------


void QA_bfcread_hist_to_ps(
  const Char_t *MainFile="/afs/rhic/star/data/test/dev/tfs_Solaris/Thu/year_2a/psc0208_01_40evts.hist.root",
  const Char_t *psFile="QA_hist.ps")
{

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StTreeMaker");
    gSystem->Load("St_QA_Maker");
    gSystem->Load("StarClassLibrary");



//  Input Tree
  treeMk = new StTreeMaker("treeRead",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();
  treeMk->SetBranch("*",0,"0");  		//deactivate all branches
  treeMk->SetBranch("histBranch",0,"r");	//activate histBranch

//  add other makers to chain:
   St_QA_Maker  *QA  = new St_QA_Maker;

// ONLY use StTreeMaker in chain 
//   - not St_QA_Maker
//   - I'm just using methods from St_QA_Maker...

// --- now execute chain member functions
  treeMk->Init();
  treeMk->Clear();
  treeMk->Make();


// method to print out list of histograms
// - can do this anytime after they're booked
   Int_t NoHist=0;
   NoHist = QA->ListHists();
   cout << " in QA_bfcread_hist_to_ps: Num of Hist = " << NoHist << endl;

// Set the default canvas style to plain (so it won't print out grey!)
    gROOT->SetStyle("Plain");

    QA->SetDraw(kTRUE);
    QA->SetHistsNamesDraw("*","*");
    QA->SetPostScriptFile(psFile);
    QA->SetZones();
    QA->SetPaperSize();
    QA->SetDefaultLogYList();

    Int_t numLog = 0;
    numLog = QA->ExamineLogYList();
    cout <<" QA_bfcread_hist_to_ps.C, Number hist to plot with log scale = " << numLog << endl;

//  Now draw the actual histograms to canvas and to ps file
    QA->DrawHists();
   
    cout <<" QA_bfcread_hist_to_ps.C, passed QA->DrawHists" << endl;
}
 






