// $Id: bfcread_hist_file_to_ps.C,v 1.3 1999/07/13 00:29:54 kathy Exp $
// $Log: bfcread_hist_file_to_ps.C,v $
// Revision 1.3  1999/07/13 00:29:54  kathy
// updated macros to take out StRootEvent
//
// Revision 1.2  1999/06/22 18:14:25  kathy
// change default input files and fix name of macro
//
// Revision 1.1  1999/06/11 20:08:56  kathy
// new macro that reads the *.hist.root file produced from bfc.C and uses all histogram utility methods in St_QA_Maker to manipulate the histograms
//
//======================================================================
// owner:  Kathy Turner
// what it does:  see below
//=======================================================================
// bfcread_hist_file_to_ps.C 
//
// Kathy's notes (6/10/99):
//   - adapted from bfcread.C macro and changed so it could read in
//     .hist.root file produced from bfc.C in 99e
//   - reads hist file and then draws QA histograms and sends to ps file
//
//======================================================================

class St_DataSet;
St_DataSet *Event;

class StChain;
StChain *chain;

class StTreeMaker;
StTreeMaker *treeMk=0;

//------------------------------------------------------------------------

void bfcread_hist_file_to_ps(Int_t nevents=1, 
  const Char_t *MainFile="/disk00000/star/test/dev/tfs_Linux/Thu/year_2a/psc0208_01_40evts.hist.root",
  const Char_t *psFile="QA_hist.ps",
  const Char_t *firstHist="*",
  const Char_t *lastHist="*")
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
   cout << " in bfcread_hist_file_to_ps: Num of Hist = " << NoHist << endl;

// Set the default canvas style to plain (so it won't print out grey!)
    gROOT->SetStyle("Plain");

    QA->SetDraw(kTRUE);
    QA->SetHistsNamesDraw(firstHist,lastHist);
    QA->SetPostScriptFile(psFile);
    QA->SetZones();
    QA->SetPaperSize();
    QA->SetDefaultLogYList();

    Int_t numLog = 0;
    numLog = QA->ExamineLogYList();
    cout <<" bfcread_hist_file_to_ps.C, Number hist to plot with log scale = " << numLog << endl;

//  Now draw the actual histograms to canvas and to ps file
    QA->DrawHists();
   
    cout <<" bfcread_hist_file_to_ps.C, passed QA->DrawHists" << endl;
}
 

