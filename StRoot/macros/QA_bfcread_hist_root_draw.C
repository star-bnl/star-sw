// $Id: QA_bfcread_hist_root_draw.C,v 1.1 1999/05/21 21:13:43 kathy Exp $
// $Log: QA_bfcread_hist_root_draw.C,v $
// Revision 1.1  1999/05/21 21:13:43  kathy
// QA_bfcread_hist_root_draw.C - new macro to read in .hist.root file from 99e bfc.C and draw histograms - currently just pops browser
// 
//======================================================================
// owner:  Kathy Turner
// what it does: 
//=======================================================================
// QA_bfcread_hist_root_draw.C
//
// Kathy's notes (5/21/99):
//   - started with bfcread.C macro and changed so it could read in
//     .hist.root file produced from bfc.C in 99e
// 
//======================================================================

class StChain;
class St_DataSet;
St_DataSet *Event;
StChain *chain;
TBrowser *brow=0;

void QA_bfcread_hist_root_draw(Int_t nevents=1, const char
*MainFile="/disk00000/star/test/dev/tfs_Linux/Mon/year_2a/psc0208_01_40evts.hist.root")

{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StRootEvent");


//  Input Tree
  StTreeMaker *treeMk = new StTreeMaker("treeRead",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();
  treeMk->SetBranch("*",0,"0");  		//deactivate all branches
  treeMk->SetBranch("histBranch",0,"r");	//activate histBranch


// --- now execute chain member functions
  treeMk->Init();
  treeMk->Clear();
  treeMk->Make();


// Now look at histograms:
//create browser with name=BName,title=Btitle
    Event = treeMk->GetDataSet("hist");
    if (Event) {
          Event->ls(9);
          brow = new TBrowser("BName","BTitle");    
    }

}
 
