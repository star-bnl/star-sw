// $Id: bfcread_hist_list.C,v 1.1 1999/07/13 00:42:33 kathy Exp $ 
// $Log: bfcread_hist_list.C,v $
// Revision 1.1  1999/07/13 00:42:33  kathy
// updated all default input files, removed unneccessary macros, renamed other to make more standard
//
// Revision 1.4  1999/07/13 00:29:54  kathy
// updated macros to take out StRootEvent
//
// Revision 1.3  1999/06/22 18:14:26  kathy
// change default input files and fix name of macro
//
// Revision 1.2  1999/06/03 17:55:26  kathy
// changed DrawDstHist from using St_io_Maker to Victor's new StIOMaker - actually just copied doEvents to DrawDstHist and hacked it. Fixed comments in read_bfc_hist_list.C
// 

//=======================================================================
// owner: Kathy Turner
// what it does: reads the *.hist.root file produced from bfc.C and
//               then lists all histogram branches and the names and
//               titles of the histograms in the branches
//=======================================================================


class StChain;
class St_DataSet;
St_DataSet *Event;
StChain *chain;
TBrowser *brow=0;

void bfcread_hist_list(Int_t nevents=1, const char
*MainFile="/afs/rhic/star/data/test/dev/tfs_Solaris/Thu/year_2a/psc0208_01_40evts.hist.root")

{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StarClassLibrary");
   

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
    Event = treeMk->GetDataSet("hist");
      if (Event) {
	cout << "Here is list of all histograms: " << endl;
          Event->ls(9); 
      }

   St_DataSetIter nextHistList(Event);
   St_ObjectSet *histContainer = 0;
   TList *dirList = 0;
   while (histContainer = (St_ObjectSet *)nextHistList()) {
     dirList = (TList *) histContainer->GetObject();
     TIter nextHist(dirList);
     TObject  *o = 0;
     cout << " --- " << histContainer->GetName() << endl;
     while (o= nextHist()) {
       cout << " Hist name: " << o->GetName() << " ==> Title: " 
	    << o->GetTitle() << endl; 
     }
   }

}



