// $Id: bfcread_hist_list_all.C,v 1.1 1999/09/20 20:09:02 kathy Exp $ 
// $Log: bfcread_hist_list_all.C,v $
// Revision 1.1  1999/09/20 20:09:02  kathy
// bfcread_hist_list_all now lists all histograms in hist.root file; bfcread_hist_list now only lists those that are in the Maker that is input; bfcread_hist_to_ps prints and draws the hist that are in the input Maker, bfcread_dst_QAhist.C reads .dst.root file - runs QA_Maker and prints and draws the QA histograms
//
//=======================================================================
// owner: Kathy Turner
// what it does: reads the *.hist.root file produced from bfc.C and
//               then lists all histogram branches and the names and
//               titles of the histograms in the branches
//=======================================================================


class StChain;
class St_DataSet;

StChain *chain;
St_DataSet *Event;

TBrowser *brow=0;

void bfcread_hist_list_all(Int_t nevents=1, const char
*MainFile="/disk00000/star/test/dev/tfs_Solaris/Wed/year_1b/set0352_01_35evts.hist.root")

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



