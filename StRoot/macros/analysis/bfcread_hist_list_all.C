// $Id: bfcread_hist_list_all.C,v 1.5 1999/11/03 21:35:35 kathy Exp $ 
// $Log: bfcread_hist_list_all.C,v $
// Revision 1.5  1999/11/03 21:35:35  kathy
// small fixes for use of StIOMaker - had it wrong before
//
// Revision 1.4  1999/11/03 19:02:54  kathy
// changes to default input files and output file names - needed by perl script for testing
//
// Revision 1.3  1999/11/03 17:13:00  kathy
// fixed macros so they use StIOMaker instead of StTreeMaker
//
// Revision 1.2  1999/09/21 15:07:03  kathy
// change to have notes on input values at top of each macro, also clean up notes on usage and remove the usage of method St_QA_Maker::SetPntrToHistUtil which is not going to be used now that I made St_QA_Maker totally independent of the histogram printing
//
// Revision 1.1  1999/09/20 20:09:02  kathy
// bfcread_hist_list_all now lists all histograms in hist.root file; bfcread_hist_list now only lists those that are in the Maker that is input; bfcread_hist_to_ps prints and draws the hist that are in the input Maker, bfcread_dst_QAhist.C reads .dst.root file - runs QA_Maker and prints and draws the QA histograms
//
//=======================================================================
// owner: Kathy Turner
// what it does: reads the *.hist.root file produced from bfc.C and
//               then lists all histogram branches and the names and
//               titles of the histograms in the branches
// inputs: MainFile - *.hist.root file from bfc output
//=======================================================================

class StChain;
class St_DataSet;

StChain *chain;
St_DataSet *Event;

TBrowser *brow=0;

void bfcread_hist_list_all(const char
*MainFile="/disk00000/star/test/new/tfs_Solaris/year_1b/set0352_01_35evts.hist.root")

{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
   

// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
//  IOMk->SetBranch("tpc_tracks",0,"r"); //activate tpc_tracks Branch
//  IOMk->SetBranch("geantBranch",0,"r"); //activate geant Branch
  IOMk->SetBranch("histBranch",0,"r"); //activate dst Branch


// --- now execute chain member functions
  IOMk->Init();
  IOMk->Clear();
  IOMk->Make();

// Now look at histograms:
    Event = IOMk->GetDataSet("hist");
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



