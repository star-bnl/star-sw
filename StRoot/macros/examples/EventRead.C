// $Id: EventRead.C,v 1.2 1999/06/11 18:35:13 fine Exp $
// $Log: EventRead.C,v $
// Revision 1.2  1999/06/11 18:35:13  fine
// *** empty log message ***
//
// Revision 1.13  1999/06/07 17:31:23  kathy
// clean up some macros
//
// Revision 1.12  1999/05/21 15:33:57  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.11  1999/05/20 18:43:21  kathy
// removing unecessary macros
//
//======================================================================
// owner:  Victor Perevoztchikov
// what it does: 
//=======================================================================
// EventRead.C
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
class StEvent;
StEvent *Event;
StChain *chain;
TBrowser *brow=0;

void EventRead(Int_t nevents=1, const char
*MainFile="/disk00000/star/test/new/tfs_Solaris/year_2a/psc0210_01_40evts.dst.root")

{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StTreeMaker");
//    gSystem->Load("St_QA_Maker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StRootEvent");

    cout << "  .. EventRead.C, have loaded libraries " << endl;

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
//  Input Tree
  StTreeMaker *treeMk = new StTreeMaker("treeRead",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();
  //  treeMk->SetBranch("*",0,"0");  		//deactivate all branches
  //  treeMk->SetBranch("eventBranch",0,"r");	//activate EventBranch
  chain->SetInput("StEvent","treeRead/.data/bfcTree/eventBranch/StEvent");
//  add other makers to chain:
//   St_QA_Maker  *qa  = new St_QA_Maker;
  
// --- now execute chain member functions
  chain->Init();
 
  for (int iev=0;iev<nevents; iev++)
  {
    chain->Clear();
    int iret = chain->Make();
    if (iret) break;

  }

//  You are out of event loop now.  
// Now print out contents of dataset and pop browser
// for the last event.
// Create browser with name=BName,title=Btitle
    Event = (StEvent *) chain->GetDataSet("StEvent");
    if (Event) {
          Event->ls(9);
          brow = new TBrowser("Event",Event);    
    }

// Comment out for now because it clears data so you can't
// look at data in browser!  
//  Call finish routines:
// chain->Finish();    
  
}
 


