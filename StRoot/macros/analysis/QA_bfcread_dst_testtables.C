// $Id: QA_bfcread_dst_testtables.C,v 1.1 1999/08/13 17:14:53 kathy Exp $
// $Log: QA_bfcread_dst_testtables.C,v $
// Revision 1.1  1999/08/13 17:14:53  kathy
// add new macro written by Aya Ishihara that reads a dst and runs St_QATestTables_Maker to QA info about dst tables
//
//
//======================================================================
// owner: Kathy Turner
// revised by: Aya Ishihara
// what it does: see below 
//=======================================================================
// QA_bfcread_dst_testtables.C
//
// Kathy's notes (6/25/99):
//     - read dstbranch from *.dst.root file
//     - read 1 event and print out information from the tables
//     - will be an "official" QA macro
//======================================================================

class StChain;
StChain *chain;

class St_DataSet;
St_DataSet *Event;

void QA_bfcread_dst_testtables(
 Int_t nevents=1, 
 const char *MainFile=
 "/disk00000/star/test/dev/tfs_Solaris/Mon/year_1b/psc0050_01_40evts.dst.root")


{
//
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_QA_Maker");


// Setup top part of chain
  chain = new StChain("bfc");
  chain->SetDebug();
   
//  Input Tree
  StTreeMaker *treeMk = new StTreeMaker("treeRead",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();
  treeMk->SetBranch("*",0,"0");  		//deactivate all branches
  treeMk->SetBranch("dstBranch",0,"r");	//activate EventBranch

//  Input Maker
  St_QATestTables_Maker *ttMk = new St_QATestTables_Maker;


// --- now execute chain member functions
  chain->Init();
 

// Loop over events
// Event loop
  int iev=1, iret=0;
  EventLoop: if ( iev<=nevents && !iret )
    {
      cout << "*** Event #: " << iev << " start *** " << endl;
      chain->Clear();
      iret = chain->Make();
      if (iret) { cout << " Finishing the Event loop " << endl;}
      iev++;
      goto EventLoop;
     }

// ------------------------------------------------------------
  chain->Finish();      
}























