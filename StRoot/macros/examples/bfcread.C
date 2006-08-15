// $Id: bfcread.C,v 1.26 2006/08/15 21:43:07 jeromel Exp $
// $Log: bfcread.C,v $
// Revision 1.26  2006/08/15 21:43:07  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.25  2000/04/18 20:37:25  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
// Revision 1.24  2000/04/13 21:46:22  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.23  2000/04/12 16:13:40  kathy
// have changed so that macro loads only table libraries needed instead of all table libraries
//
// Revision 1.22  2000/03/20 17:50:40  kathy
// fix all macros so that they set all branches on that are needed - otherwise won't work with soft links
//
// Revision 1.21  2000/01/19 15:46:05  kathy
// change default input files to point to ones in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.20  2000/01/05 22:11:57  kathy
// changed input file to current one
//
// Revision 1.19  1999/09/13 14:33:49  kathy
// update bfcread.C so that it now uses IOMaker instead of TreeMaker - tested it for .dst.root,.dst.xdf, .*.root files - works for all
//
// Revision 1.18  1999/07/27 00:47:04  kathy
// remove the for loop from bfcread.C and replace with basically while and go to statements;  this is due to the for loop problem in CINT - see my email on 26Jul99 to starsoft
//
// Revision 1.17  1999/07/13 01:13:02  kathy
// moved rch.C to obsolete, put in id,log,owner into HbtExample, removed loading of StRootEvent and changed default input file in bfcread.C and Example_readdst_qa_tables.C
//
// Revision 1.16  1999/06/27 22:45:34  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.15  1999/06/22 18:09:02  kathy
// change default input file
//
// Revision 1.14  1999/06/17 18:26:15  kathy
// bfcread: change default input file; Example.. fix so works if you just execute it
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
// what it does:  reads output files from bfc and displays data in browser
//                - more info below
//=======================================================================
// bfcread.C
//
// Kathy's notes (9/13/99):
//     - example to show how to read in file (.dst.xdf, .dst.root, .*.root) 
//       produced from bfc.C and:
//       - run another maker (St_QA_Maker)  - commented out for now
//       - look at it using the browser
//
// This example is reading in the "dst" branch of the root file.
// (i.e. the input file is .dst.root)
// If you want to read in a different branch, you must change:
//    - the input file name, e.g. *.bname.root (or .dst.xdf)
//    - IOMk->SetBranch("bnameBranch",0,"r");
//    - chain->GetDataSet("bname");
//
//
// This example is for debugging/testing purposes and  therefore does
//   not have chain->Finish(); at end
//
//======================================================================

class StChain;
class TDataSet;

TDataSet *Event;
StChain *chain;
TBrowser *brow=0;

void bfcread(
 Int_t nevents=1, 
 const char *MainFile=
"/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root")
{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");

  gSystem->Load("libglobal_Tables");
  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");

    gSystem->Load("StIOMaker");


//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch


// How to add other makers to chain (must also load library!)
//   St_QA_Maker  *qa  = new St_QA_Maker;
  
// --- now execute chain member functions
  chain->Init();
 
// Event loop
  int istat=0,i=1;
EventLoop: if (i <= nevents && !istat) {
    cout << "============================ Event " << i << " start" << endl;
    chain->Clear();
    istat = chain->Make(i);
    cout << "     istat value returned from chain Make = " << istat << endl;
    if (istat) {cout << "Last event processed. Status = " << istat << endl;}
    i++; goto EventLoop;
   }


  cout << " bfcread: passed event loop " << endl;

      Event = chain->GetDataSet("dst");
    if (Event) {
          Event->ls(9);
          brow = new TBrowser("BName","BTitle");    
    }

// Comment out for now because it clears data so you can't
// look at data in browser!  
//  Call finish routines:
// chain->Finish();    

}
 


