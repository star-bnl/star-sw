// $Id: QA_bfcread_dst_tables.C,v 1.25 2000/05/02 21:15:52 kathy Exp $
// $Log: QA_bfcread_dst_tables.C,v $
// Revision 1.25  2000/05/02 21:15:52  kathy
// updated print statements in macro to be explicit
//
// Revision 1.24  2000/04/18 20:44:48  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
// Revision 1.23  2000/04/13 21:46:33  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.22  2000/04/13 20:25:46  kathy
// make prettier output
//
// Revision 1.21  2000/04/12 15:31:50  kathy
// keep libtpc table library in by default
//
// Revision 1.20  2000/04/12 15:29:05  kathy
// comment out libtpc by default
//
// Revision 1.19  2000/04/12 15:06:50  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.18  2000/03/15 22:38:21  kathy
// new version of macro that now keeps track of how many events there were and how many tables per event - doesn't keep track of missing tables anymore since autoQA does that
//
// Revision 1.17  2000/01/19 16:29:50  kathy
// update macros to use default input files in /afs/rhic/star/data/samples
//
// Revision 1.16  2000/01/05 22:12:03  kathy
// changed input file to current one
//
// Revision 1.15  1999/11/19 20:13:21  kathy
// cleaned up macros to remove uneccessary lines; also added info about new tables to QA* macros
//
// Revision 1.14  1999/11/10 17:06:58  kathy
// remove check on globtrk_aux and primtrk_aux tables for 99i and above since these tables are obsolete now - in QA_bfcread_dst_tables.C
//
// Revision 1.13  1999/11/10 14:57:57  kathy
// changed QA_bfcread_dst_tables.C to take into account new tables in 99i and above
//
// Revision 1.12  1999/11/03 19:05:02  kathy
// another small fix for output file name - now .out instead of .txt
//
// Revision 1.11  1999/11/03 17:12:58  kathy
// fixed macros so they use StIOMaker instead of StTreeMaker
//
// Revision 1.10  1999/08/12 16:28:36  kathy
// changed QA_bfcred_dst_tables so that it can loop over many events - before was hardwired to only 1 event
//
// Revision 1.9  1999/08/06 15:08:12  kathy
// removed for loop and put in goto checks - due to CINT problems
//
// Revision 1.8  1999/07/26 20:54:16  kathy
// changed output text to QAInfo: so that the QA sripts can tag on it; also cleaned up a bit and set to newer default input file
//
// Revision 1.7  1999/07/17 00:48:46  kathy
// change check on dst_TrgDet to test on TrgDet table
//
// Revision 1.6  1999/07/13 00:42:32  kathy
// updated all default input files, removed unneccessary macros, renamed other to make more standard
//
// Revision 1.5  1999/07/13 00:29:53  kathy
// updated macros to take out StRootEvent
//
// Revision 1.4  1999/07/07 14:35:47  kathy
// add code to check which tables are missing and print out this info
//
// Revision 1.3  1999/06/28 20:17:41  kathy
// updated version - cleaned up
//
// Revision 1.2  1999/06/28 16:53:20  kathy
// updated version - now checks to see how many tables are really there, not just ones we know about
//
// Revision 1.1  1999/06/25 19:43:47  kathy
// new macros for official QA use
//
//
//======================================================================
// owner: Kathy Turner
// revised by: Curtis Lansdell, Kathy,
// what it does: see below 
//=======================================================================
// QA_bfcread_dst_tables.C
//
// Kathy's notes (6/25/99):
//     - read dstbranch from *.dst.root file
//     - read 1 event and print out information from the tables
//     - will be the "official" QA macro
//======================================================================

class StChain;
StChain *chain;

void QA_bfcread_dst_tables(
 Int_t nevents=2, 
 const char *MainFile=
 "/afs/rhic/star/data/samples/gstar.dst.root",
  const char *fname="qa_dst_tables.out")
{
//
  cout << " events to process  = " << nevents << endl;
  cout << " Input File Name = " << MainFile << endl;
  cout << " Output file containing printouts = " << fname << endl;

  ofstream fout(fname);

  fout << " Running: QA_bfcread_dst_tables.C " << endl;
  fout << " events to process  = " << nevents << endl;
  fout << " Input File Name = " << MainFile << endl;
  fout << " Output file containing printouts = " << fname << endl;
  fout << endl << endl;

    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("StIOMaker");

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch

// --- now execute chain member functions
  chain->Init();

  TDataSet *ds=0;
  TTable   *tabl=0;
  TDataSet *obj=0;


  Float_t tottabcntr=0;
  Float_t totobjcntr=0;


  int istat=0;
  int i=0;
  int countev=0;
  int countevdds=0;
  int countevobj=0;
  int countevtab=0;

// Event loop
EventLoop: if (i < nevents && !istat) {

    chain->Clear();
    istat = chain->Make(i);
    
//  count # times Make is called
    i++;

    cout << " Call Make # " << i << endl; 
    cout << "     istat value returned from chain Make = " << istat << endl;

// Now look at the data in the event:
    int countObj=0;
    int countTable=0;

    if (!istat) {

    countev++;

    cout << " start event # " << countev << endl;

      ds=chain->GetDataSet("dst");
      TDataSetIter tabiter(ds);
      if (ds) {
        countevdds++;
//        ds->ls(2);  
        while (obj = tabiter.Next()) {

	  cout << " QAInfo: found object: " << obj->GetName() << endl;
	  fout << " QAInfo: found object: " << obj->GetName() << endl;

          countObj++;
          totobjcntr++;

//.. count all tables that exist:
          if (obj->InheritsFrom("TTable")) {
            tabl = (TTable *)tabiter.Find(obj->GetName());
            if (tabl) {
              countTable++;
              tottabcntr++;
              cout << " QAInfo: it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
              fout << " QAInfo: it's a table with #rows = " 
                        << tabl->GetNRows() << endl;


	    } // tabl
          }  // obj
//.. end of counting all tables that exist
	}  // while
           if (countObj) countevobj++;
           if (countTable) countevtab++;
      }   // ds

    cout << " QAInfo: event # " << countev << ", # objects found = " 
         << countObj << ", # tables found = " << countTable << endl << endl;
    fout << " QAInfo: event # " << countev << ", # objects found = " 
         << countObj << ", # tables found = " << countTable << endl << endl;

    }  // istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
    }

    goto EventLoop;

}  // EventLoop
     
  tottabcntr /= countev;
  totobjcntr /= countev;
 
  cout << endl;
  cout << "QAInfo: End of Job " << endl; 
  cout << "QAInfo: # times Make called = " << i << endl;
  cout << "QAInfo:  # events read = " << countev << endl;
  cout << "QAInfo:   # events with dst dataset = " << countevdds << endl;
  cout << "QAInfo:     # with objects = " << countevobj << endl;
  cout << "QAInfo:     # with tables  = " << countevtab << endl;
  cout << "QAInfo: avg # tables per event  = " << tottabcntr << endl;
  cout << "QAInfo: avg # objects per event = " << totobjcntr << endl << endl;

  fout << endl;
  fout << "QAInfo: # times Make called = " << i << endl;
  fout << "QAInfo:  # events read = " << countev << endl;
  fout << "QAInfo:   # events with dst dataset = " << countevdds << endl;
  fout << "QAInfo:     # with objects = " << countevobj << endl;
  fout << "QAInfo:     # with tables  = " << countevtab << endl;
  fout << "QAInfo: avg # tables per event  = " << tottabcntr << endl;
  fout << "QAInfo: avg # objects per event = " << totobjcntr << endl << endl;

 chain->Finish();   

}
 

 


