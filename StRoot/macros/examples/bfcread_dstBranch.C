// $Id: bfcread_dstBranch.C,v 1.8 2000/04/20 22:05:17 kathy Exp $
// $Log: bfcread_dstBranch.C,v $
// Revision 1.8  2000/04/20 22:05:17  kathy
// update to print out more detailed info
//
// Revision 1.7  2000/04/18 20:37:26  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
// Revision 1.6  2000/04/13 21:46:22  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.5  2000/04/12 16:13:40  kathy
// have changed so that macro loads only table libraries needed instead of all table libraries
//
// Revision 1.4  2000/03/20 17:50:40  kathy
// fix all macros so that they set all branches on that are needed - otherwise won't work with soft links
//
// Revision 1.3  2000/03/13 17:50:17  kathy
// putting in example macro to show how to read and navigate through each type of bfc DST production output files - still working on tags.root
//
// Revision 1.2  2000/01/19 15:46:05  kathy
// change default input files to point to ones in /afs/rhic/star/data/samples
//
// Revision 1.1  2000/01/07 19:22:52  kathy
// add bfcread_dstBranch,bfcread_runcoBranch macros to show how to read dst produced from bfc.C and navigate through different branches of the file and find and list table information
//

//======================================================================
// owner:  Kathy Turner
// what it does:  reads .dst.root file produced from bfc & shows how
//                to find the dst tables
//                 - sets branch to dstBranch
//                 - gets Data Set "dst"
//                 - prints out list of tables & # rows it finds
//
//=======================================================================

class StChain;
StChain *chain;

void bfcread_dstBranch(
 Int_t nevents=2, 
 const char *MainFile=
 "/afs/rhic/star/data/samples/gstar.dst.root",
  const char *fname="qa_dst.out")
{
//
  cout << " events to process  = " << nevents << endl;
  cout << " Input File Name = " << MainFile << endl;
  cout << " Output file containing printouts = " << fname << endl;

  ofstream fout(fname);

  fout << " Running: bfcread_dstBranch.C " << endl;
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
 

 


