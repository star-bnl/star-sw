// $Id: bfcread_dst_geant_Branch.C,v 1.6 2006/08/15 21:43:09 jeromel Exp $
// $Log: bfcread_dst_geant_Branch.C,v $
// Revision 1.6  2006/08/15 21:43:09  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.5  2000/05/09 20:15:44  kathy
// transfer obsolete macros to /macros/obsolete;  update other macros so that they use standard default inputs plus only few events by default so they'll be easy to run in autoQA macro testing
//
// Revision 1.4  2000/05/03 18:25:46  kathy
// update to make consistent with other macros of same type
//
// Revision 1.3  2000/04/18 20:37:26  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
// Revision 1.2  2000/04/14 14:40:16  kathy
// put in correct default input data set
//
// Revision 1.1  2000/04/14 14:36:58  kathy
// new example to read and examine 2 different branchs - dst & geant - of DST output
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

void bfcread_dst_geant_Branch(
 Int_t nevents=2, 
 const char *MainFile=
 "/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
 const char *fname="qa_dst_geant.out")
{
//

  cout << " events to process  = " << nevents << endl;
  cout << " Input File Name = " << MainFile << endl;
  cout << " Output file containing printouts = " << fname << endl;

  ofstream fout(fname);

  fout << " Running: bfcread_dst_geant_Branch.C " << endl;
  fout << " events to process  = " << nevents << endl;
  fout << " Input File Name = " << MainFile << endl;
  fout << " Output file containing printouts = " << fname << endl;
  fout << endl << endl;


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
  IOMk->SetBranch("geantBranch",0,"r"); //activate dst Branch

// --- now execute chain member functions
  chain->Init();

  TDataSet *ds=0;
  TTable   *tabl=0;
  TDataSet *obj=0;

  Float_t tottabcntrdst=0;
  Float_t tottabcntrgeant=0;

  Float_t totobjcntrdst=0;
  Float_t totobjcntrgeant=0;

  int istat=0;
  int i=0;

  int countev=0;

  int countevgds=0;
  int countevgobj=0;
  int countevgtab=0;

  int countevdds=0;
  int countevdobj=0;
  int countevdtab=0;

// Event loop
EventLoop: if (i < nevents && !istat) {

    chain->Clear();
    istat = chain->Make(i);

//  count # times Make is called
    i++;

    //    cout << " Call Make # " << i << endl; 
    //    cout << "     istat value returned from chain Make = " << istat << endl;


// Now look at the data in the event:
    int countObjDst=0;
    int countObjGeant=0;
    int countTableDst=0;
    int countTableGeant=0;

    if (!istat) {

    countev++;

    cout << " start event # " << countev << endl;

// ------------ dst branch ------------------------------

      ds=chain->GetDataSet("dst");
      TDataSetIter tabiter(ds);
      if (ds) {
        countevdds++;

        while (obj = tabiter.Next()) {
	  cout << " QAInfo: dst, found object: " << obj->GetName() << endl;
	  fout << " QAInfo: dst, found object: " << obj->GetName() << endl;

          countObjDst++;
          totobjcntrdst++;

//.. count all tables that exist:
          if (obj->InheritsFrom("TTable")) {
            tabl = (TTable *)tabiter.Find(obj->GetName());
            if (tabl) {
              countTableDst++;
              tottabcntrdst++;
              cout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
              fout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
	    } // tabl
          }  // obj

//.. end of counting all tables that exist
	}  // while
           if (countObjDst) countevdobj++;
           if (countTableDst) countevdtab++;
      }   // ds

// ------------ dst branch ------------------------------           


// ------------ geant branch ------------------------------

      ds=chain->GetDataSet("geant");
      TDataSetIter tabiter(ds);
      if (ds) {
        countevgds++;

        while (obj = tabiter.Next()) {
	  cout << " QAInfo: geant, found object: " << obj->GetName() << endl;
	  fout << " QAInfo: geant, found object: " << obj->GetName() << endl;

          countObjGeant++;
          totobjcntrgeant++;

//.. count all tables that exist:
          if (obj->InheritsFrom("TTable")) {
            tabl = (TTable *)tabiter.Find(obj->GetName());
            if (tabl) {
              countTableGeant++;
              tottabcntrgeant++;
              cout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
              fout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
	    } // tabl
          }  // obj

//.. end of counting all tables that exist
	}  // while
           if (countObjGeant) countevgobj++;
           if (countTableGeant) countevgtab++;
      }   // ds

// ------------ geant branch ------------------------------           

    cout << " QAInfo: event # " << countev << 
            ", DST Branch - # objects found = " 
         << countObjDst << ", # tables found = " 
         << countTableDst << endl;
    cout << " QAInfo: event # " << countev << 
            ", GEANT Branch - # objects found = " 
         << countObjGeant << ", # tables found = " 
         << countTableGeant << endl << endl;


    fout << " QAInfo: event # " << countev << 
            ", DST Branch - # objects found = " 
         << countObjDst << ", # tables found = " 
         << countTableDst << endl;
    fout << " QAInfo: event # " << countev << 
            ", GEANT Branch - # objects found = " 
         << countObjGeant << ", # tables found = " 
         << countTableGeant << endl << endl;

   } // istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
    }

    goto EventLoop;

}  // EventLoop
     
  tottabcntrdst   /= countev;
  totobjcntrdst   /= countev;
  tottabcntrgeant /= countev;
  totobjcntrgeant /= countev;
 
  cout << endl;
  cout << "QAInfo: End of Job " << endl; 
  cout << "QAInfo: # times Make called = " << i << endl;
  cout << "QAInfo:  # events read = " << countev << endl;
  cout << "QAInfo:   # events with geant dataset = " << countevgds << endl;
  cout << "QAInfo:     # with objects = " << countevgobj << endl;
  cout << "QAInfo:     # with tables  = " << countevgtab << endl;
  cout << "QAInfo:     avg # tables per event  = " << tottabcntrgeant << endl;
  cout << "QAInfo:     avg # objects per event = " << totobjcntrgeant << endl 
            << endl;
  cout << "QAInfo:   # events with dst dataset = " << countevdds << endl;
  cout << "QAInfo:     # with objects = " << countevdobj << endl;
  cout << "QAInfo:     # with tables  = " << countevdtab << endl;
  cout << "QAInfo:     avg # tables per event  = " << tottabcntrdst << endl;
  cout << "QAInfo:     avg # objects per event = " << totobjcntrdst << endl
            << endl;

  fout << endl;
  fout << "QAInfo: # times Make called = " << i << endl;
  fout << "QAInfo:  # events read = " << countev << endl;
  fout << "QAInfo:   # events with geant dataset = " << countevgds << endl;
  fout << "QAInfo:     # with objects = " << countevgobj << endl;
  fout << "QAInfo:     # with tables  = " << countevgtab << endl;
  fout << "QAInfo:     avg # tables per event  = " << tottabcntrgeant << endl;
  fout << "QAInfo:     avg # objects per event = " << totobjcntrgeant << endl 
            << endl;
  fout << "QAInfo:   # events with dst dataset = " << countevdds << endl;
  fout << "QAInfo:     # with objects = " << countevdobj << endl;
  fout << "QAInfo:     # with tables  = " << countevdtab << endl;
  fout << "QAInfo:     avg # tables per event  = " << tottabcntrdst << endl;
  fout << "QAInfo:     avg # objects per event = " << totobjcntrdst << endl
            << endl;


 chain->Finish();   
}
 


 


