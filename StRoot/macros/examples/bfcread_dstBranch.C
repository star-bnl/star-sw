// $Id: bfcread_dstBranch.C,v 1.11 2000/05/31 21:25:17 kathy Exp $
// $Log: bfcread_dstBranch.C,v $
// Revision 1.11  2000/05/31 21:25:17  kathy
// updated so it now finds all tables/objects (e.g. BfcStatus) under dstBranch
//
// Revision 1.10  2000/05/03 18:36:30  kathy
// ooopps - my mistake - autoQA is using bfcread_dstBranch now instead of QA_bfcread_dst_tables - so put it back in...
//
//======================================================================
// owner:  Kathy Turner
// what it does:  reads .dst.root file produced from bfc & then goes to
//                the dst branch
//                - finds data set "dst"
//                - loops over and prints out list of objects 
//                   and tables
//
// Inputs to macro:
//   nevents  -  # events to process
//   MainFile - input *.dst.root file  (you can use any branch here)
//   fname    - output file name with qa info
//   
//
//======================================================================

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

  TTable   *tabl=0;

  TDataSet *obj=0;
  TDataSet *ddb=0;
  TDataSet *ddstBranch=0;

  int istat=0;
  int iev=0;

  int countev=0;

  Float_t countevdstB=0.0;
  Float_t countevdst=0.0;
  Float_t countevobj=0.0;
  Float_t countevtab=0.0;

// Event loop
EventLoop: if (iev < nevents && !istat) {

  Int_t Countevobj=0;
  Int_t Countevtab=0;

    chain->Clear();
    istat = chain->Make(iev);
    
//  count # times Make is called
    iev++;

    cout << " Call Make # " << iev << endl; 
    cout << "     istat value returned from chain Make = " << istat << endl;

    if (!istat) {

    countev++;
    cout << " start event # " << countev << endl;

    ddstBranch=chain->GetDataSet("dstBranch");

    TDataSetIter dstbranchIter(ddstBranch);

    if (ddstBranch) {

    countevdstB++;

    cout << endl << " QAInfo: in dstBranch " << endl;
    fout << endl << " QAInfo: in dstBranch " << endl;

    while (ddb=dstbranchIter.Next()) {

      cout << endl << " QAInfo:   found object: " << ddb->GetName() << endl;
      fout << endl << " QAInfo:   found object: " << ddb->GetName() << endl;
     
      countevobj++;
      Countevobj++;

      if (ddb->InheritsFrom("TTable")) { 
	 countevtab++;
         Countevtab++;

         tabl = (TTable *)ddb;
         cout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
      }

      TString dsName =  ddb->GetName();

// now look under dst branch
      if (dsName == "dst") {

        countevdst++;

    cout << " QAInfo:    in dst object " << endl;
    fout << " QAInfo:    in dst object " << endl;

// look for dst objects/tables

        TDataSetIter tabiter(ddb);

//        ddb->ls(2);  
        while (obj = tabiter.Next()) {

	  cout << " QAInfo:     found object: " << obj->GetName() << endl;
	  fout << " QAInfo:     found object: " << obj->GetName() << endl;

          countevobj++;
          Countevobj++;

//.. count all tables that exist:
          if (obj->InheritsFrom("TTable")) {
            tabl = (TTable *)tabiter.Find(obj->GetName());
            if (tabl) {
	      countevtab++;
              Countevtab++;

             cout << " QAInfo:       it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
             fout << " QAInfo:       it's a table with #rows = " 
                        << tabl->GetNRows() << endl;

	    } // tabl
          }  // obj
//.. end of counting all tables that exist

	}  // while obj

      } // dsName = dst

    } // while dstBranch

    cout << endl << " QAInfo: event # " << countev << 
            ", # objects found = " << Countevobj << 
            ", # tables found = " <<  Countevtab << 
             endl << endl;

    fout << endl << " QAInfo: event # " << countev << 
            ", # objects found = " << Countevobj << 
            ", # tables found = " <<  Countevtab << 
             endl << endl;


      } // if dstBranch

    }  // istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
      }

    goto EventLoop;

}  // EventLoop
     
    countevobj /= countev;
    countevtab /= countev;

  cout << endl;
  cout << "QAInfo: End of Job " << endl; 
  cout << "QAInfo: # times Make called = " << iev << endl;
  cout << "QAInfo:  # events read = " << countev << endl;
  cout << "QAInfo:   # events with dstBranch dataset = " << 
                      countevdstB << endl;
  cout << "QAInfo:   # events with dst dataset = " << 
                      countevdst << endl;
  cout << "QAInfo: avg # tables per event  = " << countevtab << endl;
  cout << "QAInfo: avg # objects per event = " << countevobj << endl << endl;


  fout << endl;
  fout << "QAInfo: End of Job " << endl; 
  fout << "QAInfo: # times Make called = " << iev << endl;
  fout << "QAInfo:  # events read = " << countev << endl;
  fout << "QAInfo:   # events with dstBranch dataset = " << 
                      countevdstB << endl;
  fout << "QAInfo:   # events with dst dataset = " << 
                      countevdst << endl;
  fout << "QAInfo: avg # tables per event  = " << countevtab << endl;
  fout << "QAInfo: avg # objects per event = " << countevobj << endl << endl;

 chain->Finish();   

}


