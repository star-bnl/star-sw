// $Id: bfcread_geantBranch.C,v 1.4 2000/03/23 19:54:35 kathy Exp $
// $Log $

//======================================================================
// owner:  Kathy Turner
// what it does:  reads .dst.root file produced from bfc & then goes to
//                the geant.root branch
//                - finds data set "geant"
//                - loops over and prints out list of objects 
//                   and tables
//
// Inputs to macro:
//   nevents  -  # events to process
//   MainFile - input *.dst.root file  (you can use any branch here)
//   fname    - output file name with qa info
//   
//
//=======================================================================

class StChain;
StChain *chain;

void bfcread_geantBranch(
 Int_t nevents=2, 
 const char *MainFile=
 "/afs/rhic/star/data/samples/gstar.dst.root",
 const char *fname="qa_geant.out") 
{
//
  cout << " events to process  = " << nevents << endl;
  cout << " Input File Name = " << MainFile << endl;
  cout << " Output file containing printouts = " << fname << endl;

  ofstream fout(fname);

  fout << " Running: bfcread_geantBranch.C " << endl;
  fout << " events to process  = " << nevents << endl;
  fout << " Input File Name = " << MainFile << endl;
  fout << " Output file containing printouts = " << fname << endl;
  fout << endl << endl;

    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StIOMaker");

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("geantBranch",0,"r"); //activate geant Branch

// --- now execute chain member functions
  chain->Init();

  St_DataSet *ds=0;
  St_Table   *tabl=0;
  St_DataSet *obj=0;

  Int_t tottabcntr=0;
// Event loop
  int istat=0,i=0;

EventLoop: if (i < nevents && !istat) {

    chain->Clear();
    istat = chain->Make(i);
    cout << "     istat value returned from chain Make = " << istat << endl;


// Now look at the data in the event:
    int countObj=0;
    int countTable=0;

    if (!istat) {

    i++;
    cout << " start event # " << i << endl;

      ds=chain->GetDataSet("geant");
      St_DataSetIter tabiter(ds);
      if (ds) {
//        ds->ls(2);  
        while (obj = tabiter.Next()) {
//	  cout << " I have found an object! " <<endl;
          countObj++;

//.. count all tables that exist:
          if (obj->InheritsFrom("St_Table")) {
            tabl = (St_Table *)tabiter.Find(obj->GetName());
            if (tabl) {
              countTable++;
	      // cout << " Found Object (Table) " << endl;
              cout << " QAInfo: Found table, Name = " 
                   <<  obj->GetName() << 
                      "    # rows =  " << tabl->GetNRows() << endl;

             fout << " QAInfo: Found table, Name = " 
                   <<  obj->GetName() << 
                      "    # rows =  " << tabl->GetNRows() << endl;
              tottabcntr++;
             }
          }
//.. end of counting all tables that exist

	}      
      }
    }
    cout << " QAInfo: event # " << i << ", # objects found = " 
         << countObj << ", # tables found = " << countTable << endl << endl;
    fout << " QAInfo: event # " << i << ", # objects found = " 
         << countObj << ", # tables found = " << countTable << endl << endl;


    if (istat) {
      cout << "Last event processed. Status = " << istat << endl;
    }


    goto EventLoop;
   }
     
  tottabcntr /= i;

  cout <<  endl << "QAInfo:  total # events read  = " << i << endl;
  cout <<  "QAInfo:  # tables per event   = " << tottabcntr << endl << endl;

  fout <<  endl << "QAInfo:  total # events read  = " << i << endl;
  fout <<  "QAInfo:  # tables per event   = " << tottabcntr << endl << endl;

 chain->Finish();   
}
 




