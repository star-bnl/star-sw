// $Id: bfcread_geantBranch.C,v 1.2 2000/03/20 17:50:41 kathy Exp $
// $Log $

//======================================================================
// owner:  Kathy Turner
// what it does:  reads .dst.root file produced from bfc & then goes to
//                the geant.root branch
//                - finds data set "geant"
//                - loops over and prints out list of objects 
//                   and tables
//=======================================================================

class StChain;
StChain *chain;

void bfcread_geantBranch(
 Int_t nevents=2, 
 const char *MainFile=
 "/afs/rhic/star/data/samples/gstar.dst.root")
{
//
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

// Event loop
  int istat=0,i=1;

EventLoop: if (i <= nevents && !istat) {
    cout << "============================ Event " << i << " start" << endl;
    chain->Clear();
    istat = chain->Make(i);
    cout << "     istat value returned from chain Make = " << istat << endl;


// Now look at the data in the event:
    int countObj=0;
    int countTable=0;

    if (!istat) {
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
              cout << " Found Object (Table) " << endl;
              cout << "   Name = " <<  obj->GetName() << 
                      "    # rows =  " << tabl->GetNRows() << endl;
             }
          }
//.. end of counting all tables that exist


	}      
      }
    }
    cout << " End of Event " << i << endl;
    cout << "  # objects found = " << countObj << endl;
    cout << "  # tables found = " << countTable << endl;


    if (istat) {
      cout << "Last event processed. Status = " << istat << endl;
    }

    i++;
    goto EventLoop;
   }

  cout << " bfcread GEANT Branch: passed event loop " << endl;

 chain->Finish();   
}
 


