// $Id: bfcread_runcoBranch.C,v 1.1 2000/01/07 19:22:52 kathy Exp $
// $Log: bfcread_runcoBranch.C,v $
// Revision 1.1  2000/01/07 19:22:52  kathy
// add bfcread_dstBranch,bfcread_runcoBranch macros to show how to read dst produced from bfc.C and navigate through different branches of the file and find and list table information
//
//======================================================================
// owner:  Kathy Turner
// what it does:  reads .dst.root file produced from bfc & shows how
//                to find the runco tables
//                 - sets branch to runcoBranch
//                 - gets Data Set "dstRunco"
//                 - prints out list of tables & # rows it finds
//
//=======================================================================

class StChain;
StChain *chain;

void bfcread_runcoBranch(
 Int_t nevents=1, 
 const char *MainFile=
 "/star/rcf/test/dev/tfs_Linux/Mon/year_1b/hc_lowdensity/gstar.dst.root")
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
  IOMk->SetBranch("runcoBranch",0,"r"); //activate dst Branch

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
    i++;
    cout << "     istat value returned from chain Make = " << istat << endl;
    if (!istat) {
      ds=chain->GetDataSet("dstRunco");
      St_DataSetIter tabiter(ds);
      if (ds) {
        ds->ls(2);  
        while (obj = tabiter.Next()) {
//	  cout << " in loop over dataset " << endl;
//.. count all tables that exist:
          if (obj->InheritsFrom("St_Table")) {
//           	  cout << " in loop over tables " << endl;
            tabl = (St_Table *)tabiter.Find(obj->GetName());
            if (tabl) {
              cout << " Table Name " << obj->GetName() << " # rows   " 
                << tabl->GetNRows() << endl;
             }
          }
	}
      }
    }
    if (istat) {cout << "Last event processed. Status = " << istat << endl;}
    goto EventLoop;
   }

  cout << " bfcread: passed event loop " << endl;

 chain->Finish();   
}
 


