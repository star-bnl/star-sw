// $Id: bfcread_dstBranch.C,v 1.6 2000/04/13 21:46:22 kathy Exp $
// $Log: bfcread_dstBranch.C,v $
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
 "/afs/rhic/star/data/samples/gstar.dst.root")
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
      ds=chain->GetDataSet("dst");
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
              cout << " Found Object (Table) "<< endl;
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

  cout << " bfcread DST Branch: passed event loop " << endl;

 chain->Finish();   
}
 


 


