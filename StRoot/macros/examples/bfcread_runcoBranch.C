// $Id: bfcread_runcoBranch.C,v 1.3 2000/03/13 17:50:18 kathy Exp $
// $Log: bfcread_runcoBranch.C,v $
// Revision 1.3  2000/03/13 17:50:18  kathy
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
// what it does:  reads .dst.root file produced from bfc & then goes to
//                the runco.root branch
//                - finds data set "runco"
//                - loops over and prints out list of objects 
//                  - then for each object (which is a directory)
//                    prints out all objects (I'm calling them sub-objects)
//                    inside it
//                  - also checks if the sub-object is a table and prints
//                    info if it is
//
//  - the runco branch only has 1 "event" per dst run!
//=======================================================================

class StChain;
StChain *chain;

void bfcread_runcoBranch(
 Int_t nevents=1, 
 const char *MainFile=
"/star/rcf/test/new/tfs_redhat61/year_1h/hc_standard/hc_standard.40_evts.dst.root")
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
  IOMk->SetBranch("runcoBranch",0,"r"); //activate runco Branch

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
    int countObjInDir=0;
    int countTable=0;

    if (!istat) {
      ds=chain->GetDataSet("runco");
      St_DataSetIter tabiter(ds);
      if (ds) {
//        ds->ls(2);  
        while (obj = tabiter.Next()) {
//	  cout << " I have found an object! " <<endl;
          cout << "  found object = " << obj->GetName() << endl;
          countObj++;

//.. now loop over each subdirectory and look at objects in it

  St_DataSet *objindir=0;
  St_DataSetIter objindiriter(obj);
        while(objindir = objindiriter.Next()) {
         cout << "     has sub-object = " << objindir->GetName() << endl;
         countObjInDir++;

	 // if it's a table, print more info:
         if (objindir->InheritsFrom("St_Table")) {           
            tabl = (St_Table *)objindiriter.Find(objindir->GetName());
            if (tabl) {
              countTable++;
              cout << "      which is a table with # rows =  "
                   << tabl->GetNRows() << endl;
            }
          }
         
        }

	}      
      }
    }

    cout << " End of Event " << i << endl;
    cout << "  # objects found = " << countObj << endl;
    cout << "  # sub-objects found = " << countObjInDir << endl;
    cout << "  # tables found = " << countTable << endl;

    if (istat) {
      cout << "Last event processed. Status = " << istat << endl;
    }

    i++;
    goto EventLoop;
   }

  cout << " bfcread RUNCO Branch: passed event loop " << endl;

 chain->Finish();   
}
 


