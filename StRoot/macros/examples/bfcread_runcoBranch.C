// $Id: bfcread_runcoBranch.C,v 1.8 2000/04/12 16:20:12 kathy Exp $
// $Log: bfcread_runcoBranch.C,v $
// Revision 1.8  2000/04/12 16:20:12  kathy
// fixing library loads to only do minimum
//
// Revision 1.6  2000/03/23 19:54:36  kathy
// update branch macros so they write out to a file, and clean up
//
// Revision 1.5  2000/03/21 15:45:07  kathy
// updated the bfcread_*Branch.C macros so they printout info that can be used by autoQA system
//
// Revision 1.4  2000/03/20 17:50:41  kathy
// fix all macros so that they set all branches on that are needed - otherwise won't work with soft links
//
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
//
// Inputs to macro:
//   nevents  -  # events to process  (should always leave at 1 !!)
//   MainFile - input *.dst.root file  (you can use any branch here)
//   fname    - output file name with qa info
//  
//=======================================================================

class StChain;
StChain *chain;

void bfcread_runcoBranch(
 Int_t nevents=1, 
 const char *MainFile=
 "/afs/rhic/star/data/samples/gstar.dst.root",
  const char *fname="qa_runco.out")
{
//
  cout << " events to process  = " << nevents << endl;
  cout << " Input File Name = " << MainFile << endl;
  cout << " Output file containing printouts = " << fname << endl;

  ofstream fout(fname);

  fout << " Running: bfcread_runcoBranch.C " << endl;
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
  IOMk->SetBranch("runcoBranch",0,"r"); //activate runco Branch

// --- now execute chain member functions
  chain->Init();

  St_DataSet *ds=0;
  St_Table   *tabl=0;
  St_DataSet *obj=0;

// Event loop
  int istat=0,i=0;

EventLoop: if (i < nevents && !istat) {

    chain->Clear();
    istat = chain->Make(i);
    cout << "     istat value returned from chain Make = " << istat << endl;


// Now look at the data in the event:
    int countObj=0;
    int countObjInDir=0;
    int countTable=0;

    if (!istat) {

    i++;
    cout << " start event # " << i << endl;

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
              cout << "  QAInfo: found table, #rows = " << 
                 objindir->GetName() << ",  " << tabl->GetNRows() << endl;
              fout << "  QAInfo: found table, #rows = " << 
                 objindir->GetName() << ",  " << tabl->GetNRows() << endl;
            }
          }
         
        }

	}      
      }
    }

    cout << endl << endl <<" QAInfo: finished event " << i << endl;
    cout << " QAInfo: # objects (directories) found = " << countObj << endl;
    cout << " QAInfo: # sub-objects, tables found = "
	 << countObjInDir << ", " << countTable << endl << endl;

    fout << endl << endl <<" QAInfo: finished event " << i << endl;
    fout << " QAInfo: # objects (directories) found = " << countObj << endl;
    fout << " QAInfo: # sub-objects, tables found = "
	 << countObjInDir << ", " << countTable << endl << endl;

    if (istat) {
      cout << "Last event processed. Status = " << istat << endl;
    }
    
    goto EventLoop;
   }

  cout << " bfcread RUNCO Branch: passed event loop " << endl;

 chain->Finish();   
}
 


