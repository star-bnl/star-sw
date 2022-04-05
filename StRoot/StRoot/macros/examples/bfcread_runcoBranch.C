// $Id: bfcread_runcoBranch.C,v 1.12 2006/08/15 21:43:13 jeromel Exp $
// $Log: bfcread_runcoBranch.C,v $
// Revision 1.12  2006/08/15 21:43:13  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.11  2000/06/22 21:24:07  kathy
// force only 1 event to be processed in runco Branch
//
// Revision 1.10  2000/05/03 19:04:34  kathy
// update to make consistent with other macros of same type
//
// Revision 1.9  2000/04/18 20:37:26  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
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
// change default input files to point to ones in /afs/rhic.bnl.gov/star/data/samples
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
 "/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
  const char *fname="qa_runco.out")
{
//
  if (nevents != 1){
    cout << " there is only 1 event in this branch!! " << endl;
  }
  nevents=1;

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

  TDataSet *ds=0;
  TTable   *tabl=0;
  TDataSet *obj=0;


  int istat=0;
  int i=0;
  int countev=0;
  int countevrds=0;

// Event loop
EventLoop: if (i < nevents && !istat) {

    chain->Clear();
    istat = chain->Make(i);

//  count # times Make is called
    i++;

// Now look at the data in the event:
    int countObj=0;
    int countObjInDir=0;
    int countTable=0;

    if (!istat) {

    countev++;

    cout << " start event # " << countev << endl;

      ds=chain->GetDataSet("runco");
      TDataSetIter tabiter(ds);
      if (ds) {
        countevrds++;

       while (obj = tabiter.Next()) {
	  cout << " QAInfo: found object: " << obj->GetName() << endl;
	  fout << " QAInfo: found object: " << obj->GetName() << endl;

          countObj++;

//.. now loop over each subdirectory and look at objects in it

  TDataSet *objindir=0;
  TDataSetIter objindiriter(obj);
        while(objindir = objindiriter.Next()) {
         cout << " QAInfo:   has sub-object = " << objindir->GetName() << endl;
         fout << " QAInfo:   has sub-object = " << objindir->GetName() << endl;
         countObjInDir++;

	 // if it's a table, print more info:
         if (objindir->InheritsFrom("TTable")) {           
            tabl = (TTable *)objindiriter.Find(objindir->GetName());
            if (tabl) {
              countTable++;
              cout << " QAInfo:     it's a table with #rows = " 
		   << objindir->GetName() << ",  " << tabl->GetNRows() << endl;
              fout << " QAInfo:     it's a table with #rows = " 
		   << objindir->GetName() << ",  " << tabl->GetNRows() << endl;

            } //tabl
	 }  //objindir
        } //while objindir
       } //while obj
      } //ds

    cout << " QAInfo: event # " << countev
            << ", # directories found = " << countObj 
            << ", # sub-objects found = " << countObjInDir 
	    << ", # tables found = " << countTable 
            << endl << endl;

    fout << " QAInfo: event # " << countev
            << ", # directories found = " << countObj 
            << ", # sub-objects found = " << countObjInDir 
	    << ", # tables found = " << countTable 
            << endl << endl;

    } //istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
    }


    goto EventLoop;
} //EventLoop



  cout << endl;
  cout << "QAInfo: End of Job " << endl; 
  cout << "QAInfo: # times Make called = " << i << endl;
  cout << "QAInfo:  # events read = " << countev << endl;
  cout << "QAInfo:   # events with runco dataset = " << countevrds << endl;

  fout << endl;
  fout << "QAInfo: End of Job " << endl;
  fout << "QAInfo: # times Make called = " << i << endl;
  fout << "QAInfo:  # events read = " << countev << endl;
  fout << "QAInfo:   # events with runco dataset = " << countevrds << endl;

 chain->Finish();   
}
 


