// $Id: bfcread_dst_geant_Branch.C,v 1.3 2000/04/18 20:37:26 kathy Exp $
// $Log: bfcread_dst_geant_Branch.C,v $
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
 Int_t nevents=50, 
 const char *MainFile=
 "/afs/rhic/star/data/samples/gstar.dst.root",
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
    int countTableDst=0;
    int countTableGeant=0;


    if (!istat) {

// ------------ dst branch ------------------------------

      ds=chain->GetDataSet("dst");
      TDataSetIter tabiter(ds);
      if (ds) {
//        ds->ls(2);  
	cout << " In dataset: dst " << endl;
	fout << " In dataset: dst " << endl;
        while (obj = tabiter.Next()) {
//	  cout << " I have found an object! " <<endl;
          countObj++;

//.. count all tables that exist:
          if (obj->InheritsFrom("TTable")) {
            tabl = (TTable *)tabiter.Find(obj->GetName());
            if (tabl) {
              countTable++;
              countTableDst++;
              cout << " Found Object (Table) "<< endl;
              cout << "   Found Table Name = " <<  obj->GetName() << 
                      "    # rows =  " << tabl->GetNRows() << endl;
              fout << "   Found Table Name = " <<  obj->GetName() << 
                      "    # rows =  " << tabl->GetNRows() << endl;

            }
          }

	}
      }

// ------------ dst branch ------------------------------           


// ------------ geant branch ------------------------------

      ds=chain->GetDataSet("geant");
      TDataSetIter tabiter(ds);
      if (ds) {
//        ds->ls(2);  
	cout << " In dataset: geant " << endl;
	fout << " In dataset: geant " << endl;
        while (obj = tabiter.Next()) {
//	  cout << " I have found an object! " <<endl;
          countObj++;

//.. count all tables that exist:
          if (obj->InheritsFrom("TTable")) {
            tabl = (TTable *)tabiter.Find(obj->GetName());
            if (tabl) {
              countTable++;
              countTableGeant++;
              cout << " Found Object (Table) "<< endl;
              cout << "   Found Table Name = " <<  obj->GetName() << 
                      "    # rows =  " << tabl->GetNRows() << endl;
              fout << "   Found Table Name = " <<  obj->GetName() << 
                      "    # rows =  " << tabl->GetNRows() << endl;

            }
          }

	}
      }

// ------------ geant branch ------------------------------             

// end of istat check
    }

    cout << " End of Event " << i << endl;
    cout << "  tot # objects found = " << countObj << endl;
    cout << "  # dst,geant, tot tables = " << 
               countTableDst << "  "  << 
               countTableGeant << "  "  << 
               countTable << "  "  << 
               endl << endl << endl;

    fout << " End of Event " << i << endl;
    fout << "  # dst,geant, tot tables = " << 
               countTableDst << "  "  << 
               countTableGeant << "  "  << 
               countTable << "  "  << 
               endl << endl << endl;

    if (istat) {
      cout << "Last event processed. Status = " << istat << endl;
    }

    i++;
    goto EventLoop;
   }

  cout << " bfcread DST Branch: passed event loop " << endl;

 chain->Finish();   
}
 


 


