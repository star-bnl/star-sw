// $Id: QA_bfcread_dst_tables.C,v 1.3 1999/06/28 20:17:41 kathy Exp $
// $Log: QA_bfcread_dst_tables.C,v $
// Revision 1.3  1999/06/28 20:17:41  kathy
// updated version - cleaned up
//
// Revision 1.2  1999/06/28 16:53:20  kathy
// updated version - now checks to see how many tables are really there, not just ones we know about
//
// Revision 1.1  1999/06/25 19:43:47  kathy
// new macros for official QA use
//
//
//======================================================================
// owner: Kathy Turner
// revised by: Curtis Lansdell, Kathy,
// what it does: see below 
//=======================================================================
// QA_bfcread_dst_tables.C
//
// Kathy's notes (6/25/99):
//     - read dstbranch from *.dst.root file
//     - read 1 event and print out information from the tables
//     - will be the "official" QA macro
//======================================================================

class StChain;
StChain *chain;

class St_DataSet;
St_DataSet *Event;

void QA_bfcread_dst_tables(const char 
*MainFile="/afs/rhic/star/data/test/dev/tfs_Solaris/Mon/year_1b/psc0050_01_40evts.dst.root",
const char *fname="qa_tables.txt")

{
//
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StRootEvent");

  ofstream fout(fname);
  fout << "Tester: " << endl;
  fout << "Date tested: " << endl;
  fout << "Date DST created: " << endl;
  fout << "QA-> " << MainFile << endl << endl;
  fout << "QA-> table name";
  fout.width(18);
  fout << "# rows";
  fout.width(15);
  fout << "data looks" << endl;
  fout << "QA-> __________";
  fout.width(18);
  fout << "______";
  fout.width(15);
  fout << "__________" << endl << endl;


//  Setup top part of chain
  chain = new StChain("bfc");
  chain->SetDebug();
   
//  Input Tree
  StTreeMaker *treeMk = new StTreeMaker("treeRead",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();
  treeMk->SetBranch("*",0,"0");  		//deactivate all branches
  treeMk->SetBranch("dstBranch",0,"r");	//activate EventBranch
  
// --- now execute chain member functions
  chain->Init();
 
  St_DataSet *ds=0;
  St_Table *tabl=0;
  St_DataSet *obj=0;
  Int_t tabcntr=0;

// We will always just check 1 event!
  Int_t nevents=1;

// Loop over events
  for (int iev=0;iev<nevents; iev++)
    {
      chain->Clear();
      int iret = chain->Make();
      if (iret) break;

    ds=chain->GetDataSet("dst");
    St_DataSetIter tabiter(ds);

    tabcntr=0;

      if (ds) {

// ls() returns a virtual void, so don't have to set it = to anything
      ds->ls(2);

      cout << "QA-> Tester: " << endl;
      cout << "QA-> Date tested: " << endl;
      cout << "QA-> Date DST created: " << endl;
      cout << "QA-> " << MainFile << endl << endl;
      cout << "QA-> table name";
      cout.width(18);
      cout << "# rows";
      cout.width(15);
      cout << "data looks" << endl;
      cout << "QA-> ----------";
      cout.width(18);
      cout << "------";
      cout.width(15);
      cout << "----------" << endl << endl;

      while (obj = tabiter.Next()) {
//.. count all tables that exist:
        if (obj->InheritsFrom("St_Table")) {
          tabcntr++;
          //cout << "object  = " << obj->GetName() << endl;
          //cout << "tabcntr = " << tabcntr << endl;

          tabl = (St_Table *)tabiter.Find(obj->GetName());
          if (tabl) {
            cout << "QA-> " << obj->GetName();
            cout.width(28-strlen(obj->GetName()));
            cout << tabl->GetNRows() << endl;

            fout << "QA-> " << obj->GetName();
            fout.width(28-strlen(obj->GetName()));
            fout << tabl->GetNRows()<< endl;
          }
        }
      }

// ------------------------------------------------------------

      cout << endl << "QA-> total # tables = " << tabcntr << endl;
      fout << endl << "QA-> total # tables = " << tabcntr << endl;


      fout.close();
      chain->Finish();    
      }
    }
}
