// $Id: QA_bfcread_dst_tables.C,v 1.2 1999/06/28 16:53:20 kathy Exp $
// $Log: QA_bfcread_dst_tables.C,v $
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
*MainFile="/afs/rhic/star/data/test/dev/tfs_Linux/Thu/year_2a/psc0208_01_40evts.dst.root",
const char *fname="QA_dst.txt")

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
 
  Int_t numtables=0;
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

    numtables=0;
    tabcntr=0;

      if (ds) {

// ls() returns a virtual void, so don't have to set it = to anything
      ds->ls(2);

      cout << "QA-> Tester: " << endl;
      cout << "QA-> Date tested: " << endl << endl;
      cout << "QA-> DST file: " << MainFile << endl;
      cout << "QA-> Date DST created: " << endl << endl;
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
//	       cout << "object  = " << obj->GetName() << endl;
//             cout << "tabcntr = " << tabcntr << endl;
            }
      }

//.. go to event_header

        tabl = (St_Table *)tabiter.Find("event_header");
        

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(16);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(16);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to event_summary

        tabl = (St_Table *)tabiter.Find("event_summary");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(15);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(15);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to globtrk

        tabl = (St_Table *)tabiter.Find("globtrk");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(21);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(21);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to globtrk_aux

        tabl = (St_Table *)tabiter.Find("globtrk_aux");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(17);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(17);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to vertex

        tabl = (St_Table *)tabiter.Find("vertex");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(22);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(22);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to globtrk2

        tabl = (St_Table *)tabiter.Find("globtrk2");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(20);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(20);
          fout << tabl->GetNRows()<< endl;
 
        }

//.. go to primtrk

        tabl = (St_Table *)tabiter.Find("primtrk");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(21);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(21);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to primtrk_aux

        tabl = (St_Table *)tabiter.Find("primtrk_aux");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(17);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(17);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to dst_v0_vertex

        tabl = (St_Table *)tabiter.Find("dst_v0_vertex");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(15);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(15);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to dst_xi_vertex

        tabl = (St_Table *)tabiter.Find("dst_xi_vertex");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(15);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(15);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to dst_dedx

        tabl = (St_Table *)tabiter.Find("dst_dedx");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(20);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(20);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to point

        tabl = (St_Table *)tabiter.Find("point");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(23);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(23);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to monitor_soft

        tabl = (St_Table *)tabiter.Find("monitor_soft");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(16);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(16);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to particle

        tabl = (St_Table *)tabiter.Find("particle");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(20);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(20);
          fout << tabl->GetNRows()<< endl;

        }

//.. go to g2t_rch_hit

        tabl = (St_Table *)tabiter.Find("g2t_rch_hit");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(17);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(17);
          fout << tabl->GetNRows()<< endl;
        }


//.. go to dst_TriggerDetectors        

       tabl = (St_Table *)tabiter.Find("dst_TriggerDetectors");

        if (tabl) {
          numtables++;
          cout << "QA-> " << tabl->GetName();
          cout.width(8);
          cout << tabl->GetNRows()<< endl;
          fout << "QA-> " << tabl->GetName();
          fout.width(8);
          fout << tabl->GetNRows()<< endl;

        }
      }

// ------------------------------------------------------------

      cout << endl <<"QA-> total # tables = " << tabcntr << endl;
      fout << endl << "QA-> total # tables = " << tabcntr << endl;

      cout << "QA-> total # tables checked = " << numtables << endl;
      fout << "QA-> total # tables checked = " << numtables << endl;

      fout.close();
      chain->Finish();    
    }
}
