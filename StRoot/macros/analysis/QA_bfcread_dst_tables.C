// $Id: QA_bfcread_dst_tables.C,v 1.1 1999/06/25 19:43:47 kathy Exp $
// $Log: QA_bfcread_dst_tables.C,v $
// Revision 1.1  1999/06/25 19:43:47  kathy
// new macros for official QA use
//
//
//======================================================================
// owner: Kathy Turner
// revised by: Curtis Lansdell
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

void QA_bfcread_dst_tables(const char *MainFile=
"/disk00000/star/test/new/tfs_Solaris/year_1b/psc0050_01_40evts.dst.root",
const char *fname="QA_test.txt")

{
//
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StRootEvent");


  ofstream fout(fname);
  fout << "QA-> " << MainFile << endl;
  fout << "  " << endl;
  cout << "QA-> " << MainFile << endl;
  cout << "  " << endl;

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

// We will always just check 1 event!
  Int_t nevents=1;
  for (int iev=0;iev<nevents; iev++)
    {
      chain->Clear();
      int iret = chain->Make();
      if (iret) break;


    St_DataSet *ds=chain->GetDataSet("dst");
    St_DataSetIter tabiter(ds);
    St_Table  *tabl=0;

      if (ds) {

// ls() returns a virtual void, so don't have to set it = to anything
      ds->ls(2);
 
//.. go to event_header

        tabl = (St_Table *)tabiter.Find("event_header");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to event_summary

        tabl = (St_Table *)tabiter.Find("event_summary");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to globtrk

        tabl = (St_Table *)tabiter.Find("globtrk");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to globtrk_aux

        tabl = (St_Table *)tabiter.Find("globtrk_aux");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to vertex

        tabl = (St_Table *)tabiter.Find("vertex");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to globtrk2

        tabl = (St_Table *)tabiter.Find("globtrk2");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
 
        }

//.. go to primtrk

        tabl = (St_Table *)tabiter.Find("primtrk");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to primtrk_aux

        tabl = (St_Table *)tabiter.Find("primtrk_aux");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to dst_v0_vertex

        tabl = (St_Table *)tabiter.Find("dst_v0_vertex");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to dst_xi_vertex

        tabl = (St_Table *)tabiter.Find("dst_xi_vertex");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to dst_dedx

        tabl = (St_Table *)tabiter.Find("dst_dedx");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to point

        tabl = (St_Table *)tabiter.Find("point");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to monitor_soft

        tabl = (St_Table *)tabiter.Find("monitor_soft");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to particle

        tabl = (St_Table *)tabiter.Find("particle");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

//.. go to g2t_rch_hit

        tabl = (St_Table *)tabiter.Find("g2t_rch_hit");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
        }


//.. go to dst_TriggerDetectors        

       tabl = (St_Table *)tabiter.Find("dst_TriggerDetectors");

        if (tabl) {
          numtables++;
          cout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;
          fout << " QA-> Table: " << tabl->GetName() << ",  #rows: " << tabl->GetNRows()<< endl;

        }

      }


// ------------------------------------------------------------

      cout << " " << endl;
      cout << " QA-> total # tables = " << numtables << endl;

      fout << " " << endl;
      fout << " QA-> total # tables = " << numtables << endl;

      fout.close();
      chain->Finish();    
  
    }
}
 


        

