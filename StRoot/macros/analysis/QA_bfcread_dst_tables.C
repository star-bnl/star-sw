// $Id: QA_bfcread_dst_tables.C,v 1.6 1999/07/13 00:42:32 kathy Exp $
// $Log: QA_bfcread_dst_tables.C,v $
// Revision 1.6  1999/07/13 00:42:32  kathy
// updated all default input files, removed unneccessary macros, renamed other to make more standard
//
// Revision 1.5  1999/07/13 00:29:53  kathy
// updated macros to take out StRootEvent
//
// Revision 1.4  1999/07/07 14:35:47  kathy
// add code to check which tables are missing and print out this info
//
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
*MainFile="/afs/rhic/star/data/test/dev/tfs_Solaris/Thu/year_2a/psc0208_01_40evts.dst.root",
const char *fname="qa_tables.txt")

{
//
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StarClassLibrary");

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
  Int_t tabmiss=0;


   Int_t cnt_event_header=0;
   Int_t cnt_event_summary=0;
   Int_t cnt_globtrk=0;
   Int_t cnt_globtrk_aux=0;
   Int_t cnt_vertex=0;
   Int_t cnt_point=0;
   Int_t cnt_globtrk2=0;
   Int_t cnt_primtrk=0;
   Int_t cnt_primtrk_aux=0;
   Int_t cnt_dst_v0_vertex=0;
   Int_t cnt_dst_xi_vertex=0;
   Int_t cnt_dst_dedx=0;
   Int_t cnt_particle=0;
   Int_t cnt_dst_TrgDet=0;
   Int_t cnt_monitor_soft=0;
   Int_t cnt_g2t_rch_hit=0;

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
    tabmiss=0;

      if (ds) {

// ls() returns a virtual void, so don't have to set it = to anything
      ds->ls(2);


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

            if (strcmp(obj->GetName(),"event_header")==0) 
               cnt_event_header++;
            if (strcmp(obj->GetName(),"event_summary")==0) 
               cnt_event_summary++;
            if (strcmp(obj->GetName(),"globtrk")==0) 
               cnt_globtrk++;
            if (strcmp(obj->GetName(),"globtrk_aux")==0) 
               cnt_globtrk_aux++;
            if (strcmp(obj->GetName(),"vertex")==0) 
               cnt_vertex++;
            if (strcmp(obj->GetName(),"point")==0) 
               cnt_point++;
            if (strcmp(obj->GetName(),"globtrk2")==0) 
               cnt_globtrk2++;
            if (strcmp(obj->GetName(),"primtrk")==0) 
               cnt_primtrk++;
            if (strcmp(obj->GetName(),"primtrk_aux")==0) 
               cnt_primtrk_aux++;
            if (strcmp(obj->GetName(),"dst_v0_vertex")==0) 
               cnt_dst_v0_vertex++;
            if (strcmp(obj->GetName(),"dst_xi_vertex")==0) 
               cnt_dst_xi_vertex++;
            if (strcmp(obj->GetName(),"dst_dedx")==0) 
               cnt_dst_dedx++;
            if (strcmp(obj->GetName(),"particle")==0) 
               cnt_particle++;
            if (strcmp(obj->GetName(),"dst_TrgDet")==0) 
               cnt_dst_TrgDet++;
            if (strcmp(obj->GetName(),"monitor_soft")==0) 
               cnt_monitor_soft++;
            if (strcmp(obj->GetName(),"g2t_rch_hit")==0) 
               cnt_g2t_rch_hit++;

          }
        }
      }

// ------------------------------------------------------------


      cout << endl << "QA-> total # tables (expect 16) = " << tabcntr << endl;
      fout << endl << "QA-> total # tables (expect 16) = " << tabcntr << endl;


      if (cnt_event_header == 0){
        cout << endl << "QA-> missing table: " << "event_header" << endl;
        fout << endl << "QA-> missing table: " << "event_header" << endl;
        tabmiss++;
      } 
      if (cnt_event_summary == 0){
        cout << endl << "QA-> missing table: " << "event_summary" << endl;
        fout << endl << "QA-> missing table: " << "event_summary" << endl;
        tabmiss++;
      } 
      if (cnt_globtrk == 0){
        cout << endl << "QA-> Missing Table: " << "globtrk" << endl;
        fout << endl << "QA-> Missing Table: " << "globtrk" << endl;
        tabmiss++;
      } 
      if (cnt_globtrk_aux == 0){
        cout << endl << "QA-> Missing Table: " << "globtrk_aux" << endl;
        fout << endl << "QA-> Missing Table: " << "globtrk_aux" << endl;
        tabmiss++;
      } 
      if (cnt_vertex == 0){
        cout << endl << "QA-> Missing Table: " << "vertex" << endl;
        fout << endl << "QA-> Missing Table: " << "vertex" << endl;
        tabmiss++;
      } 
      if (cnt_point == 0){
        cout << endl << "QA-> Missing Table: " << "point" << endl;
        fout << endl << "QA-> Missing Table: " << "point" << endl;
        tabmiss++;
      } 
      if (cnt_globtrk2 == 0){
        cout << endl << "QA-> Missing Table: " << "globtrk2" << endl;
        fout << endl << "QA-> Missing Table: " << "globtrk2" << endl;
        tabmiss++;
      } 
      if (cnt_primtrk == 0){
        cout << endl << "QA-> Missing Table: " << "primtrk" << endl;
        fout << endl << "QA-> Missing Table: " << "primtrk" << endl;
        tabmiss++;
      } 
      if (cnt_primtrk_aux == 0){
        cout << endl << "QA-> Missing Table: " << "primtrk_aux" << endl;
        fout << endl << "QA-> Missing Table: " << "primtrk_aux" << endl;
        tabmiss++;
      } 
      if (cnt_dst_v0_vertex == 0){
        cout << endl << "QA-> Missing Table: " << "dst_v0_vertex" << endl;
        fout << endl << "QA-> Missing Table: " << "dst_v0_vertex" << endl;
        tabmiss++;
      } 
      if (cnt_dst_xi_vertex == 0){
        cout << endl << "QA-> Missing Table: " << "dst_xi_vertex" << endl;
        fout << endl << "QA-> Missing Table: " << "dst_xi_vertex" << endl;
        tabmiss++;
      } 
      if (cnt_dst_dedx == 0){
        cout << endl << "QA-> Missing Table: " << "dst_dedx" << endl;
        fout << endl << "QA-> Missing Table: " << "dst_dedx" << endl;
        tabmiss++;
      } 
      if (cnt_particle == 0){
        cout << endl << "QA-> Missing Table: " << "particle" << endl;
        fout << endl << "QA-> Missing Table: " << "particle" << endl;
        tabmiss++;
      } 
      if (cnt_dst_TrgDet == 0){
        cout << endl << "QA-> Missing Table: " << "dst_TrgDet" << endl;
        fout << endl << "QA-> Missing Table: " << "dst_TrgDet" << endl;
        tabmiss++;
      } 
      if (cnt_monitor_soft == 0){
        cout << endl << "QA-> Missing Table: " << "monitor_soft" << endl;
        fout << endl << "QA-> Missing Table: " << "monitor_soft" << endl;
        tabmiss++;
      } 
      if (cnt_g2t_rch_hit == 0){
        cout << endl << "QA-> Missing Table: " << "g2t_rch_hit" << endl;
        fout << endl << "QA-> Missing Table: " << "g2t_rch_hit" << endl;
        tabmiss++;
      } 


      cout << endl << "QA-> # tables missing = " << tabmiss << endl;
      fout << endl << "QA-> # tables missing = " << tabmiss  << endl;


      fout.close();
      chain->Finish();    
      }
    }
}
