// $Id: QA_bfcread_dst_full_tables.C,v 1.10 1999/11/03 21:35:35 kathy Exp $
// $Log: QA_bfcread_dst_full_tables.C,v $
// Revision 1.10  1999/11/03 21:35:35  kathy
// small fixes for use of StIOMaker - had it wrong before
//
// Revision 1.9  1999/11/03 19:05:02  kathy
// another small fix for output file name - now .out instead of .txt
//
// Revision 1.8  1999/11/03 19:02:53  kathy
// changes to default input files and output file names - needed by perl script for testing
//
// Revision 1.7  1999/11/03 17:12:58  kathy
// fixed macros so they use StIOMaker instead of StTreeMaker
//
// Revision 1.6  1999/07/30 17:05:49  kathy
// changed to use goto in loop instead of for loop because of CINT problems
//
// Revision 1.5  1999/07/26 20:54:15  kathy
// changed output text to QAInfo: so that the QA sripts can tag on it; also cleaned up a bit and set to newer default input file
//
// Revision 1.4  1999/07/17 00:48:45  kathy
// change check on dst_TrgDet to test on TrgDet table
//
// Revision 1.3  1999/07/13 00:42:31  kathy
// updated all default input files, removed unneccessary macros, renamed other to make more standard
//
// Revision 1.2  1999/07/13 00:29:52  kathy
// updated macros to take out StRootEvent
//
// Revision 1.1  1999/07/01 19:01:53  kathy
// new QA macro to read dst, loop over all events, count # times each DST table is there
//
//
//======================================================================
// owner: Kathy Turner
// 
// what it does: see below 
//=======================================================================
// QA_bfcread_dst_full_tables.C
//
// Kathy's notes (7/1/99):
//     - read dstbranch from *.dst.root file
//     - reads all events and count # times each table is on DST
//       (default # events to check = 10 - change at input line)
//     - will be an "official" QA macro
//======================================================================

class StChain;
StChain *chain;

class St_DataSet;
St_DataSet *Event;


void QA_bfcread_dst_full_tables(
 Int_t nevents=15, 
 const char *MainFile=
 "/disk00000/star/test/new/tfs_Linux/year_2a/psc0208_01_40evts.dst.root",
 const char *fname="qa_full_tables.out")
{
//
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");

  ofstream fout(fname);

//  Setup top part of chain
  chain = new StChain("bfc");
  chain->SetDebug();

   
// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
//  IOMk->SetBranch("tpc_tracks",0,"r"); //activate tpc_tracks Branch
//  IOMk->SetBranch("geantBranch",0,"r"); //activate geant Branch
  IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch

  
  fout << "QAInfo: " << MainFile << endl << endl;
  cout << "QAInfo: " << MainFile << endl << endl;


// --- now execute chain member functions
  chain->Init();
 
  St_DataSet *ds=0;
  St_Table *tabl=0;
  St_DataSet *obj=0;
  Int_t tabcntr=0;

  Int_t evcntr=0;
  Int_t dstcntr=0;
  Int_t mkcntr=0;

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
  Int_t cnt_TrgDet=0;
  Int_t cnt_monitor_soft=0;
  Int_t cnt_g2t_rch_hit=0;
  Int_t cnt_kinkVertex=0;
  Int_t cnt_ev0_eval=0;
  Int_t cnt_l3Track=0;

// Loop over events
  int iret=0,iev=0;
//for (iev=0;iev<nevents; iev++) {     // for loop code
 EventLoop: if (iev<nevents && !iret) {      // goto loop code
   chain->Clear();
   iret = chain->Make();
   iev++;                                   // goto loop code
   cout << "   ...iret = " << iret << endl;
   //if (iret != 0) break;                  // for loop code
   if (!iret) {                             // goto loop code
     evcntr++;
     ds=chain->GetDataSet("dst");
     St_DataSetIter tabiter(ds);
    
     tabcntr=0;
     if (ds) {
       dstcntr++;

       while (obj = tabiter.Next()) {
//.. count all tables that exist:
         if (obj->InheritsFrom("St_Table")) {
           tabcntr++;
           tabl = (St_Table *)tabiter.Find(obj->GetName());
         
           if (tabl) {
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
             if (strcmp(obj->GetName(),"TrgDet")==0) 
               cnt_TrgDet++;
             if (strcmp(obj->GetName(),"monitor_soft")==0) 
               cnt_monitor_soft++;
             if (strcmp(obj->GetName(),"g2t_rch_hit")==0) 
               cnt_g2t_rch_hit++;
             if (strcmp(obj->GetName(),"kinkVertex")==0) 
               cnt_kinkVertex++;
             if (strcmp(obj->GetName(),"ev0_eval")==0) 
               cnt_ev0_eval++;
             if (strcmp(obj->GetName(),"l3Track")==0) 
               cnt_l3Track++;
           }
         }
       }
     }
   }
   goto EventLoop;                        // goto loop code
 }

  cout << "iret = " << iret << endl;
  fout << "iret = " << iret << endl;

  cout  << "QAInfo: # times Make called = " << iev << endl;
  fout  << "QAInfo: # times Make called = " << iev << endl;

  cout  << "# events found = " << evcntr << endl;
  fout  << "# events found = " << evcntr << endl;

  cout  << "QAInfo: # times found dst = " << dstcntr << endl << endl;
  fout  << "QAInfo: # times found dst = " << dstcntr << endl << endl;

  cout << "QAInfo: total # event_header tables = " << cnt_event_header << endl;
  fout << "QAInfo: total # event_header tables = " << cnt_event_header << endl;
 
  cout << "QAInfo: total # event_summary tables = "<<cnt_event_summary << endl;
  fout << "QAInfo: total # event_summary tables = "<<cnt_event_summary << endl;

  cout << "QAInfo: total # globtrk tables = " << cnt_globtrk << endl;
  fout << "QAInfo: total # globtrk tables = " << cnt_globtrk << endl;

  cout << "QAInfo: total # globtrk_aux tables = " << cnt_globtrk_aux << endl;
  fout << "QAInfo: total # globtrk_aux tables = " << cnt_globtrk_aux << endl;

  cout << "QAInfo: total # vertex tables = " << cnt_vertex << endl;
  fout << "QAInfo: total # vertex tables = " << cnt_vertex << endl;

  cout << "QAInfo: total # point tables = " << cnt_point << endl;
  fout << "QAInfo: total # point tables = " << cnt_point << endl;

  cout << "QAInfo: total # globtrk2 tables = " << cnt_globtrk2 << endl;
  fout << "QAInfo: total # globtrk2 tables = " << cnt_globtrk2 << endl;

  cout << "QAInfo: total # primtrk tables = " << cnt_primtrk << endl;
  fout << "QAInfo: total # primtrk tables = " << cnt_primtrk << endl;

  cout << "QAInfo: total # primtrk_aux tables = " << cnt_primtrk_aux << endl;
  fout << "QAInfo: total # primtrk_aux tables = " << cnt_primtrk_aux << endl;

  cout << "QAInfo: total # dst_v0_vertex tables = "<<cnt_dst_v0_vertex << endl;
  fout << "QAInfo: total # dst_v0_vertex tables = "<<cnt_dst_v0_vertex << endl;

  cout << "QAInfo: total # ev0_eval tables = " << cnt_ev0_eval << endl;
  fout << "QAInfo: total # ev0_eval tables = " << cnt_ev0_eval << endl;

  cout << "QAInfo: total # dst_xi_vertex tables = "<<cnt_dst_xi_vertex << endl;
  fout << "QAInfo: total # dst_xi_vertex tables = "<<cnt_dst_xi_vertex << endl;

  cout << "QAInfo: total # kinkVertex tables = " << cnt_kinkVertex << endl;
  fout << "QAInfo: total # kinkVertex tables = " << cnt_kinkVertex << endl;

  cout << "QAInfo: total # dst_dedx tables = " << cnt_dst_dedx << endl;
  fout << "QAInfo: total # dst_dedx tables = " << cnt_dst_dedx << endl;

  cout << "QAInfo: total # particle tables = " << cnt_particle << endl;
  fout << "QAInfo: total # particle tables = " << cnt_particle << endl;

  cout << "QAInfo: total # TrgDet tables = " << cnt_TrgDet << endl;
  fout << "QAInfo: total # TrgDet tables = " << cnt_TrgDet << endl;

  cout << "QAInfo: total # monitor_soft tables = " << cnt_monitor_soft << endl;
  fout << "QAInfo: total # monitor_soft tables = " << cnt_monitor_soft << endl;

  cout  << "QAInfo: total # g2t_rch_hit tables = " << cnt_g2t_rch_hit << endl;
  fout  << "QAInfo: total # g2t_rch_hit tables = " << cnt_g2t_rch_hit << endl;

  cout << "QAInfo: total # l3Track tables = " << cnt_l3Track << endl;
  fout << "QAInfo: total # l3Track tables = " << cnt_l3Track << endl;

// ------------------------------------------------------------

  fout.close();
  chain->Finish();    
     
}
