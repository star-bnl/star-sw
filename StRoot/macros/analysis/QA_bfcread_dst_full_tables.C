// $Id: QA_bfcread_dst_full_tables.C,v 1.3 1999/07/13 00:42:31 kathy Exp $
// $Log: QA_bfcread_dst_full_tables.C,v $
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

//"/afs/rhic/star/data/test/dev/tfs_Solaris/Mon/year_1b/psc0050_01_40evts.dst.root",

// /disk00001/star/auau200/two_photon/starlight/twogam/year_1b/hadronic_on/tfs/rcf070_01_25000evts.dst.root

// /disk00001/star/auau200/two_photon/starlight/rho/year_1b/hadronic_on/tfs/rcf052_01_15000evts.dst.root

// /disk00001/star/augas100/venus412/hydrogen/b0_10/year_1b/hadronic_on/tfs/rcf062_01_1522evts.dst.root

void QA_bfcread_dst_full_tables(Int_t nevents=10, const char 
*MainFile="/afs/rhic/star/data/test/dev/tfs_Solaris/Thu/year_2a/psc0208_01_40evts.dst.root",
const char *fname="qa_full_tables.txt")

{
//
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StarClassLibrary");


  ofstream fout(fname);

  fout << "QA-> " << MainFile << endl << endl;
  cout << "QA-> " << MainFile << endl << endl;

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


   Int_t evcntr=0;
   Int_t dstcntr=0;

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


// Loop over events
  for (int iev=0;iev<nevents; iev++)
    {
      chain->Clear();
      int iret = chain->Make();
      if (iret) break;

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
            if (strcmp(obj->GetName(),"dst_TrgDet")==0) 
               cnt_dst_TrgDet++;
            if (strcmp(obj->GetName(),"monitor_soft")==0) 
               cnt_monitor_soft++;
            if (strcmp(obj->GetName(),"g2t_rch_hit")==0) 
               cnt_g2t_rch_hit++;

          }
        }
       }
      }
    }


      cout  << "QA-> # times Make called = " << evcntr << endl;
      fout  << "QA-> # times Make called = " << evcntr << endl;

      cout  << "QA-> # times found dst = " << dstcntr << endl << endl;
      fout  << "QA-> # times found dst = " << dstcntr << endl << endl;

      cout << "QA-> total # event_header tables = " << 
         cnt_event_header << endl;
      fout << "QA-> total # event_header tables = " << 
         cnt_event_header << endl;

      cout << "QA-> total # event_summary tables = " << 
         cnt_event_summary << endl;
      fout << "QA-> total # event_summary tables = " << 
         cnt_event_summary << endl;

      cout << "QA-> total # globtrk tables = " << 
         cnt_globtrk << endl;
      fout << "QA-> total # globtrk tables = " << 
         cnt_globtrk << endl;

      cout << "QA-> total # globtrk_aux tables = " << 
         cnt_globtrk_aux << endl;
      fout << "QA-> total # globtrk_aux tables = " << 
         cnt_globtrk_aux << endl;

      cout << "QA-> total # vertex tables = " << 
         cnt_vertex << endl;
      fout << "QA-> total # vertex tables = " << 
         cnt_vertex << endl;

      cout << "QA-> total # point tables = " << 
         cnt_point << endl;
      fout << "QA-> total # point tables = " << 
         cnt_point << endl;

      cout << "QA-> total # globtrk2 tables = " << 
         cnt_globtrk2 << endl;
      fout << "QA-> total # globtrk2 tables = " << 
	 cnt_globtrk2 << endl;


      cout << "QA-> total # primtrk tables = " << 
         cnt_primtrk << endl;
      fout << "QA-> total # primtrk tables = " << 
	 cnt_primtrk << endl;


      cout << "QA-> total # primtrk_aux tables = " << 
         cnt_primtrk_aux << endl;
      fout << "QA-> total # primtrk_aux tables = " << 
	 cnt_primtrk_aux << endl;


      cout << "QA-> total # dst_v0_vertex tables = " << 
         cnt_dst_v0_vertex << endl;
      fout << "QA-> total # dst_v0_vertex tables = " << 
	 cnt_dst_v0_vertex << endl;


      cout << "QA-> total # dst_xi_vertex tables = " << 
         cnt_dst_xi_vertex << endl;
      fout << "QA-> total # dst_xi_vertex tables = " << 
	 cnt_dst_xi_vertex << endl;


      cout << "QA-> total # dst_dedx tables = " << 
         cnt_dst_dedx << endl;
      fout << "QA-> total # dst_dedx tables = " << 
	 cnt_dst_dedx << endl;


      cout << "QA-> total # particle tables = " << 
         cnt_particle << endl;
      fout << "QA-> total # particle tables = " << 
	 cnt_particle << endl;


      cout << "QA-> total # dst_TrgDet tables = " << 
         cnt_dst_TrgDet << endl;
      fout << "QA-> total # dst_TrgDet tables = " << 
	 cnt_dst_TrgDet << endl;


      cout << "QA-> total # monitor_soft tables = " << 
         cnt_monitor_soft << endl;
      fout  << "QA-> total # monitor_soft tables = " << 
	 cnt_monitor_soft << endl;


      cout  << "QA-> total # g2t_rch_hit tables = " << 
         cnt_g2t_rch_hit << endl;
      fout  << "QA-> total # g2t_rch_hit tables = " << 
	 cnt_g2t_rch_hit << endl;


// ------------------------------------------------------------

      fout.close();
      chain->Finish();    
     
}





