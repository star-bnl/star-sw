// $Id: QA_bfcread_dst_tables.C,v 1.22 2000/04/13 20:25:46 kathy Exp $
// $Log: QA_bfcread_dst_tables.C,v $
// Revision 1.22  2000/04/13 20:25:46  kathy
// make prettier output
//
// Revision 1.21  2000/04/12 15:31:50  kathy
// keep libtpc table library in by default
//
// Revision 1.20  2000/04/12 15:29:05  kathy
// comment out libtpc by default
//
// Revision 1.19  2000/04/12 15:06:50  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.18  2000/03/15 22:38:21  kathy
// new version of macro that now keeps track of how many events there were and how many tables per event - doesn't keep track of missing tables anymore since autoQA does that
//
// Revision 1.17  2000/01/19 16:29:50  kathy
// update macros to use default input files in /afs/rhic/star/data/samples
//
// Revision 1.16  2000/01/05 22:12:03  kathy
// changed input file to current one
//
// Revision 1.15  1999/11/19 20:13:21  kathy
// cleaned up macros to remove uneccessary lines; also added info about new tables to QA* macros
//
// Revision 1.14  1999/11/10 17:06:58  kathy
// remove check on globtrk_aux and primtrk_aux tables for 99i and above since these tables are obsolete now - in QA_bfcread_dst_tables.C
//
// Revision 1.13  1999/11/10 14:57:57  kathy
// changed QA_bfcread_dst_tables.C to take into account new tables in 99i and above
//
// Revision 1.12  1999/11/03 19:05:02  kathy
// another small fix for output file name - now .out instead of .txt
//
// Revision 1.11  1999/11/03 17:12:58  kathy
// fixed macros so they use StIOMaker instead of StTreeMaker
//
// Revision 1.10  1999/08/12 16:28:36  kathy
// changed QA_bfcred_dst_tables so that it can loop over many events - before was hardwired to only 1 event
//
// Revision 1.9  1999/08/06 15:08:12  kathy
// removed for loop and put in goto checks - due to CINT problems
//
// Revision 1.8  1999/07/26 20:54:16  kathy
// changed output text to QAInfo: so that the QA sripts can tag on it; also cleaned up a bit and set to newer default input file
//
// Revision 1.7  1999/07/17 00:48:46  kathy
// change check on dst_TrgDet to test on TrgDet table
//
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


void QA_bfcread_dst_tables(
 Int_t nevents=500, 
 const char *MainFile=
   "/afs/rhic/star/data/samples/gstar.dst.root",
 const char *fname="qa_dst.out") 
{
  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");
  gSystem->Load("libtpc_Tables");

  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");

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

  
// --- now execute chain member functions
  chain->Init();

  St_DataSet *ds=0;
  St_Table *tabl=0;
  St_DataSet *obj=0;

  Float_t tottabcntr=0.;

  Int_t tabcntr=0;
  Int_t tabmiss=0;

  Int_t cnt_globtrk=0;
  Int_t cnt_globtrk2=0;
  Int_t cnt_primtrk=0;
  Int_t cnt_vertex=0;
  Int_t cnt_dst_v0_vertex=0;
  Int_t cnt_ev0_eval=0;
  Int_t cnt_dst_xi_vertex=0;
  Int_t cnt_kinkVertex=0;
  Int_t cnt_particle=0;
  Int_t cnt_g2t_rch_hit=0;
  Int_t cnt_TrgDet=0;
  Int_t cnt_l3Track=0;
  Int_t cnt_event_header=0;
  Int_t cnt_event_summary=0;
  Int_t cnt_point=0;
  Int_t cnt_dst_dedx=0;
  Int_t cnt_mon_soft_ftpc=0;
  Int_t cnt_mon_soft_glob=0;
  Int_t cnt_mon_soft_svt=0;
  Int_t cnt_mon_soft_tpc=0;
  Int_t cnt_mon_soft_ctb=0;
  Int_t cnt_mon_soft_emc=0;
  Int_t cnt_mon_soft_l3=0;
  Int_t cnt_mon_soft_rich=0;


  ofstream fout(fname);
  fout << "QAInfo: " << MainFile << endl << endl;
  fout << "QAInfo: table name";
  fout.width(18);
  fout << "# rows" << endl;
  fout << "QA->     __________";
  fout.width(18);
  fout << "______"<< endl << endl;


// Loop over events
  int iret=0,iev=0;
  EventLoop: if (iev<nevents && !iret) {         // goto loop code

   tabcntr=0;
   tabmiss=0;

   cnt_event_header=0;
   cnt_event_summary=0;
   cnt_globtrk=0;
   cnt_vertex=0;
   cnt_point=0;
   cnt_globtrk2=0;
   cnt_primtrk=0;
   cnt_dst_v0_vertex=0;
   cnt_dst_xi_vertex=0;
   cnt_dst_dedx=0;
   cnt_particle=0;
   cnt_TrgDet=0;
   cnt_mon_soft_ftpc=0;
   cnt_mon_soft_glob=0;
   cnt_mon_soft_svt=0;
   cnt_mon_soft_tpc=0;
   cnt_mon_soft_ctb=0;
   cnt_mon_soft_emc=0;
   cnt_mon_soft_l3=0;
   cnt_mon_soft_rich=0;
   cnt_g2t_rch_hit=0;
   cnt_l3Track=0;
   cnt_kinkVertex=0;
   cnt_ev0_eval=0;

    chain->Clear();
    iret = chain->Make();

    // cout << "   ...iret = " << iret << endl;
    if (!iret) {

    iev++;                                      // goto loop code

    cout  << "QAInfo: reading dst.root Ev# " << iev << endl;
    fout  << "QAInfo: reading dst.root Ev# " << iev << endl;

      ds=chain->GetDataSet("dst");
      St_DataSetIter tabiter(ds);

      tabcntr=0;
      tabmiss=0;

      if (ds) {

// ls() returns a virtual void, so don't have to set it = to anything
	//   ds->ls(2);

        while (obj = tabiter.Next()) {
//.. count all tables that exist:
          if (obj->InheritsFrom("St_Table")) {
            tabcntr++;

            //cout << "object  = " << obj->GetName() << endl;
            //cout << "tabcntr = " << tabcntr << endl;

            tabl = (St_Table *)tabiter.Find(obj->GetName());
            if (tabl) {
              cout << "QAInfo: " << obj->GetName();
              cout.width(28-strlen(obj->GetName()));
              cout << tabl->GetNRows() << endl;

              fout << "QAInfo: " << obj->GetName();
              fout.width(28-strlen(obj->GetName()));
              fout << tabl->GetNRows()<< endl;


            }
          }
        }
      }
// ------------------------------------------------------------

      cout <<  "QAInfo:  # tables in event = " << tabcntr << endl << endl;
      fout <<  "QAInfo:  # tables in event = " << tabcntr << endl << endl;

     tottabcntr += tabcntr;
    }
    goto EventLoop;
 }

    cout << endl << endl << "QAInfo:  total # events read  = " << iev << endl;
    fout << endl << endl << "QAInfo:  total # events read  = " << iev << endl;

      tottabcntr /= iev;

      cout <<  "QAInfo:  # tables per event   = " << tottabcntr << endl;
      fout <<  "QAInfo:  # tables per event   = " << tottabcntr << endl;


        fout.close();
        chain->Finish();   
}
