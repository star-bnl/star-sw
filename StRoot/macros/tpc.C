// macro to read fz files, xdf files and minidaq data 
// (can use all detectors not just tpc)
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//#define GTRACK
#define FZIN
//#define MINIDAQ
#ifdef MINIDAQ
#define CTEST
#define StMagF
#endif   
//#define CTEST
#define TPC
#define TRS
//#define TSS
//#define FTPC
//#define FSS
//#define SVT
//#define EMC
//#define CTF
#ifndef GTRACK
//#define StMagF
#endif
//#define XDFOUT
//#define GLOBAL

TBrowser *b = 0;
class StChain;
StChain  *chain=0;
class St_geant_Maker;
St_geant_Maker *geant;

void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
    gSystem->Load("libmsg");
    gSystem->Load("libtls");
    gSystem->Load("St_db_Maker");
    gSystem->Load("St_xdfin_Maker");
#ifndef MINIDAQ
    gSystem->Load("St_g2r"); 
    gSystem->Load("St_geant_Maker");
#endif
#ifndef StMagF
    gSystem->Load("geometry");
#else
    gSystem->Load("StMagF");
#endif
#ifdef TPC
    gSystem->Load("St_tpc");
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");
#ifdef TRS
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTrsMaker"); 
    gSystem->Load("St_tpcdaq_Maker");
#else
#ifdef  TSS
    gSystem->Load("St_tss_Maker");
#endif
#endif
#endif
#ifdef FTPC
    gSystem->Load("St_ftpc");
#ifdef  FSS
    gSystem->Load("St_fss_Maker");
#endif
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fpt_Maker");
#endif
#ifdef EMC
    gSystem->Load("St_emc");
    gSystem->Load("St_ems_Maker");
    gSystem->Load("St_emc_Maker");
#endif
#ifdef CTF
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("St_mwc");
    gSystem->Load("St_mwc_Maker");
    gSystem->Load("St_trg");
    gSystem->Load("St_trg_Maker");
    gSystem->Load("St_l3");
    gSystem->Load("St_l3t_Maker");
#endif
    //    gSystem->Load("StRchMaker");
    gSystem->Load("St_svt");
#ifdef SVT
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
#endif
#ifdef MINIDAQ
    gSystem->Load("StMinidaqMaker");
#endif
#ifdef GLOBAL
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    //    gSystem->Load("St_run_summary_Maker");
    gSystem->Load("St_QA_Maker");
    gSystem->Load("StTreeMaker");
#endif
}

void tpc (const Int_t Nevents=1,
	  const Char_t *infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf", 
	  TString* FileOut=0)

{
  // redefine input file
#ifndef MINIDAQ
#ifndef FZIN
	  // xdf file
	  infile ="/afs/rhic/star/data/samples/hijet-g2t.xdf";
#endif
#endif
#ifdef MINIDAQ
	  //minidaq file
	  infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf";
#endif
#ifdef FZIN
	  // zebra file
	  infile ="/afs/rhic/star/tpc/data/trs_muon_10cmdrift_good.fzd";
#endif

  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
#ifndef GTRACK
  //set I/O for crs
  TString InFile = "$input_file";
  gSystem->ExpandPathName(InFile);
  Int_t NoEvents = Nevents;
  if (!strcmp("$input_file",InFile.Data())) {
    InFile = infile; 
  }
  else {
    Int_t NoEvents = atoi(strrchr(InFile.Data(),'_')+1);
    if (NoEvents <=0) {NoEvents = Nevents;}  
    if (NoEvents > Nevents) {NoEvents = Nevents;}  
    printf("Input file name = %s with No.Events to process = %i \n",InFile.Data(),NoEvents);
  }
  if (!FileOut){
    FileOut = new TString(gSystem->BaseName(InFile.Data()));
    FileOut->ReplaceAll(".fzd",".root");
    FileOut->ReplaceAll(".fz",".root");
    FileOut->ReplaceAll(".xdf",".root");
    FileOut->Strip();
  }
#else
  if (!FileOut) FileOut = new TString("gtrack.root");
  Int_t NoEvents = Nevents;
#endif
  printf("File for chain %s\n",FileOut.Data());

#ifdef XDFOUT
  St_XDFFile  *xdf_out = 0;
  TString *XdfFile = new TString(FileOut->Data());
  XdfFile->ReplaceAll(".root","_dst.xdf");
  xdf_out = new St_XDFFile(XdfFile->Data(),"wb"); 
  printf ("Open xdf file  = %s \n +++++++++++++++++++++++++++++++++++++++++++++++\n",XdfFile->Data());
#endif

  // Create the main chain object
  if (chain) delete chain;
  chain = new StChain("bfc");
  chain->SetDebug();
  chain->SetInput("EvtHddr",".make/geant/.const/EvtHddr");    

#ifdef MINIDAQ
  // defined for MINIDAQ
  chain->SetInput("TPC_DATA",".make/xdfin/.data/TPC_DATA");
  chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
#endif

#ifndef FZIN
#ifndef GTRACK
  // fix for xdf files to get geant input
  chain->SetInput("geant",".make/xdfin/.data/event/geant/Event");
#endif
#endif


#ifndef MINIDAQ
  geant = new St_geant_Maker("geant");
  geant->SetNwGEANT(10 000 000);
#endif

  //  Create the makers to be called by the current chain
  const char *mainDB = "$STAR/StDb";
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  chain->SetInput("params","db:StDb/params");
  dbMk->SetDebug();  
#ifdef CTEST
  // TPC test Data Base
  cout<<"creating DB for TPC test"<<endl;
  const char *tpcDB = "/afs/rhic/star/tpc/ctest/StDb";
  St_db_Maker *dbMktpc = new St_db_Maker("tpcdb",tpcDB);
  dbMktpc->SetDebug();
#else
  // TPC Data Base
  cout<<"creating DB for TPC"<<endl;
  const char *tpcDB = "/afs/rhic/star/tpc/base/StDb";
  St_db_Maker *dbMktpc = new St_db_Maker("tpcdb",tpcDB);
  dbMktpc->SetDebug();
#endif


  const char *calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  chain->SetInput("calib","calib:calib");
  calibMk->SetDebug();  

  // open input file 
  // if GTRACK is defined random events are generated
  // if FZIN is defined a fz file is opened
  // otherwise a xdf file is opened
#ifndef GTRACK
#ifdef FZIN
  geant->SetInputFile(InFile.Data());
#else
  xdfin = new St_xdfin_Maker("xdfin",InFile.Data());
#endif
#endif

#ifdef MINIDAQ
  StMagF         *field   = new StMagFC("field","STAR no field",0.00002);
  StMinidaqMaker *tpc_raw = new StMinidaqMaker("tpc_raw");
  cout<<"initializing input for the tpc DB"<<endl;
  tpc_raw->SetInput("params","tpcdb:StDb/params"); 
#endif

#ifdef GTRACK
  //  geant->SetIwtype(1);
  //  geant->SetDebug();
  geant->LoadGeometry("detp geometry YEAR_1B");
  geant->Do("subevent 0;");
  // gkine #particles partid ptrange yrange phirange vertexrange 
  geant->Do("gkine 10 6 1. 1. .5 1. 0 6.28  0. 0.;");
  geant->Do("mode g2tm prin 1;");
  //  geant->Do("next;");
  //  geant->Do("dcut cave z 1 10 10 0.03 0.03;");
  //  geant->Do("debug on;");
  geant->Do("swit 2 3;");
  // geant->LoadGeometry("detp geometry field_only field_off");
#endif

  chain->SetInput("geom","geant:geom");

#ifdef FSS
//		fss
  St_fss_Maker *fssMk 	= new St_fss_Maker("ftpc_raw");
  fssMk->SetDebug();
#endif  

#ifdef TRS
//		trs
  StTrsMaker   *trs = new StTrsMaker;
  cout<<"initializing input for the trs DB"<<endl;
  trs->SetInput("params","tpcdb:StDb/params");

  St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker;
#else
#ifdef TSS
//		tss
  St_tss_Maker *tssMk 	= new St_tss_Maker("tpc_raw");
  cout<<"initializing input for the tpc DB"<<endl;
  tssMk->SetInput("params","tpcdb:StDb/params"); 
  //  tssMk->SetDebug();
#endif
#endif

#ifdef EMC
//		emc
  St_ems_Maker  *emsMk = new St_ems_Maker("emc_raw" );
  emsMk->SetDebug();
  St_emc_Maker  *emcMk = new St_emc_Maker("emc_hits");
  emcMk->SetDebug();
#endif

#ifdef TPC
//		tcl
  St_tcl_Maker *tclMk = new St_tcl_Maker("tpc_hits");
  cout<<"initializing input for the tpc DB"<<endl;
  tclMk->SetInput("params","tpcdb:StDb/params");  
  tclMk->SetDebug();
  tclMk->SetDebug();
  tclMk->tclPixTransOn();
#ifndef MINIDAQ
  //Turn on the hit finder evaluation
  tclMk->tclEvalOn();
#endif  
#endif

#ifdef SVT
//		svt
  St_srs_Maker *srsMk 	= new St_srs_Maker("svt_hits");
  srsMk->SetDebug();  
#endif

#ifdef FTPC
//		fcl

  St_fcl_Maker *fclMk 	= new St_fcl_Maker("ftpc_hits");  
  fclMk->SetDebug();
#endif

#ifdef TPC
//		tpt
  St_tpt_Maker *tptMk 	= new St_tpt_Maker("tpc_tracks");
#ifdef MINIDAQ
 // Turn on the final ntuple.
  tptMk->Set_final(kTRUE);
#else
  //Turn on the tpc evaluation
  tptMk->tteEvalOn();
#endif
  tptMk->SetDebug();
#endif

#ifdef CTF
//		l3t
//  St_l3t_Maker  *l3tMk  = new St_l3t_Maker("l3Tracks");
//  l3tMk->SetDebug();
#endif

#ifdef SVT
//		stk
  St_stk_Maker *stkMk 	= new St_stk_Maker("svt_tracks");
  stkMk->SetDebug();
#endif

#ifdef FTPC
//		fpt
  St_fpt_Maker *fptMk 	= new St_fpt_Maker("ftpc_tracks");
  fptMk->SetDebug();
#endif

#ifdef GLOBAL
//		global
  St_glb_Maker *glbMk = new St_glb_Maker("global");
  glbMk->SetDebug();
//		dst
  St_dst_Maker *dstMk = new St_dst_Maker("dst");
  dstMk->SetDebug();
  St_QA_Maker          *qa         = new St_QA_Maker;  

//		Tree
  StTreeMaker *treeMk = new StTreeMaker("tree",FileOut.Data());
  treeMk->SetIOMode("w");
  treeMk->SetDebug();
  treeMk->SetInput("dst","dst");
  //  treeMk->SetInput("global","global");
  //treeMk->SetInput(".default","Others");
#endif  
  
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's involved
  //   chain->MakeDoc();

  chain->PrintInfo();

  // Init the chain and all its makers
  //  chain->SetDebug(1);
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");

  Int_t i=0;
  for (Int_t i =1; i <= NoEvents; i++){
    if (chain->Make(i)==2) break;
    //    gSystem->Exec("ps ux");
    if (i != NoEvents) chain->Clear();
    printf ("=========================================== Done with Event no. %d\n",i);
  }

  if (NoEvents > 1) {
    //    chain->Finish();
    printf ("Run completed ");
    gSystem->Exec("date");
  }
}

