// $Id: bfcz.C,v 1.11 1999/04/01 23:39:46 fisyak Exp $
#define GTRACK
#define GEANT
//#define TRS
//#define TSS
#define FTPC
//#define FSS
#define SVT
//#define EMC
#define CTF
#ifndef GTRACK
//#define StMagF
#endif
//#define XDF
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
#ifndef StMagF
    gSystem->Load("geometry");
#else
    gSystem->Load("StMagF");
#endif
#ifdef GEANT
    gSystem->Load("St_g2r"); 
    gSystem->Load("St_geant_Maker");
    gSystem->Load("St_tpc");
#endif
#ifdef TRS
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTrsMaker"); 
    gSystem->Load("St_tpcdaq_Maker");
#else
#ifdef  TSS
    gSystem->Load("St_tss_Maker");
#endif
#endif
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");
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
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    //    gSystem->Load("St_run_summary_Maker");
    gSystem->Load("St_QA_Maker");
    gSystem->Load("StTreeMaker");
}

void bfcz (const Int_t Nevents=1000000,
     const Char_t *fzfile ="/disk1/star/test/psc0049_08_40evts.fzd",
     TString* FileOut=0)
{                              
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
#ifndef GTRACK
  //set I/O for crs
  TString InFile = "$input_file";
  gSystem->ExpandPathName(InFile);
  Int_t NoEvents = Nevents;
  if (!strcmp("$input_file",InFile.Data())) {
    InFile = fzfile; 
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
    FileOut->Strip();
  }
#else
  if (!FileOut) FileOut = new TString("gtrack.root");
  Int_t NoEvents = Nevents;
#endif
  printf("File for chain %s\n",FileOut.Data());
#ifdef XDF
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

  //  Create the makers to be called by the current chain
  const char *mainDB = "$STAR/StDb";
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  chain->SetInput("params","db:StDb/params");
  dbMk->SetDebug();  
  const char *calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  chain->SetInput("calib","calib:calib");
  calibMk->SetDebug();  

  geant = new St_geant_Maker("geant");
  geant->SetNwGEANT(10 000 000);
#ifdef GTRACK
  geant->SetIwtype(1);
  geant->SetDebug();
  geant->LoadGeometry("detp geometry YEAR_1B");
  geant->Do("subevent 0;");
  geant->Do("gkine 25 6 1. 1. -1. 1. 0 6.28  -1. 1.;");
  geant->Do("mode g2tm prin 1;");
  //  geant->Do("next;");
  //  geant->Do("dcut cave z 1 10 10 0.03 0.03;");
  geant->Do("debug on;");
  geant->Do("swit 2 3;");
#else
  geant->SetInputFile(InFile.Data());
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
  St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker;
#else
#ifdef TSS
//		tss
  St_tss_Maker *tssMk 	= new St_tss_Maker("tpc_raw");
  tssMk->SetDebug();
#endif
#endif
#ifdef EMC
//		emc
  St_ems_Maker  *emsMk = new St_ems_Maker("emc_raw" );
  emsMk->SetDebug();
  St_emc_Maker  *emcMk = new St_emc_Maker("emc_hits");
  emcMk->SetDebug();
#endif
//		tcl
  St_tcl_Maker *tclMk = new St_tcl_Maker("tpc_hits");
  tclMk->SetDebug();  

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
//		tpt
  St_tpt_Maker *tptMk 	= new St_tpt_Maker("tpc_tracks");
  tptMk->SetDebug();
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
GLOBAL:
//		global
  St_glb_Maker *glbMk = new St_glb_Maker("global");
  glbMk->SetDebug();
//		dst
  St_dst_Maker *dstMk = new St_dst_Maker("dst");
  dstMk->SetDebug();
  St_QA_Maker          *qa         = new St_QA_Maker;  
//  goto CHAIN;
//		Tree
  StTreeMaker *treeMk = new StTreeMaker("tree",FileOut.Data());
  treeMk->SetIOMode("w");
  treeMk->SetDebug();
  treeMk->SetInput("dst","dst");
  //  treeMk->SetInput("global","global");
//treeMk->SetInput(".default","Others");
  
  
CHAIN:
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
    chain->Finish();
    printf ("Run completed ");
    gSystem->Exec("date");
  }
}
