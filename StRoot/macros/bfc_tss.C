// $Id: bfc_tss.C,v 1.17 1999/04/18 23:45:06 fisyak Exp $
// $Log: bfc_tss.C,v $
// Revision 1.17  1999/04/18 23:45:06  fisyak
// New schema
//
// macro to read fz files, xdf files and minidaq data 
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//  default is g2t data
//#define MINIDAQ /* TPC minidaq data */
#define FZIN    /* GEANT fz-file */
//#define GTRACK  /* GEANT simulation on flight */
#ifndef MINIDAQ
#endif /* GTRACK or FZIN */
#ifdef FZIN 
#define GEANT
#endif /* FZIN */
#ifdef GTRACK
#define GEANT
#endif /* GTRACK */
#ifndef GEANT
#define StMagF
#endif
#ifdef MINIDAQ
#define CTEST
#define StMagF
#undef FZIN
#undefine GTRACK
#endif /* MINIDAQ */
#define TPC
//#define tclPixTransOn
//#define TRS
#define TSS
#if defined(FZIN) || defined(GTRACK)
#define FTPC
#define FSS
#define SVT
#define EMC
#define CTF
#define L3
#define GLOBAL
#endif  /* new data only FZIN or GTRACK */
//#define XDFOUT
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
#ifdef StMagF
    gSystem->Load("StMagF");
#else /* no StMagF, GEANT field */
    gSystem->Load("geometry");
#endif /* StMagF */
#ifdef TPC
    gSystem->Load("St_tpc");
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");
#ifdef TRS
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTrsMaker"); 
    gSystem->Load("St_tpcdaq_Maker");
#else /* no TRS */
#ifdef  TSS
    gSystem->Load("St_tss_Maker");
#endif /* TSS */
#endif /* TPC */
#ifdef MINIDAQ
    gSystem->Load("StMinidaqMaker");
#endif /* MINIDAQ */
#ifdef GEANT
    gSystem->Load("St_g2r"); 
    gSystem->Load("St_geant_Maker");
#endif /*  GEANT */
#ifdef FTPC
    gSystem->Load("St_ftpc");
#ifdef  FSS
    gSystem->Load("St_fss_Maker");
#endif /* FSS */
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fpt_Maker");
#endif /* FTPC */
#ifdef EMC
    gSystem->Load("St_emc");
    gSystem->Load("St_ems_Maker");
#if 0
    gSystem->Load("St_emc_Maker");
#endif
#endif /* EMC */
#ifdef CTF
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("St_mwc");
    gSystem->Load("St_mwc_Maker");
    gSystem->Load("St_trg");
    gSystem->Load("St_trg_Maker");
#endif /* CTF */
#ifdef L3
    gSystem->Load("St_l3");
    gSystem->Load("St_l3t_Maker");
#endif /* L3 */
    //    gSystem->Load("StRchMaker");
#if defined(SVT) || defined(GLOBAL)
    gSystem->Load("St_svt");
#endif
#ifdef SVT
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
#endif /* SVT */
#ifdef GLOBAL
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    gSystem->Load("St_QA_Maker");
    gSystem->Load("StTreeMaker");
#endif /* GLOBAL */
}

void bfc_tss (const Int_t Nevents=1000,Char_t *infile=0, Char_t *outfile=0)
{
  Int_t NoEvents = Nevents;
  // define input file
  if (!infile) {
#ifdef MINIDAQ
    // laser data
    *infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf";
#else /*not MINIDAQ */
#ifdef FZIN
    // zebra file
    //    infile ="/afs/rhic/star/tpc/data/trs_muon_10cmdrift_good.fzd";
    infile ="/disk1/star/test/psc0049_08_40evts.fzd";
#else 
#ifndef GTRACK
    // g2t xdf file
    infile ="/afs/rhic/star/data/samples/hijet-g2t.xdf";
#endif /* no GTRACK */
#endif /* FZIN */
#endif /* MINIDAQ */
  }
#ifdef GTRACK
  if (!outfile) FileOut = new TString("gtrack.root");
  else          FileOut = new TString(outfile);
  printf("Output root file name %s\n", FileOut.Data());
  printf("==============================================\n");
#else /*not GTRACK */
#ifdef FZIN 
  // zebra file, set I/O for crs using "input_file" varaible if any
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
  }
#else  /* not FZIN and not GTRACK */
  TString InFile(infile);
#endif /* FZIN */
  if (outfile) FileOut = new TString(outfile);
  else {
    FileOut = new TString(gSystem->BaseName(InFile.Data()));
    FileOut->ReplaceAll(".fzd",".root");
    FileOut->ReplaceAll(".fz",".root");
    FileOut->ReplaceAll(".xdf",".root");
    FileOut->Strip();
  }
  printf("==============================================\n");
  printf("Input file name = %s with No. of Events to process = %i\n"
	 ,InFile.Data(),NoEvents);
  printf("Output root file name %s\n", FileOut.Data());
  printf("==============================================\n");
#endif /* GTRACK */
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
#endif

#ifdef XDFOUT
  if (!FileOut) {
    St_XDFFile  *xdf_out = 0;
    TString *XdfFile = new TString(FileOut->Data());
    XdfFile->ReplaceAll(".root","_dst.xdf");
    xdf_out = new St_XDFFile(XdfFile->Data(),"wb"); 
    printf ("Open xdf file  = %s \n +++++++++++++++++++++++++++++++++++++++++++++++\n",XdfFile->Data());
  }
#endif

  // Create the main chain object
  chain = new StChain("bfc");
  chain->SetDebug();
  chain->SetInput("EvtHddr",".make/geant/.const/EvtHddr");    
  //  Create the makers to be called by the current chain
  const char *mainDB = "$STAR/StDb";
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  chain->SetInput("params","db:StDb/params");
  dbMk->SetDebug();
  St_db_Maker *dbMktpc = 0;
#ifdef CTEST
  // TPC test Data Base
  cout<<"creating DB for TPC test"<<endl;
  const char *tpcDB = "/afs/rhic/star/tpc/ctest/StDb";
  dbMktpc = new St_db_Maker("tpcdb",tpcDB);
  dbMktpc->SetDebug();  
#endif /* CTEST */

  const char *calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  chain->SetInput("calib","calib:calib");
  calibMk->SetDebug();  
#ifndef GEANT
  St_xdfin_Maker *xdfMk = new St_xdfin_Maker("xdfin",InFile.Data());
#ifdef MINIDAQ
  // defined for MINIDAQ
  chain->SetInput("TPC_DATA",".make/xdfin/.data/TPC_DATA");
  chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
#else /* no MINIDAQ */
  // fix for xdf files to get geant input
  chain->SetInput("geant",".make/xdfin/.data/event/geant/Event");
#endif /* MINIDAQ */
#else  /* GEANT */
  geant = new St_geant_Maker("geant");
  geant->SetNwGEANT(10 000 000);
#ifdef GTRACK
  geant->SetIwtype(1);
  geant->SetDebug();
  geant->LoadGeometry("detp geometry YEAR_1B");
  geant->Do("subevent 0;");
  // gkine #particles partid ptrange yrange phirange vertexrange 
  geant->Do("gkine 10 6 1. 1. -1. 1. 0 6.28  0. 0.;");
  geant->Do("mode g2tm prin 1;");
  //  geant->Do("next;");
  //  geant->Do("dcut cave z 1 10 10 0.03 0.03;");
  //  geant->Do("debug on;");
  geant->Do("swit 2 3;");
  // geant->LoadGeometry("detp geometry field_only field_off");
#else /* no GTRACK */
  geant->SetInputFile(InFile.Data());
#endif /* GTRACK */
  chain->SetInput("geom","geant:geom");
#endif /* GEANT */



#ifdef MINIDAQ
  StMagF         *field   = new StMagFC("field","STAR no field",0.00002);
  StMinidaqMaker *tpc_raw = new StMinidaqMaker("tpc_raw");
  if (dbMktpc) {
    cout<<"initializing input for the tpc DB"<<endl;
    tpc_raw->SetInput("params","tpcdb:StDb/params"); 
  }
#endif
#ifdef FSS
//		fss
  St_fss_Maker *fssMk 	= new St_fss_Maker("ftpc_raw");
  fssMk->SetDebug();
#endif  

#ifdef TRS
//		trs
  StTrsMaker   *trs = new StTrsMaker;
  if (dbMktpc) {
    cout<<"initializing input for the trs DB"<<endl;
    trs->SetInput("params","tpcdb:StDb/params");
  }
  St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker;
#else /* not TRS */
#ifdef TSS
//		tss
  St_tss_Maker *tssMk 	= new St_tss_Maker("tpc_raw");
  if (dbMktpc) {
    cout<<"initializing input for the tpc DB"<<endl;
    tssMk->SetInput("params","tpcdb:StDb/params"); 
  }
  //  tssMk->SetDebug();
#endif /* TSS */
#endif /* TRS */

#ifdef EMC
//		emc
  St_ems_Maker  *emsMk = new St_ems_Maker("emc_raw" );
  emsMk->SetDebug();
#if 0
  St_emc_Maker  *emcMk = new St_emc_Maker("emc_hits");
  emcMk->SetDebug();
#endif 
#endif /* EMC */

#ifdef TPC
//		tcl
  St_tcl_Maker *tclMk = new St_tcl_Maker("tpc_hits");
  if (dbMktpc){
    cout<<"initializing input for the tpc DB"<<endl;
    tclMk->SetInput("params","tpcdb:StDb/params");  
  }
  tclMk->SetDebug();
#ifdef tclPixTransOn
  tclMk->tclPixTransOn();
#endif /* tclPixTransOn */
#ifndef MINIDAQ
  //Turn on the hit finder evaluation
  tclMk->tclEvalOn();
#endif  /* MINIDAQ */
#endif  /* TPC */

#ifdef SVT
//		svt
  St_srs_Maker *srsMk 	= new St_srs_Maker("svt_hits");
  srsMk->SetDebug();  
#endif /* SVT */

#ifdef FTPC
//		fcl

  St_fcl_Maker *fclMk 	= new St_fcl_Maker("ftpc_hits");  
  fclMk->SetDebug();
#endif /* FTPC */

#ifdef TPC
//		tpt
  St_tpt_Maker *tptMk 	= new St_tpt_Maker("tpc_tracks");
#ifdef MINIDAQ
 // Turn on the final ntuple.
  tptMk->Set_final(kTRUE);
#else /* not MINIDAQ */
  //Turn on the tpc evaluation
  tptMk->tteEvalOn();
#endif /* MINIDAQ */
  tptMk->SetDebug();
#endif /* TPC */

#ifdef L3
//		l3t
  St_l3t_Maker  *l3tMk  = new St_l3t_Maker("l3Tracks");
  l3tMk->SetDebug();
#endif /* L3 */

#ifdef SVT
//		stk
  St_stk_Maker *stkMk 	= new St_stk_Maker("svt_tracks");
  stkMk->SetDebug();
#endif /* SVT */

#ifdef FTPC
//		fpt
  St_fpt_Maker *fptMk 	= new St_fpt_Maker("ftpc_tracks");
  fptMk->SetDebug();
#endif /* FTPC */
  St_dst_Maker *dstMk = 0;
#ifdef GLOBAL
//		global
  St_glb_Maker *glbMk = new St_glb_Maker("global");
  glbMk->SetDebug();
//		dst
                dstMk = new St_dst_Maker("dst");
  dstMk->SetDebug();
  St_QA_Maker          *qa         = new St_QA_Maker;  
#endif  /* GLOBAL */
//		Tree
  if (dstMk) {
    StTreeMaker *treeMk = new StTreeMaker("tree",FileOut.Data());
    treeMk->SetIOMode("w");
    treeMk->SetDebug();
    treeMk->SetInput("dst","dst");
    //  treeMk->SetInput("global","global");
    //treeMk->SetInput(".default","Others");
  }
  
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
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;
    //    gSystem->Exec("ps ux");
    printf ("=========================================== Done with Event no. %d\n",i);
  }

  if (NoEvents > 1) {
    chain->Finish();
    printf ("Run completed ");
    gSystem->Exec("date");
  }
  else {b = new TBrowser("BFC chain",chain);}
}

