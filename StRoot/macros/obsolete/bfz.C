//#define BATCH
//#define TRS
//#define EMC
#define CTF
//#define SVT
//#define FTPC
///#ifdef BATCH
///void Load()
///#endif
//
class StChain;
StChain  *chain=0;
void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("xdf2root");
    gSystem->Load("libmsg");
    gSystem->Load("libtls");


    gSystem->Load("St_db_Maker");
///    gSystem->Load("St_xdfin_Maker");

    gSystem->Load("geometry");
    gSystem->Load("St_g2t");
    gSystem->Load("St_geant_Maker");

//    gSystem->Load("tpc");
    gSystem->Load("St_tpc");
#ifndef TRS
    gSystem->Load("St_tss_Maker");
#endif
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");

//    gSystem->Load("svt");
    gSystem->Load("St_svt");
#ifdef SVT
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
#endif
#ifdef EMC
    gSystem->Load("St_ems_Maker");
    gSystem->Load("St_emc_Maker");
#endif

#ifdef FTPC
    gSystem->Load("St_ftpc");
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fss_Maker");
    gSystem->Load("St_fpt_Maker");
#endif
#ifdef CTF
//    gSystem->Load("ctf");
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("St_l3");
//    gSystem->Load("St_l3t_Maker");
    gSystem->Load("StRchMaker");

    gSystem->Load("St_trg");
    gSystem->Load("St_trg_Maker");

//    gSystem->Load("mwc");
    gSystem->Load("St_mwc");
    gSystem->Load("St_mwc_Maker");
#endif

/////  gSystem->Load("St_strange");
//    gSystem->Load("global");
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker"); 
/////  gSystem->Load("St_run_summary_Maker");
/////  gSystem->Load("St_QA_Maker");


   gSystem->Load("StTreeMaker");

} //End of load
void bfz(const Int_t Nevents=1,const Char_t *inputFile ="/disk1/star/test/muons.fz")
{                              
  if (gClassTable->GetID("StChain") < 0) Load();
//	TOP maker
  chain = new StChain("bfc");
  chain->SetDebug();
  chain->SetInput("EvtHddr",".make/geant/.const/EvtHddr");    
//  		StChainSpy chain("bfc");
  
//  		Create the makers to be called by the current chain
 
 const char *mainDB = "$STAR/StDb";
 St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
 chain->SetInput("params","db:StDb/params");
 dbMk->SetDebug();  


 
 const char *calibDB = "$STAR_ROOT/calib";
 St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
 chain->SetInput("calib","calib:calib");
 calibMk->SetDebug();  

 //goto CHAIN;

///  St_db_Maker *paMk = new St_db_Maker("params","$STAR/params");
///  chain->SetInput("params","params:params");
///  paMk->SetDebug();  


//  const char *inputFile="/afs/rhic/star/data/samples/hijet-g2t.xdf";
//  const char *inputFile="/disk1/star/fisyak/gsrun.xdf";
//		xdfin
// St_xdfin_Maker *xdfin = new St_xdfin_Maker("xdfin",inputFile);
//  chain->SetInput("tpc_raw","xdfin:tpc_raw");
//  xdfin->SetDebug();

//		geant
St_geant_Maker *geantMk = new St_geant_Maker("geant");
  geantMk->SetNwGEANT(40 000 000);
//  geantMK->SetNwPAW(1000000);
  geantMk->SetDebug();
  geantMk->SetInputFile(inputFile);
  chain->SetInput("geom","geant:geom");

#ifdef FTPC
//		fss
  St_fss_Maker *fssMk 	= new St_fss_Maker("ftpc_raw");
  fssMk->SetDebug();
#endif  
#ifndef TRS
//		tss
  St_tss_Maker *tssMk 	= new St_tss_Maker("tpc_raw");
  tssMk->SetDebug();
#else
//		trs
  StTrsMaker   *trs = new StTrsMaker;
  St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker;
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
#ifdef CTF
//		ctf
  St_ctf_Maker *ctfMk  	= new St_ctf_Maker("ctf");
  ctfMk->SetDebug();

//		mwc
  St_mwc_Maker *mwcMk 	= new St_mwc_Maker("mwc");
  mwcMk->SetDebug();

  St_trg_Maker *trgMk 	= new St_trg_Maker("trg");
  trgMk->SetDebug();

//		rch
  StRchMaker *rchMk  	= new StRchMaker("rch");
  rchMk->SetDebug();
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
  //  goto CHAIN;
//		Tree
  StTreeMaker *treeMk = new StTreeMaker("tree",inputFile);
  treeMk->SetIOMode("w");
  treeMk->SetDebug();
  treeMk->SetInput("dst","DST");
  treeMk->SetInput("global","Global");
//treeMk->SetInput(".default","Others");
  
  
CHAIN:;  

  TDatime evt;
  //  chain->SetEventTime(evt);
  chain->Init();
  
  Int_t iEv=0;
  for (iEv =1; iEv <= Nevents; iEv++){
    printf("\n\n******************* Start EVENT %d ***************************\n\n",iEv);
    chain->Make(iEv);
    chain->Clear();
    printf("\n\n******************* End   EVENT %d ***************************\n\n",iEv);
  }
///  treeMk->Finish();
}
