// $Id: bfc.C,v 1.57 1999/05/21 14:46:39 fisyak Exp $
// $Log: bfc.C,v $
// Revision 1.57  1999/05/21 14:46:39  fisyak
// More Iwona's correction
//
// Revision 1.56  1999/05/20 01:33:18  fisyak
// Iwona's changes of access to TPC parameters
//
// Revision 1.55  1999/05/14 15:48:01  didenko
// correct error in input_file
//
// Revision 1.54  1999/05/14 00:16:16  fisyak
// take out analysis of input_file environment variable and redefiniton no. event to process from file name
//
// Revision 1.53  1999/05/13 23:48:24  snelling
// changed switch QA to SQA because -qa at end of input would not work: changed EvalTPC to Eval to make it more general
//
// Revision 1.52  1999/05/13 20:34:54  snelling
// added a ChainFlag for TPC evaluation
//
// Revision 1.51  1999/05/12 12:52:38  fisyak
// be sure that default calls once, add printout
//
// Revision 1.50  1999/05/11 12:43:51  fisyak
// gtrack -> gstar
//
// Revision 1.49  1999/05/11 12:40:38  fisyak
// Add tree for SL99c
//
// Revision 1.48  1999/05/10 19:15:41  fisyak
// Remove bfc_*.C
//
// Revision 1.47  1999/05/09 21:45:12  fisyak
// change default chain
//
// Revision 1.46  1999/05/09 02:12:53  fisyak
// Add to Loac Chain as a parameter
//
// Revision 1.45  1999/05/08 03:16:51  fisyak
// Merged version of bfc
//
// Revision 1.44  1999/05/06 12:39:39  fisyak
// Generic version of bfc
//
// Revision 1.43  1999/05/06 03:21:24  fisyak
// synchronize FTPC and TPC slow/fast
//
// Revision 1.42  1999/05/06 03:14:02  fisyak
// Merged version of bfc's
//
// Revision 1.18  1999/05/04 22:42:29  fisyak
// bfc with StEvent
//
// Revision 1.17  1999/05/01 01:47:37  fisyak
// Add new set of bfc s'
//
// Revision 1.16  1999/04/29 23:54:18  fisyak
// Add StRootEvent and test on existing of input file
//
// Revision 1.15  1999/04/27 21:01:11  snelling
// fixed a few switches
//
// Revision 1.14  1999/04/20 12:56:08  fisyak
// Add ctf/mwc
//
// Revision 1.12  1999/04/19 13:43:19  fisyak
// Take out L3
//
// Revision 1.11  1999/04/18 23:45:05  fisyak
// New schema
//
// macro to read fz files, xdf files and minidaq data 
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef __CINT__
#include "TBrowser.h"
#include "TString.h"
#include "TSystem.h"
#include "St_XDFFile.h"
#endif
TBrowser *b = 0;
class StChain;        
StChain  *chain=0;
class St_xdfin_Maker; St_xdfin_Maker *xdfMk=0;     
class St_XDFFile;     St_XDFFile     *xdf_out = 0; 
class St_geant_Maker; St_geant_Maker *geant   = 0;   
class St_db_Maker;    St_db_Maker    *dbMk    = 0; St_db_Maker    *dbMktpc = 0;
class StMagF;         StMagF         *field   = 0;           
class St_fss_Maker;   St_fss_Maker   *fssMk   = 0;     
class St_tcl_Maker;   St_tcl_Maker   *tclMk   = 0;     
class St_tpt_Maker;   St_tpt_Maker   *tptMk   = 0;
class St_ems_Maker;   St_ems_Maker   *emsMk   = 0;    
class St_l3t_Maker;   St_l3t_Maker   *l3tMk   = 0;    
class St_glb_Maker;   St_glb_Maker   *glbMk   = 0;     
class St_dst_Maker;   St_dst_Maker   *dstMk   = 0;     
class StEventMaker;   StEventMaker   *evMk    = 0;
class StTreeMaker;    StTreeMaker    *treeMk  = 0;
TString *InFile = 0;
TString *FileOut= 0;
TString *XdfFile = 0;
Int_t NoEvents = 0;
Bool_t DefaultSet = kFALSE;
//_____________________________________________________________________
enum EChainOptions { 
  kNULL =0 ,kEval    ,
  kXINDF   ,kXOUTDF  ,kGSTAR   ,kMINIDAQ ,kFZIN    ,kGEANT   ,kCTEST   ,
  kField_On,kNo_Field,kTPC     ,kTSS     ,kTRS     ,kTFS     ,kFPC     ,
  kFSS     ,kEMC     ,kCTF     ,kL3      ,kRICH    ,kSVT     ,kGLOBAL  ,
  kDST     ,kSQA     ,kEVENT   ,kANALYS  ,kTREE    ,kAllEvent,kLAST    
};
Char_t *ChainOptions[] = {
  "NULL    ","Eval    ",
  "XINDF   ","XOUTDF  ","GSTAR   ","MINIDAQ ","FZIN    ","GEANT   ","CTEST   ",
  "FieldOn ","NoField ","TPC     ","TSS     ","TRS     ","TFS     ","FPC     ",
  "FSS     ","EMC     ","CTF     ","L3      ","RICH    ","SVT     ","GLOBAL  ",
  "DST     ","SQA     ","EVENT   ","ANALYS  ","TREE    ","AllEvent","LAST    "
};
Char_t *ChainComments[] = {
  "Nothing to comment",
  "Turn on evaluation switch for different makers",
  "Read XDF input file with g2t",
  "Write dst to XDF file",
  "Run gstar for 10 muon track with pT = 10 GeV in |eta|<1",
  "Run minidaq chain",
  "read GSTAR fz-file",
  "initailize GEANT",
  "test DB with MINIDAQ constans",
  "Use nominal STAR field",
  "No Field option",
  "TPC in chain",
  "TPC with TSS",
  "TPC with TRS",
  "TPC with TFS",
  "FTPC in chain",
  "FTPC with FSS",
  "EMC in chain",
  "CTF,MWC & TRG in chain",
  "L3 in chain",
  "RICH in chain",
  "SVT in chain",
  "GLOBAL in chain",
  "DST in chain",
  "STAR QA in chain",
  "StEvent in chain",
  "Analysis with StEvent in chain",
  "write event to StTree",
  "Write whole event to StTree",
  "Nothing to comment",
};
UChar_t  ChainFlags[] = {
  kFALSE   ,kFALSE   ,
  kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,kFALSE   ,
  kTRUE    ,kFALSE   ,kTRUE    ,kTRUE    ,kTRUE    ,kTRUE    ,kTRUE    ,kFALSE  ,
  kTRUE    ,kTRUE    ,kTRUE    ,kTRUE    ,kTRUE    ,kTRUE    ,kTRUE    ,
  kFALSE   ,kFALSE   ,kTRUE    ,kFALSE   ,kFALSE   ,kFALSE   
};
//_____________________________________________________________________
void SetChainOff(){// set all OFF
  for (Int_t k = kEval;k<=kLAST;k++) ChainFlags[k] = kFALSE;
}
//_____________________________________________________________________
void SetDefaultChain(){// default for standard chain
  if (! DefaultSet) {
    printf ("Set default options\n");
    for (Int_t k = kTPC;k<kLAST;k++) if (k != kTSS && k != kTFS) ChainFlags[k] = kTRUE;
    DefaultSet = kTRUE;
  } 
}
//_____________________________________________________________________
void SetFlags(const Char_t *Chain="gstar tfs"){// parse Chain request
  TString STAR_VERSION("$STAR_VERSION");
  gSystem->ExpandPathName(STAR_VERSION);
  if (!strcmp("SL99c",STAR_VERSION.Data())) ChainFlags[kEVENT] = kTRUE; 
  printf ("==============================================\n");
  printf ("============= You are in %s ===============\n",STAR_VERSION.Data());
  Int_t k, kgo;
  if (!strlen(Chain)) {
                               printf ("============= \tPossible Chain Options are: \n");
    for (k=kEval;k<kLAST;k++) printf ("============ %2d \t[-]%s : \t%s \n",k,ChainOptions[k],ChainComments[k]);
    gSystem->Exit(1);
  }
  TString opt;
  TString nopt;
  TString tChain(Chain);
  tChain.ToLower(); //printf ("Chain %s\n",tChain.Data());
  SetChainOff();
  for (k = kEval; k<kLAST; k++){
    opt = TString(ChainOptions[k],3);
    opt.ToLower();
    if (!strstr(tChain.Data(),opt.Data())) continue;
    kgo = k;
    nopt = TString("-");
    nopt += opt;
    if (strstr(tChain.Data(),nopt.Data())) kgo = -k;
    //    printf ("Option %s/%s %d\n",opt.Data(),nopt.Data(),kgo);
    switch (kgo) {
    case kMINIDAQ:
      SetChainOff();
      ChainFlags[kXINDF]   = kTRUE;
      ChainFlags[kMINIDAQ] = kTRUE;
      ChainFlags[kTPC]     = kTRUE;
      ChainFlags[kCTEST]   = kTRUE;
      ChainFlags[kNo_Field]= kTRUE;
      ChainFlags[kEval] = kTRUE;
      printf(" Switch off everything but\n");
      printf(" Switch on  %s\n",ChainOptions[kXINDF]);
      printf(" Switch on  %s\n",ChainOptions[kMINIDAQ]);
      printf(" Switch on  %s\n",ChainOptions[kTPC]);
      printf(" Switch on  %s\n",ChainOptions[kCTEST]);
      printf(" Switch on  %s\n",ChainOptions[kNo_Field]);
      printf(" Switch on  %s\n",ChainOptions[kEval]);
      break;
    case kGSTAR:
      SetDefaultChain();
      ChainFlags[kGEANT]   = kTRUE;
      ChainFlags[kGSTAR]   = kTRUE;
      printf(" Switch on  %s\n",ChainOptions[kGEANT]);
      printf(" Switch on  %s\n",ChainOptions[kGSTAR]);
      break;
    case kTSS:
      SetDefaultChain(); 
      ChainFlags[k]    = kTRUE;
      ChainFlags[kTRS] = kFALSE;
      printf(" Switch on  %s\n",ChainOptions[kTSS]);
      printf(" Switch off %s\n",ChainOptions[kTRS]);
      break;
    case -kTSS:
      SetDefaultChain();
      ChainFlags[kTSS] = kFALSE;
      ChainFlags[kTRS] = kTRUE;
      printf(" Switch off %s\n",ChainOptions[kTSS]);
      printf(" Switch on  %s\n",ChainOptions[kTRS]);
      break;
    case kTRS:
      SetDefaultChain();
      ChainFlags[kTSS] = kFALSE;
      ChainFlags[kTRS] = kTRUE;
      printf(" Switch off %s\n",ChainOptions[kTSS]);
      printf(" Switch on  %s\n",ChainOptions[kTRS]);
      break;
    case -kTRS:
      SetDefaultChain();
      ChainFlags[kTSS] = kTRUE;
      ChainFlags[kTRS] = kFALSE;
      printf(" Switch on  %s\n",ChainOptions[kTSS]);
      printf(" Switch off %s\n",ChainOptions[kTRS]);
      break;
    case  kTFS:
      SetDefaultChain();
      ChainFlags[kTFS] = kTRUE;
      ChainFlags[kTSS] = kFALSE;
      ChainFlags[kTRS] = kFALSE;
      ChainFlags[kFSS] = kFALSE;
      printf(" Switch on  %s\n",ChainOptions[kTFS]);
      printf(" Switch off %s\n",ChainOptions[kTSS]);
      printf(" Switch off %s\n",ChainOptions[kTRS]);
      printf(" Switch off %s\n",ChainOptions[kFSS]);
      break;
    default:
      if (k <= 0 || k > kLAST ) {printf ("Option %s unrecognized\n",ChainOptions[k]);}
      SetDefaultChain();
      if (kgo<0) {ChainFlags[k] = kFALSE; printf(" Switch off %s\n",ChainOptions[k]);}
      else       {ChainFlags[k] = kTRUE;  printf(" Switch on  %s\n",ChainOptions[k]);}
    }
  }
  if (!strcmp("SL99c",STAR_VERSION.Data())) {
    //    ChainFlags[kTREE]     = kFALSE;
    ChainFlags[kEVENT]    = kFALSE;
    ChainFlags[kANALYS]   = kFALSE;
  }
  if (!ChainFlags[kXINDF] && !ChainFlags[kGSTAR]) {
    ChainFlags[kFZIN]  = kTRUE;
    ChainFlags[kGEANT] = kTRUE;
  }
  if (!ChainFlags[kGEANT] && !ChainFlags[kNo_Field]) ChainFlags[kField_On] = kTRUE;
  if (ChainFlags[kAllEvent]) {ChainFlags[kEval] = kTRUE; printf(" Switch on  %s\n",ChainOptions[kEval]);}
  for (k = kEval; k<kLAST;k++) 
    if (ChainFlags[k]     ) 
      printf ("================== %2d \t%s \tis ON: \t%s \n",k,ChainOptions[k],ChainComments[k]);
  //  gSystem->Exit(1);
}
//_____________________________________________________________________
void Load(const Char_t *Chain="tfs"){
  SetFlags(Chain);
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("St_db_Maker");
  if (ChainFlags[kXINDF]) gSystem->Load("St_xdfin_Maker");
  if (ChainFlags[kNo_Field] || ChainFlags[kField_On]) gSystem->Load("StMagF");
  else        gSystem->Load("geometry");
  gSystem->Load("StarClassLibrary");
  if (ChainFlags[kTPC]) {
    gSystem->Load("St_tpc");
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");
    if (ChainFlags[kTRS]) {
      gSystem->Load("StTrsMaker"); 
      gSystem->Load("St_tpcdaq_Maker");
    }
    else {if  (ChainFlags[kTSS])gSystem->Load("St_tss_Maker");}
  }
  if (ChainFlags[kMINIDAQ]) gSystem->Load("StMinidaqMaker");
  if (ChainFlags[kGEANT])  {
    gSystem->Load("St_g2r"); 
    gSystem->Load("St_geant_Maker");
  }
  if (ChainFlags[kFPC]) {
    gSystem->Load("St_ftpc");
    if (ChainFlags[kFSS]) gSystem->Load("St_fss_Maker");
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fpt_Maker");
  }
  if (ChainFlags[kEMC]) {
    gSystem->Load("St_emc");
    gSystem->Load("St_ems_Maker");
#if 0
    gSystem->Load("St_emc_Maker");
#endif
  }
  if (ChainFlags[kCTF]) {
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("St_mwc");
    gSystem->Load("St_mwc_Maker");
    gSystem->Load("St_trg");
    gSystem->Load("St_trg_Maker");
  }
  if (ChainFlags[kL3]){
    gSystem->Load("St_l3");
    gSystem->Load("St_l3t_Maker");
  }
  if (ChainFlags[kRICH]) gSystem->Load("StRchMaker");
  if (ChainFlags[kSVT] || ChainFlags[kGLOBAL]) gSystem->Load("St_svt");
  if (ChainFlags[kSVT]) {
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
  }
  if (ChainFlags[kGLOBAL]) {
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    if (ChainFlags[kEVENT]) {
      gSystem->Load("StRootEvent");
      gSystem->Load("StEventMaker");
      if (ChainFlags[kANALYS]) gSystem->Load("StRAnalysisMaker");
    }
    gSystem->Load("St_QA_Maker");
  }
  if (ChainFlags[kTREE]) gSystem->Load("StTreeMaker");
}
//_____________________________________________________________________
void Set_IO_Files(const Char_t *infile=0, const Char_t *outfile=0 ){
  // define input file
  if (!infile) {
    if (ChainFlags[kMINIDAQ])
      infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf"; // laser data
      //      infile ="/scratch/sakrejda/tpc_s01w_981021_21h_cos_t7_f3.xdf"; // laser data
    else {
      if (ChainFlags[kFZIN])  
	//infile ="/disk1/star/test/psc0049_08_40evts.fzd";                     // zebra file
	infile = "/afs/rhic/star/tpc/data/trs_muon_10cmdrift_good.fzd";
      else 
	if (!ChainFlags[kGSTAR]) 
	  infile ="/afs/rhic/star/data/samples/hijet-g2t.xdf";	       // g2t xdf file
    }
  }
  if (infile) {
    InFile = new TString(infile);
    //      Check the input file
    if (gSystem->AccessPathName(InFile->Data())) {// file does not exist
      printf(" *** NO FILE: %s, exit!\n", InFile->Data());
      gSystem->Exit(1); 
    }
  }
  if (ChainFlags[kGSTAR]) {
    if (!outfile) FileOut = new TString("gtrack.root");
    else          FileOut = new TString(outfile);
    printf("Output root file name %s\n", FileOut->Data());
    printf("==============================================\n");
  }
  else {
    if (outfile) FileOut = new TString(outfile);
    else {
      FileOut = new TString(gSystem->BaseName(InFile->Data()));
      FileOut->ReplaceAll(".fzd",".root");
      FileOut->ReplaceAll(".fz",".root");
      FileOut->ReplaceAll(".xdf",".root");
      FileOut->Strip();
    }
    printf("==============================================\n");
    printf("Input file name = %s with No. of Events to process = %i\n"
	   ,InFile->Data(),NoEvents);
    printf("Output root file name %s\n", FileOut->Data());
    printf("==============================================\n");
  }
}
//_____________________________________________________________________
void bfc (const Int_t Nevents=1, const Char_t *Chain="gstar",Char_t *infile=0, Char_t *outfile=0)
{ // Chain variable define the chain configuration 
  // Only the first 3 symbols are significant
  // "-" sign before requiest means that this option is disallowed
  // Chain = "gstar" run GEANT on flight with 10 muons in range |eta| < 1 amd pT = 1GeV/c (default)
  // Chain = "" || "xdf" run STANDARD chain using xd-files as an input
  // Chain = "minidaq" read miniDAQ xdf file and process 
  NoEvents = Nevents;
  // Create the main chain object
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load(Chain);
  Set_IO_Files(infile,outfile);
  if (!chain) delete chain;
  chain = new StChain("bfc");
  chain->SetDebug();

//  Create the makers to be called by the current chain
  const char *mainDB = "$STAR/StDb/params";
  dbMk = new St_db_Maker("db",mainDB);
  dbMk->SetDebug();
  const char *calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  calibMk->SetDebug();  
  if (ChainFlags[kCTEST]){// ChainFlags[kTPC] test Data Base
    //Switch off baseline files
    dbMk->SetOff("params/tpc/trspars/Trs");
    dbMk->SetOff("params/tpc/tsspars/tsspar");
    dbMk->SetOff("params/tpc/tptpars/tpt_pars");
    dbMk->SetOff("params/tpc/tpgpar/tpg_detector"); 
  }
  else{
    //Switch off TestData files
    dbMk->SetOff("params/tpc/trspars/TrsTestData");
    dbMk->SetOff("params/tpc/tsspars/tssparTestData");
    dbMk->SetOff("params/tpc/tptpars/tpt_parsTestData");
    dbMk->SetOff("params/tpc/tpgpar/tpg_detectorTestData");
  } 
  if (ChainFlags[kXINDF]) {
    xdfMk = new St_xdfin_Maker("xdfin",InFile->Data());
    chain->SetInput("geant",".make/xdfin/.data/event/geant/Event");
  }
  if (ChainFlags[kMINIDAQ]) {// defined for ChainFlags[kMINIDAQ]
    chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
    chain->SetInput("ChainFlags[kTPC]_DATA",".make/xdfin/.data/ChainFlags[kTPC]_DATA");
  }
  if (ChainFlags[kGEANT]) {
    geant = new St_geant_Maker("geant");
    geant->SetNwGEANT(10000000);
    chain->SetInput("geant",".make/geant/.data");
    //  geant->SetIwtype(1);
    //  geant->SetDebug();
    if (ChainFlags[kGSTAR]) {
      geant->LoadGeometry("detp geometry YEAR_1B");
      geant->Do("subevent 0;");
      // gkine #particles partid ptrange yrange phirange vertexrange 
      geant->Do("gkine 10 6 1. 1. -1. 1. 0 6.28  0. 0.;");
      geant->Do("mode g2tm prin 1;");
      //  geant->Do("next;");
      //  geant->Do("dcut cave z 1 10 10 0.03 0.03;");
      //  geant->Do("debug on;");
      geant->Do("swit 2 3;");
      // geant->LoadGeometry("detp geometry ChainFlags[kField_On] field_off");
    }
    if (ChainFlags[kFZIN]) geant->SetInputFile(InFile->Data());
  }
  if (ChainFlags[kNo_Field]) field   = new StMagFC("field","STAR no field",0.00002);
  if (ChainFlags[kField_On]) field   = new StMagFC("field","STAR Normal field",1.);
  //  S I M U L A T I O N  or D A Q
  if (ChainFlags[kMINIDAQ]) {
    StMinidaqMaker *tpc_raw = new StMinidaqMaker("tpc_raw");
    if (dbMktpc) {
      cout<<"initializing input for the tpc DB"<<endl;
      //      tpc_raw->SetInput("params","tpcdb:params"); 
    }
  }
  else {  
    if (ChainFlags[kTRS]) {//		trs
      StTrsMaker   *trs = new StTrsMaker;
      if (dbMktpc) {
	cout<<"initializing input for the trs DB"<<endl;
	//	trs->SetInput("params","tpcdb:params");
      }
      St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker;
    }
    else { 
      if (ChainFlags[kTSS]) {//		tss
	St_tss_Maker *tssMk 	= new St_tss_Maker("tpc_raw");
	if (dbMktpc) {
	  cout<<"initializing input for the tpc DB"<<endl;
	  //	  tssMk->SetInput("params","tpcdb:params"); 
	}
	tssMk->SetDebug();
      }
    }
  }
  if (ChainFlags[kFSS]) {//		fss
    St_fss_Maker *fssMk 	= new St_fss_Maker("ftpc_raw");
    fssMk->SetDebug();
  }
  
  if (ChainFlags[kEMC]) {//		emc
    emsMk = new St_ems_Maker("emc_raw" );
    emsMk->SetDebug();
#if 0
    St_emc_Maker  *emcMk = new St_emc_Maker("emc_hits");
    emcMk->SetDebug();
#endif 
  }
  // L O C A L    R E C O N S T R U C T I O
  if (ChainFlags[kTPC]) {//		tcl
    tclMk = new St_tcl_Maker("tpc_hits");
    if (dbMktpc){
      cout<<"initializing input for the tpc DB"<<endl;
      //      tclMk->SetInput("params","tpcdb:params");  
    }
    if (ChainFlags[kEval]) {//Additional switches
      tclMk->tclPixTransOn(); //Turn on flat adcxyz table
      tclMk->tclEvalOn(); //Turn on the hit finder evaluation
    }
    tclMk->SetDebug();
    if (ChainFlags[kSVT]) {//		svt
      St_srs_Maker *srsMk 	= new St_srs_Maker("svt_hits");
      srsMk->SetDebug();  
    }
    if(ChainFlags[kFPC]){//		fcl
      St_fcl_Maker *fclMk 	= new St_fcl_Maker("ftpc_hits");  
      fclMk->SetDebug();
    }
  }
  // T R A C K I N G
  if (ChainFlags[kTPC]) {
    tptMk = new St_tpt_Maker("tpc_tracks");
    if (ChainFlags[kMINIDAQ]) {
      tptMk->Set_final(kTRUE);// Turn on the final ntuple.
    }
    if (ChainFlags[kEval]) {//Additional switches
      tptMk->tteEvalOn();   //Turn on the tpc evaluation
      tptMk->tptResOn();    // Turn on the residual table
    }
    tptMk->SetDebug();
  }
  if (ChainFlags[kSVT]) {//		stk
    St_stk_Maker *stkMk 	= new St_stk_Maker("svt_tracks");
    stkMk->SetDebug();
  }
  if (ChainFlags[kFPC]){//		fpt
    St_fpt_Maker *fptMk 	= new St_fpt_Maker("ftpc_tracks");
    fptMk->SetDebug();
  }
  // T R I G G E R
  if (ChainFlags[kCTF]) {
    St_ctf_Maker         *ctf      = new St_ctf_Maker("ctf");
    St_mwc_Maker         *mwc      = new St_mwc_Maker("mwc");
    St_trg_Maker         *trg      = new St_trg_Maker("trg");
  }
  // G L O B A L
  if (ChainFlags[kGLOBAL]) {//		global
    glbMk = new St_glb_Maker("global");
    chain->SetInput("dst",".make/global/.data/dst");
    glbMk->SetDebug();
  }
  if (ChainFlags[kL3]) {//		l3t
    l3tMk  = new St_l3t_Maker("l3Tracks");
    l3tMk->SetDebug();
  }
  if (ChainFlags[kGLOBAL]) {
  //		dst
    if (ChainFlags[kDST]){
      dstMk = new St_dst_Maker("dst");
      chain->SetInput("dst",".make/dst/.data/dst");
      dstMk->SetDebug();
    }
    if (ChainFlags[kEVENT]){
      evMk  = new StEventMaker;
      if (ChainFlags[kANALYS]) {
	StAnalysisMaker *anaMk = new StAnalysisMaker;
	anaMk->SetDebug(0);
      }
    }
    if (ChainFlags[kSQA]) St_QA_Maker *qa = new St_QA_Maker;  
  }
  if (ChainFlags[kTREE]) {//		Tree
    treeMk = new StTreeMaker("tree",FileOut.Data());
    treeMk->SetIOMode("w");
    treeMk->SetDebug();
    if (dstMk) {
      //  treeMk->SetBranch("dstBranch",FileOut.Data());
      treeMk->IntoBranch("dstBranch","dst");
    }
    else if (glbMk) {
      //  treeMk->SetBranch("globalBranch",FileOut.Data());
      treeMk->IntoBranch("globalBranch","global/.data/dst");
    }
    if (evMk){
      //  treeMk->SetBranch("EventBranch",FileOut.Data());
      treeMk->IntoBranch("EventBranch","StEvent");
    }
    if (ChainFlags[kAllEvent]) {
      if (geant) {
	treeMk->IntoBranch("geantBranch","geant");
	//  treeMk->SetBranch("geantBranch",FileOut.Data());
	//    treeMk->IntoBranch("geantBranch","geant/.data/particle");
	//    treeMk->IntoBranch("geantBranch","geant/.data/g2t_rch_hit");
      }
      if (fssMk) {
	//  treeMk->SetBranch("ftpc_rawBranch",FileOut.Data());
	treeMk->IntoBranch("ftpc_rawBranch","ftpc_raw/.data");
      }
      if (emsMk) {
	//  treeMk->SetBranch("emc_rawBranch",FileOut.Data());
	treeMk->IntoBranch("emc_rawBranch","emc_raw/.data");
      }
      if (ChainFlags[kTSS] || ChainFlags[kTRS]) { 
	//  treeMk->SetBranch("tpc_rawBranch",FileOut.Data());
	treeMk->IntoBranch("tpc_rawBranch","tpc_raw/.data");
      }
      if (tclMk) treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
      if (tptMk) treeMk->IntoBranch("tpc_tracksBranch","tpc_tracks/.data");
      if (ChainFlags[kCTF]) {
	//  treeMk->SetBranch("trgBranch",FileOut.Data());
	treeMk->IntoBranch("trgBranch","ctf mwc trg");
      }
      if (l3tMk) {
	//  treeMk->SetBranch("l3TBranch",FileOut.Data());
	treeMk->IntoBranch("l3TBranch","l3Tracks");
      }
    }      
    treeMk->SetBranch("histBranch");
  }
  chain->PrintInfo();
  // START the chain (may the force be with you)
  // Create HTML docs of all Maker's inv#ifdef ChainFlags[kCTF]
  // chain->MakeDoc();
  
  // Init the chain and all its makers
  //  chain->SetDebug(1);
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  if (ChainFlags[kXOUTDF] && FileOut) {
    XdfFile = new TString(FileOut->Data());
    XdfFile->ReplaceAll(".root","_dst.xdf");
    xdf_out = new St_XDFFile(XdfFile->Data(),"wb"); 
    printf ("Open output xdf file  = %s \n ++++++++++++++++++++++\n",XdfFile->Data());
  }
  Int_t iMake = 0;
  for (Int_t i =1; i <= NoEvents; i++){
    chain->Clear();
    iMake = chain->Make(i);
    if (iMake <kStEOF && xdf_out){
      St_DataSet *dstSet = chain->GetInputDS("dst");
      if (dstSet) xdf_out->NextEventPut(dstSet); // xdf output
    }
    //    gSystem->Exec("ps ux");
    printf ("=============== Done with Event no. %d (%d)\n",i,iMake);
    if (iMake>=kStEOF) break;
  }
  
  if (NoEvents > 1) {
    chain->Finish();
    if (xdf_out) delete xdf_out;
    printf ("Run completed ");
    gSystem->Exec("date");
  }
  //  else {b = new TBrowser("BFC chain",chain);}
}
