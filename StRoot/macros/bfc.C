// $Id: bfc.C,v 1.42 1999/05/06 03:14:02 fisyak Exp $
// $Log: bfc.C,v $
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
TBrowser *b = 0;
class StChain;        
StChain  *chain=0;
class St_xdfin_Maker; St_xdfin_Maker *xdfMk=0;     
class St_XDFFile;     St_XDFFile     *xdf_out = 0; 
class St_geant_Maker; St_geant_Maker *geant = 0;   
class StMagF;         StMagF *field = 0;           
class St_fss_Maker;   St_fss_Maker *fssMk = 0;     
class St_tcl_Maker;   St_tcl_Maker *tclMk = 0;     
class St_tpt_Maker;   St_tpt_Maker *tptMk = 0;
class St_ems_Maker;   St_ems_Maker  *emsMk = 0;    
class St_l3t_Maker;   St_l3t_Maker  *l3tMk = 0;    
class St_glb_Maker;   St_glb_Maker *glbMk = 0;     
class St_dst_Maker;   St_dst_Maker *dstMk = 0;     
class StEventMaker;   StEventMaker *evMk  = 0;
class StTreeMaker;    StTreeMaker  *treeMk = 0;
//_____________________________________________________________________
Bool_t XDFIN   = kTRUE;
Bool_t XDFOUT  = kFALSE;
Bool_t GTRACK  = kFALSE;
Bool_t MINIDAQ = kFALSE;
Bool_t FZIN    = kFALSE;
Bool_t GEANT   = kFALSE;
Bool_t CTEST   = kFALSE;
Bool_t STMAGF  = kFALSE;
Bool_t FTPC    = kFALSE;
Bool_t FSS     = kFALSE;
Bool_t TPC     = kTRUE;
Bool_t TSS     = kFALSE;
Bool_t TRS     = kTRUE;
Bool_t EMC     = kFALSE;
Bool_t CTF     = kFALSE;
Bool_t L3      = kFALSE;
Bool_t RICH    = kFALSE;
Bool_t SVT     = kFALSE;
Bool_t GLOBAL  = kFALSE;
Bool_t DST     = kFALSE;
Bool_t ANALYSIS= kFALSE;
Bool_t TREE    = kTRUE;
//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("St_db_Maker");
  gSystem->Load("St_xdfin_Maker");
  if (STMAGF) gSystem->Load("StMagF");
  else        gSystem->Load("geometry");
  gSystem->Load("StarClassLibrary");
  if (TPC) {
    gSystem->Load("St_tpc");
    gSystem->Load("St_tcl_Maker");
    gSystem->Load("St_tpt_Maker");
    if (TRS) {
      gSystem->Load("StTrsMaker"); 
      gSystem->Load("St_tpcdaq_Maker");
    }
    else {if  (TSS)gSystem->Load("St_tss_Maker");}
  }
  if (MINIDAQ) gSystem->Load("StMinidaqMaker");
  if (GEANT) {
    gSystem->Load("St_g2r"); 
    gSystem->Load("St_geant_Maker");
  }
  if (FTPC) {
    gSystem->Load("St_ftpc");
    if (FSS) gSystem->Load("St_fss_Maker");
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fpt_Maker");
  }
  if (EMC) {
    gSystem->Load("St_emc");
    gSystem->Load("St_ems_Maker");
#if 0
    gSystem->Load("St_emc_Maker");
#endif
  }
  if (CTF) {
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("St_mwc");
    gSystem->Load("St_mwc_Maker");
    gSystem->Load("St_trg");
    gSystem->Load("St_trg_Maker");
  }
  if (L3){
    gSystem->Load("St_l3");
    gSystem->Load("St_l3t_Maker");
  }
  if (RICH) gSystem->Load("StRchMaker");
  if (SVT || GLOBAL) gSystem->Load("St_svt");
  if (SVT) {
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
  }
  if (GLOBAL) {
    gSystem->Load("St_global");
    gSystem->Load("StRootEvent");
    gSystem->Load("St_dst_Maker");
    gSystem->Load("StEventMaker");
    if (ANALYSIS) gSystem->Load("StRAnalysisMaker");
    gSystem->Load("St_QA_Maker");
  }
  if (TREE) gSystem->Load("StTreeMaker");
}
//_____________________________________________________________________
void bfc (const Int_t Nevents=1, const Char_t *Chain="gtrack",Char_t *infile=0, Char_t *outfile=0)
{// Chain = "gtrack"
  Int_t NoEvents = Nevents;
  // parse Chain request
  if (strlen(Chain)) {
    printf ("============= Chain: %s  =================\n",Chain);
    if (strstr(Chain,"min") || strstr(Chain,"MIN")) {// MINIDAQ
      XDFIN   = kTRUE;
      MINIDAQ = kTRUE;
      TPC     = kTRUE;
      CTEST   = kTRUE;
      GTRACK  = kFALSE;
      GEANT   = kFALSE;
      FZIN    = kFALSE;
      TSS     = kFALSE;
      TRS     = kFALSE;
    }
    else {
      FZIN   = kTRUE;
      GEANT  = kTRUE;
      GTRACK = kFALSE;
      if (strstr(Chain,"GTR") || strstr(Chain,"gtr")) GTRACK = kTRUE;
      TPC     = kTRUE;
      TSS     = kFALSE;
      TRS     = kTRUE;
      if (strstr(Chain,"-tss") || strstr(Chain,"-TSS")) TSS = kFALSE;
      else {if (strstr(Chain,"tss") || strstr(Chain,"TSS")) TSS = kTRUE;}
      if (strstr(Chain,"-trs") || strstr(Chain,"-TRS")) TRS = kFALSE;
      else {if (strstr(Chain,"trs") || strstr(Chain,"TRS")) TRS = kTRUE;}
      if (strstr(Chain,"tfs") || strstr(Chain,"TFS")) {TRS = kFALSE; TSS = kFALSE;}
      FTPC    = kTRUE;
      FSS     = kTRUE;
      EMC     = kTRUE;
      CTF     = kTRUE;
      //      L3      = kTRUE;
      RICH    = kTRUE;
      SVT     = kTRUE;
      GLOBAL  = kTRUE;
      ANALYSIS= kTRUE;
    }
    TREE    = kTRUE;
    if (strstr(Chain,"-tree") || strstr(Chain,"-TREE")) TREE = kFALSE;
  }
  if (GTRACK || FZIN) XDFIN = kFALSE;
  if (MINIDAQ)        STMAGF = kTRUE;
  // define input file
  if (!infile) {
    if (MINIDAQ) infile ="/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf"; // laser data
    else {
      if (FZIN)  infile ="/disk1/star/test/psc0049_08_40evts.fzd";                     // zebra file
      else 
	if (!GTRACK) infile ="/afs/rhic/star/data/samples/hijet-g2t.xdf";	       // g2t xdf file
    }
    TString InFile(infile);
    //      Check the input file
    if (gSystem->AccessPathName(InFile.Data())) {// file does not exist
      printf(" *** NO FILE: %s, exit!\n", InFile.Data());
      gSystem->Exit(1); 
    }
  }
  if (GTRACK) {
    if (!outfile) FileOut = new TString("gtrack.root");
    else          FileOut = new TString(outfile);
    printf("Output root file name %s\n", FileOut.Data());
    printf("==============================================\n");
  }
  else {
    if (FZIN) {// zebra file, set I/O for crs using "input_file" varaible if any
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
    }
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
  }
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  
  if (XDFOUT && FileOut) {
    TString *XdfFile = new TString(FileOut->Data());
    XdfFile->ReplaceAll(".root","_dst.xdf");
    xdf_out = new St_XDFFile(XdfFile->Data(),"wb"); 
    printf ("Open output xdf file  = %s \n ++++++++++++++++++++++\n",XdfFile->Data());
  }
  // Create the main chain object
  chain = new StChain("bfc");
  chain->SetDebug();
  //  Create the makers to be called by the current chain
  if (CTEST){// TPC test Data Base
    cout<<"creating DB for TPC test"<<endl;
    const char *tpcDB = "/afs/rhic/star/tpc/ctest/StDb/params";
    dbMktpc = new St_db_Maker("tpcdb",tpcDB);
    dbMktpc->SetDebug();  
  }
  else {// Standard DataBase
    const char *mainDB = "$STAR/StDb/params";
    dbMk = new St_db_Maker("db",mainDB);
    dbMk->SetDebug();
    St_db_Maker *dbMktpc = 0;
    const char *calibDB = "$STAR_ROOT/calib";
    St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
    calibMk->SetDebug();  
  }
  if (XDFIN) xdfMk = new St_xdfin_Maker("xdfin",InFile.Data());
  if (MINIDAQ) {// defined for MINIDAQ
    chain->SetInput("BEGIN_RUN",".make/xdfin/.const/BEGIN_RUN");
    chain->SetInput("TPC_DATA",".make/xdfin/.data/TPC_DATA");
  }
  if (GEANT) {
    geant = new St_geant_Maker("geant");
    geant->SetNwGEANT(10 000 000);
    chain->SetInput("geant",".make/geant/.data");
    if (GTRACK) {
      //  geant->SetIwtype(1);
      //  geant->SetDebug();
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
    }
    else  geant->SetInputFile(InFile.Data());
  }
  if (STMAGF) field   = new StMagFC("field","STAR no field",0.00002);
  //  S I M U L A T I O N  or D A Q
  if (MINIDAQ) {
    StMinidaqMaker *tpc_raw = new StMinidaqMaker("tpc_raw");
    if (dbMktpc) {
      cout<<"initializing input for the tpc DB"<<endl;
      //      tpc_raw->SetInput("params","tpcdb:params"); 
    }
  }
  else {  
    if (TRS) {//		trs
      StTrsMaker   *trs = new StTrsMaker;
      if (dbMktpc) {
	cout<<"initializing input for the trs DB"<<endl;
	//	trs->SetInput("params","tpcdb:params");
      }
      St_tpcdaq_Maker *tpc_raw = new St_tpcdaq_Maker;
    }
    else { 
      if (TSS) {//		tss
	St_tss_Maker *tssMk 	= new St_tss_Maker("tpc_raw");
	if (dbMktpc) {
	  cout<<"initializing input for the tpc DB"<<endl;
	  //	  tssMk->SetInput("params","tpcdb:params"); 
	}
	tssMk->SetDebug();
      }
    }
  }
  if (FSS) {//		fss
    St_fss_Maker *fssMk 	= new St_fss_Maker("ftpc_raw");
    fssMk->SetDebug();
  }
  
  if (EMC) {//		emc
    emsMk = new St_ems_Maker("emc_raw" );
    emsMk->SetDebug();
#if 0
    St_emc_Maker  *emcMk = new St_emc_Maker("emc_hits");
    emcMk->SetDebug();
#endif 
  }
  // L O C A L    R E C O N S T R U C T I O
  if (TPC) {//		tcl
    tclMk = new St_tcl_Maker("tpc_hits");
    if (dbMktpc){
      cout<<"initializing input for the tpc DB"<<endl;
      //      tclMk->SetInput("params","tpcdb:params");  
      tclMk->tclPixTransOn();
      tclMk->tclEvalOn(); //Turn on the hit finder evaluation
    }
    tclMk->SetDebug();
    if (SVT) {//		svt
      St_srs_Maker *srsMk 	= new St_srs_Maker("svt_hits");
      srsMk->SetDebug();  
    }
    if(FTPC){//		fcl
      St_fcl_Maker *fclMk 	= new St_fcl_Maker("ftpc_hits");  
      fclMk->SetDebug();
    }
  }
  // T R A C K I N G
  if (TPC) {
    tptMk 	= new St_tpt_Maker("tpc_tracks");
    if (MINIDAQ) {
      tptMk->tptResOn();      // Turn on the residual table
      tptMk->Set_final(kTRUE);// Turn on the final ntuple.
    }
    else tptMk->tteEvalOn(); //Turn on the tpc evaluation
    tptMk->SetDebug();
  }
  if (SVT) {//		stk
    St_stk_Maker *stkMk 	= new St_stk_Maker("svt_tracks");
    stkMk->SetDebug();
  }
  if (FTPC){//		fpt
    St_fpt_Maker *fptMk 	= new St_fpt_Maker("ftpc_tracks");
    fptMk->SetDebug();
  }
  // T R I G G E R
  if (CTF) {
    St_ctf_Maker         *ctf      = new St_ctf_Maker("ctf");
    St_mwc_Maker         *mwc      = new St_mwc_Maker("mwc");
    St_trg_Maker         *trg      = new St_trg_Maker("trg");
  }
  // G L O B A L
  if (GLOBAL) {//		global
    glbMk = new St_glb_Maker("global");
    chain->SetInput("dst",".make/global/.data/dst");
    glbMk->SetDebug();
  }
  if (L3) {//		l3t
    l3tMk  = new St_l3t_Maker("l3Tracks");
    l3tMk->SetDebug();
  }
  //		dst
  if (DST){
    dstMk = new St_dst_Maker("dst");
    chain->SetInput("dst",".make/dst/.data/dst");
    dstMk->SetDebug();
  }
  if (GLOBAL) {
    evMk  = new StEventMaker;
    if (ANALYSIS) {
      StAnalysisMaker *anaMk = new StAnalysisMaker;
      anaMk->SetDebug(0);
    }
    St_QA_Maker          *qa         = new St_QA_Maker;  
  }
  if (TREE) {//		Tree
    treeMk = new StTreeMaker("tree",FileOut.Data());
    treeMk->SetIOMode("w");
    treeMk->SetDebug();
    if (geant) {
      treeMk->IntoBranch("geantBranch","geant/.data");
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
    if (TSS || TRS) { 
      //  treeMk->SetBranch("tpc_rawBranch",FileOut.Data());
      treeMk->IntoBranch("tpc_rawBranch","tpc_raw/.data");
    }
    if (tclMk) treeMk->IntoBranch("tpc_hitsBranch","tpc_hits/.data");
    if (tptMk) treeMk->IntoBranch("tpc_tracksBranch","tpc_tracks/.data");
    if (CTF) {
      //  treeMk->SetBranch("trgBranch",FileOut.Data());
      treeMk->IntoBranch("trgBranch","ctf");
      treeMk->IntoBranch("trgBranch","mwc");
      treeMk->IntoBranch("trgBranch","trg");
    }
    if (l3tMk) {
      //  treeMk->SetBranch("l3TBranch",FileOut.Data());
      treeMk->IntoBranch("l3TBranch","l3Tracks");
    }
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
      treeMk->IntoBranch("EventBranch","StEventMaker/.data/Event");
    }
    // treeMk->SetInput(".default","Others");
    // START the chain (may the force be with you)
    // Create HTML docs of all Maker's inv#ifdef CTF
    // chain->MakeDoc();
  }
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
  //  else {b = new TBrowser("BFC chain",chain);}
}


