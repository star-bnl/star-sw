//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
//                                                                      //
// $Id: bfc.C,v 1.128 2000/03/27 02:57:12 fine Exp $
//////////////////////////////////////////////////////////////////////////
TBrowser *b = 0;
class StBFChain;        
StBFChain  *chain=0;
class StEvent;
StEvent *Event;
class St_geant_Maker;
class StIOMaker;
class St_XDFFile;
class StEventDisplayMaker; StEventDisplayMaker *dsMk = 0;
class StEventMaker; StEventMaker *evMk = 0;
//_____________________________________________________________________
void Load(){
  gSystem->Load("libSTAR");
  gSystem->Load("Star2Root");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StBFChain");
  if (chain) delete chain;
  chain = new StBFChain;   // Create the main chain object
  if (!chain) gSystem->Exit(1);
}
//_____________________________________________________________________
void bfc(const Int_t First,
	 const Int_t Last,
	 const Char_t *Chain="gstar Cy2a tfs",Char_t *infile=0, Char_t *outfile=0, Char_t *TreeFile=0)
{ // Chain variable define the chain configuration 
  // All symbols are significant (regardless of case)
  // "-" sign before requiest means that this option is disallowed
  // Chain = "gstar" run GEANT on flight with 10 muons in range |eta| < 1 amd pT = 1GeV/c (default)
  // Chain = "" || "xdf" run STANDARD chain using xd-files as an input
  // Chain = "minidaq" read miniDAQ xdf file and process 
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();
  
  chain->SetFlags(Chain);
  if (TreeFile) chain->SetTFile(new TFile(TreeFile,"RECREATE"));
  if (chain->GetOption("LAna") && !chain->GetTFile()) chain->SetTFile(new TFile("Laser.root","RECREATE"));
  printf ("QAInfo:Process [First=%6i/Last=%6i/Total=%6i] Events\n",First,Last,Last-First+1);
  chain->Set_IO_Files(infile,outfile);
  chain->Load();
  chain->Instantiate();
  if (chain->GetOption("DISPLAY")) dsMk = (StEventDisplayMaker *) chain->GetMaker("EventDisplay");
#if 0
  // Insert your maker before "tpc_hits"
  Char_t *myMaker = "St_TLA_Maker";
  if (gClassTable->GetID(myMaker) < 0) gSystem->Load(myMaker);
  StMaker *myMk = chain->GetMaker(myMaker);
  if (myMk) delete myMk;
  myMk = chain->New(myMaker,"before");
  if (myMk) {
    Char_t *before = "tpc_hits";
    StMaker *tclmk = chain->GetMaker(before);
    if (tclmk) chain->AddBefore(before,myMk);
  }
  // Insert your maker after "tpc_hits"
  myMk = chain->New(myMaker,"after");
  if (myMk) {
    Char_t *after = "tpc_hits";
    StMaker *tclmk = chain->GetMaker(after);
    if (tclmk) chain->AddAfter(after,myMk);
  }
#endif
  if (chain->GetOption("TCL") && chain->GetOption("Eval")) {
    St_tcl_Maker *tclMk= (St_tcl_Maker *) chain->GetMaker("tpc_hits");
    if (tclMk) {
      tclMk->tclPixTransOn(); //Turn on flat adcxyz table
      tclMk->tclEvalOn();     //Turn on the hit finder evaluation
    }
  }
  if (chain->GetOption("TPT")) {
    St_tpt_Maker *tptMk= (St_tpt_Maker *) chain->GetMaker("tpc_tracks");
    if (tptMk && chain->GetOption("MINIDAQ"))  tptMk->Set_final(kTRUE);// Turn on the final ntuple.
    if (tptMk && chain->GetOption("Eval")) {
      tptMk->tteEvalOn();   //Turn on the tpc evaluation
      tptMk->tptResOn();    // Turn on the residual table
    }
  }
  if (chain->GetOption("McAss")) {
    // Define the cuts for the Associations

    StMcParameterDB* parameterDB = StMcParameterDB::instance();  
    // TPC
    parameterDB->setXCutTpc(.5); // 5 mm
    parameterDB->setYCutTpc(.5); // 5 mm
    parameterDB->setZCutTpc(.2); // 2 mm
    parameterDB->setReqCommonHitsTpc(3); // Require 3 hits in common for tracks to be associated
    // FTPC
    parameterDB->setRCutFtpc(.3); // 3 mm
    parameterDB->setPhiCutFtpc(5*(3.1415927/180.0)); // 5 degrees
    parameterDB->setReqCommonHitsFtpc(3); // Require 3 hits in common for tracks to be associated
    // SVT
    parameterDB->setXCutSvt(.1); // 1 mm
    parameterDB->setYCutSvt(.1); // 1 mm
    parameterDB->setZCutSvt(.1); // 1 mm
    parameterDB->setReqCommonHitsSvt(1); // Require 1 hits in common for tracks to be associated
  }
  if (chain->GetOption("V0") && chain->GetOption("Eval")) {
    StV0Maker    *v0Mk = (StV0Maker *) chain->GetMaker("v0");
    if (v0Mk) 	v0Mk->ev0EvalOn();   //Turn on the ev0 evaluatio
  }
  {
    TDatime t;
    printf ("QAInfo:Run is started at Date/Time %i/%i\n",t.GetDate(),t.GetTime());
  }
  printf ("QAInfo:Run on %s in %s\n",
	  gSystem->HostName(),
	gSystem->WorkingDirectory());
printf ("QAInfo: with %s\n", chain->GetCVS());

// Init the chain and all its makers
if (Last >= 0) {
  Int_t iInit = chain->Init();
  // skip if any
  St_geant_Maker *geant = (St_geant_Maker *) chain->GetMaker("geant");
  St_XDFFile *xdf_out = chain->GetXdfOut();
  if (chain->GetOption("Event")) evMk  = (StEventMaker   *) chain->GetMaker("StEventMaker");  
  if (geant && First > 1) geant->Skip(First-1);
  else {
    StIOMaker *inpMk      = (StIOMaker *)      chain->GetMaker("inputStream");
    if (inpMk && First > 1) {printf ("Skip %i Events\n",First-1);inpMk->Skip(First-1);}
  }
  
}
TBenchmark evnt;
Int_t iMake = 0, i = First;
EventLoop: if (i <= Last && iMake != kStEOF && iMake != kStFatal) {
  evnt->Reset();
  evnt->Start("QAInfo:");
  chain->Clear();
  iMake = chain->Make(i);
  if (iMake <kStEOF && xdf_out){
    St_DataSet *dstSet = chain->GetInputDS("dst");
    if (dstSet) xdf_out->NextEventPut(dstSet); // xdf output
  }
  //    gSystem->Exec("ps ux");
  evnt->Stop("QAInfo:");
  //  evnt->Show("QAInfo:");
  printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time %d.%d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n",
	  i,chain->GetRunNumber(),chain->GetEventNumber(),chain->GetDate(), chain->GetTime(),
	  iMake,evnt->GetRealTime("QAInfo:"),evnt->GetCpuTime("QAInfo:"));
  i++; goto EventLoop;
}
fflush(stdout);
printf ("QAInfo:Run completed ");
gSystem->Exec("date");
if (evMk) Event = (StEvent *) chain->GetInputDS("StEvent");
{
  TDatime t;
  printf ("\nQAInfo:Run is finished at Date/Time %i/%i\n",t.GetDate(),t.GetTime());
}
}
//_____________________________________________________________________
void bfc (const Int_t Last, 
	  const Char_t *Chain="gstar Cy2a tfs",Char_t *infile=0, Char_t *outfile=0, Char_t *TreeFile=0)
{
  bfc(1,Last,Chain,infile,outfile,TreeFile);
}
//_____________________________________________________________________
void bfc (const Char_t *Chain="",Char_t *infile=0, Char_t *outfile=0, Char_t *TreeFile=0)
{
  if (!Chain || !strlen(Chain)) {
    Usage();
  }
}
//____________________________________________________________
void Usage() {
#if 0
  printf ("============= \tImportant two changes:\n"
	  "              \tIt is required exact matching in Chain definition\n"
	  "              \tAll Chain options set in supplyed order\n"); 
#endif
  if (gClassTable->GetID("StBFChain") < 0) Load();
  chain = new StBFChain;
  chain->SetFlags("");
  printf ("============= \t U S A G E =============\n");
  printf ("bfc(Int_t First, Int_t Last, Char_t *Chain, Char_t *infile, Char_t *outfile,, Char_t *TreeFile)\n");
  printf ("bfc(Int_t Last, Char_t *Chain, Char_t *infile, Char_t *outfile, Char_t *TreeFile)\n");
  printf ("bfc(Char_t *Chain, Char_t *infile, Char_t *outfile)\n");
  printf ("where\n");
  printf (" First   \t- First event to process \t(Default = 1)\n");
  printf (" Last    \t- Last  event to process \t(Default = 1)\n");
  printf (" Chain   \t- Chain specification    \t(without First &  Last: Default is \"\" which gives this message)\n");
  printf ("         \t                         \t with    First || Last: Default is \"gstar tfs\")\n");
  printf (" infile  \t- Name of Input file     \t(Default = 0, i.e. use preset file names depending on Chain)\n"); 
  printf (" outfile \t- Name of Output file    \t(Default = 0, i.e. define Output file name from Input one)\n");
  printf ("Examples:\n"); 
  printf (" root4star  bfc.C                   \t// Create this message\n");
  printf (" root4star 'bfc.C(1)'               \t// Run one event with default Chain=\"gstar tfs\"\n");
  printf (" root4star 'bfc.C(1,1)'             \t// the same\n");
  printf ("\n root4star 'bfc.C(1,\"off tdaq tpc HalfField global dst display\",\"/star/rcf/daq/1999/12/st_cluster2_0351027_raw_0001.daq\")'\n");
  printf ("                                    \t// create a DST by processing a DAQ data from <st_cluster2_0351027_raw_0001.daq>\n\n");
  printf (" root4star 'bfc.C(2,40,\"Cy1b fzin\")'\t// run for configuration year_1b, \n");
  printf ("                                    \t// reading /star/rcf/disk1/star/test/psc0050_01_40evts.fzd\n");
  printf ("                                    \t// skipping the 1-st event and processing the remaining 39 events\n");
  printf (" root4star 'bfc.C(40,\"Cy1b fzin\",\"/star/rcf/disk1/star/test/psc0050_01_40evts.fzd\")'\n");
  printf (" root4star 'bfc.C(40,\"Cy1b fzin\")'\t// the same as  above\n");
  printf (" root4star 'bfc.C(2,40,\"Cy1b fzin -l3t\")'//the as above but remove L3T from chain\n");
  printf (" root4star 'bfc.C(40,\"Cy2a fzin\",\"/star/rcf/disk0/star/test/venus412/b0_3/year_2a/psc0208_01_40evts.fz\")'\n");
  printf (" root4star 'bfc.C(40,\"Cy2a fzin\")'\t// the same as  above\n");
  printf (" root4star 'bfc.C(5,10,\"Cy1b in xout\",\"/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf\")'\n");
  printf ("                                    \t// skipping the 4 events and processing the remaining 6 events\n");
  printf (" root4star 'bfc.C(1,\"off in tpc FieldOff sd97 eval\",\"Mini_Daq.xdf\")'\t// the same as Chain=\"minidaq\"\n");
  printf (" root4star 'bfc.C(1,\"gstar Cy1a tfs allevent\")' \t// run gstar and write all event into file branches\n");
  printf (" root4star 'bfc.C(1,\"off in Cy1a l3t\",\"gtrack.tpc_hits.root\")'\t// run l3t only with prepaired file\n");
  printf (" root4star 'bfc.C(1,\"tdaq display\",\"/star/rcf/disk1/star/daq/990727.3002.01.daq\")' \n");
  printf (" \t//Cosmics (56) events with full magnetic field, TPC only \n");
  printf (" root4star 'bfc.C(1,\"tdaq FieldOn\",\"/star/rcf/disk1/star/daq/990624.306.daq\")' \n");
  printf (" \t//Cosmics (56) events with full magnetic field \n");
  printf (" root4star 'bfc.C(1,\"tdaq HalfField\",\"/star/rcf/disk1/star/daq/990630.602.daq\")' \n");
  printf (" \t//Laser (10) events with half magnetic field \n");
  printf (" root4star 'bfc.C(1,\"tdaq FieldOff\",\"/star/rcf/disk1/star/daq/990701.614.daq\")' \n");
  printf (" \t//Laser (12) events with no magnetic field \n");
  gSystem->Exit(1);
}
