//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
//                                                                      //
// $Id: bfc.C,v 1.115 1999/11/04 22:21:48 fisyak Exp $
//////////////////////////////////////////////////////////////////////////
TBrowser *b = 0;
class StBFChain;        
StBFChain  *chain=0;
class StEvent;
StEvent *Event;
class St_geant_Maker;
class StIOMaker;
class St_XDFFile;
class StEventMaker; StEventMaker *evMk = 0;
//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StUtilities");
  gSystem->Load("StChain");
  gSystem->Load("StBFChain");}
//_____________________________________________________________________
void bfc(const Int_t First,
	 const Int_t Last,
	 const Char_t *Chain="gstar tfs",Char_t *infile=0, Char_t *outfile=0)
{ // Chain variable define the chain configuration 
  // All symbols are significant (regardless of case)
  // "-" sign before requiest means that this option is disallowed
  // Chain = "gstar" run GEANT on flight with 10 muons in range |eta| < 1 amd pT = 1GeV/c (default)
  // Chain = "" || "xdf" run STANDARD chain using xd-files as an input
  // Chain = "minidaq" read miniDAQ xdf file and process 
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();
  // Create the main chain object
  if (!chain) delete chain;
  chain = new StBFChain;
  chain->SetFlags(Chain);
  printf ("QAInfo:Process [First=%6i/Last=%6i/Total=%6i] Events\n",First,Last,Last-First+1);
  chain->Set_IO_Files(infile,outfile);

  chain->Load();
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
  Int_t iInit = chain->Init();
  // skip if any
  St_geant_Maker *geant = (St_geant_Maker *) chain->GetMaker("geant");
  StIOMaker *inpMk      = (StIOMaker *)      chain->GetMaker("inputStream");
  St_XDFFile *xdf_out = chain->GetXdfOut();
  if (chain->GetOption("Event")) evMk  = (StEventMaker   *) chain->GetMaker("StEventMaker");  
  if (geant && First > 1) geant->Skip(First-1);
  if (inpMk && First > 1) {printf ("Skip %i Events\n",First-1);inpMk->Skip(First-1);}
  TBenchmark evnt;
  Int_t iMake = 0, i = First;
 EventLoop: if (i <= Last && iMake < kStEOF) {
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
   evnt->Show("QAInfo:");
   printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n",
	   i,chain->GetRunNumber(),chain->GetEventNumber(),
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
	  const Char_t *Chain="gstar tfs",Char_t *infile=0, Char_t *outfile=0)
{
  bfc(1,Last,Chain,infile,outfile);
}
//_____________________________________________________________________
void bfc (const Char_t *Chain="",Char_t *infile=0, Char_t *outfile=0)
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
  // Create the main chain object
  if (!chain) delete chain;
  chain = new StBFChain;
  chain->SetFlags("");
  printf ("============= \t U S A G E =============\n");
  printf ("bfc(Int_t First, Int_t Last, Char_t *Chain, Char_t *infile, Char_t *outfile)\n");
  printf ("bfc(Int_t Last, Char_t *Chain, Char_t *infile, Char_t *outfile)\n");
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
  printf (" root4star 'bfc.C(2,40,\"y1b fzin\")'\t// run for configuration year_1b, \n");
  printf ("                                    \t// reading /disk1/star/test/psc0050_01_40evts.fzd\n");
  printf ("                                    \t// skipping the 1-st event and processing the remaining 39 events\n");
  printf (" root4star 'bfc.C(40,\"y1b fzin\",\"/disk1/star/test/psc0050_01_40evts.fzd\")'\n");
  printf (" root4star 'bfc.C(40,\"y1b fzin\")'\t// the same as  above\n");
  printf (" root4star 'bfc.C(2,40,\"y1b fzin -l3t\")'//the as above but remove L3T from chain\n");
  printf (" root4star 'bfc.C(40,\"y2a fzin\",\"/disk0/star/test/venus412/b0_3/year_2a/psc0208_01_40evts.fz\")'\n");
  printf (" root4star 'bfc.C(40,\"y2a fzin\")'\t// the same as  above\n");
  printf (" root4star 'bfc.C(5,10,\"y1b in xout\",\"/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf\")'\n");
  printf ("                                    \t// skipping the 4 events and processing the remaining 6 events\n");
  printf (" root4star 'bfc.C(1,\"off in tpc FieldOff sd97 eval\",\"Mini_Daq.xdf\")'\t// the same as Chain=\"minidaq\"\n");
  printf (" root4star 'bfc.C(1,\"gstar y1a tfs allevent\")' \t// run gstar and write all event into file branches\n");
  printf (" root4star 'bfc.C(1,\"off in y1a l3t\",\"gtrack.tpc_hits.root\")'\t// run l3t only with prepaired file\n");
  printf (" root4star 'bfc.C(1,\"tdaq display\",\"/disk1/star/daq/990727.3002.01.daq\")' \n");
  printf (" \t//Cosmics (56) events with full magnetic field, TPC only \n");
  printf (" root4star 'bfc.C(1,\"tdaq FieldOn\",\"/disk1/star/daq/990624.306.daq\")' \n");
  printf (" \t//Cosmics (56) events with full magnetic field \n");
  printf (" root4star 'bfc.C(1,\"tdaq HalfField\",\"/disk1/star/daq/990630.602.daq\")' \n");
  printf (" \t//Laser (10) events with half magnetic field \n");
  printf (" root4star 'bfc.C(1,\"tdaq FieldOff\",\"/disk1/star/daq/990701.614.daq\")' \n");
  printf (" \t//Laser (12) events with no magnetic field \n");
  gSystem->Exit(1);
}
