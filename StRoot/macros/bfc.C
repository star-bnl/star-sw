//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
//                                                                      //
// $Id: bfc.C,v 1.105 1999/08/11 13:30:31 fisyak Exp $
//////////////////////////////////////////////////////////////////////////
TBrowser *b = 0;
class StBFChain;        
StBFChain  *chain=0;
class StEvent;
StEvent *Event;
Int_t NoEvents = 0;
class St_geant_Maker;
class StIOMaker;
class St_XDFFile;
class StEventMaker; StEventMaker *evMk = 0;
//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
}
//_____________________________________________________________________
void bfc(const Int_t First,
	 const Int_t Nevents=1,
	 const Char_t *Chain="gstar tfs",Char_t *infile=0, Char_t *outfile=0)
{ // Chain variable define the chain configuration 
  // All symbols are significant (regardless of case)
  // "-" sign before requiest means that this option is disallowed
  // Chain = "gstar" run GEANT on flight with 10 muons in range |eta| < 1 amd pT = 1GeV/c (default)
  // Chain = "" || "xdf" run STANDARD chain using xd-files as an input
  // Chain = "minidaq" read miniDAQ xdf file and process 
  NoEvents = Nevents;
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();
  // Create the main chain object
  if (!chain) delete chain;
  chain = new StBFChain;
  chain->SetFlags(Chain);
  if (!Chain || !strlen(Chain)) {
#if 0
    printf ("============= \tImportant two changes:\n"
            "              \tIt is required exact matching in Chain definition\n"
            "              \tAll Chain options set in supplyed order\n"); 
#endif
    printf ("============= \t U S A G E =============\n");
    printf (
"bfc(Int_t First, Int_t Nevents, Char_t *Chain, Char_t *infile, Char_t *outfile)\n"
"bfc(Int_t Nevents, Char_t *Chain, Char_t *infile, Char_t *outfile)\n"
"bfc(Char_t *Chain, Char_t *infile, Char_t *outfile)\n"
"where\n"
" First   \t- First event to process \t(Default = 1)\n"
" Nevents \t- Total No. of events    \t(Default = 1)\n"
" Chain   \t- Chain specification    \t(without First &  Nevents: Default is \"\" which gives this message)\n"
"         \t                         \t with    First || Nevents: Default is \"gstar tfs\")\n"
" infile  \t- Name of Input file     \t(Default = 0, i.e. use preset file names depending on Chain)\n" 
" outfile \t- Name of Output file    \t(Default = 0, i.e. define Output file name from Input one)\n");
    printf (
"Examples:\n" 
" root4star  bfc.C                   \t// Create this message\n"
" root4star 'bfc.C(1)'               \t// Run one event with default Chain=\"gstar tfs\"\n"
" root4star 'bfc.C(1,1)'             \t// the same\n"
" root4star 'bfc.C(2,40,\"y1b fzin\")'\t// run for configuration year_1b, \n"
"                                    \t// reading /disk1/star/test/psc0050_01_40evts.fzd\n"
"                                    \t// skipping the 1-st event for the rest 39 events\n");
    printf (
" root4star 'bfc.C(40,\"y1b fzin\",\"/disk1/star/test/psc0050_01_40evts.fzd\")'\n"
" root4star 'bfc.C(40,\"y1b fzin\")'\t// the same as  above\n"
" root4star 'bfc.C(2,40,\"y1b fzin -l3t\")'//the as above but remove L3T from chain\n"
" root4star 'bfc.C(40,\"y2a fzin\",\"/disk0/star/test/venus412/b0_3/year_2a/psc0208_01_40evts.fz\")'\n"
" root4star 'bfc.C(40,\"y2a fzin\")'\t// the same as  above\n"
" root4star 'bfc.C(5,10,\"y1b in xout\",\"/afs/rhic/star/tpc/data/tpc_s18e_981105_03h_cos_t22_f1.xdf\")'\n"
"                                    \t// skipping the 4 events for the rest 6 events\n");
    printf (
" root4star 'bfc.C(1,\"off in tpc FieldOff sd97 eval\",\"Mini_Daq.xdf\")'\t// the same as Chain=\"minidaq\"\n"
" root4star 'bfc.C(1,\"gstar y1a tfs allevent\")' \t// run gstar and write all event into file branches\n"
" root4star 'bfc.C(1,\"off in y1a l3t\",\"gtrack.tpc_hits.root\")'\t// run l3t only with prepaired file\n");
    printf (
" root4star 'bfc.C(1,\"tdaq display\",\"/disk1/star/daq/990727.3002.01.daq\")' \n"
" \t//Cosmics (56) events with full magnetic field, TPC only \n"
" root4star 'bfc.C(1,\"tdaq FieldOn\",\"/disk1/star/daq/990624.306.daq\")' \n"
" \t//Cosmics (56) events with full magnetic field \n"
" root4star 'bfc.C(1,\"tdaq HalfField\",\"/disk1/star/daq/990630.602.daq\")' \n"
" \t//Laser (10) events with half magnetic field \n"
" root4star 'bfc.C(1,\"tdaq FieldOff\",\"/disk1/star/daq/990701.614.daq\")' \n"
" \t//Laser (12) events with no magnetic field \n");
    gSystem->Exit(1);
  }
  printf ("QAInfo:No. of Events to process = %i\n",NoEvents);
  chain->Set_IO_Files(infile,outfile);

  chain->Load();
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
  evMk  = (StEventMaker   *) chain->GetMaker("StEventMaker");  
  if (geant && First > 1) geant->Skip(First-1);
  if (inpMk && First > 1) {printf ("Skip %i Events\n",First-1);inpMk->Skip(First-1);}
  TBenchmark evnt;
  Int_t iMake = 0, i = First;
 EventLoop: if (i <= NoEvents && iMake < kStEOF) {
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
   printf ("QAInfo: Done with Event no. %d (%d) Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n",
	   i,iMake,evnt->GetRealTime("QAInfo:"),evnt->GetCpuTime("QAInfo:"));
   i++; goto EventLoop;
 }
  if (NoEvents > First || gROOT->IsBatch()) {
    chain->Finish();
    if (xdf_out) delete xdf_out;
    fflush(stdout);
    printf ("QAInfo:Run completed ");
    gSystem->Exec("date");
  }
  else {
    if (evMk) Event = (StEvent *) chain->GetInputDS("StEvent");
  }

  {
    TDatime t;
    printf ("\nQAInfo:Run is finished at Date/Time %i/%i\n",t.GetDate(),t.GetTime());
  }

}
//_____________________________________________________________________
void bfc (const Int_t Nevents, 
	  const Char_t *Chain="gstar tfs",Char_t *infile=0, Char_t *outfile=0)
{
  bfc(1,Nevents,Chain,infile,outfile);
}
//_____________________________________________________________________
void bfc (const Char_t *Chain="",Char_t *infile=0, Char_t *outfile=0)
{
  bfc(1,1,Chain,infile,outfile);
}
