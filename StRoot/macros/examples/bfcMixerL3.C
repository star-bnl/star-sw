//////////////////////////////////////////////////////////////////////////
//
//
// Macro for running chain with different inputs
//
// owner:  Yuri Fisyak
//
//
//
// $Id: bfcMixerL3.C,v 1.4 2000/09/05 21:28:09 pfachini Exp $
//////////////////////////////////////////////////////////////////////////

TBrowser *b = 0;
class StChain;
class StBFChain;
StChain  *chain=0;
class StMaker;
StMaker    *treeMk=0;
StBFChain *chain1, *chain2, *chain3;
class StEvent;
StEvent *Event;
class St_geant_Maker;
class StIOMaker;
class StEventDisplayMaker; StEventDisplayMaker *dsMk = 0;
class StEventMaker; StEventMaker *evMk = 0;
class StMixerMaker;
class StEvtHddr;
//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StBFChain");
  if (chain) delete chain;
}
//_____________________________________________________________________
void bfcMixerL3(const Int_t Nevents=9999,
             const Char_t *kind1="fz",
             const Char_t *file1="/star/rcf/data06/reco/embedding/fz/pi-/st_physics_1185015_raw_0002.fz",
             const Char_t *kind2="daq",
             const Char_t *file2="/star/rcf/data08/daq/2000/07/st_physics_1185015_raw_0002.daq")
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();
  chain = new StChain("Embedding");   // Create the main chain object

  StMaker *saveMk = 0;
  // File2 -> trs or daq
  if (!strcmp(kind2,"trs")) {
    chain2 = new StBFChain("Two");
    saveMk = chain2->cd();
    chain2->SetFlags("NoInput tpcDB tpc trs -tcl -tpt -PreVtx -tpc_daq"); //  
    chain2->Set_IO_Files(0);
    chain2->Load();
    chain2->Instantiate();
    StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Two");
    trsMk->readFile(file2);
    saveMk->cd();
  }
  if (!strcmp(kind2,"daq")) {
    chain2 = new StBFChain("Two");
    saveMk = chain2->cd();
    chain2->SetFlags("in db"); //  
    chain2->Set_IO_Files(file2);
    chain2->Load();
    chain2->Instantiate();
    saveMk->cd();
  }
  if (!strcmp(kind1,"fz")) {
    kind1="trs";
    chain1 = new StBFChain("One");
    saveMk = chain1->cd();
    chain1->SetFlags("fzin gen_T geomT sim_T tpc trs -tcl -tpt -PreVtx -tpc_daq"); // 
    chain1->Set_IO_Files(file1);
    chain1->Load();
    chain1->Instantiate();
    saveMk->cd();
  }


  // Mixer
  gSystem->Load("StMixerMaker");
  StMixerMaker  *mixer = new StMixerMaker("Mixer",kind1,kind2);

  chain1->SetInput("Input1","Event");

  if (!strcmp(kind2,"trs")) {
    chain2->SetInput("Input2","Event");
  }
  if (!strcmp(kind2,"daq")) {
    chain2->SetInput("Input2","StDAQReader");
  }
  mixer->writeFile("mixer.trs",Nevents);
  chain3 = new StBFChain("Three");
  saveMk = chain3->cd();
  //chain3->SetFlags("NoInput tpc tpc_daq cdst allevent tree");
  //chain3->SetFlags("P00h NoInput -xin GeantOut -QA -EventQA debug");
  //chain3->SetFlags("P00h NoInput l3 -in -xin -tags AllEvent");
  //chain3->SetFlags("P00h NoInput DbV0713 -y1h -in -xin -tags AllEvent");
  chain3->SetFlags("NoInput DbV0819 ry1h in tpc_daq tpc global dst Kalman Tree event evout l3 GeantOut AllEvent");
  //ry1h,in,tpc_daq,tpc,rich,trg,Cdst,Kalman,tags,Tree,evout
  TString OutputFileName(gSystem->BaseName(file2));
  OutputFileName.ReplaceAll("*","");
  OutputFileName.ReplaceAll(".daq","");
  OutputFileName.Append(".root");
  chain3->Set_IO_Files(0,OutputFileName.Data());
  chain3->Load();
  chain3->Instantiate();
  St_geant_Maker *geantMk = chain->GetMaker("geant");
  geantMk->SetActive(kTRUE);
  StMaker *tpcdaqMk = chain3->GetMaker("tpc_raw");
  tpcdaqMk->SetMode(1); // Trs
  tpcdaqMk->SetInput("Event","MixerEvent");
  saveMk->cd();
  {
    TDatime t;
    printf ("QAInfo:Run is started at Date/Time%i/%i\n",t.GetDate(),t.GetTime());
  }
  printf ("QAInfo:Run on %s in %s\n",
          gSystem->HostName(),
        gSystem->WorkingDirectory());
  printf ("QAInfo: with %s\n", chain->GetCVS());

// Init the chain and all its makers
  if (Nevents >= 0) {
    Int_t iInit = chain->Init();
  }
  //chain->SetDEBUG();
  treeMk = chain->GetMaker("tree");
  TBenchmark evnt;
  Int_t iMake = 0, i = 1, iBad = 0;
 EventLoop: if (i <= Nevents && iMake != kStEOF && iMake != kStFatal) {
   evnt->Reset();
   evnt->Start("QAInfo:");
   chain->Clear();
   iMake = chain->Make(i);
   if (treeMk && iMake == kStErr) {treeMk->Make(i); iBad++;}
   StEvtHddr *fEvtHddr = (StEvtHddr*)chain->GetDataSet("EvtHddr");
   StEvtHddr *fEvtHddrDaq = (StEvtHddr*)chain2->GetDataSet("EvtHddr");
   *fEvtHddr = *fEvtHddrDaq;
  //    gSystem->Exec("ps ux");
  evnt->Stop("QAInfo:");
  //  evnt->Show("QAInfo:");
  printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time%d.%d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n",
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
    printf ("\nQAInfo:Run is finished at Date/Time%i/%i\n",t.GetDate(),t.GetTime());
  }
}
