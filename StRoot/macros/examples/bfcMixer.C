//////////////////////////////////////////////////////////////////////////
//
//
// Macro for running chain with different inputs
//
// owner:  Yuri Fisyak
//
//
//
// $Id: bfcMixer.C,v 1.1 2000/03/16 00:36:47 fisyak Exp $
//////////////////////////////////////////////////////////////////////////

TBrowser *b = 0;
class StChain;
class StBFChain;
StChain  *chain=0;
StBFChain *chain1, *chain2, *chain3;
class StEvent;
StEvent *Event;
class St_geant_Maker;
class StIOMaker;
class StEventDisplayMaker; StEventDisplayMaker *dsMk = 0;
class StEventMaker; StEventMaker *evMk = 0;
class StMixerMaker;
//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StBFChain");
  if (chain) delete chain;
}
//_____________________________________________________________________
void bfcMixer(const Int_t Nevents=1,
             const Char_t *kind1="fz",
             const Char_t *file1="/afs/rhic/star/data1/pfachini/Mixer/Files/Pion1Evt.fz",
             const Char_t *kind2="trs",
             const Char_t *file2="/afs/rhic/star/data1/pfachini/Mixer/Files/hij_1evt.trs")
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();
  chain = new StChain("Embedding");   // Create the main chain object
  if (!chain) gSystem->Exit(1);
  StMaker *saveMk = 0;
  if (!strcmp(kind1,"fz")) {
    kind1="trs";
    chain1 = new StBFChain("One");
    saveMk = chain1->cd();
    chain1->SetFlags("fzin y1h gen_T geomT sim_T tpc trs -tcl -tpt -PreVtx -tpc_daq"); // 
    chain1->Set_IO_Files(file1);
    chain1->Load();
    chain1->Instantiate();
    saveMk->cd();
  }

   if (!strcmp(kind2,"trs")) {
    chain2 = new StBFChain("Two");
    saveMk = chain2->cd();
    chain2->SetFlags("NoInput tpcDB tpc trs -tcl -tpt -PreVtx -tpc_daq"); //  
    chain2->Set_IO_Files(0);
    chain2->Load();
    chain2->Instantiate();
    StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Two/.make/tpc/.make/Trs");
    trsMk->readFile(file2);
 
    saveMk->cd();
  }

  // Mixer
  gSystem->Load("StMixerMaker");
  StMixerMaker  *mixer = new StMixerMaker("Mixer",kind1,kind2);
  //  mixer->SetInput("Input1","mixer/.make/DaqFirst/.const/StDAQReader");
  mixer->SetInput("Input1","Embedding/.make/One/.make/tpc/.make/Trs/.const/Event");
  //  mixer->SetInput("Input2","mixer/.make/DaqSecond/.const/StDAQReader");
  mixer->SetInput("Input2","Embedding/.make/Two/.make/tpc/.make/Trs/.const/Event");
  mixer->writeFile("mixer.trs",Nevents);
  chain3 = new StBFChain("Three");
  saveMk = chain3->cd();
  chain3->SetFlags("NoInput cy1h tpc_daq ");
  chain3->Set_IO_Files(0,"output");
  chain3->Load();
  chain3->Instantiate();
  //  St_geant_Maker *geantMk = chain->GetMaker("geant");
  //  geantMk->SetActive(kTRUE);
  StMaker *tpcdaqMk = chain3->GetMaker("tpc_raw");
  tpcdaqMk->SetMode(1); // Trs
  tpcdaqMk->SetInput("Event","Embedding/.make/Mixer/.const/MixerEvent");
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
  TBenchmark evnt;
  Int_t iMake = 0, i = 1;
 EventLoop: if (i <= Nevents && iMake != kStEOF && iMake != kStFatal) {
   evnt->Reset();
   evnt->Start("QAInfo:");
   chain->Clear();
   iMake = chain->Make(i);
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
