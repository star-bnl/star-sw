//////////////////////////////////////////////////////////////////////////
//
//
// Macro for running chain with different inputs
//
// owner:  Yuri Fisyak
//
//
//
// $Id: bfcMixer_v4.C,v 1.2 2007/04/20 00:12:32 andrewar Exp $
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
void bfcMixer_v4(const Int_t Nevents=25,
             const Char_t *kind1="fz",
             const Char_t *file1="ppvertices.fz",
             const Char_t *kind2="trs",
             const Char_t *file2="/beta/starprod/abortgap/mixer3004016.trs",
             const Char_t *file3="/auto/pdsfdv09/starprod/tags/P02ge/2001/hijingabort_3004016_realvtx.6228.tags.root",
             const Float_t zvertex_low=-50.0,
             const Float_t zvertex_high=50.0)
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
    //    chain2->SetFlags("in db"); // P00hm 
    //    chain2->SetFlags("in db NoDefault"); // P01he gstar for P00hm 
    //    chain2->SetFlags("in DbV0523 HalfField db NoDefault"); // P01he  
    chain2->SetFlags("in db NoDefault"); // P01hj  
    chain2->Set_IO_Files(file2);
    chain2->Load();
    chain2->Instantiate();
    saveMk->cd();
  }
  if (!strcmp(kind1,"fz")) {
    kind1="trs";
    chain1 = new StBFChain("One");
    saveMk = chain1->cd();
    chain1->SetFlags("fzin gen_T geomT sim_T tpc trs rrs -tcl -tpt -PreVtx -tpc_daq -Mixer"); // 
    chain1->Set_IO_Files(file1);
    chain1->Load();
    chain1->Instantiate();
    saveMk->cd();
  }

  // Mixer
  gSystem->Load("StMixerMaker");
  StMixerMaker  *mixer = new StMixerMaker("Mixer",kind1,kind2);

  cout << "Load the rich mixer maker" << endl;
  gSystem->Load("StEvent");
  gSystem->Load("StRichMixerMaker");
  StRichMixerMaker *richmixer = new StRichMixerMaker("richMixer");

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
  // P00hm:
  //     chain3->SetFlags("NoInput in tpc_daq tpc global dst Kalman Tree GeantOut");
  // P01he:
     //  chain3->SetFlags("NoInput HalfField DbV0523 tpc_daq tpc global dst Kalman Tree GeantOut"); 
  // P01hj: (took out DbV1007 rich tags Physics evout Nohits, etc.)
  chain3->SetFlags("Simu ppOpt DbV20020402 beamLine NoDefault NoInput db tpc_daq tpc global dst Kalman Cdst Tree GeantOut evout"); 

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

  StIOMaker *inpMk = (StIOMaker *)chain2->GetMaker("inputStream");
  FILE *fp = fopen(file3,"r");
  Float_t x, y, z;
  Int_t ncols, eventnumber, mult, skip=0, oldskip = 0, skiptest=0;
  printf("zvertex_low = %f zvertex_high = %f\n",zvertex_low, zvertex_high);

  primMk = (StPrimaryMaker*) chain3->GetMaker("primary");

 EventLoop: if (i <= Nevents && iMake != kStEOF && iMake != kStFatal) {
   evnt->Reset();
   evnt->Start("QAInfo:");
   chain->Clear();

   //   ncols = fscanf(fp,"%d %d %d %f %f %f",&eventnumber,&skip,&mult,&x,&y,&z);
   //   if(ncols<0) break;

   //   printf("\nEvent number: %d Multiplicity = %d\n",eventnumber,mult);
   //   printf("vertex in bfcMixer: %16f %16f %16f\n",x,y,z);

   // skip events in chain2 (daq file) only.
   // fz file does not have these events.
   // strange initialization due to details of inpMk->Skip
   // ...not well understood.  elh

   //   printf ("bfcMixer: skip = %i\n",skip);
   //   printf ("bfcMixer: oldskip = %i\n",oldskip);
   //   if (inpMk && skip>0) {
   //     if(i == 1) {skip++;}
   //     skiptest = inpMk->Skip(skip-oldskip-1);
   //     printf("bfcMixer: skiptest = %i\n",skiptest);
   //     if(i == 1) {skip--;}
   //   }
   //   oldskip = skip;

   //  use this to force the vertex position to match:
   //   primMk->FixVertex(x,y,z);

   iMake = chain->Make(i);
   if (treeMk && iMake == kStErr) {treeMk->Make(i); iBad++;}
   StEvtHddr *fEvtHddr = (StEvtHddr*)chain->GetDataSet("EvtHddr");
   StEvtHddr *fEvtHddrDaq = (StEvtHddr*)chain2->GetDataSet("EvtHddr");
   *fEvtHddr = *fEvtHddrDaq;
   //    gSystem->Exec("ps ux");
   evnt->Stop("QAInfo:");
   //evnt->Show("QAInfo:");
   printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time%d.%d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n", i,chain->GetRunNumber(),chain->GetEventNumber(),chain->GetDate(), chain->GetTime(),
	  iMake,evnt->GetRealTime("QAInfo:"),evnt->GetCpuTime("QAInfo:"));

   // Be sure to get the same event from daq and fz files...  elh
   //   if(eventnumber == chain->GetEventNumber()){
   //     printf("bfcMixer:  Event number %d found in both .fz file and .daq file\n",eventnumber);
   //   }else{
   //     printf("bfcMixer: Error! Event number from .fz file: %d\n",eventnumber);
   //     printf("bfcMixer: Error! Event number from .daq file: %d\n",chain->GetEventNumber());
   //     printf("bfcMixer: Error! Events are out-of-sync, exiting!\n");
   //     break;
   //   }
   i++;
   goto EventLoop;
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

