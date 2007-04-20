//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_v4.C,v 1.1 2007/04/20 00:11:13 andrewar Exp $
//
//////////////////////////////////////////////////////////////////////////

//TBrowser *b = 0;
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
  //Extra things to load for the acceptance filter
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StV0AccMaker.so");

  if (chain) delete chain;
}
//_____________________________________________________________________
void bfcMixer_v4(const Int_t Nevents=10,
             const Char_t *file1="/beta/starprod/daq/2001/ProductionMinBias/FullField/st_physics_2275002_raw_0001.daq",
	     const Char_t *file2="/home/starofl/embedding/GSTAR/GSTAR.test.2275002_0001.13350.fz",
             const Char_t *file3="/home/starofl/embedding/P02ge/st_physics_2275002_raw_0001.vertices.dat",
             const Float_t zvertex_low=-100.0,
             const Float_t zvertex_high=100.0,
	     const Char_t *mode="strange",
	     const Char_t *acc_mode="off" )
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();

  // Create the main chain object
  chain = new StChain("Embedding");

  StMaker *saveMk = 0;

  // Create chain1 object
  chain1 = new StBFChain("One");
  saveMk = chain1->cd();
  chain1->SetFlags("in Physics NoDefault");
  chain1->Set_IO_Files(file1);
  chain1->Load();
  chain1->Instantiate();

  saveMk->cd();
  
  // Create chain2 object
  chain2 = new StBFChain("Two");
  saveMk = chain2->cd();
  chain2->SetFlags("fzin gen_T geomT sim_T tpc trs -tcl -tpt -PreVtx -tpc_daq");   // 
  chain2->Set_IO_Files(file2);
  chain2->Load();
  chain2->Instantiate();
  St_geant_Maker *geantMk = chain2->GetMaker("geant");
  if (geantMk) geantMk->SetMode(1);   // Mixer mode - do not modify EvtHddr
  
  // Add the acceptance filter maker before TRS  
  if (!strcmp(mode,"strange")){
    if (!strcmp(acc_mode,"on")){
      
      Char_t *extraMaker = "StV0AccMaker";
      if (gClassTable->GetID(extraMaker) < 0) gSystem->Load(extraMaker);
      StMaker *extraMk = (StMaker *)chain1->GetMaker(extraMaker);
      if(extraMk) delete extraMk;
      extraMk = chain->New(extraMaker,"before");
      if (extraMk) {
	Char_t *before = "Trs";
	StMaker *trsmk = chain1->GetMaker(before);
	if (trsmk) chain1->AddBefore(before,extraMk);
	StV0AccCuts *cuts = ((StV0AccMaker *)extraMk)->GetCutsPtr();
	cuts->SetFilter();
	cuts->SetV0MinDecayLen(0.);
	cuts->SetV0DaughterMinImpact(0);
	cuts->SetV0DaughterMinHit(10.);
	cuts->SetXiV0MaxImpact(5);
	cuts->SetXiMinDecayLen(2.);
	cuts->SetXiV0PiMinImpact(0.);
	cuts->SetXiDaughterMinHit(10.);
	cuts->SetKinkMinDecayRad(128.);
	cuts->SetKinkMaxDecayRad(184.);
      }
    }
  }
  // end additional maker code

  saveMk->cd();

  // Mixer
  gSystem->Load("StMixerMaker");
  StMixerMaker  *mixer = new StMixerMaker("Mixer","daq","trs");
  chain1->SetInput("Input1","StDAQReader");
  chain2->SetInput("Input2","Event");
  mixer->writeFile("mixer.trs",Nevents);

  // Create chain3 object
  chain3 = new StBFChain("Three");
  saveMk = chain3->cd();
  chain3->SetFlags("Simu DbV20020226 NoDefault NoInput db tpc_daq tpc global dst Kalman event QA Tree GeantOut evout"); // removed -nohits option on 20/2/03 - MACL
  //  chain3->SetFlags("Simu ppOpt DbV20020402 beamline NoDefault NoInput db tpc_daq tpc global dst Kalman Cdst event QA Tree GeantOut"); 
    printf ("ELH mark1\n");

  TString OutputFileName(gSystem->BaseName(file1));
    printf ("ELH mark1\n");
  OutputFileName.ReplaceAll("*","");
    printf ("ELH mark1\n");
  OutputFileName.ReplaceAll(".daq","");
    printf ("ELH mark1\n");
  OutputFileName.Append(".root");
    printf ("ELH mark1\n");
  chain3->Set_IO_Files(0,OutputFileName.Data());
    printf ("ELH mark1\n");
  chain3->Load();
    printf ("ELH mark1\n");
  chain3->Instantiate();
    printf ("ELH mark1\n");
  St_geant_Maker *geantMk = (St_geant_Maker *) chain->GetMaker("geant");
    printf ("ELH mark1\n");
  geantMk->SetActive(kTRUE);
    printf ("ELH mark1\n");
  StMaker *tpcdaqMk = chain3->GetMaker("tpc_raw");
    printf ("ELH mark1\n");

    //  if(chain3->GetOption("ittf")){
    //    printf ("ITTF option active\n");
    //  }else{
    //    printf ("ITTF option not active\n");
    //  }

  tpcdaqMk->SetMode(1);   // Trs
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
  // chain->SetDEBUG();
  treeMk = chain->GetMaker("tree");
  TBenchmark evnt;
  Int_t iMake = 0, i = 1, iBad = 0;

  StIOMaker *inpMk = (StIOMaker *)chain1->GetMaker("inputStream");
  FILE *fp = fopen(file3,"r");
  Float_t x, y, z;
  Int_t ncols, eventnumber, mult, skip=0, oldskip = 0, skiptest=0;
  printf("zvertex_low = %f zvertex_high = %f\n",zvertex_low, zvertex_high);

  vtxMk = (StVertexMaker*) chain3->GetMaker("Vertex");

EventLoop: if (i <= Nevents && iMake != kStEOF && iMake != kStFatal) {
   evnt->Reset();
   evnt->Start("QAInfo:");
   chain->Clear();
   
   ncols = fscanf(fp,"%d %d %d %f %f %f",&eventnumber,&skip,&mult,&x,&y,&z);
   if(ncols<0) break;

   printf("\nEvent number: %d Multiplicity = %d\n",eventnumber,mult);
   printf("vertex in bfcMixer: %16f %16f %16f\n",x,y,z);

   // skip events in chain1 (daq file) only.
   // fz file does not have these events.
   // strange initialization due to details of inpMk->Skip
   // ...not well understood.  elh

   printf ("bfcMixer: i = %i\n",i);
   printf ("bfcMixer: skip = %i\n",skip);
   printf ("bfcMixer: oldskip = %i\n",oldskip);
   if (inpMk && skip>0) {
     if(i == 1) {skip++;}
     skiptest = inpMk->Skip(skip-oldskip-1);
     skiptest = inpMk->Skip();
     printf("bfcMixer: skiptest = %i\n",skiptest);
     if(i == 1) {skip--;}
   }
   oldskip = skip;

   // use this to force the vertex position to match:
   vtxMk->FixVertex(x,y,z);
   printf ("bfcMixer: fixing vertex to %f %f %f\n",x,y,z);

   iMake = chain->Make(i);
   if (treeMk && iMake == kStErr) {treeMk->Make(i); iBad++;}
   StEvtHddr *fEvtHddr = (StEvtHddr*)chain->GetDataSet("EvtHddr");
   StEvtHddr *fEvtHddrDaq = (StEvtHddr*)chain1->GetDataSet("EvtHddr");
   *fEvtHddr = *fEvtHddrDaq;
   // gSystem->Exec("ps ux");
   evnt->Stop("QAInfo:");
   // evnt->Show("QAInfo:");
   printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time%d.%d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n", i,chain->GetRunNumber(),chain->GetEventNumber(),chain->GetDate(), chain->GetTime(),
	  iMake,evnt->GetRealTime("QAInfo:"),evnt->GetCpuTime("QAInfo:"));

   chain->ls(9);

   // Be sure to get the same event from daq and fz files...  elh
   if(eventnumber == chain->GetEventNumber()){
     printf("bfcMixer:  Event number %d found in both .fz file and .daq file\n",eventnumber);
   }else{
     printf("bfcMixer: Error! Event number from .fz file: %d\n",eventnumber);
     printf("bfcMixer: Error! Event number from .daq file: %d\n",chain->GetEventNumber());
     printf("bfcMixer: Error! Events are out-of-sync, exiting!\n");
     break;
   }
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


