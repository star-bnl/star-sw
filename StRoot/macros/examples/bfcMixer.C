//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer.C,v 1.19 2004/09/01 14:35:00 jeromel Exp $
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
class StFtpcMixerMaker;
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
void bfcMixer(const Int_t Nevents=10,
	      const Char_t *file1="/star/rcf/test/embedding/st_physics_2270008_raw_0030.daq",
	      const Char_t *file2="/star/rcf/test/embedding/gtest.fz",
	      const Char_t *file3="/star/rcf/test/embedding/st_physics_2270008_raw_0030.vertices.dat",
	      const Float_t zvertex_low=-175.0,
	      const Float_t zvertex_high=175.0,
	      const Char_t *mode="strange",
	      const Char_t *acc_mode="off",
	      const Int_t  doFCF=1,
	      const Int_t  doITTF=1)
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();

  // Create the main chain object
  chain = new StChain("Embedding");

  StMaker *saveMk = 0;

  // Create chain1 object
  chain1 = new StBFChain(doITTF?2:1,"One");
  saveMk = chain1->cd();

  // WARNING !!
  //   (1) Timestamp is hardcoded making option tracking hard to impossible
  //   (2) Timestamp MUST match between chain 1 and chain 2 / changed logic to avoid mess
  //
  TString TheChain1("in Physics NoDefault");
  TString DBTimeStamp(" DbV20030408");       // beware of needed leading space
  TheChain1 += DBTimeStamp;

  chain1->SetFlags(TheChain1);
  chain1->Set_IO_Files(file1);
  chain1->Load();
  chain1->Instantiate();

  saveMk->cd();
  
  // Create chain2 object
  chain2 = new StBFChain(doITTF?2:1,"Two");
  saveMk = chain2->cd();

  // WARNING !!
  //   As provided, the timestamp is hardcoded. Option tracking becomes
  //   hard if not impossible.
  //
  TString TheChain2("fzin gen_T geomT sim_T trs -tcl -tpt -PreVtx -tpc_daq fss ftpcT");
  if (doITTF) TheChain2 += " tpcI";
  else        TheChain2 += " tpc";
  TheChain2 += DBTimeStamp;

  chain2->SetFlags(TheChain2);  
  chain2->Set_IO_Files(file2);
  chain2->Load();
  chain2->Instantiate();
  St_geant_Maker *geantMk = chain2->GetMaker("geant");
  if (geantMk) geantMk->SetMode(1);   // Mixer mode - do not modify EvtHddr
  
  if (chain2->GetOption("TRS")){
    StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
    trsMk->setNormalFactor(1.23);
  }
  
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
  if (doFCF)
    mixer->SetSequenceMerging(0);               

  gSystem->Load("StFtpcMixerMaker");
  StFtpcMixerMaker  *ftpcmixer = new StFtpcMixerMaker("FtpcMixer","daq","trs");

  // Create chain3 object
  chain3 = new StBFChain(doITTF?2:1,"Three");
  saveMk = chain3->cd();


  // WARNING   !!
  //  (1) This macro was left as-is and as-provided. However, ppOpt appears valid
  //      ONLY for pp and d+Au samples.
  //  (2) In ITTF mode, there are a few other caveats
  //      a- VertexFinder is a generic one  VFMinuit is the default (good for simulation, 
  //         Au+Au). Others may be used depending on sample.
  //      b- SvtI option is not turn ON by default as it stands below. Tracking
  //         with SVT will NOT happen until then.
  //  (3) Option QA is obsolete. If used, global would be back in the chain and would
  //      class with ITTF
  //
  TString TheChain3("Simu ppOpt beamline NoDefault NoInput db tpc_daq ftpc emcDY2 event evout EventQA Tree GeantOut ctf tofDat -Prevtx");
  if (doFCF)  TheChain3 += " -tcl fcf";
  else        TheChain3 += " tcl";
  if (doITTF) TheChain3 += " Idst tpcI ITTF";
  else        TheChain3 += " dst tpc global Kalman";

  chain3->SetFlags(TheChain3.Data());


  TString OutputFileName(gSystem->BaseName(file1));
  OutputFileName.ReplaceAll("*","");
  OutputFileName.ReplaceAll(".daq","");
  OutputFileName.Append(".root");
  chain3->Set_IO_Files(0,OutputFileName.Data());
  chain3->Load();
  chain3->Instantiate();
  //chain3->PrintInfo();

  St_geant_Maker *geantMk = (St_geant_Maker *) chain->GetMaker("geant");
  geantMk->SetActive(kTRUE);
  StMaker *tpcdaqMk = chain3->GetMaker("tpc_raw");
  tpcdaqMk->SetMode(1);   // Trs
  tpcdaqMk->SetInput("Event","MixerEvent");

  StMaker *ftpccluMk = chain3->GetMaker("ftpc_hits");
  ftpccluMk->SetInput("ftpc_raw","FtpcMixer");

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

  // vtxMk = (StVertexMaker*) chain3->GetMaker("vertex");

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

   use this to force the vertex position to match:
   // vtxMk->FixVertex(x,y,z);

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


   // This doesn't work...  elh
   // TString *fnam = OutputFileName.Data();
   // TFile *fileptr = TFILE::Open(fnam);
   // TTree *tree = (TTree*)fileptr->Get("Tag");
   // tree->GetEvent(i);
   // Float_t zvert = tree->GetLeaf("primaryVertexX")->GetValue();
   // printf("zvert = %f\n",zvert);

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

