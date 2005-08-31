//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer.C,v 1.21 2005/08/31 19:47:14 perev Exp $
//
//////////////////////////////////////////////////////////////////////////
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Stiostream.h"
#include "TSystem.h"
#include "TClassTable.h"
#include "TApplication.h"
#include "TInterpreter.h"
#include "TBenchmark.h"
#include "StBFChain.h"
//#include "StEvent.h"
//#include "St_geant_Maker/St_geant_Maker.h"
#include "StIOMaker/StIOMaker.h"
//#include "StEventMaker/StEventMaker.h"
#include "StMixerMaker/StMixerMaker.h"
#include "StEvtHddr.h"
#include "StTrsMaker/StTrsMaker.h"
#else 
class StMaker;
class StChain;
class StBFChain;
class StEvent;
//class St_geant_Maker;
class StIOMaker;
class StEventMaker; 
class StMixerMaker;
class StEvtHddr;
class StMixerMaker;
//class StFtpcMixerMaker;
class StTrsMaker;
#endif
StChain  *chain=0;
StMaker  *treeMk=0;
StBFChain *chain1, *chain2, *chain3;
//StEvent *Event;
//StEventMaker *evMk = 0;
//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  Bool_t needLogger = gSystem->Load("liblog4cxx.so") <= 0; 
  if (needLogger) {
    gSystem->Load("StStarLogger.so");
    //    gInterpreter->ProcessLine("StLoggerManager::StarLoggerInit();");//    TMemStat::PrintMem("load StStarLogger");
    gROOT->ProcessLine("StLoggerManager::StarLoggerInit();");//    TMemStat::PrintMem("load StStarLogger");
 }
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StBFChain");
  //Extra things to load for the acceptance filter
#if 0
  gSystem->Load("StarClassLibrary");
#endif
  gSystem->Load("StAnalysisUtilities");

  if (chain) delete chain;
}
//_____________________________________________________________________
void bfcMixer(const Int_t Nevents=100,
	      const Char_t *file1="/star/data03/daq/2004/090/st_physics_adc_5090009_raw_2060002.daq",
	      const Char_t *file2="./gtest.fz",
	      const Char_t *file3="./st_physics_adc_5090009_raw_2060002.vertices.dat",
	      const Float_t zvertex_low=-175.0,
	      const Float_t zvertex_high=175.0,
	      const Char_t *mode="strange",
	      const Char_t *acc_mode="off" )
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();
  Int_t iV0Acc = 0;
  if (! strcmp(mode,"strange") && !strcmp(acc_mode,"on") && gSystem->DynamicPathName("StV0AccMaker",1)) {
      gSystem->Load("StV0AccMaker.so");
      iV0Acc = 1;
  }
  // Create the main chain object
  chain = new StChain("Embedding");

  StMaker *saveMk = 0;

  // Create chain1 object
  chain1 = new StBFChain("One");
  saveMk = chain1->cd();
  //  chain1->SetFlags("in NoDefault");
  //  chain1->SetFlags("in alltrigger NoDefault");
  //  chain1->SetFlags("in Physics DbV20020226 NoDefault");
  chain1->SetFlags("in Physics DbV20050515 MagF NoDefault");
  chain1->Set_IO_Files(file1);
  chain1->Load();
  chain1->Instantiate();

  saveMk->cd();
  
  // Create chain2 object
  chain2 = new StBFChain("Two");
  saveMk = chain2->cd();
  //  chain2->SetFlags("fzin DbV20020226 gen_T geomT sim_T tpc trs -tcl -tpt -PreVtx -tpc_daq");   // 
  chain2->SetFlags("fzin DbV20050515 gen_T geomT sim_T trs -tpc_daq");   // 
  chain2->Set_IO_Files(file2);
  chain2->Load();
  chain2->Instantiate();
  StMaker *geantMk = chain2->GetMaker("geant");
  if (geantMk) geantMk->SetMode(1);   // Mixer mode - do not modify EvtHddr
  
  if (chain2->GetOption("TRS")){
    StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
    trsMk->setNormalFactor(1.22);
  }
  // Add the acceptance filter maker before TRS  
  if (iV0Acc) {
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
  // end additional maker code

  saveMk->cd();

  // Mixer
  gSystem->Load("StMixerMaker");
  StMixerMaker  *mixer = new StMixerMaker("Mixer","daq","trs");
  chain1->SetInput("Input1","StDAQReader");
  chain2->SetInput("Input2","Event");
  mixer->writeFile("mixer.trs",Nevents);

  //  gSystem->Load("StFtpcMixerMaker");
  //  StFtpcMixerMaker  *ftpcmixer = new StFtpcMixerMaker("FtpcMixer","daq","trs");

  // Create chain3 object
  chain3 = new StBFChain("Three");
  saveMk = chain3->cd();

  // use Simu NoDefault NoInput onlraw -onlcl and standard chain options
  // then take apart e.g. P2004/B2004 and remove corrections as well as 
  // in, physics, analyis and Event QA from Cdst, tags, SCEbyE
  // also don't use hitfilt
  //  took out svtdEdx, emcDY2 too

  //  chain3->SetFlags("Simu NoDefault NoInput onlraw -onlcl DbV20050515 ry2005b tpc_daq tpc svt_daq SvtD event Kalman Tree evout useCDV SCEbyE tofdat EST xiSvt pmdRaw Xi2 V02 Kink2 CMuDst");

  chain3->SetFlags("Simu NoDefault NoInput onlraw -onlcl DbV20050515 ry2005b tpc_daq tpc emcDY2 global dst Kalman event evout QA Tree GeantOut fcf ctf -Prevtx -nohits CMuDST ZDCvtx tofDat Xi2 Kink2 EST ToF svt_daq SvtD svtdEdx xiSvt l3onl fpd eemcD pmdRaw analysis "); 

  //  StRTSClientFCF *fcfMk = (StRTSClientFCF *) chain3->GetMaker("");
  //  fcfMk->SetMode("0x1");


  TString OutputFileName(gSystem->BaseName(file1));
  OutputFileName.ReplaceAll("*","");
  OutputFileName.ReplaceAll(".daq","");
  OutputFileName.Append(".root");
  chain3->Set_IO_Files(0,OutputFileName.Data());
  chain3->Load();
  chain3->Instantiate();
#if 0
  geantMk = chain->GetMaker("geant");
  geantMk->SetActive(kTRUE);
#endif
  StMaker *tpcdaqMk = chain3->GetMaker("tpc_raw");
  tpcdaqMk->SetMode(1);   // Trs
  tpcdaqMk->SetInput("Event","MixerEvent");

  //  StMaker *ftpccluMk = chain3->GetMaker("ftpc_hits");
  //  ftpccluMk->SetInput("ftpc_raw","FtpcMixer");

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
    chain->Init();
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
   evnt.Reset();
   evnt.Start("QAInfo:");
  printf ("ELH3\n");
   chain->Clear();
  printf ("ELH4\n");
   
   ncols = fscanf(fp,"%d %d %d %f %f %f",&eventnumber,&skip,&mult,&x,&y,&z);
   if(ncols<0) goto EndLoop;;

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
     printf("bfcMixer: skiptest1 = %i\n",skiptest);
     skiptest = inpMk->Skip();
     printf("bfcMixer: skiptest2 = %i\n",skiptest);
     if(i == 1) {skip--;}
   }
   oldskip = skip;

   //   use this to force the vertex position to match:
   // vtxMk->FixVertex(x,y,z);

   iMake = chain->Make(i);
   if (treeMk && iMake == kStErr) {treeMk->Make(); iBad++;}
   StEvtHddr *fEvtHddr = (StEvtHddr*)chain->GetDataSet("EvtHddr");
   StEvtHddr *fEvtHddrDaq = (StEvtHddr*)chain1->GetDataSet("EvtHddr");
   *fEvtHddr = *fEvtHddrDaq;
   // gSystem->Exec("ps ux");
   evnt.Stop("QAInfo:");
   // evnt->Show("QAInfo:");
   printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time%d.%d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n", i,chain->GetRunNumber(),chain->GetEventNumber(),chain->GetDate(), chain->GetTime(),
	  iMake,evnt.GetRealTime("QAInfo:"),evnt.GetCpuTime("QAInfo:"));


   // Be sure to get the same event from daq and fz files...  elh
   if(eventnumber == chain->GetEventNumber()){
     printf("bfcMixer:  Event number %d found in both .fz file and .daq file\n",eventnumber);
   }else{
     printf("bfcMixer: Error! Event number from .fz file: %d\n",eventnumber);
     printf("bfcMixer: Error! Event number from .daq file: %d\n",chain->GetEventNumber());
     printf("bfcMixer: Error! Events are out-of-sync, exiting!\n");
     goto EndLoop;;
   }
   i++;
   goto EventLoop;
 }
 EndLoop:
  fflush(stdout);
  printf ("QAInfo:Run completed ");
  gSystem->Exec("date");
  //  if (evMk) Event = (StEvent *) chain->GetInputDS("StEvent");
  {
    TDatime t;
    printf ("\nQAInfo:Run is finished at Date/Time%i/%i\n",t.GetDate(),t.GetTime());
  }
}

