//////////////////////////////////////////////////////////////////////////
//
// Macro for embedding on TPC+BEMC+EEMC
//  - has not yet gone through peer review
//
//  - setup for pp2006 production 
//
//  \author Wei-Ming Zhang, KSU 
//  \author Jan Balewski, IUCF
//  \author Adam Kocoloski, MIT
//
// $Id: bfcMixer_pp2006.C,v 1.3 2007/09/12 21:47:47 kocolosk Exp $
//
//////////////////////////////////////////////////////////////////////////


class StBFChain;
StBFChain  *chain=0;
class StMaker;
class StEvent;

StBFChain *chain1, *chain2, *chain3;
//_____________________________________________________________________
void bfcMixer_pp2006( Int_t Nevents=10,
			  Char_t *file1="star/data03/daq/2006/120/7120049/st_physics_adc_7120049_raw_1050001.daq", // probably ppLong 2006 run, NOT for real embedding - just for testing
			  //Char_t *file1="st_zerobias_7118049_raw_1110001.daq",// contains 127 events, good foor real embedding
			  // Char_t *file2="eleB.fzd", // one-particle events from Naresh
			  Char_t *file2="mcpi0_hipt_run140_gid7_1000evts.fzd",
			  Int_t useEndcapSlowSim=1
			  // note, Barrel slow sim is always ON, said Adam 
			  ){
  TString path1="/";
  //TString path2="/star/data04/sim/subbanly/electron2006/oneTrack2Keve/fzd/";
    TString path2="/star/u/wzhang/links/gc2002/EEmc/embedData/fzd/";

  // Dynamically link some shared libs
  if (gClassTable->GetID("StBFChain") < 0) Load();

  // Create the main chain object
  chain = new StBFChain("Embedding");
  
  StMaker *saveMk = 0;
  chain->SetFlags("-ittf,NoDefault");
  // Create chain1 object
  chain1 = new StBFChain("One");
  
  saveMk = chain1->cd();
  chain1->SetFlags("in Physics  NoDefault -ittf -trg"); //Akio said OK for '-trg'.
  chain1->Set_IO_Files(path1+file1);
  chain1->Load();
  chain1->Instantiate();

  saveMk->cd();
  
  // Create chain2 object
  chain2 = new StBFChain("Two");
  saveMk = chain2->cd();
  chain2->SetFlags("fzin gen_T geomT sim_T tpc trs -tcl -tpt -PreVtx -tpc_daq -ittf ");   
  chain2->Set_IO_Files(path2+file2);
  chain2->Load();
  chain2->Instantiate();
  St_geant_Maker *geantMk = chain2->GetMaker("geant");
  if (geantMk) geantMk->SetMode(1);   // Mixer mode - do not modify EvtHddr

  // do not rescale dEdx in TRS, it leads to wrong cluster formation, from Jamie
 
  saveMk->cd();

  // Mixer for TPC
  gSystem->Load("StMixerMaker");
  StMixerMaker  *mixer = new StMixerMaker("Mixer","daq","trs");
  chain1->SetInput("Input1","StDAQReader");
  chain2->SetInput("Input2","Event");
  mixer->writeFile("mixer.trs",Nevents);


  // Create chain3 object
  chain3 = new StBFChain("Three");
  saveMk = chain3->cd();
  
  //  options for 2006pp, production=DbV20060915,pp2006b,ITTF,hitfilt
  chain3->SetFlags("Simu NoDefault NoInput onlraw -onlcl  ry2006,tpc_daq,tpcI,svt_daq,SvtD,Physics,Idst,l0,Tree,evout l3onl  fcf emcDY2 fpd trgd ZDCvtx useCDV ITTF tofDat -SvtIT  MuDST -trg  VFPPVnoCTB beamline GeantOut CMuDst dEdxY2 -EventQA");
  // Note, do not freez the DB time stamp or you will not see the latest gains/peds/stat  

  TString tt1=file1;
  TString tt2=file2;
  tt1.ReplaceAll(".daq","");
  tt2.ReplaceAll(".fzd","");
  TString OutputFileName=tt1+"_"+tt2+".root";
  cout <<"BFC: Setting file output to: " <<OutputFileName.Data()<<endl;
  
  chain3->Set_IO_Files(0,OutputFileName.Data());
  chain3->Load();
  chain3->Instantiate();  

  //............. begin of EMC embedding makers................

  //.............. Add BEmc stuff here ....................
  StMcEventMaker* mcEventMaker = new StMcEventMaker();
  StEmcSimulatorMaker *bemcSim   = new StEmcSimulatorMaker();
  StEmcMixerMaker     *bemcMixer = new StEmcMixerMaker();
  chain3->AddAfter("emcRaw",bemcMixer); 
  chain3->AddAfter("emcRaw",bemcSim); 
  chain3->AddAfter("emcRaw",mcEventMaker);
  bemcMixer->SetDebug(0); // set it to 1 for more printouts
 // note, Barrel slow sim is always ON, said Adam 

  //........... Add EEmc Stuff ( Simu, and Mixer) here ..............
  StEEmcFastMaker  *eemcFastSim = new StEEmcFastMaker();
  StEEmcMixerMaker *eemcMixer   = new StEEmcMixerMaker();

  /* position B+E EMC makers in the chain 
     (order is reverse because 'After' is used - looks funny but is right)
  */
  chain3->AddAfter("emcRaw",eemcMixer); 
  chain3->AddAfter("emcRaw",eemcFastSim); 

  
  eemcFastSim->SetEmbeddingMode();
  //  eemcFastSim->SetDebug();
  // eemcMixer->SetDebug();
  
  if(useEndcapSlowSim) { // turn Endcap slow simu On/Off 
    StEEmcSlowMaker *slowSim=new StEEmcSlowMaker();
    chain3->AddAfter("EEmcFastSim",slowSim); 
    slowSim->setEmbeddingMode();
  }

  //............. end of EMC embedding makers................
  
  St_geant_Maker *geantMk = (St_geant_Maker *) chain->GetMaker("geant");
  geantMk->SetActive(kTRUE);
  StMaker *tpcdaqMk = chain3->GetMaker("tpc_raw");
  if(!tpcdaqMk )    {
    cout <<" Error: no tpc daq maker. End. "<<endl;
    return;
  }
  tpcdaqMk->SetMode(1);   // Trs
  tpcdaqMk->SetInput("Event","MixerEvent");

  saveMk->cd(); {
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
    chain->ls(5); // list the final chain 
  }


  // chain->SetDEBUG();
  treeMk = chain->GetMaker("tree");
  TBenchmark evnt;
  Int_t iMake = 0, i = 1, iBad = 0;


  StIOMaker *inpMk = (StIOMaker *)chain1->GetMaker("inputStream");
  Int_t ncols, eventnumber, mult, skip=0, oldskip = 0, skiptest=0;
  
  cout <<"BFC - Entering Event Loop"<<endl;
 EventLoop: if (i <= Nevents && iMake != kStEOF && iMake != kStFatal) {
   evnt.Reset();
   evnt.Start("QAInfo:");
   chain->Clear();
   
   iMake = chain->Make(i);
   if (treeMk && iMake == kStErr) {treeMk->Make(i); iBad++;}
   StEvtHddr *fEvtHddr = (StEvtHddr*)chain->GetDataSet("EvtHddr");
   StEvtHddr *fEvtHddrDaq = (StEvtHddr*)chain1->GetDataSet("EvtHddr");
   *fEvtHddr = *fEvtHddrDaq;
   // gSystem->Exec("ps ux");
   evnt.Stop("QAInfo:");
   // evnt->Show("QAInfo:");
   printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time%d.%d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n", i,chain->GetRunNumber(),chain->GetEventNumber(),chain->GetDate(), chain->GetTime(),
	  iMake,evnt.GetRealTime("QAInfo:"),evnt.GetCpuTime("QAInfo:"));
   
   i++;
   goto EventLoop;
 }

  fflush(stdout);
  printf ("QAInfo:Run completed ");
  gSystem->Exec("date");

}


//_____________________________________________________________________
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StBFChain");
  gSystem->Load("StAnalysisUtilities");

  // required by StEEmcSimulatorMaker
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcSimulatorMaker");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StEmcMixerMaker");

  // use this powerfull Logger from Valeri
  gROOT->Macro("LoadLogger.C");
  if (chain) delete chain;
}

