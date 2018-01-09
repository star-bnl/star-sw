//////////////////////////////////////////////////////////////////////////
// Owner:  Yuri Fisyak
//
// $Id:$
//////////////////////////////////////////////////////////////////////////
// root.exe -q -b -x 'bfcMixerVMC.C(10)'
//#define __NO_DAQ_CLUSTERS__
//#define __TrackingOnly__
//#define __IST_SLOW_SIM__ 
class StBFChain;
StBFChain *Chain = 0, *chain1, *chain2, *chain3, *chain4;
//_____________________________________________________________________
void bfcMixerVMC(Int_t First, Int_t Last, const Char_t *opt,
		 const Char_t *daqfile,
		 const Char_t *MuDstfile,
		 Int_t RunG, const Char_t *triggersC) {
  if (gClassTable->GetID("TGiant3") >= 0) {
    cout << "bfcMixerVMC does not intent to run with root4star. Please use root.exe" << endl;
    return;
  }
  //________________________________________________________________________________
  //  gSystem->Load("StFtpcMixerMaker");
  //  StFtpcMixerMaker  *ftpcmixer = new StFtpcMixerMaker("FtpcMixer","daq","trs");
  //________________________________________________________________________________
  // Dynamically link some shared libs1
  gROOT->LoadMacro("bfc.C");
  if (gClassTable->GetID("StBFChain") < 0) Load();
  //______________Create the main chain object______________________________________
  Chain = (StBFChain *) StMaker::New("StBFChain","Embedding");
#if 1
  Chain->SetFlags("TObjTable,nodefault");
#else
  Chain->SetFlags("nodefault");
#endif
  StMaker::lsMakers(Chain);
  //________________________________________________________________________________
  TString Opt(opt);
  TString chain1Opt("in,daq,magF,tpcDb,MakeEvent,trgd,NoDefault,NoOutput,DbV20170830");
  chain1 = bfc(-1,chain1Opt,daqfile,0,0,"DAQ");
  Chain->cd();
  //________________________________________________________________________________
  TString chain2Opt(Form("%s,gen_T,geomT,sim_T,CorrX,OSpaceZ2,OGridLeak3D,nodefault,Rung.%i",Opt.Data(),RunG));
#ifdef  __IST_SLOW_SIM__
  chain2Opt += ",TpcRS,pxlFastSim,istSlowSim,sstfast,bbcSim,btofsim,emcSim"; 
#else
  chain2Opt += ",TpcRS,pxlFastSim,istFastSim,sstfast,bbcSim,btofsim,emcSim"; 
#endif
#ifndef __NO_DAQ_CLUSTERS__
  chain2Opt += ",TpxRaw,NoAnnotateCL";
  chain2Opt += ",pxlHit,istHit,sstHit";
  chain2Opt += ",BtofDat,EmcRaw";
#endif
  bool useEndcapSlowSim = true;
  if(useEndcapSlowSim) { // turn Endcap slow simu On/Off 
    chain2Opt += ",EEss";
  } else {
    chain2Opt += ",EEfs";
  }
  chain2 = bfc(-1,chain2Opt,0,0,0,"MC");
  StMaker *geant = chain2->Maker("geant");
  if (! geant) return;
  geant->SetAttr("MuDstFile",MuDstfile);
  geant->SetAttr("GoodTriggers",triggersC);
#ifdef __IST_SLOW_SIM__
  StIstRawHitMaker *ist_raw_hit = (StIstRawHitMaker *) chain2->Maker("ist_raw_hit");
  if (ist_raw_hit) {// Bingchu Huang, 08/18/2017
    ist_raw_hit->SetAttr("DoEmbedding",1);
    ist_raw_hit->setDataType(1);  //non zero-suppressed data
  }
#endif /*  __IST_SLOW_SIM__ */
  Chain->cd();
  TString chain3Opt("noInput,-in,NoInput");
  chain3Opt += ",TpcMixer";
#ifndef __TrackingOnly__
  chain3Opt += ",BEmcMixer,EEmcMixer";
#if 1
  StBTofSimMaker *tofSim = (StBTofSimMaker *) chain2->Maker("TofSim");
  //  if (tofSim) tofSim->setEmbeddingMode(kTRUE);
  chain3Opt += ",btofMixer"; 
#endif 
#endif 
  chain3 = bfc(-1,chain3Opt,0,0,0,"Mixer");
  Chain->cd();
  //________________________________________________________________________________  
  TString OutputFileName(gSystem->BaseName(daqfile));
  OutputFileName.ReplaceAll("*","");
  OutputFileName.ReplaceAll(".daq","");
  //  OutputFileName.Append("_emb.root");
  OutputFileName.Append(".root");
  TString chain4Opt("P2016,btof,mtd,pxlHit,istHit,sstHit,BEmcChkStat,QAalltrigs,CorrX,OSpaceZ2,OGridLeak3D");
  chain4Opt += ",noInput,-in,useInTracker,-hitfilt,StiCA"; // ,MiniMcMk,McAna,GeantOut
  chain4Opt += ",TpxClu,TpcHitMover,BEmcChkStat,btof,btofMatch,btofCalib,eemcA2E,fmsdat,-evout"; // ,evout
  chain4Opt += ",NoSsdIt,NoSvtIt,StiHftC,Idst,BAna,-hitfilt";
  chain4Opt += ",noTags,noHistos,noRunco";
  //  chain4Opt += ",KFVertex";
  chain4 = bfc(-1,chain4Opt,0,OutputFileName,0,"RC");
#if 0
  StMaker *MuDstMk = chain4->Maker("MuDst");
  if (MuDstMk) {
    cout << "Deactivate " << MuDstMk->GetName() << endl;
    MuDstMk->SetActive(kFALSE);
  }
#endif
  Chain->cd();
#ifndef __TrackingOnly__
  // Twicks
  StEmcSimulatorMaker *bemcSim = (StEmcSimulatorMaker *) chain2->Maker("EmcSimulator");
  if (! bemcSim) {
    cout << "EmcSimulator has not been found" << endl;
  }
  StEmcMixerMaker     *bemcMixer = (StEmcMixerMaker *) chain3->Maker("emcEmbed");
  if (! bemcMixer) {
    cout << " emcEmbed has not been found" << endl;
  } else {
    bemcMixer->SetDebug(0); // set it to 1 for more printouts
  }
 // note, Barrel slow sim is always ON, said Adam 
  
  //........... Add EEmc Stuff ( Simu, and Mixer) here ..............
  StEEmcFastMaker  *eemcFastSim = (StEEmcFastMaker *) chain2->Maker("eefs");
  if (! eemcFastSim ) {
    cout << "EEmcFastSim has not been found" << endl;
  } else {
    eemcFastSim->SetEmbeddingMode();
  }
#if 0
  StEEmcSlowMaker  *eemcSlowSim = (StEEmcSlowMaker *) chain2->Maker("eess");
  if (! eemcSlowSim ) {
    cout << "EEmcSlowSim has not been found" << endl;
  } else {
    eemcSlowSim->SetEmbeddingMode();
  }
#endif  
#endif /* !__TrackingOnly__ */
  //________________________________________________________________________________
  {
    TDatime t;
    gMessMgr->QAInfo() << Form("Run is started at Date/Time %i/%i",t.GetDate(),t.GetTime()) << endm;
  }
  gMessMgr->QAInfo() << Form("Run on %s in %s",gSystem->HostName(),gSystem->WorkingDirectory()) << endm;
  gMessMgr->QAInfo() << Form("with %s", Chain->GetCVS()) << endm;
  //  TAttr::SetDebug(0);
  Chain->SetAttr(".Privilege",0,"*"                ); 	//All  makers are NOT priviliged
  Chain->SetAttr(".Privilege",1,"StBFChain::*" ); 	//StBFChain is priviliged
  Chain->SetAttr(".Privilege",1,"StIOInterFace::*" ); 	//All IO makers are priviliged
  Chain->SetAttr(".Privilege",1,"StVMCMaker::*"); 	//It is also IO maker
  StMaker::lsMakers(Chain);
  Int_t iInit = Chain->Init();
  if (iInit >=  kStEOF) {Chain->FatalErr(iInit,"on init"); return;}
  if (First <= Last) {
    Chain->EventLoop(First,Last);
    gMessMgr->QAInfo() << "Run completed " << endm;
    gSystem->Exec("date");
  }
}
//________________________________________________________________________________
void bfcMixerVMC(Int_t Last=1, const Char_t *opt = "Vmc,Lc3pi,VMCAlignment",
#if 0
		 const Char_t *daqfile="/star/data03/daq/2016/125/17125034/st_physics_adc_17125034_raw_1000007.daq",
		 const Char_t *MuDstfile="/star/subsys/tpc/fisyak/Tpc/TpcRS/daq_2016_AuAu200.DEV2/st_physics_adc_17125034_raw_1000007.MuDst.root",
#else
		 const Char_t *daqfile="/net/l404/data/fisyak/daq/2016/125/17125034/st_physics_adc_17125034_raw_1000007.daq",
		 const Char_t *MuDstfile="/net/l404/data/fisyak/reco/2016/AuAu200_adc/st_physics_adc_17125034_raw_1000007.MuDst.root",
#endif
		 Int_t RunG=0, const Char_t *triggersC = "520001, 520011, 520021, 520031, 520041, 520051") {
  bfcMixerVMC(1,Last,opt,daqfile,MuDstfile,RunG,triggersC);
}
