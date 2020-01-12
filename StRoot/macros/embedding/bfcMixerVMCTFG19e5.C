//////////////////////////////////////////////////////////////////////////
// Owner:  Yuri Fisyak
//
// $Id:$
//////////////////////////////////////////////////////////////////////////
// root.exe -q -b -x 'bfcMixerVMCTFG19e5.C(10)'
// Guannan: TFG19e5 chain = "P2019a,in,tpxraw,tpxclu,McTpcAna,-hitfilt,mtd,btof,BEmcChkStat,-CorrY,-OGridLeakFull,CorrX,useCDV,DBV20190709,OSpaceZ2,OGridLeak3D,-evout,NoHistos,noTags,noRunco,StiCA,picoWrite,PicoVtxVpdOrDefault"
// Original         chain = "P2019a,-hitfilt,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeak3D,-evout,NoHistos,noTags,noRunco,Stx,KFVertex,VFMinuitX,picoWrite,PicoVtxVpdOrDefault
//                  chain = "P2019a,tpxraw,tpxclu,-hitfilt,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeak3D,-evout,NoHistos,noTags,noRunco,Stx,KFVertex,VFMinuitX,picoWrite,PicoVtxVpdOrDefault,DbV20190709
//#define __NO_DAQ_CLUSTERS__
//#define __TrackingOnly__
class StBFChain;
StBFChain *Chain = 0, *chain1, *chain2, *chain3, *chain4;
//_____________________________________________________________________
void bfcMixerVMCTFG19e5(Int_t First, Int_t Last, const Char_t *opt,
		 const Char_t *daqfile,
		 const Char_t *MuDstfile,
		 Int_t RunG, const Char_t *triggersC) {
  if (gClassTable->GetID("TGiant3") >= 0) {
    cout << "bfcMixerVMCTFG19e5 does not intent to run with root4star. Please use root.exe" << endl;
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
#if 0
  Chain->SetFlags("TObjTable,nodefault");
#else
  Chain->SetFlags("nodefault");
#endif
  StMaker::lsMakers(Chain);
  //________________________________________________________________________________
  TString Opt(opt);
  TString chain1Opt("in,daq,magF,tpcDb,MakeEvent,trgd,NoDefault,DbV20190709");
  chain1 = bfc(-1,chain1Opt,daqfile,0,0,"DAQ");
  Chain->cd();
  //________________________________________________________________________________
  TString chain2Opt(Form("Vmc,VMCAlignment,%s,gen_T,geomT,sim_T,CorrY,OPr40,OSpaceZ2,OGridLeakFull,nodefault,Rung.%i,TpcRS",Opt.Data(),RunG));
  chain2Opt += ",BtofDat,EmcRaw";
#ifndef __NO_DAQ_CLUSTERS__
  chain2Opt += ",bbcSim,btofsim,emcSim"; 
  chain2Opt += ",TpxRaw,NoAnnotateCL";
  bool useEndcapSlowSim = true;
  if(useEndcapSlowSim) { // turn Endcap slow simu On/Off 
    chain2Opt += ",EEss";
  } else {
    chain2Opt += ",EEfs";
  }
#endif
  chain2 = bfc(-1,chain2Opt,0,0,0,"MC");
  StMaker *geant = chain2->Maker("geant");
  if (! geant) return;
  geant->SetAttr("MuDstFile",MuDstfile);
  geant->SetAttr("GoodTriggers",triggersC);
  Chain->cd();
  TString chain3Opt("noInput,-in,NoInput");
  chain3Opt += ",TpcMixer";
#if !defined(__TrackingOnly__) && !defined(__NO_DAQ_CLUSTERS__)
  chain3Opt += ",BEmcMixer,EEmcMixer";
#if 0
  StBTofSimMaker *tofSim = (StBTofSimMaker *) chain2->Maker("TofSim");
  //  if (tofSim) tofSim->setEmbeddingMode(kTRUE);
  //  chain3Opt += ",btofMixer"; 
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
  //  TString chain4Opt("P2019a,tpxclu,-hitfilt,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeakFull,-evout,NoHistos,noTags,noRunco,Stx,KFVertex,VFMinuitX,picoWrite,PicoVtxVpdOrDefault,DbV20190709");
  TString chain4Opt("P2019a,tpxclu,-hitfilt,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeakFull,-evout,NoHistos,noRunco,Stx,KFVertex,VFMinuitX,picoWrite,PicoVtxVpdOrDefault,DbV20190709");
  //                "P2016,btof,mtd,pxlHit,istHit,sstHit,BEmcChkStat,QAalltrigs,CorrX,OSpaceZ2,OGridLeakFull");
  chain4Opt += ",noInput,-in,useInTracker,-hitfilt,StiCA"; // ,MiniMcMk,McAna,GeantOut
  chain4Opt += ",TpxClu,TpcHitMover,BEmcChkStat,btof,btofMatch,btofCalib,eemcA2E,fmsdat,-evout"; // ,evout
  chain4Opt += ",NoSsdIt,NoSvtIt,StiHftC,Idst,BAna,-hitfilt";
  chain4Opt += ",noTags,noHistos,noRunco";
  //  chain4Opt += ",picoWrite";
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
#if !defined(__TrackingOnly__) &&  !defined(__NO_DAQ_CLUSTERS__)
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
  StVMCMaker::instance()->SetVxSigma(-1);
  StVMCMaker::instance()->SetAttr("SmearVertex",1); // To smear production vertex accoudingly vertex errors
  Int_t iInit = Chain->Init();
  if (iInit >=  kStEOF) {Chain->FatalErr(iInit,"on init"); return;}
  if (First <= Last) {
    Chain->EventLoop(First,Last);
    gMessMgr->QAInfo() << "Run completed " << endm;
    gSystem->Exec("date");
  }
}
//________________________________________________________________________________
void bfcMixerVMCTFG19e5(Int_t Last=1000, const Char_t *opt = "KNmTsq5PerCentZ70cm",
			const Char_t *daqfile="/gpfs01/star/daq/2019/091/20091001/st_physics_adc_20091001_raw_5500002.daq",
			const Char_t *MuDstfile="/star/u/xgn1992/Strangeness/Phi/AuAu_19.6GeV/hlt/test-production-picoData/data_TFG19e5/st_physics_adc_20091001_raw_5500002_0001_1000.MuDst.root",
			Int_t RunG=0, const Char_t *triggersC = "") { //520001, 520011, 520021, 520031, 520041, 520051") {
  bfcMixerVMCTFG19e5(1,Last,opt,daqfile,MuDstfile,RunG,triggersC);
}
