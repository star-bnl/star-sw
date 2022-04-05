//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_TpcOnly.C,v 1.7 2015/02/12 13:39:18 zhux Exp $
//
//////////////////////////////////////////////////////////////////////////

class StChain;
StChain  *Chain=0;
class StBFChain;
StBFChain *chain1, *chain2, *chain3;
//_____________________________________________________________________
void bfcMixer_TpcOnly(Int_t Nevents=100,
		  const Char_t *daqfile="/star/rcf/test/daq/2005/051/st_physics_adc_6051006_raw_1050001.daq",
		  const Char_t *tagfile="/star/rcf/test/embedding/cuProductionMinBias/FullField/P07ic/2005/051/st_physics_adc_6051006_raw_1050001.tags.root",
		  Double_t pt_low=0.1,
		  Double_t pt_high=5.0,
                  Double_t eta_low=-1.3,
                  Double_t eta_high=1.3,
                  Double_t vzlow = -175.0,
                  Double_t vzhigh = 175.0,
                  Double_t vr = 100.0,
		  Int_t pid=8,
		  Double_t mult=0.05,
                  std::vector<Int_t> triggers = 0,
                  const Char_t *prodName = "P07ib",
		  const Char_t* type = "FlatPt",
		  const bool bPythia = false,
		  const Char_t *fzdfile="test.fzd"
		){
  // production chain for P07ib
  TString prodP07ib("P2005b DbV20070518 MakeEvent ITTF Iana ToF ssddat spt SsdIt SvtIt pmdRaw SCEbyE OGridLeak OShortR OSpaceZ2");// KeepSvtHit hitfilt skip1row");
  TString geomP07ib("ry2005f");

  // production chains for P12ia - run6 p+p 62.4 GeV
  TString prodP12iapp62("DbV20060915,pp2006b,ITTF,DbV20081215_EMC_Calibrations,DbV20081215_EEMC_Calibrations -hitfilt");

  TString geomP12iapp62("ry2006h");

  TString chain1Opt("in,magF,tpcDb,NoDefault,-ittf,NoOutput");
  TString chain2Opt("gen_T,geomT,sim_T,trs,-ittf,-tpc_daq,nodefault");
  if(bPythia){
	  chain2Opt += ",fzin";
  }
  else {
	  chain2Opt += ",NoInput,PrepEmbed";
  }
  chain2Opt += " ";

  TString chain3Opt("");
  if (prodName == "P12iapp62")           { chain3Opt = prodP12iapp62;       chain2Opt += geomP12iapp62;}
  else if (prodName == "P07ib")           { chain3Opt = prodP07ib;       chain2Opt += geomP07ib;}
  else {
    cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible. " << endl;
    return;
  }
  chain3Opt += ",TpcMixer,bbcSim,Embedding,onlraw,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker,EmbeddingShortCut"; 

  // Dynamically link some shared libs
  gROOT->LoadMacro("bfc.C");
  if (gClassTable->GetID("StBFChain") < 0) Load();
  //______________Create the main chain object______________________________________
  Chain = new StChain("Embedding");
  //________________________________________________________________________________
  bfc(-1,chain1Opt,daqfile);
  chain1 = chain;
  chain1->SetName("One"); 
  Chain->cd();
  //________________________________________________________________________________  
  if(bPythia){
	  bfc(-1,chain2Opt,fzdfile);
  }
  else {
	  bfc(-1,chain2Opt);
  }
  chain2 = chain;
  chain2->SetName("Two"); 
  Chain->cd();
  if (chain2->GetOption("TRS")){
	  StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
	  if (! trsMk) {
		  cout << "Cannot find Trs in chain2" << endl;
		  return;
	  }
	  trsMk->setNormalFactor(2.67);
	  trsMk->SetMode(0);
  }
  //________________________________________________________________________________
  //  gSystem->Load("StFtpcMixerMaker");
  //  StFtpcMixerMaker  *ftpcmixer = new StFtpcMixerMaker("FtpcMixer","daq","trs");
  //________________________________________________________________________________
  TString OutputFileName(gSystem->BaseName(daqfile));
  OutputFileName.ReplaceAll("*","");
  OutputFileName.ReplaceAll(".daq","");
  //  OutputFileName.Append("_emb.root");
  OutputFileName.Append(".root");
  bfc(-1,chain3Opt,0,OutputFileName);
  chain3 = chain;
  chain3->SetName("Three"); 
  Chain->cd();
  Chain->cd();

  //............. begin of EMC embedding makers................

  //.............. Add BEmc stuff here ....................
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StEmcMixerMaker");
  gSystem->Load("StEEmcSimulatorMaker");

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
  
  bool useEndcapSlowSim = true;
  if(useEndcapSlowSim) { // turn Endcap slow simu On/Off 
    StEEmcSlowMaker *slowSim=new StEEmcSlowMaker();
    chain3->AddAfter("EEmcFastSim",slowSim); 
    slowSim->setEmbeddingMode();
  }

  //________________________________________________________________________________
  {
    TDatime t;
    gMessMgr->QAInfo() << Form("Run is started at Date/Time %i/%i",t.GetDate(),t.GetTime()) << endm;
  }
  gMessMgr->QAInfo() << Form("Run on %s in %s",gSystem->HostName(),gSystem->WorkingDirectory()) << endm;
  gMessMgr->QAInfo() << Form("with %s", Chain->GetCVS()) << endm;
  // embedded particle set
  if(!bPythia){
	  StPrepEmbedMaker *embMk = (StPrepEmbedMaker *) Chain->Maker("PrepEmbed");
	  if (! embMk) return;
	  cout << "bfcMixer: Setting PID: "<<pid<<endl;
	  embMk->SetTagFile(tagfile);
	  //            pTlow,ptHigh,etaLow,etaHigh,phiLow,phiHigh
	  embMk->SetOpt(  pt_low,    pt_high,  eta_low,    eta_high,    0.,   6.283185, type); 
	  //                pid, mult
	  embMk->SetPartOpt(  pid,mult);

	  // Default is no event selections
	  embMk->SetSkipMode(kTRUE);

	  // Make trigger and z-vertex cuts (only if SkipMode is true)
	  // Trigger cut
	  //   Can put multiple trigger id's 
	  if ( !triggers.empty() ){
		  for(std::vector<Int_t>::iterator iter = triggers.begin(); iter != triggers.end(); iter++){
			  embMk->SetTrgOpt((*iter)) ;
		  }
	  }
	  // z-vertex cuts
	  embMk->SetZVertexCut(vzlow, vzhigh) ;
	  // vr = sqrt{vx^2 + vy^2} cut
	  embMk->SetVrCut(vr);

	  //embMk->SetPVRankCutMode(kTRUE) ;
	  //embMk->SetPVRankCut(0) ;
  }

  TAttr::SetDebug(0);
  Chain->SetAttr(".Privilege",0,"*"                ); 	//All  makers are NOT priviliged
  Chain->SetAttr(".Privilege",1,"StBFChain::*" ); 	//StBFChain is priviliged
  Chain->SetAttr(".Privilege",1,"StIOInterFace::*" ); 	//All IO makers are priviliged
  Chain->SetAttr(".Privilege",1,"St_geant_Maker::*"); 	//It is also IO maker
  if(!bPythia)Chain->SetAttr(".Privilege",1,"StPrepEmbedMaker::*"); //It is also IO maker
  //  Chain->SetDEBUG(0);
  if (Nevents < 0) return;
  Int_t iInit = Chain->Init();
  if (iInit >=  kStEOF) {Chain->FatalErr(iInit,"on init"); return;}
  StMaker *treeMk = Chain->GetMaker("outputStream");
  Chain->EventLoop(Nevents,treeMk);
  gMessMgr->QAInfo() << "Run completed " << endm;
  gSystem->Exec("date");
}
//________________________________________________________________________________
void bfcMixer_TpcOnly(Int_t Nevents, const Char_t *daqfile, Char_t *tagfile,
		  Double_t pt_low, Double_t pt_high, Double_t eta_low, Double_t eta_high, 
		  Double_t vzlow, Double_t vzhigh, Double_t vr, Int_t pid, Double_t mult,
		  const Char_t *triggersC, const Char_t *prodName, const Char_t* type) {
  std::vector<Int_t> triggers;
  if (triggersC) {
    TPMERegexp pm(":");
    Int_t N = pm.Split(triggersC);
    for (Int_t i = 0; i < N; i++) {
      TString num(pm[i]);
      triggers.push_back(num.Atoi());
    }
  }
  bfcMixer_TpcOnly(Nevents, daqfile, tagfile, pt_low, pt_high, eta_low, eta_high, vzlow, vzhigh, vr, pid, mult, triggers, prodName, type);
}
