//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_TpxAuAu11.C,v 1.4 2012/02/17 20:47:36 fisyak Exp $
//
//////////////////////////////////////////////////////////////////////////

class StChain;
StChain  *Chain=0;
class StBFChain;
StBFChain *chain1, *chain2, *chain3;
//_____________________________________________________________________
void bfcMixer_TpxAuAu11(const Int_t Nevents=100,
		  const Char_t *daqfile="/star/rcf/test/daq/2010/embed/auau11_prod/st_physics_adc_11148048_raw_1520001.daq",
		  const Char_t *tagfile="/star/rcf/test/daq/2010/embed/auau11_prod/st_physics_adc_11148048_raw_1520001.tags.root",
		  const Double_t pt_low=0.1,
		  const Double_t pt_high=5.0,
                  const Double_t eta_low=-1.5,
                  const Double_t eta_high=1.5,
                  const Double_t vzlow = -150.0,
                  const Double_t vzhigh = 150.0,
		  const Int_t pid=9,
		  const Double_t mult=100,
                  const std::vector<Int_t> triggers = 0,
                  const Char_t *prodName = "P10ihAuAu11",
                  const Char_t* type = "FlatPt"){
  // production chains for P08ic - p+p, Au+Au 9 GeV and d+Au
  TString prodP08iepp("DbV20081117 B2008a ITTF IAna ppOpt l3onl emcDY2 fpd ftpc trgd ZDCvtx NosvtIT NossdIT Corr4 OSpaceZ2 OGridLeak3D VFMCE -hitfilt");
//  TString prodP08icpp("DbV20080712,pp2008,ITTF,OSpaceZ2,OGridLeak3D,beamLine,VFMCE,TpxClu -VFPPV -hitfilt");
//  TString prodP08icAuAu9("DbV20080709 P2008 ITTF VFMCE -hitfilt");
//  TString prodP08icAuAu200("DbV20070101 P2008 ITTF VFMCE -hitfilt");  
//  TString prodP08icdAu("DbV20080712 P2008 ITTF OSpaceZ2 OGridLeak3D beamLine, VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP08iedAu("DbV20090213 P2008 ITTF OSpaceZ2 OGridLeak3D beamLine VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10iapp("DbV20091001 pp2009c TpcRS ITTF OSpaceZ2 OGridLeak3D beamLine, VFMCE TpcRS -VFMinuit -hitfilt");

  // BES Run10 chains
  TString prodP10ihAuAu39("DbV20100909 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10ihAuAu11("DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10ihAuAu7("DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");

  TString geomP08ic("ry2008");
  TString geomP10ih("ry2010");
  TString chain1Opt("in,magF,tpcDb,NoDefault,TpxRaw,-ittf,NoOutput");
  TString chain2Opt("NoInput,PrepEmbed,gen_T,geomT,sim_T,TpcRS,-ittf,-tpc_daq,nodefault");
//  TString chain2Opt("NoInput,PrepEmbed,gen_T,geomT,sim_T,trs,-ittf,-tpc_daq,nodefault");
  chain2Opt += " ";

  TString chain3Opt("");
  if (prodName == "P08icpp")           { chain3Opt = prodP08icpp;       chain2Opt += geomP08ic; }
  else if (prodName == "P08iepp")      { chain3Opt = prodP08iepp;       chain2Opt += geomP08ic; }
  else if (prodName == "P08icAuAu9")   { chain3Opt = prodP08icAuAu9;    chain2Opt += geomP08ic; }
  else if (prodName == "P08icdAu")     { chain3Opt = prodP08icdAu;      chain2Opt += geomP08ic; }
  else if (prodName == "P08iedAu")     { chain3Opt = prodP08iedAu;      chain2Opt += geomP08ic; }
  else if (prodName == "P08icAuAu200") { chain3Opt = prodP08icAuAu200;  chain2Opt += geomP08ic; }
  else if (prodName == "P10iapp")      { chain3Opt = prodP10iapp;       chain2Opt += geomP10ih; }
  else if (prodName == "P10ihAuAu39")  { chain3Opt = prodP10ihAuAu39;   chain2Opt += geomP10ih; }
  else if (prodName == "P10ihAuAu11")  { chain3Opt = prodP10ihAuAu11;   chain2Opt += geomP10ih; }
  else if (prodName == "P10ihAuAu7")   { chain3Opt = prodP10ihAuAu7;    chain2Opt += geomP10ih; }
  else {
    cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible. " << endl;
    return;
  }
  chain3Opt += ",Embedding,TpcMixer,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker"; 
  chain3Opt += ",";

  if (prodName == "P08icpp")           { chain3Opt += geomP08ic; }
  else if (prodName == "P08iepp")      { chain3Opt += geomP08ic; }
  else if (prodName == "P08icAuAu9")   { chain3Opt += geomP08ic; }
  else if (prodName == "P08icdAu")     { chain3Opt += geomP08ic; }
  else if (prodName == "P08iedAu")     { chain3Opt += geomP08ic; }
  else if (prodName == "P08icAuAu200") { chain3Opt += geomP08ic; }
  else if (prodName == "P10iapp")      { chain3Opt += geomP10ih; }
  else if (prodName == "P10ihAuAu39")  { chain3Opt += geomP10ih; }
  else if (prodName == "P10ihAuAu11")  { chain3Opt += geomP10ih; }
  else if (prodName == "P10ihAuAu7")   { chain3Opt += geomP10ih; }
  else {
    cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible. " << endl;
    return;
  }

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
  bfc(-1,chain2Opt);
  chain2 = chain;
  chain2->SetName("Two"); 
  Chain->cd();
#if 0
  if (chain2->GetOption("TRS")){
    StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
    if (! trsMk) {
      cout << "Cannot find Trs in chain2" << endl;
      return;
    }
    trsMk->setNormalFactor(1.32);
    trsMk->SetMode(0);
  }
#endif
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
  //________________________________________________________________________________
  StTpcMixerMaker  *mixer = (StTpcMixerMaker *) chain3->Maker("TpcMixer");
  if( prodName == "P08icAuAu200")
	{
	  mixer->SetInput("Input1","MixerEvent");
	}
  else
	{
	  mixer->SetInput("Input1","TpxRaw/.data/Event");
        }
  //mixer->SetInput("Input2","Trs/.const/Event");
 
  if (chain2Opt.Contains("TpcRS",TString::kIgnoreCase)) {
   mixer->SetInput("Input2","TpcRS/Event");
  }else {
   mixer->SetInput("Input2","Trs/.const/Event");
  }
 
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
  StPrepEmbedMaker *embMk = (StPrepEmbedMaker *) Chain->Maker("PrepEmbed");
  if (! embMk) return;
  cout << "bfcMixer: Setting PID: "<<pid<<endl;
  embMk->SetTagFile(tagfile);
  //            pTlow,ptHigh,etaLow,etaHigh,phiLow,phiHigh
  embMk->SetOpt(  pt_low,    pt_high,  eta_low,    eta_high,    0.,   6.283185, type); 
  //                pid, mult
  embMk->SetPartOpt(  pid,mult);

  // Default is no event selections
  embMk->SetSkipMode(kFALSE);

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

  TAttr::SetDebug(0);
  Chain->SetAttr(".Privilege",0,"*"                ); 	//All  makers are NOT priviliged
  Chain->SetAttr(".Privilege",1,"StBFChain::*" ); 	//StBFChain is priviliged
  Chain->SetAttr(".Privilege",1,"StIOInterFace::*" ); 	//All IO makers are priviliged
  Chain->SetAttr(".Privilege",1,"St_geant_Maker::*"); 	//It is also IO maker
  Chain->SetAttr(".Privilege",1,"StPrepEmbedMaker::*"); //It is also IO maker
  //  Chain->SetDEBUG(0);
  if (Nevents < 0) return;
  Int_t iInit = Chain->Init();
  if (iInit >=  kStEOF) {Chain->FatalErr(iInit,"on init"); return;}
  StMaker *treeMk = Chain->GetMaker("outputStream");
  Chain->EventLoop(Nevents,treeMk);
  gMessMgr->QAInfo() << "Run completed " << endm;
  gSystem->Exec("date");
}

//$LOG$
