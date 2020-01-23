//_____________________________________________________________________
class StChain;
StChain  *Chain=0;
class StBFChain;
StBFChain *chain1, *chain2, *chain3;
//_____________________________________________________________________
void bfcMixer_Hft(Int_t Nevents=1  ,
		  const Char_t *daqfile="/star/data100/GRID/daq/2014/st_zerobias_adc_15167009_raw_0000011.daq",
		  const Char_t *tagfile="/star/data100/GRID/daq/2014/st_physics_adc_15167009_raw_1000012.tags.root",
		  Double_t pt_low=0.1,
		  Double_t pt_high=10.0,
                  Double_t eta_low=-1.5,
                  Double_t eta_high=1.5,
                  Double_t vzlow = -150.0,
                  Double_t vzhigh = 150.0,
                  Double_t vr = 100.0,
		  Int_t pid=9,
		  Double_t mult=100,
                  std::vector<Int_t> triggers = 0,
                  const Char_t *prodName = "P16idAuAu200hftZB",
                  const Char_t* type = "FlatPt",
		  const bool bPythia = true,
		  const Char_t *fzdfile="test_full_field_pos.fzd"
		  ){

  // Run14 AuAu 200 st_physics chain (P15ic) w/ HFT tracking, StiCA, ...
  //  TString prodP15icAuAu200hft ( "DbV20150316 P2014a PxlHit IstHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D -hitfilt  -vfminuit vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit picoWrite PicoVtxDefault"); // pxlslowsim istslowsi  //                             DbV20150316 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D -hitfilt
  TString prodP16idAuAu200hft  ( "DbV20160418 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D ODistoSmear -hitfilt -vfminuit vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit" ); // picoWrite PicoVtxDefault            DbV20150316 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D -hitfilt
  //TString prodP16idAuAu200hft  ( "DbV20160418 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D ODistoSmear -hitfilt  -vfminuit vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit" ); // picoWrite PicoVtxDefault            DbV20150316 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D -hitfilt
  TString prodP16idAuAu200hftZB( "DbV20160418 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D ODistoSmear -hitfilt  vfminuit -vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit" ); // picoWrite PicoVtxDefault            DbV20150316 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D -hitfilt
  //$$$TString prodP15icAuAu200hft ( "DbV20150316 P2014a PxlHit IstHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D -hitfilt  -vfminuit vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit "); // pxlslowsim istslowsi  //                             DbV20150316 P2014a pxlHit istHit btof mtd mtdCalib BEmcChkStat CorrX OSpaceZ2 OGridLeak3D -hitfilt

  // Run 16 AuAu200 HFT chain
  TString prodP16ijAuAu200hft  ( "DbV20161018 P2016a StiCA mtd mtdCalib btof PxlHit IstHit BEmcChkStat CorrX OSpaceZ2 OGridLeak3D ODistoSmear -hitfilt -vfminuit vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit "); // remove sstHits picoWrite PicoVtxDefault
  TString prodP16ijAuAu200hftZB( "DbV20161018 P2016a StiCA mtd mtdCalib btof PxlHit IstHit BEmcChkStat CorrX OSpaceZ2 OGridLeak3D ODistoSmear -hitfilt vfminuit -vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit "); // remove sstHits picoWrite PicoVtxDefault

  // Run 16 dAu200 HFT chain
  TString prodP17iddAu200hft  ( "DbV20161216 P2016a StiCA mtd mtdCalib btof PxlHit IstHit BEmcChkStat CorrX OSpaceZ2 OGridLeak3D ODistoSmear -hitfilt -vfminuit vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit "); // remove sstHits picoWrite PicoVtxDefault

  // Run 16 dAu62 HFT chain
  TString prodP17iddAu62hft  ( "DbV20170426 P2016a StiCA mtd mtdCalib btof PxlHit IstHit BEmcChkStat CorrX OSpaceZ2 OGridLeak3D ODistoSmear -hitfilt -vfminuit vfmce tpxclu pxlslowsim istslowsim nosvtit nossdit "); // remove sstHits picoWrite PicoVtxDefault

  //TString geomP16ij("ry2016x"); // little hack... sets y2016x as the chain2 geometry... loaded by geant... thinking this might be needed to get the geant reader correct...
  TString geomP16ij("ry2016x"); // this is consistent with chain option... but may cause problem with geant reader...
  TString geomP16id("ry2014x"); // this is consistent with chain option... but may cause problem with geant reader...
  TString geomP17id("ry2016x"); // this is consistent with chain option... but may cause problem with geant reader...

  TString chain1Opt(" in magF tpcDb NoDefault TpxRaw -ittf NoOutput");     // chain1 reads event from daq file
  TString chain2Opt(" gen_T geomT sim_T TpcRS -ittf -tpc_daq nodefault");  // chain2 runs simu or reads from mc file
  TString chain3Opt("");                                                   // chain3 runs event reconstruction and mixing

  if(bPythia){		chain2Opt += ",fzin bigbig ";	}
  else {		chain2Opt += ",NoInput,PrepEmbed "; }

  if (prodName == "P16idAuAu200hft"){ 
    chain2Opt += geomP16id; // done in analogy to P16ijAuAu200hft... need to understand "little hack" above...
    chain3Opt += prodP16idAuAu200hft; // nix chain3Opt += " btofSim -vpdSim btofMixer ";
  }
  else if (prodName == "P16idAuAu200hftZB"){ 
    chain2Opt += geomP16id; // done in analogy to P16ijAuAu200hft... need to understand "little hack" above...
    chain3Opt += prodP16idAuAu200hftZB; // nix chain3Opt += " btofSim -vpdSim btofMixer ";
  }
  else if (prodName == "P16ijAuAu200hft" ) {
      chain2Opt += geomP16ij;
      chain3Opt += prodP16ijAuAu200hft; //chain3Opt += " btofSim vpdSim btofMixer ";
    }
  else if (prodName == "P16ijAuAu200hftZB" )  {
      chain2Opt += geomP16ij;
      chain3Opt += prodP16ijAuAu200hftZB; // nix chain3Opt += " btofSim -vpdSim btofMixer StiPulls ";
    }
  else if (prodName == "P17iddAu200hft" ) {
      chain2Opt += geomP17id;
      chain3Opt += prodP17iddAu200hft; //chain3Opt += " btofSim vpdSim btofMixer ";
    }
  else if (prodName == "P17iddAu62hft" ) {
      chain2Opt += geomP17id;
      chain3Opt += prodP17iddAu62hft; //chain3Opt += " btofSim vpdSim btofMixer ";
    }

  else {
    cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible. " << endl;
    return;
  }
  chain3Opt += ",TpcMixer,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker"; 
  chain3Opt += ",mcevout"; 

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
  	chain2 = chain;
  	chain2->SetName("Two"); 
  	Chain->cd();
  	if (chain2->GetOption("TRS")){
    	StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
    	if (! trsMk) {
      	cout << "Cannot find Trs in chain2" << endl;
      	return;
    	}
    	trsMk->setNormalFactor(1.32);
  	}
	}
	else {
		bfc(-1,chain2Opt);
  	chain2 = chain;
  	chain2->SetName("Two"); 
  	Chain->cd();
	}


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
  TString OutputFileName(gSystem->BaseName(daqfile)); {
    OutputFileName.ReplaceAll("*","");
    OutputFileName.ReplaceAll(".daq","");
    //  OutputFileName.Append("_emb.root");
    OutputFileName.Append(".root");
  }
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

  if (chain2Opt.Contains("TpcRS",TString::kIgnoreCase)) {
    mixer->SetInput("Input2","TpcRS/Event");
  } else {
    mixer->SetInput("Input2","Trs/.const/Event");
  }
  Chain->cd();
  
  ////////////////////////////////////////////////////////////////////////////////////////
  //
  //
  // IST Slow Simulator setup
  //
  //
  ////////////////////////////////////////////////////////////////////////////////////////
  StIstRawHitMaker *istRawHitMaker = (StIstRawHitMaker *)chain3->GetMaker("ist_raw_hit");
  istRawHitMaker->setDataType(1); // no-zs data
  istRawHitMaker->setDoEmbedding(kTRUE);

  //
  // Verbose printout for position
  //
  //AgPosition::SetDebug(2);


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

  eemcFastSim->SetEmbeddingMode();  //  eemcFastSim->SetDebug();  // eemcMixer->SetDebug();
  
  bool useEndcapSlowSim = true;  if(useEndcapSlowSim) { // turn Endcap slow simu On/Off 
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

  	embMk->SetTemp(0.35);

	//embMk->SetRapidityMode(kFALSE);  //default is 'kTRUE'
	
	//Switch to prime mode for nucleus (with geantID > 10000) embedding, default is 'kFALSE'
	//embMk->SetPrimeMode(kTRUE);

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

	//cut on VpdVz, need moretags.root
	//embMk->SetVpdVzCutMode(kTRUE);
	//embMk->SetVpdVzCut(3);
	
	//cut on PVranking, need moretags.root
	//embMk->SetPVRankCutMode(kTRUE);
	//embMk->SetPVRankCut(0);  // pvrank > 0

	}

#if 1
  TString checkProdName( prodName );
  if ( checkProdName == "P16idAuAu200hft" || checkProdName == "P16ijAuAu200hft" || checkProdName == "P17iddAu200hft") {
  //________________________________________________________________________________
  // Setup StarEmbedMaker 
    gSystem->Load( "StarGeneratorUtil.so"  );
    gSystem->Load( "StarGeneratorEvent.so" );
    gSystem->Load( "StarGeneratorBase.so"  );
    gSystem->Load( "StarGeneratorEmbed.so" );
    StarEmbedMaker* mk = new StarEmbedMaker();
    mk->SetFileName( "starembed.genevt.root" );
    mk->SetInputFile( tagfile );
    chain2 -> AddBefore( "geant", mk );    
  //________________________________________________________________________________
  }
#endif

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

  //
  // File splitting from SUMS... skip events along the daq chain...
  //
  TString START(gSystem->Getenv("EVENTS_START"));
  if ( START != "" ) {
    int nskip = START.Atoi();    
if (nskip>0) {
    gMessMgr->QAInfo() << "SUMS is splitting this file" << endm;
    gMessMgr->QAInfo() << "EVENTS_START = " << nskip << endm;
    chain1->Skip(nskip-1);
    gMessMgr->QAInfo() << "... chain 1 has been advanced... allons y" << endm;
} 
  }

  Chain->EventLoop(Nevents,treeMk);

  gMessMgr->QAInfo() << "Run completed " << endm;
  gSystem->Exec("date");

}
//________________________________________________________________________________
void bfcMixer_Hft(Int_t Nevents, const Char_t *daqfile, Char_t *tagfile,
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
  bfcMixer_Hft(Nevents, daqfile, tagfile, pt_low, pt_high, eta_low, eta_high, vzlow, vzhigh, vr, pid, mult, triggers, prodName, type);
}
//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_Hft.C,v 1.2 2020/01/22 15:02:56 starembd Exp $
//
// $Log: bfcMixer_Hft.C,v $
// Revision 1.2  2020/01/22 15:02:56  starembd
// added options for Run16 dAu200 and dAu62 HFT embedding
//
// Revision 1.1  2018/09/26 05:21:16  zhux
// initial version from Jason Webb
//
// Revision 1.1  2018/07/06 17:32:32  jwebb
//
// Committing SUMS workflow and ROOT macros used in the QM2018 hft embedding
// productions.
//
//////////////////////////////////////////////////////////////////////////
