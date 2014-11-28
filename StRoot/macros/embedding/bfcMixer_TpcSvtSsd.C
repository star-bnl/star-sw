//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_TpcSvtSsd.C,v 1.16 2013/04/28 14:41:31 fisyak Exp $
//
//////////////////////////////////////////////////////////////////////////

class StChain;
StChain  *Chain=0;
class StBFChain;
StBFChain *chain1, *chain2, *chain3;
//_____________________________________________________________________
void bfcMixer_TpcSvtSsd(const Int_t Nevents=500,Int_t isSvtIn=1, Int_t isSsdIn=1,
		    const Char_t *daqfile="/star/rcf/test/daq/2007/113/8113044/st_physics_8113044_raw_1040042.daq",
		    const Char_t *tagfile="/star/rcf/test/embedding/2007ProductionMinBias/FullField/P08if/2007/113/8113044/st_physics_8113044_raw_1040042.tags.root",
		    const Double_t pt_low=0.1,
		    const Double_t pt_high=5.0,
		    const Double_t eta_low=-1.1,
		    const Double_t eta_high=1.1,
	            const Double_t vzlow=-175.0,
		    const Double_t vzhigh=175.0,
		    const Int_t pid=8,
		    const Double_t mult = 100.,
                    const std::vector<Int_t> triggers = 0,
                    const Char_t* prodName = "P08icAuAu",
                    const Char_t* mode="flatpt"
) {
  // Separate DB timestamp to add it in both chain1 and chain3
  TString DbVP06idpp("DbV20060729 ");
  TString DbVP07icCuCu("DbV20070518 ");
  TString DbVP08icAuAu("DbV20080418 ");

  // production chains for P06id - p+p 200 GeV (Run6)
  TString prodP06idpp("pp2006b ITTF OSpaceZ2 OGridLeak3D VFMCE -VFPPVnoCTB -hitfilt");

  // production chain for P07ib
//  TString prodP07ib("P2005b DbV20070518 MakeEvent ITTF Iana ToF spt SsdIt SvtIt pmdRaw SCEbyE OGridLeak OShortR OSpaceZ2 ssd_daq");// KeepSvtHit hitfilt skip1row");

  // production chain for P07ic - Cu+Cu 200 GeV (Run5)
  TString prodP07icCuCu("P2005b DbV20070518 MakeEvent ITTF ToF ssddat spt SsdIt SvtIt pmdRaw OGridLeak OShortR OSpaceZ2 KeepSvtHit skip1row VFMCE -VFMinuit -hitfilt");

  // production chain for P08if
//    TString prodP08if("B2007g DbV20080418 adcOnly MakeEvent ITTF Iana ToF spt SsdIt SvtIt pmdRaw SCEbyE  OShortR trgd Corr5 OSpaceZ2 ssd_daq KeepSvtHit -hitfilt VFMCE");// KeepSvtHit hitfilt skip1row");

  // Production chain for P08ic Au+Au 200 GeV (Run7)
  TString prodP08icAuAu("B2007g ITTF adcOnly IAna KeepSvtHit VFMCE -hitfilt l3onl emcDY2 fpd ftpc trgd ZDCvtx svtIT ssdIT Corr5 -dstout");

  TString geomP06id("ry2006");
  TString geomP07ic("ry2005f");
  TString geomP08ic("ry2007g");
//  TString chain1Opt("in magF tpcDb adcOnly NoDefault -ittf NoOutput");
  TString chain1Opt("in magF tpcDb NoDefault -ittf NoOutput");
  TString chain2Opt("NoInput PrepEmbed gen_T geomT sim_T trs -ittf -tpc_daq nodefault");

  TString chain3Opt("");
  if( prodName == "P06idpp") {
    chain1Opt.Prepend(DbVP06idpp);
    chain2Opt += " "; chain2Opt += geomP06id;
    chain3Opt = prodP06idpp ;
  }
  else if( prodName == "P07ic" ){
    chain1Opt.Prepend(DbVP07icCuCu);
    chain2Opt += " "; chain2Opt += geomP07ic;
    chain3Opt = prodP07icCuCu;
  }
  else if ( prodName == "P08icAuAu" ){
    chain1Opt.Prepend(DbVP08icAuAu);
    chain2Opt += " "; chain2Opt += geomP08ic;
    chain3Opt = prodP08icAuAu ;
  }
  else{
    cout << "Choice prodName does not correspond to known chain. Processing impossible. " << endl;
    return;
  }
  //  chain3Opt += " Embedding onlraw GeantOut MiniMcMk McAna IdTruth -in NoInput,useInTracker EmbeddingShortCut"; 
//  chain3Opt += " Embedding onlraw McEvent McEvOut GeantOut MiniMcMk McAna IdTruth -in NoInput,useInTracker -hitfilt EmbeddingShortCut"; 
  chain3Opt += " TpcMixer Embedding onlraw McEvent McEvOut GeantOut MiniMcMk McAna IdTruth -in NoInput,useInTracker -hitfilt -TrsPileUp -TrsToF";
  //  chain3Opt += " Embedding onlraw McEvent McEvOut GeantOut IdTruth -in NoInput -hitfilt EmbeddingShortCut"; 

  if (isSvtIn) chain3Opt += " SvtEmbed";
  if (isSsdIn) {
    chain1Opt += ",ssddat";
    chain2Opt += ",ssd,McEvent,-spt";
    chain3Opt += ",SsdEmbed";
  }

  if( prodName == "P06idpp") {
    chain3Opt.Prepend(DbVP06idpp);
    chain3Opt += " "; chain3Opt += geomP06id;
  }
  else if( prodName == "P07ic" ){
    chain3Opt.Prepend(DbVP07icCuCu);
    chain3Opt += " "; chain3Opt += geomP07ic;
  }
  else if ( prodName == "P08icAuAu" ){
    chain3Opt.Prepend(DbVP08icAuAu);
    chain3Opt += " "; chain3Opt += geomP08ic;
  }
  else{
    cout << "Choice prodName does not correspond to known chain. Processing impossible. " << endl;
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
  if (chain2->GetOption("TRS")){
    StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
    if (! trsMk) {
      cout << "Cannot find Trs in chain2" << endl;
      return;
    }
    trsMk->setNormalFactor(1.05);
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
  embMk->SetTagFile(tagfile);
  //            pTlow,ptHigh,etaLow,etaHigh,phiLow,phiHigh
  embMk->SetOpt(  pt_low,    pt_high,  eta_low,    eta_high,    0.,   6.283185, mode); 
  //                pid, mult
  embMk->SetPartOpt(  pid,mult);

  // Set Skip mode (default is OFF)
  embMk->SetSkipMode(kFALSE) ;

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
  StMaker *SsdEmbed = Chain->Maker("SsdEmbed");
  if (SsdEmbed) {
    cout << "SsdEmbed has been found ----------------------------------------" << endl;
    SsdEmbed->SetInput("SsdRealData","One/.make/SpaStrip/.data/spa_strip");
    SsdEmbed->SetInput("SsdSimuData","Two/.make/SpaStrip/.data/spa_strip");
    StMaker *SsdPoint = Chain->Maker("SsdPoint");
    if (SsdPoint) {
      cout << "SsdPoint has been found----------------------------------------" << endl;
      SsdPoint->SetInput("SpaStrip","SsdEmbed");
    }
  }
  //  Chain->SetDEBUG(0);
  if (Nevents < 0) return;
  Int_t iInit = Chain->Init();
  if (iInit >=  kStEOF) {Chain->FatalErr(iInit,"on init"); return;}
  StMaker *treeMk = Chain->GetMaker("outputStream");

  Chain->EventLoop(Nevents,treeMk);
  gMessMgr->QAInfo() << "Run completed " << endm;
  gSystem->Exec("date");
}
  
// $Log: bfcMixer_TpcSvtSsd.C,v $
// Revision 1.16  2013/04/28 14:41:31  fisyak
// Clean up after retirement of St_tpcdaq_Maker, StRTSClient and StMixer
//
// Revision 1.15  2013/04/24 15:27:28  fisyak
// Retire tpcdaq, StMixer, bug #2580
//
// Revision 1.14  2010/11/24 19:15:37  hmasui
// Separate DbV timestamp from the main chain string in order to include it into chain1
//  
