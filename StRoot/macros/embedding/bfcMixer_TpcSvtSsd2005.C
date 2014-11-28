//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_TpcSvtSsd2005.C,v 1.5 2013/04/28 14:41:31 fisyak Exp $
//
//////////////////////////////////////////////////////////////////////////

class StChain;
StChain  *Chain=0;
class StBFChain;
StBFChain *chain1, *chain2, *chain3;
//_____________________________________________________________________
void bfcMixer_TpcSvtSsd2005(const Int_t Nevents=100,Int_t isSvtIn=1, Int_t isSsdIn=1,
		    const Char_t *daqfile="/star/rcf/test/daq/2005/051/st_physics_adc_6051006_raw_1050001.daq",
		    const Char_t *tagfile="/star/rcf/test/embedding/cuProductionMinBias/FullField/P07ic/2005/051/st_physics_adc_6051006_raw_1050001.tags.root",
		    const Double_t pt_low=0.1,
		    const Double_t pt_high=5.0,
		    const Double_t eta_low=-1.0,
		    const Double_t eta_high=1.0,
		    const Int_t pid=9,
		    const Double_t mult = 0.1) {
  // production chain for P07ib
  TString prodP07ib("P2005b DbV20070518 MakeEvent ITTF Iana ToF spt SsdIt SvtIt pmdRaw SCEbyE OGridLeak OShortR OSpaceZ2 ssd_daq");// KeepSvtHit hitfilt skip1row");
  TString geomP07ib("ry2005f");
  TString chain1Opt("in magF tpcDb NoDefault -ittf NoOutput");
  TString chain2Opt("NoInput PrepEmbed gen_T geomT sim_T trs -ittf -tpc_daq nodefault");
  chain2Opt += " "; chain2Opt += geomP07ib;
  TString chain3Opt = prodP07ib;
  chain3Opt += " TpcMixer Embedding onlraw GeantOut MiniMcMk McAna IdTruth -in NoInput,useInTracker EmbeddingShortCut"; 
  if (isSvtIn) chain3Opt += " SvtEmbed";
  if (isSsdIn) {
    chain1Opt += ",ssddat";
    chain2Opt += ",ssd,McEvent,-spt";
    chain3Opt += ",SsdEmbed";
  }
  chain3Opt += " "; chain3Opt += geomP07ib;
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
    trsMk->setNormalFactor(2.67);
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
  embMk->SetOpt(  pt_low,    pt_high,  eta_low,    eta_high,    0.,   6.283185); 
  //                pid, mult
  embMk->SetPartOpt(  pid,mult);
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
  
  
