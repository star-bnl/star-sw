//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_Tpx.C,v 1.4 2009/02/23 20:58:03 fisyak Exp $
//
//////////////////////////////////////////////////////////////////////////

class StChain;
StChain  *Chain=0;
class StBFChain;
StBFChain *chain1, *chain2, *chain3;
//_____________________________________________________________________
void bfcMixer_Tpx(const Int_t Nevents=100,
		  const Char_t *daqfile="/star/rcf/test/daq/2008/070/st_physics_adc_9070006_raw_1410001.daq",
		  const Char_t *tagfile="/star/rcf/test/embedding/ppProduction2008/2008/070/st_physics_adc_9070006_raw_1410001.tags.root",
		  const Double_t pt_low=0.1,
		  const Double_t pt_high=5.0,
                  const Double_t eta_low=-1.0,
                  const Double_t eta_high=1.0,
		  const Int_t pid=9,
		  const Double_t mult=0.05,
                  const Char_t *prodName = "P08icpp") {
  // production chains for P08ic - p+p, Au+Au 9 GeV and d+Au
  TString prodP08icpp("DbV20080712,pp2008,ITTF,Iana,OSpaceZ2,OGridLeak3D,beamLine");
  TString prodP08icAuAu9("DbV20080709 P2008 ITTF");
  TString prodP08icdAu("DbV20080712 P2008 ITTF Iana OSpaceZ2 OGridLeak3D beamLine");
  TString geomP08ic("ry2008");
  TString chain1Opt("in,magF,tpcDb,NoDefault,TpxRaw,-ittf,NoOutput");
  TString chain2Opt("NoInput,PrepEmbed,gen_T,geomT,sim_T,trs,-ittf,-tpc_daq,nodefault");
  chain2Opt += " "; chain2Opt += geomP08ic;
  if (prodName == "P08icpp") {   TString chain3Opt = prodP08icpp; }
  else if (prodName == "P08icAuAu9") {   TString chain3Opt = prodP08icAuAu9; }
  else if (prodName == "P08icdAu") {   TString chain3Opt = prodP08icdAu; }
  else {
    cout << "Choice prodName does not correspond to known chain. Processing impossible. " << endl;
    return;
  }
  chain3Opt += ",TpcMixer,TpxClu,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker,nodefault"; 
  chain3Opt += ","; chain3Opt += geomP08ic;
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
  //________________________________________________________________________________
  StTpcMixerMaker  *mixer = (StTpcMixerMaker *) chain3->Maker("TpcMixer");
  mixer->SetInput("Input1","TpxRaw/.data/Event");
  mixer->SetInput("Input2","Trs/.const/Event");
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
