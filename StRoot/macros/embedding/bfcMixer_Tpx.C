//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_Tpx.C,v 1.1 2008/07/31 20:48:21 fisyak Exp $
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
		  const Float_t zvertex_low=-175.0,
		  const Float_t zvertex_high=175.0,
		  const Char_t *mode="strange",
		  const Char_t *acc_mode="off" ) {
  // production chain for P08ic
  TString prodP08ic("DbV20080712,pp2008,ITTF,OSpaceZ2,OGridLeak3D,beamLine");
  TString geomP08ic("ry2008");
  TString chain1Opt("in,magF,tpcDb,NoDefault,TpxRaw,-ittf,NoOutput");
  TString chain2Opt("NoInput,PrepEmbed,gen_T,geomT,sim_T,trs,-ittf,-tpc_daq,nodefault");
  chain2Opt += " "; chain2Opt += geomP08ic;
  TString chain3Opt = prodP08ic;
  chain3Opt += ",Embedding,TpcMixer,TpxClu,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker,nodefault"; 
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
  embMk->SetOpt(  0.1,    5.,  -1.3,    1.3,    0.,   6.28); 
  //                pid, mult
  embMk->SetPartOpt(  8,0.05);
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
