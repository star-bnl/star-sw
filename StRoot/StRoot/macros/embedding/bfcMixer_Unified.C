//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_Unified.C,v 1.7 2013/04/28 14:41:31 fisyak Exp $
//
//////////////////////////////////////////////////////////////////////////

class StChain;
StChain  *Chain=0;
class StBFChain;
StBFChain *chain1, *chain2, *chain3;
//_____________________________________________________________________
void bfcMixer_Unified(const Int_t Nevents=1,
	      const Char_t *daqfile="/star/rcf/test/daq/2005/051/st_physics_adc_6051006_raw_1050001.daq",
	      const Char_t *tagfile="/star/rcf/test/embedding/cuProductionMinBias/FullField/P07ic/2005/051/st_physics_adc_6051006_raw_1050001.tags.root",
	      const Float_t pt_low=-0.1,
	      const Float_t pt_high=5.0,
	      const Int_t pid=9,
	      const Double_t mult = 0.1,      
	      const Char_t *mode="strange",
	      const Char_t *acc_mode="off" ) {
  // production chain and geometry for P07ib
  TString prodP07ib("P2005b DbV20070518 MakeEvent ITTF Iana ToF ssddat spt SsdIt SvtIt pmdRaw SCEbyE OGridLeak OShortR OSpaceZ2");// KeepSvtHit hitfilt skip1row");
  TString geomP07ib("ry2005f");

  // production chain and geometry for P07id 20 GeV AuAu (2001) reproduction
  TString prodP07id20GeV("ry2001 in tpc_daq tpcI fcf ITTF noSsdIt ftpc IAna VFMinuit l3onl emcDY2 ZDCvtx Physics Idst l0 tags Tree evout hitfilt corr4 OSpaceZ2 OGridLeak3D");
  TString geomP07id20GeV("ry2001");

  // production chain P07id st_gamma express stream (2007 Au+Au)
  TString prodP07idStGamma("DbV20071012 P2007 ITTF pmdReco -SsdIt -SvtIt -dstout");
  TString geomP07idStGamma("ry2007");

  // Here you choose your production options and geometry from those strings defined above
  TString prod = prodP07idStGamma;
  TString geom = geomP07idStGamma;

  TString chain1Opt("in magF tpcDb NoDefault -ittf NoOutput adcOnly");
  TString chain2Opt("NoInput PrepEmbed gen_T geomT sim_T trs -ittf -tpc_daq nodefault");
  chain2Opt += " "; chain2Opt += geom;
  TString chain3Opt = prod;
  chain3Opt += " TpcMixer Embedding onlraw GeantOut MiniMcMk -in NoInput,useInTracker EmbeddingShortCut"; 
  chain3Opt += " "; chain3Opt += geom;
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
  OutputFileName.Append("_emb.root");
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
  embMk->SetOpt(  pt_low,    pt_high,  -1.3,    1.3,    0.,   6.28); 
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
  
// $Log: bfcMixer_Unified.C,v $
// Revision 1.7  2013/04/28 14:41:31  fisyak
// Clean up after retirement of St_tpcdaq_Maker, StRTSClient and StMixer
//
// Revision 1.6  2013/04/24 15:27:28  fisyak
// Retire tpcdaq, StMixer, bug #2580
//
// Revision 1.5  2010/02/18 23:55:30  fisyak
// Add EmbeddingShortCut to chain3 for embedding with Trs
//
// Revision 1.4  2009/02/23 20:58:03  fisyak
// Add to production chain Iana option to turn on dE/dx. V0, Xi, ...
//
// Revision 1.3  2008/01/22 14:52:26  lbarnby
// Add adcOnly option. Enables efficient processing of st_gamma daq files
//
// Revision 1.2  2008/01/15 15:34:31  lbarnby
// Added arguments for pt limits, PID and multiplicity fractino or number and removed Zzvertex limit arguments as they were not being used
//
// Revision 1.1  2007/12/22 01:19:57  lbarnby
// Initial revision with P07id chains for Y7 200 GeV Au+Au st_gamma and 20 GeV Au+Au reproduction
//  
