//////////////////////////////////////////////////////////////////////////
//
// $Id: bfcMixer_Jet.C,v 1.1 2012/08/27 22:43:27 pibero Exp $
//
// $Log: bfcMixer_Jet.C,v $
// Revision 1.1  2012/08/27 22:43:27  pibero
// From PDSF
//
//
// JET EMBEDDING MACRO
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 27 July 2011
//
//////////////////////////////////////////////////////////////////////////

class StChain;
StChain* Chain = 0;

class StBFChain;
StBFChain* chain1 = 0;
StBFChain* chain2 = 0;
StBFChain* chain3 = 0;

//_____________________________________________________________________
void bfcMixer_Jet(const Int_t Nevents = 1000,
		  const Char_t* daqfile = "@run10148002.list",
		  const Char_t* fzdfile = "eliza14/SL11d_embed/10148002/pt11_15_10148002_1.fzd",
		  const Char_t* prodName = "P11idpp200",
		  const Char_t* DbVoption = "DbV20120622,DbV20120622_EMC_Calibrations,DbV20120622_EEMC_Calibrations")
{
  // Production chains for P08ic - p+p, Au+Au 9 GeV and d+Au
  TString prodP08iepp("DbV20081117 B2008a ITTF IAna ppOpt l3onl emcDY2 fpd ftpc trgd ZDCvtx NosvtIT NossdIT Corr4 OSpaceZ2 OGridLeak3D VFMCE -hitfilt");
  TString prodP08iedAu("DbV20090213 P2008 ITTF OSpaceZ2 OGridLeak3D beamLine VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10iapp("DbV20091001 pp2009c TpcRS ITTF OSpaceZ2 OGridLeak3D beamLine, VFMCE TpcRS -VFMinuit -hitfilt");

  // BES Run10 chains
  TString prodP10ihAuAu39("DbV20100909 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10ihAuAu11("DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10ihAuAu7("DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");

  // Run 9 500 GeV chain for W from Jan Balewski
  TString prodP09igpp500("DbV20101215 OGGVoltErr pp2009c ITTF VFPPVnoCTB BEmcChkStat beamLine Corr4 OSpaceZ2 OGridLeak3D -dstout -evout");

  // Run 9 200 GeV chain for jet embedding
  TString prodP10icpp200("pp2009c ITTF VFPPVnoCTB beamLine BEmcChkStat btof fmsdat Corr4 OSpaceZ2 OGridLeak3D");

  // Run 9 200 GeV FF chain
  // NOTE: "Sti" needs to be added for this chain to work !!!
  TString prodP11idpp200("DbV20120622 pp2009d ITTF Sti VFPPVnoCTB beamLine BEmcChkStat btof fmsdat Corr4 OSpaceZ2 OGridLeak3D -hitfilt");

  // Additional tags needed for embedding
  prodP09igpp500 += " TpxClu -VFMinuit VFPPVnoCTB beamLine -hitfilt";
  prodP10icpp200 += " TpxClu -VFMinuit VFPPVnoCTB beamLine -hitfilt";
  prodP11idpp200 += " TpxClu -VFMinuit VFPPVnoCTB beamLine -hitfilt";

  TString geomP08ic("ry2008");
  TString geomP10ih("ry2010");
  TString geomP09ig("ry2009a");
  TString geomP10ic("ry2009a");
  TString geomP11id("ry2009d");
  TString chain1Opt("in,magF,tpcDb,NoDefault,TpxRaw,-ittf,NoOutput");
  TString chain2Opt("fzin,gen_T,geomT,sim_T,TpcRS,-ittf,-tpc_daq,nodefault");
  chain2Opt += " ";

  TString chain3Opt;
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
  else if (prodName == "P09igpp500")   { chain3Opt = prodP09igpp500;    chain2Opt += geomP09ig; }
  else if (prodName == "P10icpp200")   { chain3Opt = prodP10icpp200;    chain2Opt += geomP10ic; }
  else if (prodName == "P11idpp200")   { chain3Opt = prodP11idpp200;    chain2Opt += geomP11id; }
  else {
    cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible." << endl;
    return;
  }
  chain3Opt.Prepend(' ');
  chain3Opt.Prepend(DbVoption);
  chain3Opt += ",Embedding,TpcMixer,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker,nodefault"; 
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
  else if (prodName == "P09igpp500")   { chain3Opt += geomP09ig; }
  else if (prodName == "P10icpp200")   { chain3Opt += geomP10ic; }
  else if (prodName == "P11idpp200")   { chain3Opt += geomP11id; }
  else {
    cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible. " << endl;
    return;
  }

  // Add BEMC simulators to chain
  chain3Opt += ",emcSim";

  // Add EEMC fast simulator to chain
  chain3Opt += ",EEfs";

  // Dynamically link some shared libs
  gROOT->LoadMacro("bfc.C");
  if (gClassTable->GetID("StBFChain") < 0) Load();
  //______________Create the main chain object______________________________________
  Chain = new StChain("Embedding");
  //________________________________________________________________________________
  bfc(-1,chain1Opt,daqfile);
  chain1 = chain;
  chain1->SetName("One");
  chain1->SetAttr(".call","SetActive(0)","St_db_Maker::"); // Use DB cache to reduce overhead
  Chain->cd();
  //________________________________________________________________________________  
  bfc(-1,chain2Opt,fzdfile);
  chain2 = chain;
  chain2->SetName("Two");
  Chain->cd();
  if (chain2->GetOption("TRS")){
    StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
    if (!trsMk) {
      cout << "Cannot find Trs in chain2" << endl;
      return;
    }
    trsMk->setNormalFactor(1.32);
    trsMk->SetMode(0);
  }
  //________________________________________________________________________________
  //  gSystem->Load("StFtpcMixerMaker");
  //  StFtpcMixerMaker  *ftpcmixer = new StFtpcMixerMaker("FtpcMixer","daq","trs");
  //________________________________________________________________________________
  TString OutputFileName(gSystem->BaseName(fzdfile));
  OutputFileName.ReplaceAll("*","");
  OutputFileName.ReplaceAll(".fzd","");
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

  if (chain2Opt.Contains("TpcRS",TString::kIgnoreCase)) {
    mixer->SetInput("Input2","TpcRS/Event");
  } else {
    mixer->SetInput("Input2","Trs/.const/Event");
  }
  Chain->cd();

  //------------------------------------ EMC MIXERS ------------------------------------
  // Add BEMC mixer to chain3
  StEmcRawMaker* emcRaw = (StEmcRawMaker*)chain3->GetMaker("emcRaw");
  emcRaw->getBemcRaw()->saveAllStEvent(true); // use all 4800 BEMC towers
  gSystem->Load("StEmcMixerMaker");
  StEmcMixerMaker* bemcMixer = new StEmcMixerMaker;
  chain3->AddAfter("EmcSimulator",bemcMixer);
  // Set EEMC fast and slow simulator in embedding mode
  StEEmcFastMaker* eefs = (StEEmcFastMaker*)chain3->GetMaker("eefs");
  eefs->SetEmbeddingMode(); // Use local StEmcCollection
  eefs->UseFullTower(true); // Use full ETOW detector
  StEEmcSlowMaker* eess = new StEEmcSlowMaker;
  eess->setEmbeddingMode(true);
  // Add EEMC mixer to chain3
  StEEmcMixerMaker* eemcMixer = new StEEmcMixerMaker;
  //------------------------------------------------------------------------------------

  //----------------------------- TRIGGER FILTER -----------------------------
  // We want to achieve the following ordering for makers:
  // 1. BBC simulator
  // 2. BEMC simulator
  // 3. BEMC mixer
  // 4. EEMC fast simulator
  // 5. EEMC slow simulator
  // 6. EEMC mixer
  // 7. Pythia event maker
  // 8. Trigger simulator
  // 9. Trigger filter
  // 10. TPC maker

  // Place TPC chain after EMC makers
  chain3->AddAfter("eefs",chain3->GetMaker("tpcChain"));
  chain3->AddAfter("eefs",eemcMixer);
  chain3->AddAfter("eefs",eess);

  // Place Pythia maker after GEANT maker
  // and trigger filter after EMC makers
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StBfcTriggerFilterMaker");

  StPythiaEventMaker* pythia = new StPythiaEventMaker;
  TString pyfile = gSystem->BaseName(fzdfile);
  pyfile.ReplaceAll(".fzd",".pythia.root");
  pythia->SetPythiaFile(pyfile);
  chain3->AddAfter("geant",pythia);

  // Place trigger simulator after EMC makers
  gSystem->Load("StTriggerUtilities");
  StTriggerSimuMaker* trgsim = new StTriggerSimuMaker;
  trgsim->setMC(1);
  // BBC was not used in Run 9 jet triggers
  //trgsim->useBbc();
  //trgsim->bbc->setSource("StEvent");
  trgsim->useBemc();
  trgsim->bemc->setConfig(StBemcTriggerSimu::kOnline);
  trgsim->useEemc();
  trgsim->eemc->setSource("StEvent");

  trgsim->bemc->setBarrelJetPatchTh0(19);
  trgsim->bemc->setBarrelJetPatchTh1(26);
  trgsim->bemc->setBarrelJetPatchTh2(34);

  trgsim->emc->setOverlapJetPatchTh0(18);
  trgsim->emc->setOverlapJetPatchTh1(25);
  trgsim->emc->setOverlapJetPatchTh2(33);

  trgsim->eemc->setEndcapJetPatchTh0(17);
  trgsim->eemc->setEndcapJetPatchTh1(24);
  trgsim->eemc->setEndcapJetPatchTh2(31);

  trgsim->bemc->setBarrelHighTowerTh0(11);
  trgsim->bemc->setBarrelHighTowerTh1(15);
  trgsim->bemc->setBarrelHighTowerTh2(18);
  trgsim->bemc->setBarrelHighTowerTh3(24);

  trgsim->eemc->setEndcapHighTowerTh0(16);
  trgsim->eemc->setEndcapHighTowerTh1(25);

  StBfcTriggerFilterMaker* trgfilt = new StBfcTriggerFilterMaker;
  // The BFC trigger filter will select only JP1, AJP and BHT3 events
  trgfilt->SetJP1();
  trgfilt->SetAJP();
  trgfilt->SetBHT3();
  // Lower all jet patch thresholds by one unit from
  // their values obtained from the database using
  // the current timestamp.
  //trgfilt->changeJPThresh(-1);
  chain3->AddBefore("tpcChain",trgsim);
  chain3->AddBefore("tpcChain",trgfilt);

  // Move these makers after trigger decision
  // *** VERY IMPORTANT ***
  // The order of TpxRaw and TpcRS *must* be preserved
  // or the embedding will *not* work. [RT# 2299]
  // http://www.star.bnl.gov/rt2/Ticket/Display.html?id=2299
  StTpcRSMaker* TpcRS = (StTpcRSMaker*)chain2->GetMaker("TpcRS");
  StTpcHitMaker* TpxRaw = (StTpcHitMaker*)chain1->GetMaker("TpxRaw");
  chain3->AddBefore("TpcMixer",TpxRaw);
  chain3->AddBefore("TpcMixer",TpcRS);

#if 0
  // Turn on debugging of DB maker
  St_db_Maker* db = (St_db_Maker*)chain1->GetMaker("db");
  db->SetDebug(2);
#endif

  //--------------------------------------------------------------------------
  TString trgfile = gSystem->BaseName(fzdfile);
  trgfile.ReplaceAll(".fzd",".trig.root");
  TFile* ofile = TFile::Open(trgfile,"recreate");
  assert(ofile);
  TH2F* hBarrelHighTowerSimu = new TH2F("hBarrelHighTowerSimu","BEMC high tower simu;trigger patch;high tower",300,0,300,64,0,64);
  TH2F* hBarrelPatchSumSimu = new TH2F("hBarrelPatchSumSimu","BEMC patch sum simu;trigger patch;patch sum",300,0,300,64,0,64);
  TH2F* hEndcapHighTowerSimu = new TH2F("hEndcapHighTowerSimu","EEMC high tower simu;trigger patch;high tower",90,0,90,64,0,64);
  TH2F* hEndcapPatchSumSimu = new TH2F("hEndcapPatchSumSimu","EEMC patch sum simu;trigger patch;patch sum",90,0,90,64,0,64);
  TH2F* hBarrelJetPatchSimu = new TH2F("hBarrelJetPatchSimu","BEMC jet patch;jet patch;adc",18,0,18,160,0,160);
  TH2F* hEndcapJetPatchSimu = new TH2F("hEndcapJetPatchSimu","EEMC jet patch;jet patch;adc",6,0,6,160,0,160);
  TH2F* hOverlapJetPatchSimu = new TH2F("hOverlapJetPatchSimu","BEMC-EEMC-overlap;jet patch;adc",6,0,6,160,0,160);
  //--------------------------------------------------------------------------
  // Initialize chain
  Chain->Init();
  PrintTimer(Chain);
  puts("Order of makers in BFCMIXER:");
  StMaker::lsMakers(Chain);
  // Event loop
  int mNTotal = 0;
  int mNFailed = 0;
  TBenchmark evnt;
  StIOMaker* inputStream = (StIOMaker*)chain1->GetMaker("inputStream");
  for (int iEvent = 1; iEvent <= Nevents; ++iEvent) {
    evnt.Reset();
    evnt.Start("QAInfo:");
    Chain->Clear();
    int iMake = Chain->Make(iEvent);
    if (iMake == kStErr) ++mNFailed;
    if (inputStream->GetMakeReturn() % 10 == kStEOF) {
      inputStream->Rewind();
      --iEvent;
      continue;
    }
    //if (iMake == kStSkip) continue;
    if (iMake % 10 == kStEOF || iMake % 10 == kStFatal) break;
    ++mNTotal;
    PrintTimer(Chain);
    //--------------------------------------------------------------------------
    // BEMC high towers and trigger patches
    for (int triggerpatch = 0; triggerpatch < 300; ++triggerpatch) {
      hBarrelHighTowerSimu->Fill(triggerpatch,trgsim->bemc->getBEMC_FEE_HT_ADC()[triggerpatch]);
      hBarrelPatchSumSimu->Fill(triggerpatch,trgsim->bemc->getBEMC_FEE_TP_ADC()[triggerpatch]);
    } // for triggerpatch
    // BEMC jet patches
    for (int jetpatch = 0; jetpatch < 18; ++jetpatch) {
      hBarrelJetPatchSimu->Fill(jetpatch,trgsim->bemc->barrelJetPatchAdc(jetpatch));
    } // for jetpatch
    // EEMC high towers and trigger patches
    for (int triggerpatch = 0; triggerpatch < 90; ++triggerpatch) {
      hEndcapHighTowerSimu->Fill(triggerpatch,trgsim->eemc->getOutHT(triggerpatch));
      hEndcapPatchSumSimu->Fill(triggerpatch,trgsim->eemc->getOutTPsum(triggerpatch));
    } // for triggerpatch
    // EEMC jet patches
    for (int jetpatch = 0; jetpatch < 6; ++jetpatch) {
      hEndcapJetPatchSimu->Fill(jetpatch,trgsim->eemc->endcapJetPatchAdc(jetpatch));
    } // for jetpatch
    // BEMC-EEMC-overlap jet patches
    for (int i = 0; i < 2; ++i) {
      int jetpatch, adc;
      trgsim->emc->getOverlapJetPatchAdc(i,jetpatch,adc);
      hOverlapJetPatchSimu->Fill(jetpatch,adc);
    } // for i
    //--------------------------------------------------------------------------
    evnt.Stop("QAInfo:");
    printf("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time %d.%d/sta %d] Real Time = %10.2f seconds Cpu Time = %10.2f seconds\n",iEvent,Chain->GetRunNumber(),Chain->GetEventNumber(),Chain->GetDate(),Chain->GetTime(),chain3->GetMakeReturn(),evnt.GetRealTime("QAInfo:"),evnt.GetCpuTime("QAInfo:"));
  } // End event loop
  printf("QAInfo:EventLoop completed code %d\n",iMake);
  gSystem->Exec("date");
  TDatime t;
  printf("QAInfo:Run is finished at Date/Time %i/%i; Total events processed: %i and not completed: %i\n",t.GetDate(),t.GetTime(),mNTotal,mNFailed);
  //--------------------------------------------------------------------------
  ofile->Write();
  ofile->Close();
  //--------------------------------------------------------------------------
}

// Print timers for all makers in chain
class StMaker;
void PrintTimer(StMaker* chain)
{
  TIter next(chain->GetMakeList());
  StMaker* maker;
  while (maker = (StMaker*)next()) {
    maker->PrintTimer();
    PrintTimer(maker);
    // Hack to reset timer
    maker->StartTimer(true);
    maker->StopTimer();
  }
}
