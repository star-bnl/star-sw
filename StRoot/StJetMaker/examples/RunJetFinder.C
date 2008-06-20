void RunJetFinder(
		  int nevents = 100,
		   const char* file = "/star/data30/reco/ppProductionLong/FullField/P06ie/2006/140/7140051/st_physics_7140051_raw_2020002.MuDst.root",
                   const char* outFile = "blah.jet.root",
                   const char* skimFile = "blah.jetSkim.root"
		  )
{
  setMacroPath();
  gROOT->Macro("LoadJetLibraries.C");

  chain= new StChain("StChain"); 

  StMuDstMaker* muDstMaker = new StMuDstMaker(0, 0, "", file, "", 100000, "MuDst");

  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");

  StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");

  StSpinDbMaker* spDbMaker = new StSpinDbMaker("spinDb");

  StEmcADCtoEMaker *adc = new StEmcADCtoEMaker("Eread");

  StTriggerSimuMaker *simuTrig = new StTriggerSimuMaker("StarTrigSimu");
  simuTrig->setMC(false);
  simuTrig->useBbc();
  simuTrig->useBemc();
  simuTrig->bemc->setConfig(StBemcTriggerSimu::kOffline);
  StGenericL2Emulator* simL2Mk = new StL2_2006EmulatorMaker;
  assert(simL2Mk);
  simL2Mk->setSetupPath("./StRoot/StJetMaker/StarTrigSimuSetup/");
  char outPath[200];
  sprintf(outPath,"./out/");   
  simL2Mk->setOutPath(outPath);
  simuTrig->useL2(simL2Mk);

  bool doTowerSwapFix = true;
  StBET4pMaker* bet4pMaker = new StBET4pMaker("BET4pMaker",muDstMaker, doTowerSwapFix);
  bet4pMaker->setUse2003Cuts(false);
  bet4pMaker->setUseEndcap(true);
  bet4pMaker->setUse2006Cuts(true);

  StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", muDstMaker, outFile);

  StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muDstMaker, skimFile);

  StppAnaPars* anapars = new StppAnaPars();
  anapars->setFlagMin(0);
  anapars->setNhits(5);
  anapars->setCutPtMin(0.2);
  anapars->setAbsEtaMax(2.0);
  anapars->setJetPtMin(3.5);
  anapars->setJetEtaMax(100.0);
  anapars->setJetEtaMin(0);
  anapars->setJetNmin(0);

  StConePars* cpars = new StConePars();
  double pi = atan(1.0)*4.0;
  cpars->setGridSpacing(105, -3.0, 3.0, 120, -pi, pi);  //include EEMC
  cpars->setConeRadius(0.7);
  cpars->setSeedEtMin(0.5);
  cpars->setAssocEtMin(0.1);
  cpars->setSplitFraction(0.5);
  cpars->setPerformMinimization(true);
  cpars->setAddMidpoints(true);
  cpars->setRequireStableMidpoints(true);
  cpars->setDoSplitMerge(true);
  cpars->setDebug(false);
  emcJetMaker->addAnalyzer(anapars, cpars, bet4pMaker, "ConeJets5");

  anapars->setNhits(12);
  emcJetMaker->addAnalyzer(anapars, cpars, bet4pMaker, "ConeJets12");

  anapars->setNhits(10000000);
  emcJetMaker->addAnalyzer(anapars, cpars, bet4pMaker, "ConeJetsEMC");

  chain->Init();

  for (Int_t iev = 0; iev < nevents; ++iev) {

    if(iev % 1 == 0) {
      cout << "****************************************** " << endl;
      cout << "Working on eventNumber " << iev << endl;
      cout << "****************************************** " << endl;
    }
    chain->Clear();
    int iret = chain->Make(iev); 
    if (iret && iret!=kStSkip) {
      cout << "Bad return code!" << endl;
      break;
    }
  } 

  chain->Finish(); 

}

void setMacroPath()
{
  TString path(gROOT->GetMacroPath());
  path = TString(gSystem->Getenv("STAR")) + "/StRoot/StJetMaker/macros:" + path;
  path = "./StRoot/StJetMaker/macros:" + path;
  path = "./StJetMaker/macros:" + path;
  path = "./macros:" + path;
  path = "../macros:" + path;
  path = ".:" + path;
  gROOT->SetMacroPath(path);
}
