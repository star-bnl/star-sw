class StChain;
StChain *chain;
int total = 0;

void RunJetFinder2(
                   int nevents=100,
		   const char* file="/star/data54/reco/ppProductionTrans/FullField/P06ie/2006/117/7117002/st_physics_7117002_raw_1040035.MuDst.root",
                   const char* outfile="blah.jet.root",
                   const char* skimFile="blah.jetSkim.root"
                   )
{
     cout <<"Read file:\t"<<file<<endl;
     cout <<"Write file:\t"<<outfile<<endl;
     cout <<"Write file:\t"<<skimFile<<endl;
     
     gROOT->Macro("loadMuDst.C");
     gSystem->Load("StTpcDb");
     gSystem->Load("StDetectorDbMaker");
     gSystem->Load("StDbUtilities");
     gSystem->Load("StMcEvent");
     gSystem->Load("StMcEventMaker");
     gSystem->Load("StDaqLib");
     gSystem->Load("StEmcRawMaker");
     gSystem->Load("StEmcADCtoEMaker");
     gSystem->Load("StEpcMaker");
     gSystem->Load("StEmcSimulatorMaker");
     gSystem->Load("StDbBroker");
     gSystem->Load("St_db_Maker");
     gSystem->Load("StEEmcUtil");
     gSystem->Load("StEEmcDbMaker");
     gSystem->Load("StSpinDbMaker");
     gSystem->Load("StEmcTriggerMaker");
     gSystem->Load("StTriggerUtilities");
     gSystem->Load("StMCAsymMaker");
     gSystem->Load("StJetFinder");
     gSystem->Load("StJetSkimEvent");
     gSystem->Load("StJets");
     gSystem->Load("StJetMaker");
     
     cout << " loading done " << endl;

     chain= new StChain("StChain"); 
     chain->SetDebug(1);
     
     //Instantiate the MuDstReader
     StMuDebug::setLevel(1);
     StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",file,"",100000,"MuDst");
     
     //StMuDbReader...
     //StMuDbReader* db = StMuDbReader::instance();
     
     //StMuDst2StEventMaker
     //StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");
     
     //Database
     St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
     
     //EmcDb
     StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");

     //SpinDb
     StSpinDbMaker* spDbMaker = new StSpinDbMaker("spinDb");
     
     //cout <<"Database interface"<<endl;
     //StDetectorDbMaker* detDbMk = new StDetectorDbMaker();

     //EmcAdc2EMaker
     StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

     //Get TriggerMaker
     StTriggerSimuMaker *simuTrig = new StTriggerSimuMaker("StarTrigSimu");
     simuTrig->setMC(false); // must be before individual detectors, to be passed
     simuTrig->useBbc();
     simuTrig->useBemc();
     simuTrig->bemc->setConfig(StBemcTriggerSimu::kOffline);
     StGenericL2Emulator* simL2Mk = new StL2_2006EmulatorMaker;
     assert(simL2Mk);
     simL2Mk->setSetupPath("/afs/rhic.bnl.gov/star/users/kocolosk/public/StarTrigSimuSetup/");
     simL2Mk->setOutPath("./");
     simuTrig->useL2(simL2Mk);
     
     //trigger simulater
     //StEmcTriggerMaker *emcTrig = new StEmcTriggerMaker("bemctrigger");
     //emcTrig->setDbMaker(dbMk);
     
     //test Mike's new 4p maker:
     //here we also tag whether or not to do the swap:
     bool doTowerSwapFix = true;
     bool use2003TowerCuts = false;
     bool use2005TowerCuts = false;
     bool use2006Cuts = false;
     StBET4pMaker* bet4pMaker = new StBET4pMaker("BET4pMaker",muDstMaker, doTowerSwapFix);
     bet4pMaker->setUseEndcap(true);
     bet4pMaker->setUse2003Cuts(use2003TowerCuts);
     bet4pMaker->setUse2003Cuts(use2005TowerCuts);
     bet4pMaker->setUse2003Cuts(use2006Cuts);
     
     
     //Instantiate the JetMaker
     StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", muDstMaker, outfile);
     
     //And the StJetSkimEventMaker
     StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muDstMaker, skimFile);
     
     //Instantiate Jet Histogram Maker
     //StJetHistMaker* jetHistMaker = new StJetHistMaker(muDstMaker, histfile.Data() );
     
     //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
     StppAnaPars* anapars = new StppAnaPars();
     anapars->setFlagMin(0); //track->flag() > 0
     anapars->setNhits(20); //track->nHitsFit()>20
     anapars->setCutPtMin(0.2); //track->pt() > 0.2
     anapars->setAbsEtaMax(1.6); //abs(track->eta())<1.6
     anapars->setJetPtMin(5.0);
     anapars->setJetEtaMax(100.0);
     anapars->setJetEtaMin(0);
     anapars->setJetNmin(0);
     
     //Setup the kt finder for measured particles (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
     StKtCluPars* ktpars = new StKtCluPars();
     ktpars->setR(0.7);
     ktpars->setDebug(false);
     emcJetMaker->addAnalyzer(anapars, ktpars, bet4pMaker, "KtJet");
     
     chain->PrintInfo();
     chain->Init();
     
     cout <<"\tLoop on branches"<<endl;
     
     chain->EventLoop(nevents);
}
