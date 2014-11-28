// NOTE - This macro is ONLY for running MC simulation data!!
// If using StJetSimuWeightMaker class on simulation data before 2004 you will
// need to comment out all references to St_g2t_pythia class. These tables 
// were not included until 2004
//=========================================================================================
class  StChain *chain;
int total=0;
#include <string>

void RunJetSimuFinder(
                     int nevents = 100,
		      const char* file="/star/data40/reco/pp200/pythia6_205/above_35gev/cdf_a/y2004y/gheisha_on/trs_p05ie/rcf1230_10_4000evts.MuDst.root",
                      const char *fname="/star/data40/reco/pp200/pythia6_205/above_35gev/cdf_a/y2004y/gheisha_on/trs_p05ie/rcf1230_10_4000evts.event.root",
		      //const char* file="/star/institutions/lbl/kiryluk/Herwig_test/rcf1250_*.MuDst.root",
                      //const char *fname="/star/institutions/lbl/kiryluk/Herwig_test/rcf1250_*.event.root",
                      const char *outfile="Jets_out.root",
                      const char *soutfile="Simu_out.root"
		      )

{
  const char *dir ="";
  const char *filter = "";

  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("libPhysics");
  }
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  //gSystem->Load("StMagF");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StDbBroker");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");// needed by EEMC-Db
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StJetFinder");    
  gSystem->Load("StJetMaker");
  
  double pi = atan(1.0)*4.0;
  cout << " loading done " << endl;
  
  chain= new StChain("StChain"); 
  chain->SetDebug(1);
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOff("I");
  
  //StIOMaker - to read geant files
  StIOMaker* ioMaker = new StIOMaker();
  ioMaker->SetFile(fname);
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  
  // Instantiate StMcEventMaker - to get pythia pid from McEvent 
  class StMcEventMaker *mcEventMaker = new StMcEventMaker();
  mcEventMaker->doPrintEventInfo = false;
  mcEventMaker->doPrintMemoryInfo = false;
  
  //Instantiate the MuDstReader
  StMuDebug::setLevel(1); 
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,1e6,"MuDst");
  
  //Database -- get a real calibration (this is ok, MLM)
  St_db_Maker* dbMk = new St_db_Maker("db","$HOME/StarDb","MySQL:StarDb","$STAR/StarDb");
  dbMk->SetDateTime(20040427,101529); //run 5118011 status in DBx
  
  // ideal gains
  //St_db_Maker *dbMk =new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
  //dbMk->SetDateTime(20031120,0);
  //dbMk->SetFlavor("sim","bemcPed");
  //dbMk->SetFlavor("sim","bemcStatus");
  //dbMk->SetFlavor("sim","bemcCalib");
  //dbMk->SetFlavor("sim","bemcGain");
  //dbMk->SetFlavor("sim","eemcPMTcal");
  //dbMk->SetFlavor("sim","eemcPIXcal");
  
  
  //Database interface
  StDetectorDbMaker* detDbMk = new StDetectorDbMaker();
  
  //Endcap DB
  StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");
  
  //get BEMC calibration (this is ok, MLM)
  //StEmcADCtoEMaker *adc = new StEmcADCtoEMaker(); // this will just convert what's in MuDst to ADC, use for data only!
  StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this instead to "redo" converstion from geant->adc
  StPreEclMaker* preEcl = new StPreEclMaker(); //need this to fill new StEvent information
  
  //Alex/Mike TrigMaker
  StJetEmcTrigSim *htTrig = new StJetEmcTrigSim("StJetEmcTrigSim",dbMk,muDstMaker);
  
  //StEmcTrigger
  //Need to uncomment StEmcTrigger and StBemcTrigger lines in StJetSimuTrigMaker and StJetSimuTreeMaker
  StEmcTriggerMaker *emcTrig = new StEmcTriggerMaker("bemctrigger");
  emcTrig->setDbMaker(dbMk);

  //Instantiate Trigger maker for simulation
  StJetSimuTrigMaker *trig = new StJetSimuTrigMaker("SimuTrig");
  trig->setPrintOption(0);
  
  //Instantiate Maker with Pythia event record etc for simulation
  StJetSimuWeightMaker *weight = new StJetSimuWeightMaker("SimuWeight");
  weight->setPrintOption(0);
  weight->setUnpolPDF(101);//cteq
  weight->setPolPDF(103);//grsv

  //Instantiate Tree Maker for simulation
  StJetSimuTreeMaker *stree = new StJetSimuTreeMaker("SimuTree",soutfile);
  stree->setPrintOption(0);
  
  //test Mike's new 4p maker:
  bool doTowerSwapFix = false;
  StBET4pMaker* bet4pMaker = new StBET4pMaker("BET4pMaker",muDstMaker, doTowerSwapFix);
  
  //Pythia4pMaker
  StPythiaFourPMaker* pythiaFourPMaker = new StPythiaFourPMaker("StPythiaFourPMaker",weight, mcEventMaker);
  
  //Instantiate the JetMaker
  StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", muDstMaker, outfile);
  
  //Instantiate Jet Histogram Maker
  // StJetHistMaker* jetHistMaker = new StJetHistMaker(muDstMaker, histfile.Data() );
  
  //Now setup 3 jet analyses that use the same track/jet cuts  
  //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
  StppAnaPars* anapars = new StppAnaPars();
  anapars->setFlagMin(0); //track->flag() > 0
  anapars->setNhits(20); //track->nHitsFit()>20 (changed in cvs by MLM, 2/24/06)
  anapars->setCutPtMin(0.2); //track->pt() > 0.2
  anapars->setAbsEtaMax(1.6); //abs(track->eta())<1.6
  anapars->setJetPtMin(3.0);
  anapars->setJetEtaMax(100.0);
  anapars->setJetEtaMin(0);
  anapars->setJetNmin(0);
  
  //Setup the kt finder for measured particles (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
  //StKtCluPars* ktpars = new StKtCluPars();
  //ktpars->setR(0.7);
  //ktpars->setDebug(false);
  //emcJetMaker->addAnalyzer(anapars, ktpars, bet4pMaker, "KtJet");
  

  //Setup the cone finder with R=0.4
  StConePars* cpars4 = new StConePars();
  cpars4->setGridSpacing(56, -1.6, 1.6, 120, -pi, pi);
  cpars4->setConeRadius(0.4);
  cpars4->setSeedEtMin(0.5);
  cpars4->setAssocEtMin(0.1);
  cpars4->setSplitFraction(0.5);
  cpars4->setPerformMinimization(true);
  cpars4->setAddMidpoints(true);
  cpars4->setRequireStableMidpoints(true);
  cpars4->setDoSplitMerge(true);
  cpars4->setDebug(false);
  emcJetMaker->addAnalyzer(anapars, cpars4, bet4pMaker, "MkConeR04");
  
  //set the analysis cuts for pythia clustering: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
  StppAnaPars* pythiapars = new StppAnaPars();
  pythiapars->setFlagMin(0);
  pythiapars->setNhits(0);
  pythiapars->setCutPtMin(0.0001);
  pythiapars->setAbsEtaMax(5.0);
  pythiapars->setJetPtMin(3.0);
  pythiapars->setJetEtaMax(5.0);
  pythiapars->setJetEtaMin(0);
  pythiapars->setJetNmin(0);
    
  //Setup the kt finder for measured particles (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
  //StKtCluPars* pythia_ktpars = new StKtCluPars();
  //pythia_ktpars->setR(0.7);
  //pythia_ktpars->setDebug(false);
  //emcJetMaker->addAnalyzer(pythiapars, pythia_ktpars, pythiaFourPMaker, "PyKtJet");
  
 
  StConePars* pythia_cpars4 = new StConePars();
  pythia_cpars4->setGridSpacing(200, -5.0, 5.0, 120, -pi, pi);
  pythia_cpars4->setConeRadius(0.4);
  pythia_cpars4->setSeedEtMin(0.5);
  pythia_cpars4->setAssocEtMin(0.1);
  pythia_cpars4->setSplitFraction(0.5);
  pythia_cpars4->setPerformMinimization(true);
  pythia_cpars4->setAddMidpoints(true);
  pythia_cpars4->setRequireStableMidpoints(true);
  pythia_cpars4->setDoSplitMerge(true);
  pythia_cpars4->setDebug(false);
  emcJetMaker->addAnalyzer(pythiapars, pythia_cpars4, pythiaFourPMaker, "PythiaConeR04");
   
  chain->Init();
  chain->PrintInfo();
  
  //Note: ------------ Must do this stuff after Init()
  int controlVal = 2;
  controlEmcSimulatorMaker_st* simControl = emcSim->getControlSimulator()->GetTable();
  simControl->keyDB[0] = controlVal;
  simControl->keyDB[1] = 0;
  simControl->keyDB[2] = controlVal;
  simControl->keyDB[3] = controlVal;
  //keyDB[det] = 0 -> NO database (default value)
  //           = 1 - only gains are applied
  //           = 2 - gains and pedestals are applied
  // In other words, for pure MC should be 2, and
  // for embedding should be 1.
  
  chain->ls(3);
  
  TChain* fileChain = muDstMaker->chain();
  assert(fileChain);
  int ntotal = fileChain->GetEntries();
  
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "****************************************** " << endl;
    cout << "Working on eventNumber:\t" << iev <<"\tof:\t"<<ntotal<<endl;
    cout << "*************************1***************** " << endl;
    chain->Clear();
    int iret = chain->Make(iev);
    total++;
    if (iret) {
      cout << "Bad return code!" << endl;
      break;
    }
  }
  
  chain->Finish();
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;
}



