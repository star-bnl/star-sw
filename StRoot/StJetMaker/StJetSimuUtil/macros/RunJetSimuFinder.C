
// NOTE - This macro is ONLY for running MC simulation data!!
// If using StJetSimuWeightMaker class on simulation data before 2004 you will
// need to comment out all references to St_g2t_pythia class. These tables 
// were not included until 2004
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunJetSimuFinder(int nevents = 10,
		      const char *dir = "",
		      const char* file = "/star/data19/reco/pp200/pythia6_203/default/pt15/y2004x/gheisha_on/trs_ii/pds1214_02_5000evts.MuDst.root",
		      const char *fname="/star/data19/reco/pp200/pythia6_203/default/pt15/y2004x/gheisha_on/trs_ii/pds1214_02_5000evts.geant.root",
		      const char *filter = "",
		      const char *outfile="Jets_out.root",
		      const char *soutfile="Simu_out.root"
		      )
{
    if (gClassTable->GetID("TTable") < 0) {
	gSystem->Load("libStar");
	gSystem->Load("libPhysics");
    } 
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");  
    gSystem->Load("St_db_Maker");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StEEmcUtil");// needed by EEMC-Db
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
    ioMaker->SetBranch("eventBranch",0,"r");   //activate event Branch
    //ioMaker->SetDebug();
  
    // Instantiate StMcEventMaker - to get pythia pid from McEvent 
    class StMcEventMaker *mcEventMaker = new StMcEventMaker();
    mcEventMaker->doPrintEventInfo = false;
    mcEventMaker->doPrintMemoryInfo = false;

    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,10,"MuDst");    

    //Instantiate Trigger maker for simulation -- only need ADC from MuDst -- no calibration
    StJetSimuTrigMaker *trig=new StJetSimuTrigMaker("SimuTrig");
    trig->setPrintOption(1);
  
    //Database -- must set flavor correctly for ideal gains
    St_db_Maker *dbMk =new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
    dbMk->SetDateTime(20031120,0);
    dbMk->SetFlavor("sim","bemcPed");
    dbMk->SetFlavor("sim","bemcStatus");
    dbMk->SetFlavor("sim","bemcCalib");
    dbMk->SetFlavor("sim","bemcGain");
    dbMk->SetFlavor("sim","eemcPMTcal");
    dbMk->SetFlavor("sim","eemcPIXcal");

    //EmcDb
    StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");

    //StMuDst2StEventMaker - make StEvent from StMuDst
    StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");
  
    //get BEMC calibration
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
  
    //Instantiate Maker with Pythia event record etc for simulation
    StJetSimuWeightMaker *weight= new StJetSimuWeightMaker("SimuWeight");
    weight->setPrintOption(0);
  
    //Instantiate Tree Maker for simulation
    StJetSimuTreeMaker *stree= new StJetSimuTreeMaker("SimuTree",soutfile);
    stree->setPrintOption(0);
  
    //Instantiate the StEmcTpcFourPMaker
    StEmcTpcFourPMaker* emcFourPMaker = new StEmcTpcFourPMaker("EmcTpcFourPMaker", muDstMaker, 30, 30, .3, .3, .003, adc);
    emcFourPMaker->setUseType(StEmcTpcFourPMaker::Hits);//if don't have this line then default is 0 (which is hits)
  
    //Instantiate the JetMaker
    StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", muDstMaker, outfile);
  
    //Now setup two jet analyses that use the same track/jet cuts  
    //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
    StppAnaPars* anapars = new StppAnaPars();
    anapars->setFlagMin(0); //track->flag() > 0
    anapars->setNhits(15); //track->nHitsFit()>15
    anapars->setCutPtMin(0.2); //track->pt() > 0.2
    anapars->setAbsEtaMax(1.6); //abs(track->eta())<1.6
    anapars->setJetPtMin(5.0);
    anapars->setJetEtaMax(100.0);
    anapars->setJetEtaMin(0);
    anapars->setJetNmin(0);
  
    //Setup the cone finder (See StJetFinder/StConeJetFinder.h -> class StConePars)
    StConePars* cpars = new StConePars();
    cpars->setGridSpacing(56, -1.6, 1.6, 120, -pi, pi);
    cpars->setConeRadius(0.7);
    cpars->setSeedEtMin(0.5);
    cpars->setAssocEtMin(0.1);
    cpars->setSplitFraction(0.0);
    cpars->setPerformMinimization(true);
    cpars->setAddMidpoints(true);
    cpars->setRequireStableMidpoints(true);
    cpars->setDoSplitMerge(true);
    cpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, cpars, emcFourPMaker, "MkConeJetsPt02R07");
    
    //Setup the cone finder (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
    StKtCluPars* ktpars = new StKtCluPars();
    ktpars->setR(1.0);
    ktpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, ktpars, emcFourPMaker, "MkKtJet");

    chain->Init();
    chain->PrintInfo();
    chain->ls(3);

    //int nevents =1000000;
    for (Int_t iev=0;iev<nevents; iev++) {
	cout << "****************************************** " << endl;
	cout << "Working on eventNumber " << iev << endl;
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







