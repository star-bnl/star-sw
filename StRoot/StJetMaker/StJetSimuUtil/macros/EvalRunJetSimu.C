
// NOTE - This macro is ONLY for running MC simulation data!!
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;
void RunJetSimu(int nevents = 10,
		const char *dir = "/star/data19/reco/pp200/pythia6_203/default/pt15/y2004x/gheisha_on/trs_ii/",
		const char* file = "pds1214_02_5000evts.MuDst.root",
		const char *fname="/star/data19/reco/pp200/pythia6_203/default/pt15/y2004x/gheisha_on/trs_ii/pds1214_02_5000evts.geant.root",
		const char *filter = "",
		const char *outfile="test",
		const char *Eout = "EMC_test.root")
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
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StMcAnalysisMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StEEmcUtil");// needed by EEMC-Db
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");
    assert(gSystem->Load("StRFEvalMaker")==0);
  
    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
    
    chain= new StChain("StChain"); 
    chain->SetDebug(1);
    gMessMgr->SwitchOff("D");
    gMessMgr->SwitchOff("I");

    // StIOMaker - to read files ...
    StIOMaker* ioMaker = new StIOMaker();
    ioMaker->SetFile(fname);
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");             //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
    ioMaker->SetBranch("eventBranch",0,"r");   //activate event Branch
    //ioMaker->SetDebug();
  
    // StMcEventMaker -> get pid from pythia subprocess out
    class StMcEventMaker *mcEventMaker = new StMcEventMaker();
    mcEventReader->doPrintEventInfo = false;
    mcEventReader->doPrintMemoryInfo = false;
  
    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,10,"MuDst");    

    //make trigger from ADC in MuDst
    StJetSimuTrigMaker *trig=new StJetSimuTrigMaker("SimuTrig");
    trig->setPrintOption(0);
  
    //In simulations must set the timestamp and database flavor in order to get ideal gains for BEMC/EEMC
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

    //StMuDst2StEventMaker - make StEvent from MuDst
    StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");

    //get BEMC calibration
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker("Eread");

    //make four vectors for jet
    StEmcTpcFourPMaker* emcFourPMaker = new StEmcTpcFourPMaker("EmcTpcFourPMaker", muDstMaker, 30, 30, .3, .3, .003, adc);
    emcFourPMaker->setUseType(StEmcTpcFourPMaker::Hits);//if don't have this line then default is 0 (which is hits)

    //make weight for asymmetries
    StJetSimuWeightMaker *weight= new StJetSimuWeightMaker("SimuWeight");
    weight->setPrintOption(0);
  
    //run jet finder
    char* emcOutfile = new char[256];
    strcpy(emcOutfile, outfile); strcat(emcOutfile, "emc");
    StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", emcFourPMaker, muDstMaker, emcOutfile);
  
    //make simulation tree
    StRFEvalMaker *eval = new StRFEvalMaker("RFEval",Eout);
    eval->setPrintOption(1);

    //set1 == coneR=0.7,setConeSeedEtMin=0.5,setCutPtMin=0.2,setSplitFraction=0.0
    //set2 == coneR=0.7,setConeSeedEtMin=1.0,setCutPtMin=0.35,setSplitFraction=0.0
    //set3 == coneR=0.7,setConeSeedEtMin=0.5,setCutPtMin=0.2,setSplitFraction=0.5
    //set4 == coneR=0.7,setConeSeedEtMin=1.0,setCutPtMin=0.2,setSplitFraction=0.0
    //set5 == setClusterR(1.0),setCutPtMin(0.2),setHitRatio(hitRatio);setJetPtMin(5.0)

    //set the analysis cuts: (see StppJetAnalyzer -> class StppAnaPars )
    StppAnaPars* anapars = new StppAnaPars();
    anapars->setFlagMin(0); //track->flag() > 0
    anapars->setNhits(15);
    anapars->setCutPtMin(0.2);
    anapars->setAbsEtaMax(1.6);
    anapars->setJetPtMin(5.0);
    anapars->setJetEtaMax(100.0);
    anapars->setJetEtaMin(0);
    anapars->setJetNmin(0);

    //now setup jetfinders
    StConePars* cpars = new StConePars();
    //set the grid characteristics
    cpars->setGridSpacing(56, -1.6, 1.6, 120, -pi, pi);
    //behavior characteristics
    cpars->setConeRadius(0.7);
    cpars->setSeedEtMin(0.5);
    cpars->setAssocEtMin(0.1);
    cpars->setSplitFraction(0.0);
    cpars->setPerformMinimization(true);
    cpars->setAddMidpoints(true);
    cpars->setRequireStableMidpoints(true);
    cpars->setDoSplitMerge(true);
    cpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, cpars, "MkConeJetsPt02R07");

  
    StKtCluPars* ktpars = new StKtCluPars();
    ktpars->setR(1.0);
    ktpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, ktpars, "MkKtJet");
 

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







