#if !defined(__CINT__) || defined(__MAKECINT__)

#include <TSystem.h>
#include <TFile.h>
#include <TString.h>
#include <TROOT.h>
#include <TStopwatch.h>
#include <TChain.h>
#include <TObjArray.h>
#include <TCollection.h>

#include <St_base/StMessMgr.h>
#include <StarRoot/TMemStat.h>
#include <StChain/StChain.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst2StEventMaker.h>
#include <StIOMaker/StIOMaker.h>
#include <St_db_Maker/St_db_Maker.h>
#include <StEEmcDbMaker/StEEmcDbMaker.h>
#include <StSpinPool/StSpinDbMaker/StSpinDbMaker.h>
#include <StDetectorDbMaker/StDetectorDbMaker.h>
#include <StEmcSimulatorMaker/StEmcSimulatorMaker.h>
//#include <tables/St_controlEmcSimulatorMaker_Table.h>
#include <StMcEventMaker/StMcEventMaker.h>
#include <StEmcADCtoEMaker/StEmcADCtoEMaker.h>
#include <StEmcTriggerMaker/StEmcTriggerMaker.h>
#include <StEmcMixerMaker/StEmcPreMixerMaker.h>
#include <StEmcMixerMaker/StEmcMixerMaker.h>
#include <StPreEclMaker/StPreEclMaker.h>
#include <StEpcMaker/StEpcMaker.h>
#include <StJetMaker/StJetMaker.h>
#include <StJetMaker/StppAnaPars.h>
#include <StJetMaker/StBET4pMaker.h>
#include <StJetFinder/StConeJetFinder.h>

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>
#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

#include <StEmcPool/StTimeRandomizerMaker/StTimeRandomizerMaker.h>

#include <StEmcPool/StPi0DataMaker/StPi0DataMaker.h>
#include <StEmcPool/StPi0DataMaker/StPi0DataSaveMaker.h>

#endif

TMyDataAnalysisSettings dataAnalysisSettings;

void run_data(long nevents = 1, char *outputFile = "/dev/null", char *file = "filelist.list", char *configFile = "config_default.C") {
    {gMessMgr->Info() << "================== STARTED ===================" << endm;}
    TDatime startTime;
    TStopwatch timer;
    {gMessMgr->Info() << "Started: " << startTime.AsSQLString() << endm;}
    timer.Start();
  
    {gMessMgr->Info() << "Number of events to process: " << nevents << ((nevents < 0) ? " (all)" : "") << endm;}
    {gMessMgr->Info() << "Output file: " << outputFile << endm;}
    {gMessMgr->Info() << "Input file: " << file << endm;}
    {gMessMgr->Info() << "Configuration file: " << configFile << endm;}
    TString fileExact = findFile(file);
    {gMessMgr->Info() << "Input file (exact): " << fileExact << endm;}

    const Char_t *defaultConfigFile = "config_default.C";
    Int_t configFileError;
    {gMessMgr->Info() << "Reading default configuration file " << defaultConfigFile << endm;}
    gROOT->Macro(defaultConfigFile); // initialize configuration
    {gMessMgr->Info() << "Reading configuration file " << configFile << endm;}
    gROOT->Macro(configFile, &configFileError);
    if (configFileError != 0) {gMessMgr->Error() << "Error code " << configFileError << endm;}

    Int_t numberOfFiles = 1;

    dataAnalysisSettings.StPi0Common_Version_data = TMyDataAnalysisSettings::Class_Version();
    dataAnalysisSettings.StPi0DataStructures_Version_data = TMyEventData::Class_Version();
    dataAnalysisSettings.StPi0DataMaker_Version_data = StPi0DataMaker::Class_Version();
    dataAnalysisSettings.StPi0DataSaveMaker_Version_data = StPi0DataSaveMaker::Class_Version();
    dataAnalysisSettings.StTimeRandomizerMaker_Version_data = StTimeRandomizerMaker::Class_Version();
    dataAnalysisSettings.spinDbMakerName = "spinDb";
  
    Bool_t useEventRoot = false;
    if (fileExact.BeginsWith("@") || fileExact.BeginsWith("filelist:") || fileExact.EndsWith(".list") || fileExact.EndsWith(".lis")) {
	{gMessMgr->Info() << "Read filelist: " << fileExact << endm;}
        TString fileToRead = fileExact;
        TString prefix;
        TString pre = "://";
        Int_t colPos = fileToRead.Index(pre);
        if (colPos >= 0) {
            prefix = fileToRead(0, colPos + pre.Length());
            fileToRead = fileToRead(colPos + pre.Length(), fileToRead.Length() - (colPos + pre.Length()));
        } else {
            pre = ":";
            colPos = fileToRead.Index(pre);
            if (colPos >= 0) {
                prefix = fileToRead(0, colPos + pre.Length());
                fileToRead = fileToRead(colPos + pre.Length(), fileToRead.Length() - (colPos + pre.Length()));
            }
        }
	ifstream ifstr(fileToRead);
	if (ifstr.good()) {
	    Char_t buffer[1024];
	    ifstr.getline(buffer, 1024);
	    TString filename(buffer);
	    {gMessMgr->Info() << "Read first line in filelist: " << filename << endm;}
	    if (filename.Contains(".event.root")) useEventRoot = true;
	} else {
	    {gMessMgr->Error() << "Cannot open filelist " << file << " !" << endm;}
	    dataAnalysisSettings.useFullJetMaker = false;
	}
    } else {
	if (fileExact.Contains(".event.root")) useEventRoot = true;
    }
    {gMessMgr->Info() << "Using event.root = " << useEventRoot << endm;}

    {gMessMgr->Info() << "Creating TChain..." << endm;}
    StChain *chain = new StChain("StChain"); 
    {gMessMgr->Info() << "Created " << chain << endm;}

    StMuDstMaker *muDstMaker = 0;
    St_db_Maker *dbMaker = 0;
    StEmcSimulatorMaker *emcSim = 0;
    StEmcADCtoEMaker *adcToEMaker = 0;

    if (useEventRoot) {
        TString fileStr = fileExact;
        if ((fileStr.EndsWith(".list") || fileStr.EndsWith(".lis")) && (!fileStr.BeginsWith("@"))) fileStr.Prepend("@");
        {gMessMgr->Info() << "Creating StIOMaker..." << endm;}
        StIOMaker* ioMaker = new StIOMaker("IO","r",fileStr);
        {gMessMgr->Info() << "Created " << ioMaker << endm;}
	if (ioMaker) {
    	    ioMaker->SetIOMode("r");
    	    ioMaker->SetBranch("*",0,"0");           //deactivate all branches
    	    ioMaker->SetBranch("geantBranch",0,"r"); //activate geant Branch
    	    //ioMaker->SetBranch("dstBranch",0,"r");   //activate dst Branch
    	    ioMaker->SetBranch("eventBranch",0,"r"); //activate Event Branch
	}
	dataAnalysisSettings.useFullJetMaker = false;
    } else {
        {gMessMgr->Info() << "Creating StMuDstMaker..." << endm;}
        muDstMaker = new StMuDstMaker(0, 0, "", fileExact, "MuDst.root", 5000);
        {gMessMgr->Info() << "Created " << muDstMaker << endm;}
	if (muDstMaker) {
    	    TChain *filesChain = muDstMaker->chain();
    	    TObjArray *filesList = filesChain ? filesChain->GetListOfFiles() : 0;
	    if (filesList) numberOfFiles = filesList->GetEntries();
	}

        {gMessMgr->Info() << "Creating StMuDst2StEventMaker..." << endm;}
        StMuDst2StEventMaker *muDst2StEventMaker = new StMuDst2StEventMaker("muDst2StEvent");
        {gMessMgr->Info() << "Created " << muDst2StEventMaker << endm;}
    }

    if (dataAnalysisSettings.isSimulation) {
	{gMessMgr->Info() << "Creating StTimeRandomizerMaker..." << endm;}
	StTimeRandomizerMaker *timeRandomizer = new StTimeRandomizerMaker("timeRandomizer");
	{gMessMgr->Info() << "Created " << timeRandomizer << endm;}
	if (timeRandomizer) {
	    timeRandomizer->setRunTimesFilename(findFile(dataAnalysisSettings.timestampsFilename));
	    timeRandomizer->setNormalizeEventsTotal(dataAnalysisSettings.timeRandomNormEventsTotal);
	    timeRandomizer->setDatasetNameStEvent(dataAnalysisSettings.datasetNameStEvent);
	    timeRandomizer->setSeed(0);
	    timeRandomizer->setBaseEventId(0);
	}
    }

    {gMessMgr->Info() << "Creating St_db_Maker..." << endm;}
    if (dataAnalysisSettings.useLocalDB) {
	{gMessMgr->Info() << "Using local DB: " << dataAnalysisSettings.localDBpath << endm;}
	dbMaker = new St_db_Maker("db", dataAnalysisSettings.localDBpath, "MySQL:StarDb", "$STAR/StarDb");
    } else {
	dbMaker = new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
    }
    {gMessMgr->Info() << "Created " << dbMaker << endm;}
    if (dbMaker) {
        //dbMaker->SetFlavor("pi0");
        //dbMaker->SetFlavor("pi0", "bemcStatus");
        //dbMaker->SetDateTime(20300101, 000001);
        if (dataAnalysisSettings.isSimulation) {
	    //dbMaker->SetDateTime(20030211, 000001);
	    if (dataAnalysisSettings.useSimCalibFlavour) {
		dbMaker->SetFlavor("sim", "bsmdeCalib");
		dbMaker->SetFlavor("sim", "bsmdpCalib");
	    }
        }
    }

    if (dataAnalysisSettings.useFullJetMaker) {
        //EmcDb
        {gMessMgr->Info() << "Creating StEEmcDbMaker..." << endm;}
        StEEmcDbMaker *eemcDbMaker = new StEEmcDbMaker("eemcDb");
        {gMessMgr->Info() << "Created " << eemcDbMaker << endm;}
    }
    if (dataAnalysisSettings.useSpinDbMaker) {
        //SpinDb
        {gMessMgr->Info() << "Creating StSpinDbMaker..." << endm;}
        StSpinDbMaker *spinDbMaker = new StSpinDbMaker(dataAnalysisSettings.spinDbMakerName);
        {gMessMgr->Info() << "Created " << spinDbMaker << endm;}
    }
        //Database interface
        //{gMessMgr->Info() << "Creating StDetectorDbMaker..." << endm;}
        //StDetectorDbMaker *detectorDbMaker = new StDetectorDbMaker();
        //{gMessMgr->Info() << "Created " << detectorDbMaker << endm;}

    dataAnalysisSettings.jetMakerName = "emcJetMaker";
    dataAnalysisSettings.adcToEMakerName = dataAnalysisSettings.isEmbedding ? "Eread" : "adToEmaker"; // Do NOT change this name "Eread"! It is hardcoded in some standard BEMC makers, and in the Jet maker.
    dataAnalysisSettings.triggerFullSimulatorName = "FullTriggerSimulator";
    dataAnalysisSettings.triggerFullSimulatorNameEmbed = "FullTriggerSimulatorEmbed";
    dataAnalysisSettings.triggerFullSimulatorNameFinal = "FullTriggerSimulatorFinal";

    if (dataAnalysisSettings.isSimulation) {
	{gMessMgr->Info() << "Creating StMcEventMaker..." << endm;}
        StMcEventMaker *mcEvent = new StMcEventMaker("McEventMaker");
	{gMessMgr->Info() << "Created " << mcEvent << endm;}

	{gMessMgr->Info() << "Creating StEmcSimulatorMaker..." << endm;}
        emcSim = new StEmcSimulatorMaker("emcSim");
	{gMessMgr->Info() << "Created " << emcSim << endm;}
    } else if (dataAnalysisSettings.isEmbedding) {
        {gMessMgr->Info() << "Creating StEmcADCtoEMaker EReadEmbed1" << endm;}
        StEmcADCtoEMaker *adcToE1 = new StEmcADCtoEMaker(dataAnalysisSettings.adcToEMakerName + "1");
        {gMessMgr->Info() << "Created " << adcToE1 << endm;}
	if (adcToE1) {
    	    //adcToE1->setRemoveGhostEvent(kTRUE);
    	    adcToE1->saveAllStEvent(kTRUE);
	}
    
        if (dataAnalysisSettings.useTriggerSimulatorOriginal) {
		{gMessMgr->Info() << "Creating StEmcTriggerMaker" << endm;}
		StEmcTriggerMaker *trgFullSim = new StEmcTriggerMaker(dataAnalysisSettings.triggerFullSimulatorName);
		{gMessMgr->Info() << "Created " << trgFullSim << endm;}
	}

        {gMessMgr->Info() << "Creating StEmcPreMixerMaker" << endm;}
        StEmcPreMixerMaker *preMix = new StEmcPreMixerMaker("preEmbed");
        {gMessMgr->Info() << "Created " << preMix << endm;}

        {gMessMgr->Info() << "Creating StMcEventMaker" << endm;}
        StMcEventMaker *mcEvent = new StMcEventMaker();
	{gMessMgr->Info() << "Created " << mcEvent << endm;}

        {gMessMgr->Info() << "Creating StEmcSimulatorMaker" << endm;}
        emcSim = new StEmcSimulatorMaker();
	{gMessMgr->Info() << "Created " << emcSim << endm;}

        {gMessMgr->Info() << "Creating StEmcMixerMaker" << endm;}
        StEmcMixerMaker *mix = new StEmcMixerMaker();
        {gMessMgr->Info() << "Created " << mix << endm;}
	if (mix) {
	    mix->setEmbedAll(kTRUE);
	}
    
        {gMessMgr->Info() << "Creating StEmcADCtoEMaker EReadEmbed2" << endm;}
        StEmcADCtoEMaker *adcToE2 = new StEmcADCtoEMaker(dataAnalysisSettings.adcToEMakerName + "2");
        {gMessMgr->Info() << "Created " << adcToE2 << endm;}
	if (adcToE2) {
    	    adcToE2->setEmbeddingMode(kTRUE);
    	    adcToE2->saveAllStEvent(kTRUE);
	}

        if (dataAnalysisSettings.useTriggerSimulatorEmbed) {
    	        {gMessMgr->Info() << "Creating StEmcTriggerMaker embed" << endm;}
    	        StEmcTriggerMaker *trgFullSimEmbed = new StEmcTriggerMaker(dataAnalysisSettings.triggerFullSimulatorNameEmbed);
	        {gMessMgr->Info() << "Created " << trgFullSimEmbed << endm;}
	}

        {gMessMgr->Info() << "Creating StEmcADCtoEMaker Eread" << endm;}
        adcToEMaker = new StEmcADCtoEMaker(dataAnalysisSettings.adcToEMakerName);
        {gMessMgr->Info() << "Created " << adcToEMaker << endm;}
	if (adcToEMaker) {
	    adcToEMaker->setEmbeddingMode(kTRUE);
	}
    } else {
        {gMessMgr->Info() << "Creating StEmcADCtoEMaker Eread1" << endm;}
        StEmcADCtoEMaker *adcToE1 = new StEmcADCtoEMaker(dataAnalysisSettings.adcToEMakerName + "1");
        {gMessMgr->Info() << "Created " << adcToE1 << endm;}
        if (adcToE1) {
	    adcToE1->saveAllStEvent(kTRUE);
	}
  
        if (dataAnalysisSettings.useTriggerSimulatorOriginal) {
                {gMessMgr->Info() << "Creating StEmcTriggerMaker" << endm;}
                StEmcTriggerMaker *trgFullSim = new StEmcTriggerMaker(dataAnalysisSettings.triggerFullSimulatorName);
                {gMessMgr->Info() << "Created " << trgFullSim << endm;}
	}

        {gMessMgr->Info() << "Creating StEmcADCtoEMaker" << endm;}
        adcToEMaker = new StEmcADCtoEMaker(dataAnalysisSettings.adcToEMakerName);
        {gMessMgr->Info() << "Created " << adcToEMaker << endm;}
    }

    {gMessMgr->Info() << "Creating StEmcTriggerMaker final" << endm;}
    StEmcTriggerMaker *trgFullSimFinal = new StEmcTriggerMaker(dataAnalysisSettings.triggerFullSimulatorNameFinal);
    {gMessMgr->Info() << "Created " << trgFullSimFinal << endm;}

    {gMessMgr->Info() << "Creating StPreEclMaker" << endm;}
    StPreEclMaker *preEclMaker = new StPreEclMaker();
    {gMessMgr->Info() << "Created " << preEclMaker << endm;}
    if (preEclMaker) {
	preEclMaker->setAlgorithm((EmcClusterAlgorithm)1);//kEmcClDefault
    }
  
    {gMessMgr->Info() << "Creating StEpcMaker" << endm;}
    StEpcMaker *epcMaker = new StEpcMaker();
    {gMessMgr->Info() << "Created " << epcMaker << endm;}

    if (dataAnalysisSettings.useFullJetMaker) {
	{gMessMgr->Info() << "Creating StBET4pMaker" << endm;}
        StBET4pMaker* bet4pMaker = new StBET4pMaker("BET4pMaker",muDstMaker, dataAnalysisSettings.jetFullMakerDoTowerSwapFix);
        {gMessMgr->Info() << "Created " << bet4pMaker << endm;}
	if (bet4pMaker) {
            bet4pMaker->setUse2003Cuts(dataAnalysisSettings.jetFullMakerUse2003Cuts);
            bet4pMaker->setUse2005Cuts(dataAnalysisSettings.jetFullMakerUse2005Cuts);
            bet4pMaker->setUse2006Cuts(dataAnalysisSettings.jetFullMakerUse2006Cuts);
	}

        {gMessMgr->Info() << "Creating StJetMaker" << endm;}
        StJetMaker* emcJetMaker = new StJetMaker(dataAnalysisSettings.jetMakerName, muDstMaker, "/dev/null");
        {gMessMgr->Info() << "Created " << emcJetMaker << endm;}
     
        {gMessMgr->Info() << "Creating StppAnaPars" << endm;}
        StppAnaPars* anapars = new StppAnaPars();
        {gMessMgr->Info() << "Created " << anapars << endm;}
	if (anapars) {
    	    anapars->setFlagMin(0); //track->flag() > 0
    	    anapars->setNhits(20); //track->nHitsFit()>20
    	    anapars->setCutPtMin(0.2); //track->pt() > 0.2
    	    anapars->setAbsEtaMax(1.6); //abs(track->eta())<1.6
    	    anapars->setJetPtMin(5.0); // was 5.0
    	    anapars->setJetEtaMax(100.0);
    	    anapars->setJetEtaMin(0);
    	    anapars->setJetNmin(0);
	}
         
        {gMessMgr->Info() << "Creating StConePars" << endm;}
        StConePars* pars = new StConePars();
        {gMessMgr->Info() << "Created " << pars << endm;}
	if (pars) {
	    pars->setDebug(false);
    	    pars->setConeRadius(dataAnalysisSettings.jetConeRadius);
	}
	if (emcJetMaker) {
    	    emcJetMaker->addAnalyzer(anapars, pars, bet4pMaker, dataAnalysisSettings.jetFullMakerBranchName);
	}
    }

    {gMessMgr->Info() << "Create StPi0DataMaker" << endm;}
    StPi0DataMaker *pi0Maker = new StPi0DataMaker(dataAnalysisSettings.dataMakerName);
    {gMessMgr->Info() << "Created " << pi0Maker << endm;}
    if (pi0Maker) {
	pi0Maker->settings = dataAnalysisSettings;
    }

    {gMessMgr->Info() << "Create StPi0DataSaveMaker" << endm;}
    StPi0DataSaveMaker *pi0SaveMaker = new StPi0DataSaveMaker();
    {gMessMgr->Info() << "Created " << pi0SaveMaker << endm;}
    if (pi0SaveMaker) {
	pi0SaveMaker->outputFileName = (const Char_t *)outputFile;
	pi0SaveMaker->settings = dataAnalysisSettings;
    }

/*
    if (adcToEMaker) {
	controlADCtoE_st *ctrl = adcToEMaker->getControlTable();
	if (ctrl) {
	    ctrl->DeductPedestal[2] = 2;
	    ctrl->DeductPedestal[3] = 2;
	}
    }
*/

    if (emcSim) {
	emcSim->setCalibSpread(kBarrelEmcTowerId, dataAnalysisSettings.CalibSpreadTower);
	emcSim->setCalibOffset(kBarrelEmcTowerId, dataAnalysisSettings.CalibOffsetTower);
	emcSim->setCalibSpread(kBarrelEmcPreShowerId, dataAnalysisSettings.CalibSpreadPreshower);
	emcSim->setCalibOffset(kBarrelEmcPreShowerId, dataAnalysisSettings.CalibOffsetPreshower);
	emcSim->setCalibSpread(kBarrelSmdEtaStripId, dataAnalysisSettings.CalibSpreadSMDE);
	emcSim->setCalibOffset(kBarrelSmdEtaStripId, dataAnalysisSettings.CalibOffsetSMDE);
	emcSim->setCalibSpread(kBarrelSmdPhiStripId, dataAnalysisSettings.CalibSpreadSMDP);
	emcSim->setCalibOffset(kBarrelSmdPhiStripId, dataAnalysisSettings.CalibOffsetSMDP);
/*
        if (!emcSim->getControlSimulator()) {gMessMgr->Error() << "No controlSimulator!" << endm;}
        else {
            controlEmcSimulatorMaker_st *ctrl = emcSim->getControlSimulator()->GetTable();
            if (ctrl) {
	        ctrl->keyDB[0] = dataAnalysisSettings.isEmbedding ? 1 : 2;
	        ctrl->keyDB[1] = 0;//dataAnalysisSettings.isEmbedding ? 1 : 2;
	        ctrl->keyDB[2] = dataAnalysisSettings.isEmbedding ? 1 : 2;
	        ctrl->keyDB[3] = dataAnalysisSettings.isEmbedding ? 1 : 2;
	        ctrl->calibSpread[0] = dataAnalysisSettings.CalibSpreadTower;
	        ctrl->calibOffSet[0] = dataAnalysisSettings.CalibOffsetTower;
	        ctrl->calibSpread[1] = dataAnalysisSettings.CalibSpreadPreshower;
	        ctrl->calibOffSet[1] = dataAnalysisSettings.CalibOffsetPreshower;
	        ctrl->calibSpread[2] = dataAnalysisSettings.CalibSpreadSMDE;
	        ctrl->calibOffSet[2] = dataAnalysisSettings.CalibOffsetSMDE;
	        ctrl->calibSpread[3] = dataAnalysisSettings.CalibSpreadSMDP;
	        ctrl->calibOffSet[3] = dataAnalysisSettings.CalibOffsetSMDP;
	        //ctrl->crosstalk[0] = dataAnalysisSettings.CrosstalkTower;
	        //ctrl->crosstalk[1] = dataAnalysisSettings.CrosstalkPreshower;
	        //ctrl->crosstalk[2] = dataAnalysisSettings.CrosstalkSMDE;
	        //ctrl->crosstalk[3] = dataAnalysisSettings.CrosstalkSMDP;
            }
        }
*/
    }
  
    {gMessMgr->Info() << "Initializing chain..." << endm;}
    Int_t initStat = chain->Init(); 
    if (initStat) chain->Fatal("StChain::Init()", TString(initStat));
    {gMessMgr->Info() << "Done initializing chain." << endm;}

/*
defaults are:
bemc	4,	0.7,	0.001,	0.1,	kFALSE
bprs	1,	0.1,	0.001,	0.1,	kFALSE
bsmde	5,	0.4,	0.001,	0.1,	kFALSE
bsmdp	5,	0.4,	0.001,	0.1,	kFALSE
*/
    if (preEclMaker) {
	StEmcOldFinder* finder = (StEmcOldFinder*)preEclMaker->finder(); // gets pointer to the finder
	if (finder) {
    	    finder->setEnergySeed(1, dataAnalysisSettings.ClusterEnSeedTower);
	    finder->setEnergySeed(2, dataAnalysisSettings.ClusterEnSeedPreshower);
    	    finder->setEnergySeed(3, dataAnalysisSettings.ClusterEnSeedSMDE);
    	    finder->setEnergySeed(4, dataAnalysisSettings.ClusterEnSeedSMDP);
    	    finder->setEnergyAdd(1, dataAnalysisSettings.ClusterEnAddTower);
    	    finder->setEnergyAdd(2, dataAnalysisSettings.ClusterEnAddPreshower);
    	    finder->setEnergyAdd(3, dataAnalysisSettings.ClusterEnAddSMDE);
    	    finder->setEnergyAdd(4, dataAnalysisSettings.ClusterEnAddSMDP);
    	    finder->setEnergyThresholdAll(1, dataAnalysisSettings.ClusterEnTotTower);
    	    finder->setEnergyThresholdAll(2, dataAnalysisSettings.ClusterEnTotPreshower);
    	    finder->setEnergyThresholdAll(3, dataAnalysisSettings.ClusterEnTotSMDE);
    	    finder->setEnergyThresholdAll(4, dataAnalysisSettings.ClusterEnTotSMDP);
    	    finder->setSizeMax(1, dataAnalysisSettings.ClusterSizeMaxTower);
    	    finder->setSizeMax(2, dataAnalysisSettings.ClusterSizeMaxPreshower);
    	    finder->setSizeMax(3, dataAnalysisSettings.ClusterSizeMaxSMDE);
    	    finder->setSizeMax(4, dataAnalysisSettings.ClusterSizeMaxSMDP);
	}
    }
 
    Int_t evt=0;
    Int_t result = 0;
    TMemStat m; m.PrintMem(0);
    while (chain && ((evt < nevents) || (nevents < 0)) && (result == 0/*kStOK*/)) {
        if (((evt % 100) == 0) || dataAnalysisSettings.isSimulation || dataAnalysisSettings.isEmbedding) {gMessMgr->Info() << "Event " << evt << endm;}
        chain->Clear();
        result = chain->Make(evt);
        evt++;
        if ((evt % 1000) == 0) m.PrintMem(0);
    }
    m.PrintMem(0);

    {gMessMgr->Info() << "Finishing chain" << endm;}
    if (chain) chain->Finish();

    m.PrintMem(0);

    {gMessMgr->Info() << "Deleting chain" << endm;}
    if (chain) delete chain;
    chain = 0;

    m.PrintMem(0);

    TDatime stopTime;
    timer.Stop();
    {gMessMgr->Info() << "Finished: " << stopTime.AsSQLString() << endm;}
    timer.Print();
    {gMessMgr->Info() << "Number of files processed: " << numberOfFiles << ", " << (Int_t(Float_t(numberOfFiles) * 10.0 / (((timer.RealTime() != 0) ? timer.RealTime() : 3600.0) / 3600.0)) / 10.0) << " files per hour" << endm;}
    {gMessMgr->Info() << "================== FINISHED ==================" << endm;}
    {gMessMgr->Error() << "================ FINISH_cerr =================" << endm;}
}
