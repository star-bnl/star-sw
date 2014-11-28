void runSimuGammaTreeMaker
(
    char *inputFile =  "/star/institutions/uky/betan/mit0026_10.MuDst.root",
    char *outputFile = "test.root"
)
{

    // Set up simulation file paths
    TString muDstFile(inputFile);
    TString geantFile = muDstFile;
    geantFile.ReplaceAll("MuDst","geant");
    
    cout << "****************************************" << endl;
    cout << "Reading MuDst File   : " << muDstFile << endl;
    cout << "Reading Geant File   : " << geantFile << endl;
    cout << "Writing Output File  : " << outputFile << endl;
    
    // Load libraries
    cout << "Loading common libraries..." << endl;
    gROOT->Macro("loadMuDst.C");
    gROOT->Macro("LoadLogger.C");
    
    gSystem->Load("libMinuit");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StSpinDbMaker");
    
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    
    gSystem->Load("libgeometry_Tables");  // BEMC Only
    gSystem->Load("StDaqLib");            // BEMC Only
    gSystem->Load("StPreEclMaker");       // BEMC Only
    gSystem->Load("StEmcSimulatorMaker"); // BEMC Only
    
    gSystem->Load("StEEmcDbMaker");        // EEMC Only
    gSystem->Load("StEEmcUtil");           // EEMC Only
    gSystem->Load("StEEmcSimulatorMaker"); // EEMC Only
    gSystem->Load("StEEmcA2EMaker");       // EEMC Only
    gSystem->Load("StEEmcClusterMaker");   // EEMC Only

    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StGammaMaker");
    
    /////////////////////////////////////////////
    //             Start up the chain          //
    /////////////////////////////////////////////
    
    StChain *chain = new StChain("chain");
    
    // Instantiate IO Maker
    StIOMaker* ioMaker = new StIOMaker();
    ioMaker->SetFile(geantFile.Data());
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*", 0, "0");      
    ioMaker->SetBranch("geantBranch", 0, "r");

    StMcEventMaker *mcEventMaker = new StMcEventMaker();
    mcEventMaker->doPrintEventInfo = false;
    mcEventMaker->doPrintMemoryInfo = false;
    
    StMuDstMaker *muDstMaker = new StMuDstMaker(0, 0, "", muDstFile.Data(), "", 1e6, "MuDst");
    muDstMaker->SetStatus("*", 0);
    muDstMaker->SetStatus("MuEvent", 1);
    muDstMaker->SetStatus("Event", 1);
    muDstMaker->SetStatus("McEvent", 1);
    muDstMaker->SetStatus("PrimaryVertices", 1);
    muDstMaker->SetStatus("PrimaryTracks", 1);
    muDstMaker->SetStatus("GlobalTracks", 1);
    muDstMaker->SetStatus("EmcTow", 1);   // BEMC Only
    muDstMaker->SetStatus("EmcPrs", 1);   // BEMC only
    muDstMaker->SetStatus("EmcSmde", 1);  // BEMC Only
    muDstMaker->SetStatus("EmcSmdp", 1);  // BEMC Only
    muDstMaker->SetStatus("EEmcPrs", 1);  // EEMC Only
    muDstMaker->SetStatus("EEmcSmdu", 1); // EEMC Only
    muDstMaker->SetStatus("EEmcSmdv", 1); // EEMC Only

    St_db_Maker *StarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb", "$STAR/StarDb");
    StarDatabase->SetDateTime(20060522, 112810); // 2006 pp
    
    /////////////////////////////////////////////
    //                 Barrel                  //
    /////////////////////////////////////////////

    StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker();
    emcSim->setCalibSpread(kBarrelEmcTowerId, 0.15);
    emcSim->setCalibOffset(kBarrelEmcTowerId, 0.0);
    
    emcSim->setCalibSpread(kBarrelSmdEtaStripId, 0.25);
    emcSim->setCalibOffset(kBarrelSmdEtaStripId, 0.0);
    emcSim->setMaximumAdc(kBarrelSmdEtaStripId, 700);
    emcSim->setMaximumAdcSpread(kBarrelSmdEtaStripId, 70);
    emcSim->setMaxCrossTalkPercentage(kBarrelSmdEtaStripId, 0.5);
    
    emcSim->setCalibSpread(kBarrelSmdPhiStripId, 0.25);
    emcSim->setCalibOffset(kBarrelSmdPhiStripId, 0.0);
    emcSim->setMaximumAdc(kBarrelSmdPhiStripId, 700);
    emcSim->setMaximumAdcSpread(kBarrelSmdPhiStripId, 70);
    
    StPreEclMaker* preEcl = new StPreEclMaker();

    /////////////////////////////////////////////
    //                 Endcap                  //
    /////////////////////////////////////////////
    
    // Initialize EEMC database
    StEEmcDbMaker *EEmcDatabase = new StEEmcDbMaker("eemcDb");
    
    StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");

    StEEmcA2EMaker *EEanalysis = new StEEmcA2EMaker("mEEanalysis");
    EEanalysis->database("eemcDb");    // sets db connection
    EEanalysis->source("MuDst", 1);     // sets mudst as input
    EEanalysis->threshold(3.0, 0);      // tower threshold (ped+N sigma)
    EEanalysis->threshold(3.0, 1);      // pre1 threshold 
    EEanalysis->threshold(3.0, 2);      // pre2 threshold
    EEanalysis->threshold(3.0, 3);      // post threshold
    EEanalysis->threshold(3.0, 4);      // smdu threshold
    EEanalysis->threshold(3.0, 5);      // smdv threshold
    

    /////////////////////////////////////////////
    //               GammaMaker                //
    /////////////////////////////////////////////

    // Possible values for the second argument,
    //     kBemc (Use only the BEMC)
    //     kEemc (Use only the EEMC)
    //     kBoth (Use both detectors)

    StGammaMaker *gammaMaker = new StGammaMaker("gammaMaker", StGammaMaker::kBoth, StGammaMaker::kSimu);
    gammaMaker->setOutputFile(outputFile);
    gammaMaker->storeEmptyEvents();
    
    gammaMaker->setClusterEtThreshold(5.1);
    gammaMaker->setConeRadius(0.4);
    gammaMaker->setBsmdRange(0.05237);
    gammaMaker->setEsmdRange(20.0);
    
    gammaMaker->setSeedEnergyThreshold(3.6);
    gammaMaker->setClusterEnergyThreshold(5.1);
    
    gammaMaker->setTrackEtRawThreshold(0.0);
    gammaMaker->setTowerEtRawThreshold(0.0);
    
    gammaMaker->setEemcTowerClusterThreshold(0.1);

    /////////////////////////////////////////////
    //            Start up the chain           //
    /////////////////////////////////////////////    
    
    cout << "Starting up the chain..." << endl;

    chain->ls(3);
    chain->Init();

    int ntotal = 1e6;

    cout << "Looping over events..." << endl;

    Int_t stat  = 0;
    Int_t total = 0;
    for(int iev = 0; iev < ntotal; ++iev)
    {

        cout << "******************************************" << endl;
        cout << "Processing Event: " << iev << endl;
        cout << "*************************1****************" << endl;

        chain->Clear();
        stat = chain->Make();
        
        if(stat) 
        {
            cout << "Bad return code!" << endl;
            break;
        }
        
        ++total;

    }
    
    chain->Finish(); 
    
    cout << "****************************************** " << endl;
    cout << total << " Total Events" << endl;
    cout << "****************************************** " << endl;

    return;
    
}
