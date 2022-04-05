void runGammaTreeMaker
(
    char *inputFile = "root://rcas6132.rcf.bnl.gov:1095//home/starreco/reco/ppProductionLong/FullField/P06ie/2006/131/7131043/st_physics_7131043_raw_1020001.MuDst.root",
    char *outputFile = "test.root"
)
{
    
    // Display file paths
    cout << "****************************************" << endl;
    cout << "Reading MuDst File     : " << inputFile << endl;
    cout << "Writing Output File    : " << outputFile << endl;
    
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
    
    gSystem->Load("libgeometry_Tables"); // BEMC Only
    gSystem->Load("StDaqLib");           // BEMC Only
    gSystem->Load("StEmcRawMaker");      // BEMC Only
    gSystem->Load("StEmcADCtoEMaker");   // BEMC Only
    
    gSystem->Load("StEEmcDbMaker");      // EEMC Only
    gSystem->Load("StEEmcUtil");         // EEMC Only
    gSystem->Load("StEEmcA2EMaker");     // EEMC Only
    gSystem->Load("StEEmcClusterMaker"); // EEMC Only

    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StGammaMaker");
    
    /////////////////////////////////////////////
    //             Start up the chain          //
    /////////////////////////////////////////////
    
    StChain *chain = new StChain("chain");
    
    StMuDstMaker *muDstMaker = new StMuDstMaker(0, 0, "", inputFile, "", 1e6, "MuDst");
    muDstMaker->SetStatus("*", 0);
    muDstMaker->SetStatus("MuEvent", 1);
    muDstMaker->SetStatus("Event", 1);
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

    /*
    /////////////////////////////////////////////
    //        Filter Undesired Triggers        //
    /////////////////////////////////////////////

    // Choose your favorite triggers to keep here

    StTriggerFilterMaker* triggerFilter = new StTriggerFilterMaker();
    triggerFilter->addTrigger(117001); // minbias
    triggerFilter->addTrigger(137611); // bemcl2gamma
    triggerFilter->addTrigger(137641); // eemcl2gamma
    */
    
    /////////////////////////////////////////////
    //                 Barrel                  //
    /////////////////////////////////////////////
    
    // Only necessary if using the BEMC
    
    StEmcADCtoEMaker *bemcAdc2E = new StEmcADCtoEMaker(); // this will just convert what's in MuDst to ADC, use for data only!
    bemcAdc2E->setPrint(false);

    /////////////////////////////////////////////
    //                 Endcap                  //
    /////////////////////////////////////////////
    
    /// Only necessary if using the EEMC
    
    // Initialize EEMC database
    StEEmcDbMaker *EEmcDatabase = new StEEmcDbMaker("eemcDb");

    // ADC to energy
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

    StGammaMaker *gammaMaker = new StGammaMaker("gammaMaker", StGammaMaker::kBoth, StGammaMaker::kData);
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

    chain->Init();
    chain->ls(3);

    int ntotal = 50;
    //int ntotal = 1e6;

    Int_t stat  = 0;
    Int_t total = 0;
    for(int iev = 0; iev < ntotal; ++iev)
    {

        cout << "********** Processing Event: " << iev << " **********" << endl;

        chain->Clear();
        stat = chain->Make();
        
        if(stat && stat != kStSKIP) 
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

}

