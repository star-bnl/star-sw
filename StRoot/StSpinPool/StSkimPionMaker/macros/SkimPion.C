// Runs the Skim Pion maker for calibrations
// A. Hoffman 2007

void SkimPion(const char* fileList, const char* fileId="Skim.root") {

    int flagMC=0;  // 0== off, 1=Alan
    int useEemc=1; // 0== off
    int useBemc=1; // 0== off
    int useL2=1;   // 0== off
    int L2ConfigYear=2006; // possible: 2006, 2008
    int bemcConfig=2; // enum: kOnline=1, kOffline, kExpert
    int playConfig=100; // jan:100_199
    int emcEveDump=0; // extrating raw EMC data in a custom format
    const char *dirIn="";
    int nFiles = 100000; // make this big if you want to read all events from a run
    char *eemcSetupPath="/afs/rhic.bnl.gov/star/users/kocolosk/public/StarTrigSimuSetup/";

    TString outDir="./out2/"; 

    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");

    loadSharedLibraries();
	
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StarMagField");
    gSystem->Load("geometry");
    gSystem->Load("St_g2t");
    gSystem->Load("St_geant_Maker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("StEpcMaker");

    gSystem->Load("StSpinDbMaker");

    // Renees Trigger Maker
    //gSystem->Load("StEmcTriggerMaker");

    // Mikes relative luminosity interface
    gSystem->Load("StEsRelLum");

    //Load your maker here
    gSystem->Load("StSkimPionMaker");

    gROOT->Macro("LoadLogger.C");
    cout << "loading done " << endl;
    
    StChain *chain = new StChain("StChain");
    StMuDstMaker *mudst_mk = new StMuDstMaker(0,0,"",fileList,"MuDst.root",999); // MuDST reader

    //StMuDbReader
    StMuDbReader* db = StMuDbReader::instance();
    
    // Need St_db_Maker for Emc calibration
    St_db_Maker *db1 = new St_db_Maker("StarDb","$STAR/StarDb","MySQL:StarDb");
    
    //Database interface, needed for prescales
    StDetectorDbMaker* detDbMk = new StDetectorDbMaker();

    // Database for Spin data
    StSpinDbMaker *spDb=new StSpinDbMaker("spinDb");

    //Endcap DB
    if(useEemc || useL2) new StEEmcDbMaker("eemcDb");
    
    // Maker to apply calibration
    StEmcADCtoEMaker *adc_to_e=new StEmcADCtoEMaker();
    adc_to_e->setPrint(kFALSE);
    //adc_to_e->saveAllStEvent(true);

    //Collect all output histograms
    TObjArray* HList = new TObjArray;
    
    //Get TriggerMaker
    StTriggerSimuMaker *simuTrig = new StTriggerSimuMaker("StarTrigSimu");
    simuTrig->setHList(HList);
    simuTrig->setMC(flagMC); // must be before individual detectors, to be passed
    simuTrig->useBbc();
    
    if(useEemc) 
    {
        simuTrig->useEemc(0);//default=0:just process ADC, 1,2:comp w/trgData,see .
        simuTrig->eemc->setSetupPath(eemcSetupPath);
    } 
    if(useBemc)
        {
        simuTrig->useBemc();
        simuTrig->bemc->setConfig(bemcConfig);
    }


    if(1) 
    {
        /* 
           reads all input/setup files from  L2setup-yyyymmdd/
           writes all output files to L2out-yyyymmdd 
           depending on the DB time stamp 
           both dierectiorie MUST exist, setup must be reasonable
        */
    
        StGenericL2Emulator* simL2Mk=0;
        if(L2ConfigYear==2006) simL2Mk= new StL2_2006EmulatorMaker;
        else if(L2ConfigYear==2008) simL2Mk= new StL2_2008EmulatorMaker;
        assert(simL2Mk);
        simL2Mk->setSetupPath(eemcSetupPath);
        simL2Mk->setOutPath(outDir.Data());
        if (flagMC) simL2Mk->setMC();
        //simL2Mk->useStEvent(); // default : use muDst
        simuTrig->useL2(simL2Mk);
    }

    
    // Makers for clusterfinding
    StPreEclMaker *pre_ecl=new StPreEclMaker();
    pre_ecl->setPrint(kFALSE);
    StEpcMaker *epc=new StEpcMaker();
    epc->setPrint(kFALSE);

    //StEmcTrigger
    //StEmcTriggerMaker *emcTrig = new StEmcTriggerMaker("StEmcTriggerMaker");
    //emcTrig->setDbMaker(db1);
    
    // now add your analysis maker
    StSkimPionMaker* analysis = new StSkimPionMaker("SkimPionMaker", 1, fileId);
    
    chain->ls(3);
    chain->Init();
    
    //Tight cuts (esp. SMD)
    
    Int_t sizeMax = 4;
    Float_t energySeed = 0.4; // default is 0.7, usually 0.35
    Float_t energyAdd  = 0.05;  // default is 0.07, usually 0.035
    pre_ecl->SetClusterConditions("bemc", sizeMax, energySeed, energyAdd, 0.02, kFALSE);     
    // defaults for SMDs are ("...", 5, 0.4, 0.001, 0.1, kFALSE)
    pre_ecl->SetClusterConditions("bsmde", 5, 0.4,0.005, 0.1,kFALSE); // used 5, 0.2, 0.0005, 0.1
    pre_ecl->SetClusterConditions("bsmdp", 5, 0.4,0.005, 0.1,kFALSE);
    pre_ecl->SetClusterConditions("bprs", 1, 500., 500., 501., kFALSE);

    // TStopwatch totaltime;
    TStopwatch timer;
    TMemStat memory;
    
    Int_t n_event=100000000;
     int total =0;

    cout << "Chain initialized, starting up... " << endl;

    for(Int_t iev=0;iev<n_event; iev++) {
    
        cout << "****************************************** " << endl;
        cout << "\Processing Event " << iev << " of "<< n_event << endl;
        cout << "*************************1***************** " << endl;
        
        chain->Clear();
        
	if(iev % 500 == 0){
            cout<<"done with event "<<iev;
	    cout<<"\tcpu: "<<timer.CpuTime()<<"\treal: "<<timer.RealTime()<<"\tratio: "<<timer.CpuTime()/timer.RealTime()<<endl;
            timer.Start();
            memory.PrintMem(NULL);
        }

        int iret = chain->Make(iev);
        total++;
	
        if(iret % 10 == kStEOF || iret % 10 == kStFatal) 
        {
            cout << "Bad return code!" << endl;
            break;
        }
    
    }
  
    chain->Finish();
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;

}
