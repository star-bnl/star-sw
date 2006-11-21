void pythiaMacro(const char* dir="/star/institutions/mit/kocolosk/pythia/test",
				 const char* name  = "test.root",
				 const char* filelist = "/star/data13/reco/pp200/pythia6_205/55_65gev/cdf_a/y2004y/gheisha_on/p05ih/rcf1271_01_4000evts.MuDst.root",
				 int nFiles = 1,
				 int nEvents = 500)
{
	gROOT->LoadMacro("$STAR/StRoot/macros/LoadLogger.C");
	LoadLogger();
	gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StarMagField.so");
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEmcSimulatorMaker");
	gSystem->Load("StDbBroker");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("St_db_Maker");
	gSystem->Load("libgeometry_Tables");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StEEmcUtil");
	gSystem->Load("StEmcTriggerMaker");
	gSystem->Load("StSpinDbMaker");
		
	gSystem->Load("StChargedPionAnalysisMaker");
	
	StChain* chain = new StChain("StChain");
	//chain->SetDebug();
	
	//StIOMaker - to read geant files
    TString geantfile(filelist);
    geantfile.ReplaceAll("MuDst","geant");	
	StIOMaker* ioMaker = new StIOMaker();
	ioMaker->SetFile(geantfile.Data());
	ioMaker->SetIOMode("r");
	ioMaker->SetBranch("*",0,"0");             //deactivate all branches
	ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
	
	// Instantiate StMcEventMaker - to get pythia pid from McEvent 
	StMcEventMaker *mcEventMaker = new StMcEventMaker();
		
	StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",filelist,"",nFiles);	
	
	St_db_Maker* dbMaker = new St_db_Maker("StarDb","/star/u/kocolosk/StarDb","MySQL:StarDb","$STAR/StarDb");
	dbMaker->SetDateTime(20050506,214129); //pick up average status table in 2005

	StDetectorDbMaker* detDbMaker = new StDetectorDbMaker();
	
	StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this to "redo" conversion from geant->adc

	StTriggerDetectorCollection *trgDetColl=new StTriggerDetectorCollection();
		
	StEmcTriggerMaker *emcTrig = new StEmcTriggerMaker("bemctrigger");
	emcTrig->setDbMaker(dbMaker);
		
	TString outfile(dir);
	outfile += "/";
	outfile += name;
	StChargedPionAnalysisMaker* pionMaker = new StChargedPionAnalysisMaker("pionMaker",outfile.Data());
	pionMaker->isRealData = false;
	
	chain->Init();
	cout << "chain initialized" << endl;
	
	//set keyDBs to 2 for pure MC, but use 1 for embedding (don't pick up peds)
	controlEmcSimulatorMaker_st* simControl = emcSim->getControlSimulator()->GetTable();
	//simControl->calibSpread[0] = 0.10;
	simControl->keyDB[0] = 2; //BTOW  uses DB peds/gains/status
	simControl->keyDB[1] = 0; //BPRS  uses no DB information
	simControl->keyDB[2] = 2; //BSMDe uses DB peds/gains/status
	simControl->keyDB[3] = 2; //BSMDp uses DB peds/gains/status
	
	TStopwatch total;
	TStopwatch timer;
	TMemStat memory;
	
	int i=0;
	while(i<nEvents && chain->Make()==kStOk)
	{
		if(i % 500 == 0){
			cout<<"done with event "<<i;
			cout<<"\tcpu: "<<timer.CpuTime()<<"\treal: "<<timer.RealTime()<<"\tratio: "<<timer.CpuTime()/timer.RealTime();
			timer.Start();
			memory.PrintMem(NULL);
		}
		i++;
		chain->Clear();
	}
	
	chain->ls(3);
	chain->Finish();
	cout<<"my macro processed "<<i<<" events";
	cout<<"\tcpu: "<<total.CpuTime()<<"\treal: "<<total.RealTime()<<"\tratio: "<<total.CpuTime()/total.RealTime()<<endl;
}
