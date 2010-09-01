void bemcCalibMacro(const char* dir="/star/data07/EMC/kocolosk/offline_tower_calibration/test",
			   const char* name  = "test.root",
			   const char* filelist = "myfiles.list",
			   int nFiles = 2,
			   int nEvents = 200)
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
	gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StDbBroker");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("St_db_Maker");
	gSystem->Load("libgeometry_Tables");
	
	gSystem->Load("StEmcOfflineCalibrationMaker");
	
	StChain* chain = new StChain("StChain");
	//chain->SetDebug();
	
	StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",filelist,"",nFiles);
	St_db_Maker *dbMaker = new St_db_Maker("StarDb","MySQL:StarDb");
	
	StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
	
	//get control table so we can turn off BPRS zero-suppression and save hits from "bad" caps
	controlADCtoE_st* control_table = adc->getControlTable();
	control_table->CutOff[1] = -1;
	control_table->CutOffType[1] = 0;
	control_table->DeductPedestal[1] = 2;
	
	StEmcTriggerMaker *emcTrig = new StEmcTriggerMaker("bemctrigger");
	emcTrig->setDbMaker(dbMaker);
	
	TString outfile(dir);
	outfile += "/";
	outfile += name;
	StEmcOfflineCalibrationMaker* bemcCalibMaker = new StEmcOfflineCalibrationMaker("bemcCalibMaker",outfile.Data());
	bemcCalibMaker->subtractPedestals = true;
	
	bemcCalibMaker->addMinBiasTrigger(117001);
	bemcCalibMaker->addMinBiasTrigger(147001);
	
	bemcCalibMaker->addHighTowerTrigger(117211);
	bemcCalibMaker->addHighTowerTrigger(117212);
	bemcCalibMaker->addHighTowerTrigger(127212);
	bemcCalibMaker->addHighTowerTrigger(127213);
	bemcCalibMaker->addHighTowerTrigger(137213);
	
	
	chain->Init();
	cout<<"chain initialized"<<endl;
	
	TStopwatch total;
	TStopwatch timer;
	TMemStat memory;
	
	int i=0;
	while(i<nEvents && chain->Make()==kStOk)
	{
		if(i % 500 == 0){
			cout<<"done with event "<<i;
			cout<<"\tcpu: "<<timer.CpuTime()<<"\treal: "<<timer.RealTime()<<"\tratio: "<<timer.CpuTime()/timer.RealTime();//<<endl;
				timer.Start();
				memory.PrintMem(NULL);
		}
		i++;
		chain->Clear();
	}
	
	chain->ls(3);
	chain->Finish();
	printf("my macro processed %i events",i);
	cout<<"\tcpu: "<<total.CpuTime()<<"\treal: "<<total.RealTime()<<"\tratio: "<<total.CpuTime()/total.RealTime()<<endl;
}
