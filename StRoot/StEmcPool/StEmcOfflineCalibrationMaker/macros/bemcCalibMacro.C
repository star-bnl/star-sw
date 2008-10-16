void bemcCalibMacro(const char* dir="/star/u/mattheww/StRoot/StEmcPool/StEmcOfflineCalibrationMaker/macros/",
			   const char* name  = "test.root",
			   const char* filelist = "myfiles.list",
			   int nFiles = 1,
			   int nEvents = 272)
{
	gROOT->Macro("LoadLogger.C");
	gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    gSystem->Load("StarMagField.so");
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StDbBroker");
    gSystem->Load("libgeometry_Tables");
	
    gSystem->Load("StEmcOfflineCalibrationMaker");
	
    StChain* chain = new StChain("StChain");
	//chain->SetDebug();
	
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",filelist,"",nFiles);
    St_db_Maker *dbMaker = new St_db_Maker("StarDb","MySQL:StarDb");
	
	StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
	/*
	StDetectorId towerid = static_cast<StDetectorId>(kBarrelEmcTowerId);
	StDetectorId prsid = static_cast<StDetectorId>(kBarrelEmcTowerId+1);
	StDetectorId smdeid = static_cast<StDetectorId>(kBarrelEmcTowerId+2);
	StDetectorId smdpid = static_cast<StDetectorId>(kBarrelEmcTowerId+3);
	adc->setDoZeroSuppression(towerid,0);
	adc->setDoZeroSuppression(prsid,0);
	adc->setDoZeroSuppression(smdeid,0);
	adc->setDoZeroSuppression(smdpid,0);
	*/	
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
	/*
	bemcCalibMaker->addMinBiasTrigger(200000);
	bemcCalibMaker->addMinBiasTrigger(200002);
	bemcCalibMaker->addMinBiasTrigger(200012);
	
	bemcCalibMaker->addHighTowerTrigger(117211);
	bemcCalibMaker->addHighTowerTrigger(117212);
	bemcCalibMaker->addHighTowerTrigger(127212);
	bemcCalibMaker->addHighTowerTrigger(127213);
	bemcCalibMaker->addHighTowerTrigger(137213);
	*/
	TMemStat memory;
	memory.PrintMem(NULL);
	
	chain->Init();
	cout<<"chain initialized"<<endl;
	
	TStopwatch total;
	TStopwatch timer;
	
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
