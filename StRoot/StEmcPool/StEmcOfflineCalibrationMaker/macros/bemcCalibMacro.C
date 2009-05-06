void bemcCalibMacro(const char* dir="./",
			   const char* name  = "test.root",
			   const char* filelist = "test.list",
			   int nFiles = 1,
			   int nEvents = 100,
		           const char* outPath = "./")
{
	gROOT->Macro("LoadLogger.C");
	gROOT->Macro("loadMuDst.C");
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
    gSystem->Load("StTriggerUtilities");
    //gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StDbBroker");
    gSystem->Load("libgeometry_Tables");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StEmcOfflineCalibrationMaker");
	
    StChain* chain = new StChain("StChain");
	//chain->SetDebug();
	
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",filelist,"",nFiles);
    St_db_Maker *dbMaker = new St_db_Maker("StarDb","MySQL:StarDb");
    StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");
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

	//StEmcTriggerMaker *emcTrig = new StEmcTriggerMaker("bemctrigger");
	//emcTrig->setDbMaker(dbMaker);

    StTriggerSimuMaker* trigsim = new StTriggerSimuMaker();
    trigsim->useBbc();
    trigsim->useBemc();
    trigsim->bemc->setConfig(StBemcTriggerSimu::kOffline);
    StGenericL2Emulator* simL2Mk = new StL2_2008EmulatorMaker;
    assert(simL2Mk);
    simL2Mk->setSetupPath("/afs/rhic.bnl.gov/star/users/kocolosk/public/StarTrigSimuSetup/");
    simL2Mk->setOutPath(outPath);
    trigsim->useL2(simL2Mk);

	
	TString outfile(dir);
	outfile += "/";
	outfile += name;
	StEmcOfflineCalibrationMaker* bemcCalibMaker = new StEmcOfflineCalibrationMaker("bemcCalibMaker",outfile.Data());
	//bemcCalibMaker->subtractPedestals = true;

	/*
	//2006 Triggers	
	bemcCalibMaker->addMinBiasTrigger(200000);
	bemcCalibMaker->addMinBiasTrigger(200002);
	bemcCalibMaker->addMinBiasTrigger(200012);
	
	bemcCalibMaker->addHighTowerTrigger(117211);
	bemcCalibMaker->addHighTowerTrigger(117212);
	bemcCalibMaker->addHighTowerTrigger(127212);
	bemcCalibMaker->addHighTowerTrigger(127213);
	bemcCalibMaker->addHighTowerTrigger(137213);
	*/

	//2008 Triggers
	bemcCalibMaker->addMinBiasTrigger(220000);
	bemcCalibMaker->addHighTowerTrigger(220500);//bht0
	bemcCalibMaker->addHighTowerTrigger(220510);//bht1
	bemcCalibMaker->addHighTowerTrigger(220520);//bht2

	bemcCalibMaker->addFastTrigger(220900);//fms-fast
	bemcCalibMaker->addFastTrigger(220901);//fms-fast
	bemcCalibMaker->addFastTrigger(220910);//fms-fast tpc line
	bemcCalibMaker->addFastTrigger(220920);//fpd e fast
	bemcCalibMaker->addFastTrigger(220710);//tof reading only tpx and tof
	
	StMemStat memory;
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
