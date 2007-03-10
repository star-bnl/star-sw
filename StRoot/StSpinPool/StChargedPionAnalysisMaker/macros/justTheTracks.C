void justTheTracks(const char* dir="/star/institutions/mit/kocolosk/analysis/test",
			   const char* name  = "test",
			   const char* filelist = "myfiles.list",
			   int nFiles = 1,
			   int nEvents = 200000000)
{
	gROOT->Macro("LoadLogger.C");
	gROOT->Macro("loadMuDst.C");
   
    gSystem->Load("StMcEvent");
	gSystem->Load("StEmcTriggerMaker");
	gSystem->Load("StSpinDbMaker");
		
	gSystem->Load("StChargedPionAnalysisMaker");
	
	StChain* chain = new StChain("StChain");
	//chain->SetDebug();
		
 	StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",filelist,"",nFiles);
		
	cout<<"add my maker"<<endl;
	
	TString outfile(dir);
	outfile += "/chargedPions_";
	outfile += name;
	outfile += ".tree.root";
	StChargedPionMaker* pionMaker = new StChargedPionMaker("chargedPionMaker",outfile.Data());

	cout<<"try to init"<<endl;
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
	cout<<"my macro processed "<<i<<" events";
	cout<<"\tcpu: "<<total.CpuTime()<<"\treal: "<<total.RealTime()<<"\tratio: "<<total.CpuTime()/total.RealTime()<<endl;
}

