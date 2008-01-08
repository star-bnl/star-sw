void analysisMacro(const char* dir="/home/kocolosk/dv34/analysis/run6/test",
               const char* name  = "test",
               const char* filelist = "myfiles.list",
               int nFiles = 2,
               int nEvents = 200000000)
{
    gROOT->Macro("LoadLogger.C");
    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StarMagField.so");
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEmcSimulatorMaker");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("libgeometry_Tables");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StSpinDbMaker");
    gSystem->Load("StMCAsymMaker");
    
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");
    gSystem->Load("StChargedPionAnalysisMaker");
    
    StChain* chain = new StChain("StChain");
    //chain->SetDebug();
        
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",filelist,"",nFiles); 
    St_db_Maker *dbMaker = new St_db_Maker("StarDb","MySQL:StarDb");
    StSpinDbMaker* spDbMaker = new StSpinDbMaker("spinDb");
    StDetectorDbMaker* detDbMaker = new StDetectorDbMaker();
    
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
    
    StTriggerDetectorCollection *trgDetColl=new StTriggerDetectorCollection();

    StEmcTriggerMaker *emcTrig = new StEmcTriggerMaker("bemctrigger");
    emcTrig->setDbMaker(dbMaker);
    
    cout<<"start adding locally compiled makers"<<endl;
    
    TString jetSkimFile(dir);
    jetSkimFile += "/jetSkim_";
    jetSkimFile += name;
    jetSkimFile += ".tree.root";
    StJetSkimEventMaker *jetSkim = new StJetSkimEventMaker("jetSkimMaker",muDstMaker,jetSkimFile.Data());
        
    cout<<"add my maker"<<endl;

    TString chargedPionFile(dir);
    chargedPionFile += "/chargedPions_";
    chargedPionFile += name;
    chargedPionFile += ".tree.root";    
    StChargedPionMaker* pionMaker = new StChargedPionMaker("chargedPionMaker",chargedPionFile.Data());
    
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

