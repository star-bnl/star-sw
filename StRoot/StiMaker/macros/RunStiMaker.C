// RunStiMaker.C
// M.L. Miller
//  5/00

class StChain;
StChain *chain=0;

void RunStiMaker(Int_t nevents=1,
		 const char *MainFile="/star/data03/reco/central/P01hbOS/2000/09/*.dst.root")
    //const char *MainFile="/star/rcf/scratch/haibin/geantTest/muon_10.dst.root")
{    
    // Dynamically link needed shared libs
    
    cout <<"Loading St_base"<<endl;
    gSystem->Load("St_base");
    
    cout <<"Loading StChain"<<endl;
    gSystem->Load("StChain");
    
    cout <<"Loading St_Tables"<<endl;
    gSystem->Load("St_Tables");
    
    cout <<"Loading StUtilities"<<endl;
    gSystem->Load("StUtilities");
    
    cout <<"Loading StIOMaker"<<endl;
    gSystem->Load("StIOMaker");
    
    cout <<"Loading StarClassLibrary"<<endl;
    gSystem->Load("StarClassLibrary");
    
    cout <<"Loading DataBase"<<endl;
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTpcDb");
    
    cout <<"Loading StEvent"<<endl;
    gSystem->Load("StEvent");
    
    cout <<"Loading StEventMaker"<<endl;
    gSystem->Load("StEventMaker");
    
    cout <<"Loading Sti"<<endl;
    gSystem->Load("Sti");
    
    cout <<"Loading StiMaker"<<endl;
    gSystem->Load("StiMaker");
    
    // create a new instance of the chain
    
    chain = new StChain("StChain"); 
    chain->SetDebug();
    
    // add makers to the chain
    
    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    ioMaker->SetDebug();
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");            //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");  //activate geant Branch
    ioMaker->SetBranch("dstBranch",0,"r");    //activate Event Branch
    ioMaker->SetBranch("runcoBranch",0,"r");  //activate runco Branch
    
    const char* calibDB = "MySQL:StarDb";
    const char* paramsDB = "$STAR/StatDb";
    St_db_Maker* calibMk = new St_db_Maker("StarDb",calibDB,paramsDB);
    calibMk->SetDateTime("year_1h");
    calibMk->SetDebug();
    
    StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
    
    StEventMaker*       eventReader   = new StEventMaker("events","title");
    eventReader->doPrintEventInfo = 0;
    
    //StiMaker
    StiMaker* anaMk = StiMaker::instance();
    //anaMk->setMaterialBuildPath("/scr20/TempIttf/StiGeometryParameters/Materials");
    //anaMk->setDetectorBuildPath("/scr20/TempIttf/StiGeometryParameters/Detectors");
    anaMk->setMaterialBuildPath("/scr20/TempIttf/StiGeometryParameters/Materials");
    anaMk->setDetectorBuildPath("/scr20/TempIttf/StiGeometryParameters/Detectors");
    anaMk->setPolygonBuildPath("/scr20/TempIttf/StiGeometryParameters/Polygons");
    
    // now execute the chain member functions    
    chain->PrintInfo();
    
    //Make Control-Bar
    StiControlBar* sti = new StiControlBar();
    sti->setStChain(chain);
    
    cout <<"Calling Init() Methods "<<endl;
    chain->Init();
    
    cout <<"Starting Event Loop"<<endl;
    
    /*
      int istat=0,iev=1;
      EventLoop: if (iev<=nevents && !istat) {
      chain->Clear();
      cout << "---------------------- Processing Event : " << iev << endl;
      istat = chain->Make(iev);
      if (istat) {
      cout << "Last Event Processed. Status = " << istat << endl;
      }
      iev++; goto EventLoop;
      }
    */
    
    //chain->Finish();

    return;
}

