// RunStiMaker.C
// M.L. Miller
//  5/00

class StChain;
StChain *chain=0;

void RunStiMaker(Int_t nevents=1,
		 bool run=true,
		 //const char* MainFile="/star/data13/reco/dev/2001/08/*2235009*.event.root")
		 //const char* MainFile="/afs/rhic/star/users/mmiller/code/ITF/geant/pion_10_neg.event.root")
		 const char *MainFile="/direct/star+data02/scratch/haibin/geantTest/muon_10.dst.root")
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

    //We have to activate this to run in dev
    //cout <<"Loading StarRoot"<<endl;
    //gSystem->Load("StarRoot");
    
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
    //gSystem->Load(".i386_redhat61/LIB/Sti.so");

    cout <<"Loading StiGui"<<endl;
    gSystem->Load("StiGui");
    //gSystem->Load(".i386_redhat61/LIB/StiGui");

    cout <<"Loading StiMaker"<<endl;
    gSystem->Load("StiMaker");
    //gSystem->Load(".i386_redhat61/LIB/StiMaker");
    
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
    const char* paramsDB = "$STAR/StarDb";
    St_db_Maker* calibMk = new St_db_Maker("StarDb",calibDB,paramsDB);
    calibMk->SetDateTime("year_1h");
    calibMk->SetDebug();
    
    StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
    
    StEventMaker*       eventReader   = new StEventMaker("events","title");
    eventReader->doPrintEventInfo = 0;
    
    //StiMaker
    StiMaker* anaMk = StiMaker::instance();
    
    anaMk->setMaterialBuildPath("/scr20/ittf/StiGeometryParameters/Materials");
    anaMk->setDetectorBuildPath("/scr20/ittf/StiGeometryParameters/Detectors");
    enum StTrackType {global, primary, tpt, secondary};
    //anaMk->setDrawableStTrackType(primary);
    anaMk->setDrawableStTrackType(global);

    // now execute the chain member functions    
    chain->PrintInfo();
    
    //Make Control-Bar
    StiControlBar* sti = new StiControlBar();
    sti->setStChain(chain);
    
    cout <<"Calling Init() Methods "<<endl;
    chain->Init();
    
    cout <<"Starting Event Loop"<<endl;

    if (run) {
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
    }
    
    return;
}

