// RunStiMaker.C
// M.L. Miller
//  5/00

class StChain;
StChain *chain=0;

void RunStiMaker(Int_t nevents=1,
		 bool simulated=true,
		 //const char* MainFile="/star/data13/reco/dev/2001/09/*2251008*.event.root")
		 //const char *MainFile="/scr20/ittf/data/DEV_9_12_01/*.event.root")
		 const char *MainFile="/direct/star+data02/scratch/haibin/geantTest/muon_10.dst.root")
		 //const char *MainFile="/scr20/ittf/data/DEV_9_12_01/*.event.root")
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

    cout <<"Loading StEmcUtil"<<endl;
    gSystem->Load("StEmcUtil");
    
    cout <<"Loading StMcEvent"<<endl;
    gSystem->Load("StMcEvent");

    cout <<"Loading StMcEventMaker"<<endl;
    gSystem->Load("StMcEventMaker");

    cout <<"Loading AssociationMaker"<<endl;
    gSystem->Load("StAssociationMaker");
    
    cout <<"Loading Sti"<<endl;
    gSystem->Load("Sti");
    //gSystem->Load(".i386_redhat61/LIB/Sti.so"); //For optimized

    cout <<"Loading StiGui"<<endl;
    gSystem->Load("StiGui");
    //gSystem->Load(".i386_redhat61/LIB/StiGui"); //For optimized

    cout <<"Loading StiEvaluator"<<endl;
    gSystem->Load("StiEvaluator");
    
    cout <<"Loading StiMaker"<<endl;
    gSystem->Load("StiMaker");
    //gSystem->Load(".i386_redhat61/LIB/StiMaker"); //For optimized
    
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

    //Calibration Maker (StarDB,not a real Database!)
    const char* calibDB = "MySQL:StarDb";
    const char* paramsDB = "$STAR/StarDb";
    St_db_Maker* calibMk = new St_db_Maker("StarDb",calibDB,paramsDB);
    calibMk->SetDateTime("year_1h");
    calibMk->SetDebug();

    //Read Tpc Database access
    StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");

    //StEventMaker
    StEventMaker*       eventReader   = new StEventMaker("events","title");
    eventReader->doPrintEventInfo = 0;

    //StMcEventMaker
    StMcEventMaker* mcEventReader = 0;
    //Association
    StAssociationMaker* assocMaker = 0;

    if (simulated) {
	mcEventReader = new StMcEventMaker();
	assocMaker = new StAssociationMaker();
    }
    
    //StiMaker
    StiMaker* anaMk = StiMaker::instance();

    //enum SeedFinderType {kUndefined=0, kComposite=1, kEvaluable=2};
    anaMk->setSeedFinderType(StiMaker::kEvaluable);
    //anaMk->setSeedFinderType(StiMaker::kComposite);

    anaMk->setSimulation(simulated);
    if (simulated) {
	anaMk->setMcEventMaker(mcEventReader);
	anaMk->setAssociationMaker(assocMaker);
    }

    // now execute the chain member functions    
    chain->PrintInfo();
    
    //Make Control-Bar
    StiControlBar* sti = new StiControlBar();
    sti->setStChain(chain);
    
    cout <<"Calling Init() Methods "<<endl;
    chain->Init();
    
    cout <<"Starting Event Loop"<<endl;

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
    
    return;
}

