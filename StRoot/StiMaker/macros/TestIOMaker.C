// TestIOMaker.C
// M.L. Miller
//  5/00

class StChain;
StChain *chain=0;

void TestIOMaker(Int_t nevents=3,
		 //This file points to 30 events of 10 neg muons w/ pt=.9
		 const char* MainFile="/star/data22/ITTF/data/simple_geant/DEV_10_8_01/*.event.root")
		 //This file points to 5 muons /event
		 //const char* MainFile="/star/data22/ITTF/EvalData/MCNtuple/muon_100_neg.event.root")
    
		 //This file points to 110 events from mevsim (homebrew had. cocktail)
		 //const char* MainFile="/star/data22/ITTF/data/mevsim/10_9_01/*.event.root")
    
		 //This file points to a nightly low density hadronic cocktail reconstruction.
		 //const char* MainFile="/star/rcf/test/dev/trs_redhat61/Tue/year_2001/hc_lowdensity/*.event.root")
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

    cout <<"Loading StEmcUtil"<<endl;
    gSystem->Load("StEmcUtil");
    
    cout <<"Loading StMcEvent"<<endl;
    gSystem->Load("StMcEvent");

    cout <<"Loading StMcEventMaker"<<endl;
    gSystem->Load("StMcEventMaker");

    cout <<"Loading AssociationMaker"<<endl;
    gSystem->Load("StAssociationMaker");
    
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
    calibMk->SetDateTime("year_2b");
    //calibMk->SetDateTime("year_1h");
    calibMk->SetDebug();

    //Read Tpc Database access
    StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");

    //StEventMaker
    StEventMaker*       eventReader   = new StEventMaker("events","title");
    eventReader->doPrintEventInfo = 0;

    //StMcEventMaker
    StMcEventMaker* mcEventReader = new StMcEventMaker();
    
    //Association
    StAssociationMaker* assocMaker = new StAssociationMaker();
    
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

