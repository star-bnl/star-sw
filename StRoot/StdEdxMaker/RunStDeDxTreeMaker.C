// RunStDeDxTreeMaker.C
// M.L. Miller
//  5/00

class StChain;
StChain *chain=0;

void RunStDeDxTreeMaker(Int_t nevents=5000,
	    //ToRunData
	    const char *MainFile="/star/rcf/data09/reco/P00he/2000/07/*.dst.root")
{

    //This is the output .root file
    const char* rootFile = "/star/rcf/pwg/spectra/mmiller/DeDxTree_10000evt_P00he_2000_07.root";
    
    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");

    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");

    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTpcDb");

    gSystem->Load("StEvent");
    gSystem->Load("StEventMaker");

    gSystem->Load("StDeDxTreeMaker"); 
    
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
    St_db_Maker* calibMk = new St_db_Maker("StarDb",calibDB);
    calibMk->SetDateTime("year_1h");
    calibMk->SetDebug();
    
    StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
  
    StEventMaker*       eventReader   = new StEventMaker("events","title");
    eventReader->doPrintEventInfo = 0;
    
    //StDeDxTreeMaker (MLM)
    StDeDxTreeMaker* anaMk = new StDeDxTreeMaker;
    anaMk->setRootFile(rootFile);
    
    // now execute the chain member functions    
    chain->PrintInfo();
    cout <<"Calling Init() Methods "<<endl;
    chain->Init(); // This should call the Init() method in ALL makers
    cout <<"Starting Event Loop"<<endl;
    int istat=0,iev=1;
 EventLoop: if (iev<=nevents && !istat) {
     chain->Clear();
     cout << "---------------------- Processing Event : " << iev << endl;
     istat = chain->Make(iev); // This should call the Make() method in ALL makers
     if (istat) {
	 cout << "Last Event Processed. Status = " << istat << endl;
     }
     iev++; goto EventLoop;
 } // Event Loop
    
    chain->Finish(); // This should call the Finish() method in ALL makers,
    
}

