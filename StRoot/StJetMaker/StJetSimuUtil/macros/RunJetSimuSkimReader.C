class  StChain;
StChain *chain;
int total=0;

void RunJetSimuSkimReader(int nevents=100,
		      const char* jetInFile = "Jets_pt35_11.root",
		      const char* skimInFile = "Skim_pt35_11.root")
{


    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StSpinDbMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");

    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
   
    chain= new StChain("StChain"); 
    chain->SetDebug(1);
    gMessMgr->SwitchOn("D");
    gMessMgr->SwitchOff("I");

    //Instantiate the JetReader
    StJetSimuReader* jetReader = new StJetSimuReader("JetReader",0);
   
    chain->Init();

    //these 3 lines are critical and must be called after chain->Init()
    //Call in exactly this order
    jetReader->InitFile(jetInFile);
    jetReader->InitJetSkimFile(skimInFile);
    int ready = jetReader->preparedForDualRead();


        
    chain->PrintInfo();
    
    for (Int_t iev=0;iev<nevents; iev++) {
	cout << "****************************************** " << endl;
	cout << "Working on eventNumber " << iev << endl;
	cout << "*************************1***************** " << endl;
	chain->Clear();
	int iret = chain->Make(iev); 
	total++;
	if (iret) {
	    cout << "Bad return code!" << endl;
	    break;
	}
	//Here's where you can do your analysis, for an example look in this method
	jetReader->exampleSimuAna();
    } 
    chain->Finish(); 
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;      
}







