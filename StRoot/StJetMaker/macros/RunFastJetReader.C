class  StChain;
StChain *chain;
int total=0;

void RunFastJetReader(
					  int nevents=10000,
					  const char* jetInFile = "blah.jet.root",
					  const char* skimInFile = "blah.jetSkim.root"
					  )
{
	cout <<"hello world"<<endl;
	cout <<"Jet tree file:\t"<<jetInFile<<endl;
	cout <<"SkimEvent tree file:\t"<<skimInFile<<endl;
    
	if (gClassTable->GetID("TTable") < 0) {
		gSystem->Load("libStar");
		gSystem->Load("libPhysics");
	}
	gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
	loadSharedLibraries();
	gSystem->Load("StTpcDb");
	gSystem->Load("StDbUtilities");
	gSystem->Load("StMcEvent");
	gSystem->Load("StMcEventMaker");
	gSystem->Load("StDaqLib");
	gSystem->Load("StEmcRawMaker");
	gSystem->Load("StEmcADCtoEMaker");
	gSystem->Load("StPreEclMaker");
	gSystem->Load("StEpcMaker");
	gSystem->Load("StEmcSimulatorMaker");
	gSystem->Load("StEmcUtil");
	gSystem->Load("StDbLib");
	gSystem->Load("StDbBroker");
	gSystem->Load("StDetectorDbMaker");
	gSystem->Load("St_db_Maker");
	gSystem->Load("StEEmcDbMaker");
	gSystem->Load("StSpinDbMaker");
	gSystem->Load("StEEmcUtil");
	gSystem->Load("StJetFinder");
	gSystem->Load("StJetMaker");
	
    cout << " loading done " << endl;
	
	chain= new StChain("StChain"); 
    chain->SetDebug(1);
	
    //Instantiate the JetReader
    StJetReader* jetReader = new StJetReader("JetReader",0);
    
    chain->Init();
	
	//these 3 lines are critical and must be called after chain->Init()
	//Call in exactly this order
	jetReader->InitFile(jetInFile);
	jetReader->InitJetSkimFile(skimInFile);
	int ready = jetReader->preparedForDualRead();
    
    chain->PrintInfo();
	
	TTree* jetTree = jetReader->tree();
	int ntotal = jetTree->GetEntries();
	
	for (Int_t iev=0; iev<nevents && iev<ntotal; iev++) {
		cout << "-------------------------------  Working on eventNumber " << iev << endl;
		chain->Clear();
		int iret = chain->Make(iev); 
		total++;
		if (iret) {
			cout << "Bad return code!" << endl;
			break;
		}
		//Here's where you can do your analysis, for an example look in this method
		jetReader->exampleFastAna();
    } 
    chain->Finish(); 
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;      
	
}




