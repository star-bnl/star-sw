
// NOTE - chain needs to be declared global so for StHbtEventReader
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunJetReader(int nevents=10,
		  const char* dir = "",
		  const char* file = "/star/data44/reco/productionPP/ReversedFullField/P04ij/2004/135/st_physics_adc_5135068_raw_2050001.MuDst.root",
		  const char *filter = "",
		  const char* jetInFile = "./jets_out.root")
{
    if (gClassTable->GetID("TTable") < 0) {
	gSystem->Load("libStar");
	gSystem->Load("libPhysics");
    } 
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");  
    gSystem->Load("St_db_Maker");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");

    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
   
    chain= new StChain("StChain"); 
    chain->SetDebug(1);

    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,10,"MuDst");

    //StMuDbReader...
    StMuDbReader* db = StMuDbReader::instance();

    //StMuDst2StEventMaker
    StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");

    //Database
    St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");

    //EmcAdc2EMaker
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

    //PrecEclMaker
    //StPreEclMaker *pecl = new StPreEclMaker();

    //EpcMaker
    //StEpcMaker *epc = new StEpcMaker();

    //Instantiate the JetReader
    StJetReader* jetReader = new StJetReader("JetReader",muDstMaker);
    
    chain->Init();

    //Victor, this line works if turned on...
    if (1) {
	jetReader->InitFile(jetInFile);
    }

    //Victor, these lines don't work if turned on...
    else {
	cout <<"\nGet Chain --------"<<endl;
	TChain* c = muDstMaker->chain();
	
	cout <<"\nAdd Friend from file:\t"<<jetInFile<<endl;
	c->AddFriend("jet",jetInFile);
	
	cout <<"\nInitTree()"<<endl;
	jetReader->InitTree(c);
    }
    
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
	jetReader->exampleEventAna();
    } 
    chain->Finish(); 
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;      
}







