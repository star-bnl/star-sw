
// NOTE - chain needs to be declared global so for StHbtEventReader
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunJetSimuReader(int nevents=10,
		      const char *dir = "",
		      const char* file = "/star/data19/reco/pp200/pythia6_203/default/pt15/y2004x/gheisha_on/trs_ii/pds1214_02_5000evts.MuDst.root",
		      const char *filter = "",
		      const char* jetInFile = "Jets_out.root",
		      const char* simuInFile = "Simu_out.root")
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
    gSystem->Load("StEEmcUtil");// needed by EEMC-Db
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");

    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
   
    chain= new StChain("StChain"); 
    chain->SetDebug(1);

    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,10,"MuDst");


    //Database -- must set flavor correctly for ideal gains
    St_db_Maker *dbMk =new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
    dbMk->SetDateTime(20031120,0);
    dbMk->SetFlavor("sim","bemcPed");
    dbMk->SetFlavor("sim","bemcStatus");
    dbMk->SetFlavor("sim","bemcCalib");
    dbMk->SetFlavor("sim","bemcGain");
    dbMk->SetFlavor("sim","eemcPMTcal");
    dbMk->SetFlavor("sim","eemcPIXcal");

    //EmcDb
    StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");
    
    //StMuDst2StEventMaker
    StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");
    
    //EmcAdc2EMaker
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

    //Instantiate the JetReader
    StJetSimuReader* jetReader = new StJetSimuReader("JetReader",muDstMaker);
    //Instantiate the JetReader    
    chain->Init();

    //Victor, this line works if turned on...
    if (1) {
	jetReader->InitFile(jetInFile,simuInFile);
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
	jetReader->exampleSimuAna();
    } 
    chain->Finish(); 
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;      
}







