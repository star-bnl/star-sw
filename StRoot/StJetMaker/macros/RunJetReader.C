
// NOTE - chain needs to be declared global so for StHbtEventReader
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunJetReader(
		  const char *dir = "/star/data29/reco/pp200/pythia6_203/default/pt15/year2003/gheisha_on/trs_if/",
		  const char *file = "rcf1205_2012_1000evts.MuDst.root",
		  const char *fname="/star/data29/reco/pp200/pythia6_203/default/pt15/year2003/gheisha_on/trs_if/rcf1205_2012_1000evts.event.root",
		  const char *filter = "",
		  const char* jetInFile = "Jets_out_emc.root")
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
    
    int nevents = 20;
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







