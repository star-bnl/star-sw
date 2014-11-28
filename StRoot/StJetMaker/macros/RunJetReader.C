// Read MuDst, jet and skim trees
// NOTE - chain needs to be declared global so for StHbtEventReader
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunJetReader(int nevents=10,
          const char* file,
          const char* jetInFile,
          const char* dir = "",
          const char *filter = "")
{
    cout <<"MuDst chain file:\t"<<file<<endl;
    cout <<"Jet tree file:\t"<<jetInFile<<endl;
    //abort();
    
    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEmcSimulatorMaker");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StSpinDbMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StMCAsymMaker");
    gSystem->Load("StJets");
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("StJetEvent");
    gSystem->Load("libfastjet.so");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");

    cout << " loading done " << endl;
    
    double pi = TMath::Pi();
    cout << " loading done " << endl;
   
    chain= new StChain("StChain"); 
    chain->SetDebug(1);

    //Instantiate the MuDstReader
    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,10000000,"MuDst");

    //StMuDbReader...
    StMuDbReader* db = StMuDbReader::instance();

    //StMuDst2StEventMaker
    //StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");

    //Database
    St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");

    //EmcAdc2EMaker
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

    //PrecEclMaker
    //StPreEclMaker *pecl = new StPreEclMaker();

    //EpcMaker
    //StEpcMaker *epc = new StEpcMaker();

    //Instantiate the JetReader
    StJetReader* jetReader = new StJetReader;
    
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







