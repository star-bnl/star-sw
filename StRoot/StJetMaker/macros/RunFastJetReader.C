//
// Read jet and skim trees (no MuDst)
//

class  StChain;
StChain *chain;
int total=0;

void RunFastJetReader(
                      int nevents=10000,
                      const char* jetInFile = "blah.jets.root",
                      const char* skimInFile = "blah.skim.root"
                      )
{
    cout <<"hello world"<<endl;
    cout <<"Jet tree file:\t"<<jetInFile<<endl;
    cout <<"SkimEvent tree file:\t"<<skimInFile<<endl;
    
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StJets");

    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StMCAsymMaker");
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
    gSystem->Load("StTriggerUtilities");
    gSystem->Load("libfastjet.so");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetEvent");
    gSystem->Load("StRandomSelector");
    gSystem->Load("StJetMaker");
    
    cout << " loading done " << endl;
    
    chain= new StChain("StChain"); 
    //    chain->SetDebug(1);
    chain->SetDebug(5);
    
    //Instantiate the JetReader
    StJetReader* jetReader = new StJetReader;
    
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




