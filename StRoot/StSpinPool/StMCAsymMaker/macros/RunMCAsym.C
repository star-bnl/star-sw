class  StChain *chain;
int total=0;

void RunMCAsym(	      
    const char *dir ="",
    const char* file="/star/data13/reco/pp200/pythia6_205/55_65gev/cdf_a/y2004y/gheisha_on/p05ih/rcf1271_10_4000evts.MuDst.root",
    const char *fname="/star/data13/reco/pp200/pythia6_205/55_65gev/cdf_a/y2004y/gheisha_on/p05ih/rcf1271_10_4000evts.geant.root",
    const char *filter = "",
    )
{
    int nevents =100;

    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StJetSkimEvent");
    gSystem->Load("StMCAsymMaker");

    chain= new StChain("StChain"); 
    chain->SetDebug(1);
    gMessMgr->SwitchOff("D");
    gMessMgr->SwitchOff("I");

    StIOMaker* ioMaker = new StIOMaker();
    ioMaker->SetFile(fname);
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");             //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch

    StMuDebug::setLevel(1); 
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,1e6,"MuDst"); 
    StMcEventMaker *mcEventMaker = new StMcEventMaker();
    StMCAsymMaker *weight = new StMCAsymMaker("MCAsym");

    TChain* fileChain = muDstMaker->chain();
    int ntotal = fileChain->GetEntries();

    for (Int_t iev=0;iev<nevents; iev++) {
        cout << "****************************************** " << endl;
        cout << "Working on eventNumber:\t" << iev <<"\tof:\t"<<ntotal<<endl;
        cout << "*************************1***************** " << endl;
        chain->Clear();
        int iret = chain->Make(iev);
        
        StPythiaEvent *ev = weight->pythiaEvent();
        cout << "runId = " << ev->runId() << ", eventId = " << ev->eventId() << ", pid = " << ev->processId() << endl;
        cout << "vertexPos = (" << ev->vertex().X() << ", " << ev->vertex().Y() << ", " << ev->vertex().Z() << ")" << endl;
        cout << "GRSV STD A_LL = " << ev->ALL(StPythiaEvent::STD) << endl;
        cout << "incoming parton1 ID = " << ev->particle(0)->GetPdgCode() << endl;
        cout << "incoming parton2 status = " << ev->particle(1)->GetStatusCode() << endl;
        cout << "outgoing parton1 daughter1 = " << ev->particle(2)->GetDaughter(0) << endl;
        
        total++;
        if (iret) {
            cout << "Bad return code!" << endl;
            break;
        }
    }

    chain->Finish();
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;
}



