void SpinAnalysisTreeReader(const char *files = "StRoot/StSpinPool/StSpinTree/datasets/run5_pdsf.dataset", 
                            const char *runlist = "StRoot/StSpinPool/StSpinTree/filters/run5_chargedPions.runlist",
                            const long nevents = 20) {
    LoadSpinTreeLibs();
    
    //create a new reader
    StSpinTreeReader *reader = new StSpinTreeReader();
    
    //configure the branches you're interested in (default = true)
    reader->connectJets             = true;
    reader->connectNeutralJets      = false;
    reader->connectChargedPions     = true;
    reader->connectBemcNeutralPions = false;
    reader->connectEemcNeutralPions = false;
    reader->connectBemcElectrons    = false;
    
    //optionally filter events by run and trigger
    //reader->selectRunList(runlist);
    reader->selectRun(6119038);
    reader->selectRun(6119063);
    reader->selectRun(6119064);
    
    //select events that passed hardware OR software trigger for any trigger in list
    reader->selectTrigger(96011);
    reader->selectTrigger(96201);
    reader->selectTrigger(96211);
    reader->selectTrigger(96221);
    reader->selectTrigger(96233);
    
    //add some files to analyze, one at a time or in a text file
    //reader->addFileList(files);
    reader->addFile("./spinAnalyses_6119039.tree.root");
    reader->addFile("./spinAnalyses_6119063.tree.root");    
    reader->addFile("./spinAnalyses_6119064.tree.root");    
    
    long entries = reader->GetEntries();
    if(entries > nevents) entries = nevents;
    for(int i=0; i<entries; i++) {
        reader->GetEntry(i);

        //the basics:  run, event, etc.
        int runId   = reader->event()->runId();
        int eventId = reader->event()->eventId();
        
        printf("----------------Reading Event %d of %d----------------\n",i,entries);
        printf("basics:  Run = %d, Event = %d\n",runId,eventId);

        //triggers -- note prescale/threshold access
        TClonesArray *trigs = reader->event()->triggers();
        for(int j=0; j<trigs->GetEntries(); j++) {
            StJetSkimTrig* aTrig = (StJetSkimTrig*)trigs->At(j);
            int trigId      = aTrig->trigId();
            bool didFire    = aTrig->didFire();
            int shouldFire  = aTrig->shouldFire();
            
            StJetSkimTrigHeader *header = reader->event()->trigHeader(trigId);
            if(!header) { 
                printf("ERROR LOADING TRIGGER HEADER FOR %d\n",trigId); 
                break;
            }
            float prescale  = header->prescale;
            
            printf("trigger = %6d   prescale = %8.1f  didFire = %d shouldFire =% d\n",trigId,prescale,didFire,shouldFire);
        }
        
        //vertices -- bestVert() will return NULL if none were found
        int nVertices = reader->event()->vertices()->GetEntries();
        StJetSkimVert *bestVert = reader->event()->bestVert();
        if(bestVert) {
            printf("nVertices =     %d   position of best = %f\n",nVertices,bestVert->position()[2]);
        }
        else if(nVertices == 0){
            printf("no vertices found in this event\n");
        }
        else {
            printf("ERROR LOADING BEST VERTEX IN THIS EVENT\n");
        }
        
        //for more details on event-level quantities see StJetMaker skim event documentation and examples
        
        //jets
        printf("nJets =         %d\n",reader->nJets());
        for(int j=0; j<reader->nJets(); j++) {
            StJet* aJet = reader->jet(j);
            double R = aJet->btowEtSum/(aJet->tpcEtSum + aJet->btowEtSum + aJet->etowEtSum);
            printf("jet pt=%7.4f  jet eta=% 1.4f  E_neu/E_tot=%1.4f\n",aJet->jetPt,aJet->jetEta,R);
        }
        
        //charged pions
        printf("nChargedPions = %d\n",reader->nChargedPions());
        for(int j=0; j<reader->nChargedPions(); j++) {
            StChargedPionTrack* aPion = reader->chargedPion(j);
            printf("pt=%7.4f   eta=% 1.4f   nSigmaPion=% 1.4f\n",aPion->pt(),aPion->eta(),aPion->nSigmaPion());
        }
        
        //neutral pions
        /*printf("nNeutralPions = %d\n",reader->nBemcNeutralPions());
        for(int j=0; j<reader->nBemcNeutralPions(); j++) {
            TPi0Candidate* aPi0 = reader->bemcNeutralPion(j);
            printf("neutral pion mass=%f  pt=%f  eta=%f\n",aPi0->Mass(),aPi0->Pt(),aPi0->Eta());
        }*/
        
        cout << "-----------------------------------------------------" << endl;
    }
    
    delete reader;
}

/*void LoadSpinTreeLibs() {
    gSystem->Load("StarSpinAnalyses");
}*/

void LoadSpinTreeLibs() {
    gSystem->Load("libPhysics");
    gSystem->Load("libTable");
    gSystem->Load("StarRoot");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StEvent");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");
    gSystem->Load("StSpinDbMaker");
    gSystem->Load("StEmcTriggerMaker");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");
    gSystem->Load("StMcEvent");
    gSystem->Load("StChargedPionAnalysisMaker");
    gSystem->Load("StSpinTree");
}
