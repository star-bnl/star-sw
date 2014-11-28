void SpinAnalysisTreeReaderMIT(const long nevents = 20) {
    LoadSpinTreeLibs();
    
    //create a new reader
    StSpinTreeReader *reader = new StSpinTreeReader();
    
    //add some files to analyze, one at a time or in a text file
    reader->selectDataset("StSpinTree/datasets/run6_mit.dataset");
    //reader->selectFile("./spinAnalyses_6119039.tree.root");
    
    //configure the branches you're interested in (default = true)
    reader->connectJets             = true;
    reader->connectNeutralJets      = false; //only in Run6
    reader->connectChargedPions     = true;
    reader->connectBemcPions        = true;
    reader->connectBemcElectrons    = true;
    reader->connectEemcPions        = false; //not added yet
    
    //optionally filter events by run and trigger
    //reader->selectRunlist("StSpinTree/filters/run6_jets.runlist");
    reader->selectRun(7132007);
    //reader->removeRun(7143025);
    
    //select events that passed hardware OR software trigger for any trigger in list
    reader->selectTrigger(137221);
    reader->selectTrigger(137222);
    reader->selectTrigger(137611);
    reader->selectTrigger(137622);
    reader->selectTrigger(5);
    
    //we can change the OR to AND by doing
    reader->requireDidFire      = true;
    reader->requireShouldFire   = true;
    
    StJetSkimEvent *ev = reader->event();
    
    long entries = reader->GetEntries();
    if(entries > nevents) entries = nevents;
    for(int i=0; i<entries; i++) {
        reader->GetEntry(i);

        //the basics:  run, event, etc.
        int runId   = ev->runId();
        int eventId = ev->eventId();
        
        printf("----------------Reading Event %d of %d----------------\n",i+1,entries);
        printf("basics:  Run = %d, Event = %d\n",runId,eventId);

        //triggers -- note prescale/threshold access
        TClonesArray *trigs = ev->triggers();
        for(int j=0; j<trigs->GetEntries(); j++) {
            StJetSkimTrig* aTrig = (StJetSkimTrig*)trigs->At(j);
            int trigId      = aTrig->trigId();
            bool didFire    = aTrig->didFire();
            int shouldFire  = aTrig->shouldFire();
            
            StJetSkimTrigHeader *header = ev->trigHeader(trigId);
            if(!header) { 
                printf("ERROR LOADING TRIGGER HEADER FOR %d\n",trigId); 
                break;
            }
            float prescale  = header->prescale;
            
            printf("trigger = %6d   prescale = %8.1f  didFire = %d shouldFire =% d\n",trigId,prescale,didFire,shouldFire);
        }
        
        //vertices -- bestVert() will return NULL if none were found
        int nVertices = ev->vertices()->GetEntries();
        StJetSkimVert *bestVert = ev->bestVert();
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
        
        //jets -- could also access using jets pointer from above
        printf("nJets =         %d\n",reader->nJets());
        for(int j=0; j<reader->nJets(); j++) {
            StJet* aJet = reader->jet(j);
            double R = aJet->btowEtSum/aJet->Et();
            if(bestVert) printf("jet pt=%7.4f  jet eta=% 1.4f  det eta=% 1.4f  E_neu/E_tot=%1.4f\n",aJet->jetPt,aJet->jetEta,aJet->detEta(bestVert->position()[2]),R);
            else printf("jet pt=%7.4f  jet eta=% 1.4f  no vertex  E_neu/E_tot=%1.4f\n",aJet->jetPt,aJet->jetEta,R);
        }
        
        //charged pions
        printf("nChargedPions = %d\n",reader->nChargedPions());
        for(int j=0; j<reader->nChargedPions(); j++) {
            StChargedPionTrack* aPion = reader->chargedPion(j);
            printf("pt=%7.4f   eta=% 1.4f   nSigmaPion=% 1.4f\n",aPion->pt(),aPion->eta(),aPion->nSigmaPion());
        }
        
        //neutral pions
        printf("nNeutralPions = %d\n",reader->nBemcPions());
        for(int j=0; j<reader->nBemcPions(); j++) {
            TPi0Candidate* aPi0 = reader->bemcPion(j);
            printf("neutral pion mass=%f  pt=%f  eta=%f\n",aPi0->Mass(),aPi0->Pt(),aPi0->Eta());
        }
        
        cout << "-----------------------------------------------------" << endl;
    }
    
    delete reader;
}

void LoadSpinTreeLibs() {
    gSystem->Load("StarSpinAnalyses");
}
