void RunAssocReader(int nevents=2,
		    const char* infile = "assoc.root"
		    )
{
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");  
    gSystem->Load("St_db_Maker");
    gSystem->Load("StEEmcUtil");// needed by EEMC-Db
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");

    IoManager* myio = new IoManager();
    myio->openRead(infile);

    int ntotal = myio->recoTree()->GetEntries();
    cout <<"ntotal:\t"<<ntotal<<endl;
    
    TClonesArray* pairArray = myio->assocArray()->mArray;
    TClonesArray* recoArray = myio->recoArray()->mArray;
    TClonesArray* pythiaArray = myio->pythiaArray()->mArray;

    
    for (int i=0; i<nevents && i<ntotal; ++i) {

	cout <<"try to get an event"<<endl;
	myio->getEvent(i);
	
	int nreco = recoArray->GetLast()+1;
	cout <<"reco jets:\t"<<nreco<<endl;

	/*
	for (int j=0; nreco; ++j) {
	    TObject* temp = (*recoArray)[j];
	    StJet* rj = dynamic_cast<StJet*>(temp );
	    cout <<rj<<endl;
	    //cout <<rj->Pt()<<"\t"<<rj->Phi()<<endl;
	}
	*/
    }
}
