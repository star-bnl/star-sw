// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;

StMuDstMaker* maker;

void example() {
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StMagF");
    gSystem->Load("StUtilities");  // new addition 22jul99
    gSystem->Load("StTreeMaker");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities"); 
    gSystem->Load("StMcEvent"); 
    gSystem->Load("StMcEventMaker"); 
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StMcAnalysisMaker");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");

    cout << " loading done " << endl;
  
    StMuDebug::setLevel(0);  // switch of some debug output

    int iret=0;
    maker = new StMuDstMaker(0,0,"data/minbias/","","MuDst.root",10);   // set up maker in read mode

    int nEvents=10;
    for (int iev=0; iev<nEvents; ++iev) {
	iret = maker->Make();  // read an event 
	
	StMuEvent* e = maker->muDst()->event();
	if (e) {
	    StL0Trigger &t=e->l0Trigger();
	    StEventInfo &info=e->eventInfo();
	}
	
	int n = maker->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
	cout <<"Number of Primary Tracks:\t"<<n<<endl;
	
	//cout <<"Event "<<iev<<endl;
	for (int i=0; i<n; i++) {
	    //StMuTrack* primaryTrack = maker->muDst()->primaryTracks(i);     // get pointer to primary track
	    //printf("momentumPrimary=%8f ",primaryTrack->p().mag())<<endl;	
	}
    }
}

