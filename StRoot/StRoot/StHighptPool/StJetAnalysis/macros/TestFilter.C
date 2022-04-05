
class StMuDstMaker;
class HistMaker;

StMuDstMaker* maker;
HistMaker* myHistMaker;

void TestFilter(const char* indir, const char* infile, const char* outfile)
{
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
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
    gSystem->Load("JetFinder");

    cout << " loading done " << endl;
  
    StMuDebug::setLevel(0);  // switch of some debug output

    int iret=0;
    cout <<" --- Create StMuDstMaker --- "<<endl;
    maker = new StMuDstMaker(0,0,indir,"",infile,1);   // set up maker in read mode

    cout <<" --- Set Cuts ---"<<endl;
    //Setup cuts
    AnaCuts cuts;
    cuts.ptCut = 1.0;    // momentum (pt) (GeV)
    cuts.pseudoRapidityCutOff = 1.5;
    cuts.minNumberOfFitPoints = 22;
    cuts.minNumberOfPoints = 25;
    cuts.verbose=true;

    cout <<" --- Create Filter ---"<<endl;
    //This must match the enum in StJetFilter.h
    enum ioType {kWrite=0, kRead=1};

    myFilter = new StJetFilter(kWrite,outfile);
    myFilter.setCuts(cuts);

    cout <<" --- Loop on events ---"<<endl;
    
    //TChain* t = maker->tree();
    //int nEvents = t->GetEntries();

    int nEvents = 1000;
    int iret = 0; //iret==4 means kEOF
    
    for (int iev=0; iev<nEvents && iret!=4; ++iev) {
	iret = maker->Make();  // read an event
	cout <<"iret:\t"<<iret<<endl;
	if (iret!=4) {
	    myFilter->fill(maker);

	    //Get Tracks
	    TClonesArray* tracks = myFilter->event()->tracks();
	    cout <<"------------ From Macro -------"<<endl;
	    int nTracks = tracks->GetLast()+1; //don't know why, but they don't have a size method
	    for (int i=0; i<nTracks; ++i) {
		StMuTrack* track = static_cast<StMuTrack*>(tracks->operator[](i));
		cout <<"momentum:\t"<<track->p()<<endl;
	    }
	}
    }

    //Now get number of entries:
    int nEntries = myFilter->nEvents();
    cout <<"\n\n Wrote file:\t"<<outfile<<"\twith number of entries:\t"<<nEntries<<endl;
    
    delete maker;
    maker=0;
    delete myFilter;
    myFilter=0;

    //Now rename the file:
    cout <<"tack int onto filename"<<endl;
    char* newfile = new char[1000];
    sprintf(newfile,"%s_has_%i_events",outfile,nEntries);

    cout <<"concat the unix command"<<endl;
    char* command = new char[2000];
    sprintf(command,"mv %s %s",outfile, newfile);
    
    cout <<"Get TUnixSystem"<<endl;
    TUnixSystem* sys = static_cast<TUnixSystem*>(gSystem);

    cout <<"copy a file from:\t"<<outfile<<"\t to:\t"<<newfile<<endl;
    cout <<"command is:\t"<<command<<endl;
    sys->Exec(command);
    cout <<"Done"<<endl;
}

