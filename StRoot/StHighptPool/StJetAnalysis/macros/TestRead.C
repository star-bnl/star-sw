
class StMuDstMaker;
class HistMaker;

StMuDstMaker* maker;
HistMaker* myHistMaker;

void TestRead(const char* infile, const char* outfile)
{
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
    gSystem->Load("UpsilonAna");

    cout << " loading done " << endl;
  
    StMuDebug::setLevel(0);  // switch of some debug output

    int iret=0;

    cout <<" --- Set Cuts ---"<<endl;
    //Setup cuts
    AnaCuts cuts;
    //cuts.electronMomentumCut = 3.0;    // momentum (not pt) (GeV)
    cuts.electronMomentumCut = 2.5;    // pt for now
    cuts.pseudoRapidityCutOff = 1.0;
    cuts.minNumberOfFitPoints = 20;
    cuts.minNumberOfPoints = 25;
    cuts.lowerInvariantMassCut = 6.; //GeV
    cuts.upperInvariantMassCut = 14.; //GeV
    cuts.verbose=false;
    
    cout <<" --- Create Filter ---"<<endl;
    //This must match the enum in StUpsilonFilter.h
    enum ioType {kWrite=0, kRead=1};
    
    cout <<"Read from file:\t"<<infile<<endl;
    myFilter = new StUpsilonFilter(kRead, infile);
    myFilter->setCuts(cuts);
    
    cout <<" --- Create HistMaker ---"<<endl;
    HistMaker* myHister = new HistMaker();
    myHister->setCuts(cuts);
    
    cout <<" --- Loop on events ---"<<endl;
    int nEvents=myFilter->nEvents();
    for (int iev=0; iev<nEvents; ++iev) {
	//for (int iev=0; iev<nEvents && iev<10; ++iev) {
	if (fmod(static_cast<double>(iev),1000.)==0.) {
	    cout <<"\t --- chugging on event:\t"<<iev<<"\t of :"<<nEvents<<endl;
	}
	myFilter->fill();
	
	//Get Tracks
	StUpsilonMuEvent* event = myFilter->event();
	myHister->fill(event);
    }

    cout <<" --- Save Histograms --- "<<endl;
    TFile* ofile = new TFile(outfile,"RECREATE");
    ofile->cd();
    myHister->plusPlus()->Write();
    myHister->minusMinus()->Write();
    myHister->plusMinus()->Write();
    ofile->Close();

    delete myFilter;
    myFilter=0;
    delete myHister;
    myHister=0;
}

