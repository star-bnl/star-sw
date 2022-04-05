
class StMuDstMaker;
class HistMaker;

StMuDstMaker* maker;
HistMaker* myHistMaker;

void TestRead(const char* infile, const char* outfile)
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

    //Make histograms
    double pi=3.14159;
    TH2* dEtaVsdPhi = new TH2D("dEtaVsdPhi","#Delta#eta vs #Delta#phi",40, -pi, pi, 40, -1., 1.);
    
    cout <<" --- Set Cuts ---"<<endl;
    //Setup cuts
    AnaCuts cuts;
    cuts.ptCut = 2.0;    // pt for now
    cuts.triggerPtCut = 3.0;
    cuts.pseudoRapidityCutOff = 1.0;
    cuts.minNumberOfFitPoints = 22;
    cuts.minNumberOfPoints = 25;
    cuts.verbose=false;
    
    cout <<" --- Create Filter ---"<<endl;
    //This must match the enum in StJetFilter.h
    enum ioType {kWrite=0, kRead=1};
    
    cout <<"Read from file:\t"<<infile<<endl;
    myFilter = new StJetFilter(kRead, infile);
    myFilter->setCuts(cuts);
    
    cout <<" --- Create HistMaker ---"<<endl;
    HistMaker* myHister = new HistMaker();
    myHister->setCuts(cuts);
    myHister->setdEtaVsdPhi(dEtaVsdPhi);
    
    cout <<" --- Loop on events ---"<<endl;
    int nEvents=myFilter->nEvents();
    for (int iev=0; iev<nEvents; ++iev) {
	//for (int iev=0; iev<nEvents && iev<10; ++iev) {
	if (fmod(static_cast<double>(iev),1000.)==0.) {
	    cout <<"\t --- chugging on event:\t"<<iev<<"\t of :"<<nEvents<<endl;
	}
	myFilter->fill();
	
	//Get Tracks
	StJetMuEvent* event = myFilter->event();
	myHister->fill(event);
    }

    cout <<" --- Save Histograms --- "<<endl;
    TFile* ofile = new TFile(outfile,"RECREATE");
    ofile->cd();
    dEtaVsdPhi->Write();
    ofile->Close();

    delete myFilter;
    myFilter=0;
    delete myHister;
    myHister=0;
}

