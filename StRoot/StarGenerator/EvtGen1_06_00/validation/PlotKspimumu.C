TCanvas* theCanvas = new TCanvas("theCanvas", "", 900, 500);
gROOT->SetStyle("Plain");
theCanvas->UseCurrentStyle();

void PlotKspimumu() {

    // K0s -> pi0 mu+ mu-

    TFile* theFile = TFile::Open("Kspimumu.root", "read");
    TTree* theTree = dynamic_cast<TTree*>(theFile->Get("Data"));

    int eventId, PDGId, dVtx, daug;
    double E, px, py, pz, EL, pxL, pyL, pzL;
    theTree->SetBranchAddress("eventId", &eventId);
    theTree->SetBranchAddress("PDGId", &PDGId);
    theTree->SetBranchAddress("dVtx", &dVtx);
    theTree->SetBranchAddress("daug", &daug);
    //theTree->SetBranchAddress("E", &E);
    //theTree->SetBranchAddress("px", &px);
    //theTree->SetBranchAddress("py", &py);
    //theTree->SetBranchAddress("pz", &pz);
    theTree->SetBranchAddress("EL", &EL);
    theTree->SetBranchAddress("pxL", &pxL);
    theTree->SetBranchAddress("pyL", &pyL);
    theTree->SetBranchAddress("pzL", &pzL);

    int nEntries = theTree->GetEntries();
    int i(0);

    int oldEvent(0);
    
    int maxEvent = theTree->GetMaximum("eventId");

    // Mass of mu+ mu- pair
    TH1D* mHist = new TH1D("mHist", "", 40, 0.2, 0.4);
    mHist->SetDirectory(0);
    mHist->SetXTitle("M_mumu (GeV/c^{2})");

    // cosHelicity of mu-
    TH1D* cHist = new TH1D("cHist", "", 40, -1.0, 1.0);
    cHist->SetDirectory(0);
    cHist->SetXTitle("cosHel");

    TLorentzVector mumP4, mupP4, twoMuP4, pi0P4;
    int mumId(13), mupId(-13), pi0Id(111);
    //int K0sId(310);

    bool gotMum(false), gotMup(false), gotPi0(false);

    for (i = 0; i < nEntries; i++) {

	theTree->GetEntry(i);

	if (i%100000 == 0) {cout<<"Event = "<<nEntries-i<<endl;}

	if (gotMum && gotMup && gotPi0) {

	    // Store the mass and cosHelicity values
	    twoMuP4 = mumP4 + mupP4;
	    mHist->Fill(twoMuP4.M());

	    double cosHel = cosHelicity(mumP4, twoMuP4, pi0P4);
	    cHist->Fill(cosHel);

	    zeroVector(mumP4); gotMum = false;
	    zeroVector(mupP4); gotMup = false;
	    zeroVector(pi0P4); gotPi0 = false;
	    zeroVector(twoMuP4);

	}

	TLorentzVector p4(pxL, pyL, pzL, EL);

	if (PDGId == mumId) {
	    mumP4.SetPx(pxL); mumP4.SetPy(pyL); mumP4.SetPz(pzL); mumP4.SetE(EL);
	    gotMum = true;
	} else if (PDGId == mupId) {
	    mupP4.SetPx(pxL); mupP4.SetPy(pyL); mupP4.SetPz(pzL); mupP4.SetE(EL);
	    gotMup = true;
	} else if (PDGId == pi0Id) {
	    pi0P4.SetPx(pxL); pi0P4.SetPy(pyL); pi0P4.SetPz(pzL); pi0P4.SetE(EL);
	    gotPi0 = true;
	}

    }

    // For last event
    if (gotMum && gotMup && gotPi0) {

	// Store the mass and cosHelicity values
	twoMuP4 = mumP4 + mupP4;
	mHist->Fill(twoMuP4.M());

	double cosHel = cosHelicity(mumP4, twoMuP4, pi0P4);
	cHist->Fill(cosHel);
    }

    theCanvas->cd();
    theCanvas->Divide(2,1);
    theCanvas->cd(1);
    mHist->Draw();
    theCanvas->cd(2);
    cHist->SetMinimum(0.0);
    cHist->Draw();
    theCanvas->Print("Plots_Kspimumu.png");

}

double cosHelicity(TLorentzVector particle, TLorentzVector parent,
		   TLorentzVector grandparent) {
 
    TVector3 boosttoparent = -(parent.BoostVector());
 
    particle.Boost(boosttoparent);
    grandparent.Boost(boosttoparent);
 
    TVector3 particle3 = particle.Vect();
    TVector3 grandparent3 = grandparent.Vect();
    double numerator = particle3.Dot(grandparent3);
    double denominator = (particle3.Mag())*(grandparent3.Mag());
    double temp = numerator/denominator;
 
    return temp;
 
}

void zeroVector(TLorentzVector vect) {

    vect.SetX(0.0);
    vect.SetY(0.0);
    vect.SetZ(0.0);
    vect.SetT(0.0);

}
