// Macro to create validation plots for testing PHOTOS for
// B0 -> K*0' gamma, K*0' -> K+ pi- nGamma, where n = 0,1,2,...
// Uses the ROOT files created by genRootDecayChain program

#include <string>
using std::string;

void initHist(TH1D* theHist) {

    theHist->SetDirectory(0);

    theHist->SetTitleOffset(1.1, "Y");
    theHist->SetLineWidth(2);
    TAxis* xAxis = theHist->GetXaxis();
    xAxis->SetLabelSize(0.05);
    xAxis->SetTitleSize(0.05);
    TAxis* yAxis = theHist->GetYaxis();
    yAxis->SetLabelSize(0.05);
    yAxis->SetTitleSize(0.05);

}

void plotBKstarGamma(string inFileName = "BKstarGamma.root") {

    TFile* theFile = new TFile(inFileName.c_str(), "read");
    TTree* theTree = dynamic_cast<TTree*>(theFile->Get("Data"));

    // Create plots of K*0' mass, momentum of 1st and FSR gammas, 
    // mtm of K+, pi-, and the multiplicity of photons
    TH1D* KstMHist = new TH1D("KstMHist", "", 100, 0.0, 5.0);
    KstMHist->SetXTitle("K^{*0'} mass (GeV/c^{2})");
    KstMHist->SetYTitle("Frequency/50 (MeV/c^{2})");
    initHist(KstMHist);

    TH1D* gamPHist = new TH1D("gamPHist", "", 100, 0.0, 3.0);
    gamPHist->SetXTitle("1st #gamma mtm (GeV/c)");
    gamPHist->SetYTitle("Frequency/30 (MeV/c^{2})");
    initHist(gamPHist);

    TH1D* FSRPHist = new TH1D("FSRPHist", "", 100, 0.0, 2.0);
    FSRPHist->SetXTitle("FSR #gamma mtm (GeV/c)");
    FSRPHist->SetYTitle("Frequency/20 (MeV/c)");
    initHist(FSRPHist);

    TH1D* KPHist = new TH1D("KPHist", "", 100, 0.0, 3.0);
    KPHist->SetXTitle("K^{+} mtm (GeV/c)");
    KPHist->SetYTitle("Frequency/30 (MeV/c)");
    initHist(KPHist);

    TH1D* piPHist = new TH1D("piPHist", "", 100, 0.0, 3.0);
    piPHist->SetXTitle("#pi^{-} mtm (GeV/c)");
    piPHist->SetYTitle("Frequency/30 (MeV/c)");
    initHist(piPHist);

    TH1D* gamNHist = new TH1D("gamNHist", "", 6, 0, 6);
    gamNHist->SetXTitle("Number of photons");
    gamNHist->SetYTitle("Fraction of events");
    initHist(gamNHist);

    // Setup the TTree variables
    int eventId(0), PDGId(0), pVtx(0), daug(0);
    double pL(0.0), m(0.0);
    theTree->SetBranchStatus("*", 0);

    theTree->SetBranchStatus("eventId", 1);
    theTree->SetBranchStatus("PDGId", 1);
    theTree->SetBranchStatus("daug", 1);
    theTree->SetBranchStatus("pVtx", 1);
    theTree->SetBranchStatus("pL", 1);
    theTree->SetBranchStatus("m", 1);
    theTree->SetBranchAddress("eventId", &eventId);
    theTree->SetBranchAddress("PDGId", &PDGId);
    theTree->SetBranchAddress("pVtx", &pVtx);
    theTree->SetBranchAddress("daug", &daug);
    theTree->SetBranchAddress("pL", &pL);
    theTree->SetBranchAddress("m", &m);

    int nEntries = theTree->GetEntries();
    int i(0);

    int oldEvent(-1);
    int nPhotons(0);
    int nEvents = theTree->GetMaximum("eventId") + 1;

    for (i = 0; i < nEntries; i++) {

	theTree->GetEntry(i);

	if (oldEvent != eventId) {

	    // We have a new event
	    // Fill the photon multiplicity histogram
	    gamNHist->Fill(nPhotons*1.0);

	    // Reset photon counter
	    nPhotons = 0;
	    
	    // Set the old event index to the current one
	    oldEvent = eventId;

	}

	if (PDGId == 100313) {
	    // K*0' mass distribution
	    KstMHist->Fill(m);
	}

	if (PDGId == 22) {

	    if (pVtx == 0 && daug == 2) {
		// 1st photon momentum
		gamPHist->Fill(pL);
	    } else {
		// Other FSR photon momentum
		FSRPHist->Fill(pL);
	    }

	    // Increment the photon counter
	    nPhotons++;

	}

	if (PDGId == 321) {
	    KPHist->Fill(pL);
	}

	if (PDGId == -211) {
	    piPHist->Fill(pL);
	}

    }

    // Create the plots

    gROOT->SetStyle("Plain");
    gStyle->SetOptStat(0);
    TCanvas* theCanvas = new TCanvas("theCanvas", "", 1200, 1100);
    theCanvas->UseCurrentStyle();

    theCanvas->Divide(2,3);

    // Same normalisation number for all plots (except FSR momenta)
    double scale = 1.0/(nEvents*1.0);

    // K*0' mass
    theCanvas->cd(1);
    gPad->SetLogy(0);
    KstMHist->Scale(scale);
    KstMHist->SetMaximum(0.16);
    KstMHist->Draw();

    // 1st photon momentum
    theCanvas->cd(2);
    gPad->SetLogy(1);
    gamPHist->Scale(scale);
    gamPHist->SetMaximum(0.5);
    gamPHist->Draw();

    // K+ momentum
    theCanvas->cd(3);
    KPHist->Scale(scale);
    KPHist->SetMaximum(0.025);
    KPHist->Draw();

    // FSR photon momenta
    theCanvas->cd(4);
    gPad->SetLogy(1);
    // Scale is equal to the area of the histogram, since there may 
    // be events with more than one FSR photon
    double scale1(1.0);
    double FSRIntegral = FSRPHist->Integral();
    if (FSRIntegral > 0.0) {scale1 = 1.0/FSRIntegral;}
    FSRPHist->Scale(scale1);
    FSRPHist->SetMaximum(1.0);
    FSRPHist->Draw();

    // pi- momentum
    theCanvas->cd(5);
    piPHist->Scale(scale);
    piPHist->SetMaximum(0.025);
    piPHist->Draw();

    // Photon multiplicity
    theCanvas->cd(6);
    gamNHist->Scale(scale);
    gamNHist->SetMaximum(1.0);
    gamNHist->Draw();

    theCanvas->cd(1);
    TLatex latex;
    latex.SetNDC();
    latex.SetTextSize(0.045);
    latex.DrawLatex(0.1, 0.95, "B0 #rightarrow K^{*0'} #gamma, K^{*0'} #rightarrow K^{+} #pi^{-} with PHOTOS");

    theCanvas->Print("plotBKstarGamma.eps");
    //theCanvas->Print("plotBKstarGamma.png");


}
