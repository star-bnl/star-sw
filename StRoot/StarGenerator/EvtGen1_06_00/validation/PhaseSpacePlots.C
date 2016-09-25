#include <string>

using std::string;

void PhaseSpacePlots() {

    string fileName1 = "rootFiles/B0JpsiKpi.root";
    string fileName2 = "rootFiles/B0KpiJpsi.root";

    string decay1 = "B^{0} #rightarrow J/#psi K #pi";
    string decay2 = "B^{0} #rightarrow K #pi J/#psi";

    string xLabel = "m^{2}(K#pi)";
    string yLabel = "m^{2}(J/#psi #pi)";
    int xInt1 = 1;
    int yInt1 = 2;
    int xInt2 = 3;
    int yInt2 = 1;

    TLatex latex;
    latex.SetTextSize(0.05);
    latex.SetNDC(kTRUE);

    gROOT->SetStyle("Plain");
    gStyle->SetOptStat(0);
    TCanvas* theCanvas = new TCanvas("theCanvas", "", 1500, 600);
    theCanvas->UseCurrentStyle();
 
    theCanvas->Divide(2,1);

    theCanvas->cd(1);
    TH2D* plot1 = getDalitzPlot(fileName1, xInt1, yInt1, xLabel, yLabel);
    plot1->Draw("colz");
    latex.DrawLatex(0.3, 0.95, decay1.c_str());

    theCanvas->cd(2);
    TH2D* plot2 = getDalitzPlot(fileName2, xInt2, yInt2, xLabel, yLabel);
    plot2->Draw("colz");
    latex.DrawLatex(0.3, 0.95, decay2.c_str());

    theCanvas->Print("PhaseSpacePlots.png");

}

TH2D* getDalitzPlot(string fileName, int xInt = 1, int yInt = 2,
		    string xLabel = "m^{2}_{23}", 
		    string yLabel = "m^{2}_{13}") {

    TFile* theFile = new TFile(fileName.c_str(), "read");
    if (!theFile) {return 0;}

    TTree* theTree = dynamic_cast<TTree*>(theFile->Get("dalitzTree"));
    if (!theTree) {return 0;}

    double m12Sq(0.0), m23Sq(0.0), m13Sq(0.0);
    theTree->SetBranchAddress("invMass12Sq", &m12Sq);
    theTree->SetBranchAddress("invMass23Sq", &m23Sq);
    theTree->SetBranchAddress("invMass13Sq", &m13Sq);

    // Turn off unneeded branches
    theTree->SetBranchStatus("invMass12", 0);
    theTree->SetBranchStatus("invMass23", 0);
    theTree->SetBranchStatus("invMass13", 0);

    double m12SqMin = theTree->GetMinimum("invMass12Sq");
    double m12SqMax = theTree->GetMaximum("invMass12Sq");

    double m23SqMin = theTree->GetMinimum("invMass23Sq");
    double m23SqMax = theTree->GetMaximum("invMass23Sq");

    double m13SqMin = theTree->GetMinimum("invMass13Sq");
    double m13SqMax = theTree->GetMaximum("invMass13Sq");

    // Default: xAxis = m23Sq, yAxis = m13Sq, for xInt = 1, yInt = 2
    double xMin(m23SqMin), xMax(m23SqMax);
    double yMin(m13SqMin), yMax(m13SqMax);

    if (xInt == 2) {
	xMin = m13SqMin; xMax = m13SqMax;
    } else if (xInt == 3) {
	xMin = m12SqMin; xMax = m12SqMax;
    }

    if (yInt == 1) {
	yMin = m23SqMin; yMax = m23SqMax;
    } else if (yInt == 3) {
	yMin = m12SqMin; yMax = m12SqMin;
    }

    // Round off limits to nearest "integer"
    xMin = roundMin(xMin);
    xMax = roundMax(xMax);
    yMin = roundMin(yMin);
    yMax = roundMax(yMax);

    TH2D* DPHist = new TH2D("DPHist", "", 100, xMin, xMax, 100, yMin, yMax);
    DPHist->SetDirectory(0);

    DPHist->SetXTitle(xLabel.c_str());
    DPHist->SetYTitle(yLabel.c_str());

    int N = theTree->GetEntries();
    int i(0);

    for (i = 0; i < N; i++) {

	theTree->GetEntry(i);
	
	if (i%100000 == 0) {cout<<"i = "<<N-i<<endl;}

	double xVal = m23Sq;
	double yVal = m13Sq;

	if (xInt == 2) {
	    xVal = m13Sq;
	} else if (xInt == 3) {
	    xVal = m12Sq;
	}

	if (yInt == 1) {
	    yVal = m23Sq;
	} else if (yInt == 3) {
	    yVal = m12Sq;	    
	}

	DPHist->Fill(xVal, yVal);

    }

    return DPHist;

}

double roundMin(double x) {

    double value = floor(x)*0.98;
    return value;

}

double roundMax(double x) {

    double value = ceil(x)*1.02;
    return value;

}
