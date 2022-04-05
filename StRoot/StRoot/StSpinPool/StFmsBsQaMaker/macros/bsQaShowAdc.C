#include "TCanvas.h"
#include "TF1.h"
#include "TFile.h"
#include "TGaxis.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TH1.h"
#include "TH2.h"
#include "TMath.h"
#include "TLegend.h"
#include "TLine.h"
#include "TPaveText.h"
#include "TROOT.h"
#include "TString.h"
#include "TSystem.h"
#include "TStyle.h"

#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
using namespace std;

//Shows ADC distribution of a channel
//===================================================================
void bsQaShowAdc(string inFile, int detId, int ch, bool PRINT = true)
{
	gROOT->Reset();

	TFile *F = TFile::Open(inFile.c_str());
	if (!F || F->IsZombie()) { cout <<"Cannot open the file: " <<inFile <<endl; return; }

	//Get run number
	std::size_t strPos = inFile.find(".root");
	string runNoStr = inFile.substr(strPos-8, 8);
	const int runNo = std::atoi(runNoStr.c_str());

	TH2F* H2 = (TH2F*)F->Get(Form("Adc_d%i", detId));
	if (!H2) { cout <<"Cannot open the histogram!" <<endl; return; }

	TH1F* H1 = (TH1F*)H2->ProjectionY("", ch, ch);
	H1->SetTitle(Form("d%i_ch%i_run%i", detId, ch, runNo));

	int xMax = 1;
	for (int x=0; x<H1->GetNbinsX(); x++) { if (H1->GetBinContent(x+1) != 0) xMax = x+1; }
	H1->GetXaxis()->SetRangeUser(0, xMax + 10);
	H1->GetXaxis()->SetLabelSize(0.05);
	H1->GetXaxis()->SetTitleOffset(1.25);
	H1->GetYaxis()->SetLabelSize(0.05);

	gStyle->SetOptDate(0);
	gStyle->SetOptStat("emr");
	TCanvas *c1 = new TCanvas("c1", H1->GetTitle(), 800*1.5, 600*1.5);
	c1->Divide(1, 2);
	c1->cd(1)->SetLogy();
	H1->DrawCopy("hist e");
	c1->cd(2);
	H1->GetXaxis()->SetRangeUser(0, 32);
	H1->DrawCopy("hist e");

	if (PRINT) c1->Print(Form("%s.png", c1->GetTitle()));
	return;
}//Main
