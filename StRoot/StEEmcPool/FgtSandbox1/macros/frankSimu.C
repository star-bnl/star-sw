#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TF1.h>
#include <TSystem.h>
#include <TLatex.h>
#include <iostream>
#include <fstream>
#include <TGraph.h>
#include <TGraphErrors.h>
#include <TMath.h>
#include <TLine.h>
#include <TChain.h>
#include <TClonesArray.h>
#include <TFile.h>
#include <TPostScript.h>
#include <TNtuple.h>
#include <TString.h>
#include <TRandom.h>
#include <TRandom2.h>

void 
frankSimu(float rotAngle=0., long nEvents = 1000) {
/*
gStyle->SetPalette(1);
gStyle->SetOptStat(1);
gStyle->SetOptFit(11);
//gStyle->SetOptTitle(0);
gStyle->SetStatBorderSize(1);
gStyle->SetStatColor(10);
gStyle->SetCanvasColor(10);
gStyle->SetPadLeftMargin(0.16);
gStyle->SetPadBottomMargin(0.16);
gStyle->SetPadTickX(1);
gStyle->SetPadTickY(1);
gStyle->SetOptTitle(0);
gStyle->SetTitleSize(0.048,"xy");
gStyle->SetLabelSize(0.04,"xy");
gStyle->SetTitleOffset(1.3,"x");
gStyle->SetTitleOffset(1.3,"y");
*/


	
	
	// in order to simulate the energy loss for individual ionizing collisions a table from Bichsel is used. 
	// This is given in figure 9 in NIM A562, 154 (2006)
	// This table gives the energy loss for a given random number (from 0 to 1 in steps of 10^-4)
	// Two files, low and high beta gamma:
	// file Low: beta gamma .31623      1.00000      3.16228     10.00000     31.62278
	// file High: beta gamma 100.00000    316.22777   1000.00000   3162.27766  10000.00000
	
	// here use column 2 for High file
	
	double eLoss[10000];
	ifstream inFile("BichselELossProbHighBG.dat");
	double cl1, cl2, cl3, cl4, cl5, cl6, cl7;
	for (int i = 0; i < 10000; i++) {
		inFile >> cl1 >> cl2 >> cl3 >> cl4 >> cl5 >> cl6>> cl7;
		eLoss[i] = cl4;
	}
	
	float angle = TMath::Pi() * rotAngle / 180.;
	
	double pathLength = 3.2 / TMath::Cos(angle);
	double pairsPerMM = 4.;
	
	
	TRandom2* tR = new TRandom2();
	tR->SetSeed(0);
	
	TH1F* hMean = new TH1F("hMean", "hMean", 100, -1, 1);
	TH1F* hNP = new TH1F("hNP", "hNP", 35, -0.5, 34.5);
	hNP->SetXTitle("Number of Primary Pairs");
	TH1F* hElectron = new TH1F("hElectron", "hElectron", 250, -0.5, 249.5);
	TH1F* hEnergy = new TH1F("hEnergy", "hEnergy", 250, 0, 5.);
	hEnergy->SetXTitle("Energy Loss [keV]");
	TH1F* hEPerColl = new TH1F("hEPerColl", "hEPerColl", 100, 0, 100);
	hEPerColl->SetXTitle("Energy Loss per collision [eV]");
	
	TF1* fElectronDist = new TF1("fElectronDist", "1/(x*x*x)", 0, 100);
	float pos[1000];
	Double_t weight[1000];
	int totalElectrons  = 0;
	float totalEnergy = 0;
	double dist = 0;
	
	for (int i = 0; i < nEvents; i++) {
		
		int np = 0;
		totalElectrons = 0;
		totalEnergy = 0;
		dist = 0;
		while (1) {	
			// make a random step
			double stepLength = - TMath::Log(tR->Uniform()) / pairsPerMM;
			dist += stepLength;
			if (dist > pathLength) break;
			np++;
			pos[np-1] = dist/pathLength -0.5;//tR->Uniform() - 0.5;
			// additional weight according to secondary energy distribution, according to Bichsel dist
			int rndBin = ((int) (10000.0 * tR->Uniform()));
			double eL = eLoss[rndBin];
			// alternative approach: subtract Ar binding energy , see how much is left over
			int ns = 1 + ((int) ((eL-15.4)/26.));
			totalEnergy += eL;
			hEPerColl->Fill(eL);
				if (ns < 0) ns = 0;
			weight[np-1] = ns;
			totalElectrons += weight[np-1];
		}
		float mpos = TMath::Mean(np, pos, weight);
		mpos = mpos * 3.2 * TMath::Sin(angle);
		hMean->Fill(mpos);
		hNP->Fill(np);
		hElectron->Fill(totalElectrons);
		totalEnergy = totalEnergy / 1000.;
		hEnergy->Fill(totalEnergy);
	}
	
	TCanvas* c1 = new TCanvas("SimuMean", "SimuMean", 600, 450);
	hMean->Draw();
	TCanvas* c2 = new TCanvas("NumberOfPairs", "NumberOfPairs", 600, 800);
	c2->Divide(1, 2);
	c2->cd(1);
	hNP->Draw();
	c2->cd(2);
	hEPerColl->Draw();
	TCanvas* c3 = new TCanvas("NumberOfElectrons", "NumberOfElectrons", 600, 800);
	c3->Divide(1, 2);
	c3->cd(1);
	hElectron->Draw();
	c3->cd(2);
	hEnergy->Draw();
	
}
