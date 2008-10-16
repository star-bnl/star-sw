#include <iostream>
#include <fstream>
using namespace std;

#include "TFile.h"
#include "TMath.h"
#include "TH1.h"
#include "TF1.h"
#include "TPostScript.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TLatex.h"
#include "TString.h"
#include "TLine.h"

void drawTower(TH1* h, TF1* f, int id);

void mip_ring_draw(const char* infile="mipskimfile.root", const char* postscript="mipring.ps", const char* outname="2008mipring.root")
{
  const int nrings = 40;
  TFile input(infile);
  char name[100];
  TH1* ring_histo[nrings];
  TH1F* ringMIP = new TH1F("ringMIP","",nrings,-1.0,1.0);
  ringMIP->SetXTitle("#eta");
  ringMIP->SetYTitle("MIP peak (ADC)");
  for(int i = 0; i < nrings; i++)
    {
      sprintf(name,"ring_histo_%i",i+1);
      ring_histo[i] = (TH1F*)input.Get(name);
    }


	TPostScript* ps = new TPostScript(postscript);
	TCanvas* c = new TCanvas("c","",100,100,600.,800.);
	//TH1F* ringprec = new TH1F("ringprec","",40,-1.0,1.0);
	//ringprec->SetYTitle("Gain (fit)/ Target Gain");
	//ringprec->SetXTitle("#eta");
	//double ew[nrings];
	int pad = 16;
	TF1 *gaussian_fit[nrings], *landau_fit[nrings];
	for(int i=0; i<nrings; i++){
		if(pad%15 == 1){
			c->Update();
			ps->NewPage();
			c->Clear();
			c->Divide(3,5);
			pad = 1;
		}
		c->cd(pad);
	  
		cout<<"fitting ring "<<i+1<<" of "<<nrings<<endl;
		
		sprintf(name,"fit_%i",i+1);
		
		gaussian_fit[i] = new TF1(name,"gaus",7.,45.);
		landau_fit[i] = new TF1(name,"landau",7.,200.);
	
		gaussian_fit[i]->SetParameter(1,20.);
		gaussian_fit[i]->SetParameter(2,5.);
		
		landau_fit[i]->SetParameter(1,17.);
		landau_fit[i]->SetParameter(2,3.);
									
		gaussian_fit[i]->SetLineColor(kGreen);
		gaussian_fit[i]->SetLineWidth(0.6);
		
		landau_fit[i]->SetLineColor(kYellow);
		landau_fit[i]->SetLineWidth(0.6);
		
		ring_histo[i]->Fit(gaussian_fit[i],"rq");
		ring_histo[i]->GetXaxis()->SetRangeUser(3.0,50.0);

		drawTower(ring_histo[i],gaussian_fit[i],i);
		
		double histogram_top = ring_histo[i]->GetBinContent(ring_histo[i]->GetMaximumBin());
		
		double gaussian_mean = gaussian_fit[i]->GetParameter(1);
		TLine *gaussian_peak = new TLine(gaussian_mean,0.,gaussian_mean,histogram_top+15);
		gaussian_peak->SetLineColor(kGreen);
		gaussian_peak->SetLineWidth(2.0);
		gaussian_peak->Draw("same");
		
		pad++;
		float eta = (float)((i - 20) * 2 + 1)/40;
		float theta = 2*TMath::ATan(TMath::Exp(-eta));
		float mipenergy  = 0.264 * (1+0.056*eta*eta)/TMath::Sin(theta);
		cout<<mipenergy/gaussian_mean<<", ";
		ringMIP->Fill(eta,gaussian_mean);
		ringMIP->SetBinError(i+1,gaussian_fit[i]->GetParError(1));

	}
	cout<<endl;
	c->Update();
	ps->NewPage();
	c->Clear();
	c->Divide(1,2);
	c->cd(1);
	ringMIP->SetMarkerStyle(20);
	ringMIP->GetYaxis()->SetRangeUser(0,35);
	gStyle->SetOptStat(0);
	ringMIP->Draw("ep");

	ps->Close();

	TFile outfile(outname,"RECREATE");
	for(int i = 0; i < nrings; i++)
	  {
	    ring_histo[i]->Write();
	  }
	ringMIP->Write();
	outfile.Close();
}

void drawTower(TH1* h, TF1* f, int id){		
	//calculate a few quantities
	//double peak = f->GetParameter(1);
	//double mean = f->Mean(5.,200.);
	//double histo_height = h->GetBinContent(h->GetMaximumBin());
	//if(histo_height == 0) histo_height = 1.;
	
	int xLatexBin = 20;

	//histogram options
	h->SetXTitle("ADC");
	h->Draw();
	
	//draw a line through the location of the MIP peak
	char line_name[50];
	sprintf(line_name,"mip_peak_%i",id);
	//TH1* line = new TH1F(line_name,line_name,h->GetNbinsX(),h->GetBinLowEdge(1),h->GetBinLowEdge(h->GetNbinsX()+1));
	//	line->SetLineColor(kRed);
	//line->Fill(peak,1.e8);
//	line->Draw("same");
	
	//write the tower number on the plot
	char tower_title[100];
	float eta = (float)((id - 20) * 2 + 1)/40;
	sprintf(tower_title,"eta = %f",eta);
	TLatex title_latex;
	title_latex.SetTextSize(0.15);
	//if(isBadTower2006(id)) title_latex.SetTextColor(kRed);
//	title_latex.DrawLatex(xLatexBin,0.94*histo_height,tower_title);
	title_latex.DrawTextNDC(0.13,0.78,tower_title);

	
}
