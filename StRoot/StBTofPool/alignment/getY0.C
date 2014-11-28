#include <stdio>
#include "iomanip.h"

void getY0(TString align_file)
{
  gROOT->Reset();
  gROOT->LoadMacro("fitfun.C");   // fit function for localY, local Z

  const Int_t N = 120;

  //histograms to check the par2 and par3 distributions
  TH1D *hPar2 = new TH1D("Par2","",500,0., 5.);
  TH1D *hPar3 = new TH1D("Par3","",500,0., 1.);

  TFile *fin = new TFile(align_file.Data());
  TH2D *yLocal2D = (TH2D *)fin->Get("yLocal_all");
  yLocal2D->RebinY(5);
  TH1D *yLocal[N];
  for(int i=0;i<N;i++) {
    char hisname[100];
    sprintf(hisname,"Tray_%d",i+1);
    yLocal[i] = yLocal2D->ProjectionY(hisname,i+1,i+1);
    yLocal[i]->Sumw2();
  }

  TF1 *fitfun = new TF1("fitfun",fitfun,-5.,5.,5);

  double t[N];
  double y0[N], ye[N];

  gStyle->SetOptStat(101);
  gStyle->SetOptFit(100);
  gStyle->SetTextSize(0.06);
  gStyle->SetTextFont(42);
  TCanvas *c1 = new TCanvas("c1","c1",0,0,800,600);
  c1->SetFillColor(10);
  c1->SetBorderMode(0);
  c1->SetBorderSize(2);
  c1->Divide(4,3);

  TPostScript *ps = new TPostScript("fig/yLocalFit.ps",112);

  for(int i=0;i<N;i++) {
    t[i] = i+1;  // tray number

    if(i%12==0) ps->NewPage();   // 12 trays per page
    c1->cd(i%12+1);

    double par[5], err[5];
    fitfun->SetParameters(0., yLocal[i]->GetBinContent(50), 0.1, 1., 0.);
    fitfun->SetLineWidth(2);
    yLocal[i]->Draw("e");
    double entries = yLocal[i]->Integral(1,100);
    if(entries>10) {
      fitfun->SetParLimits(2, 1.55, 1.7);
      fitfun->SetParLimits(3, 0.1, 0.3);
      yLocal[i]->Fit("fitfun","R");
      fitfun->GetParameters(&par[0]);
      err[0] = fitfun->GetParError(0);
    } else {
      par[0] = 0.;
      err[0] = 0.;
    }

    hPar2->Fill(par[2]);
    hPar3->Fill(par[3]);

    char text[100];
    sprintf(text, "Y0 = %5.3f #pm %5.3f", par[0], err[0]);

    TLatex *tex = new TLatex(-3, par[1]/3., text);
    tex->SetTextSize(0.07);
    tex->SetTextFont(12);
    tex->Draw("same");

    y0[i] = par[0];
    ye[i] = err[0];
    
    c1->Update();
  }

  ps->NewPage();

  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);
  gStyle->SetEndErrorSize(0.01);
   
  TCanvas *c1 = new TCanvas("c1","c1",0,0,800,600);
  c1->SetFillColor(10);
  c1->SetBorderMode(0);
  c1->SetBorderSize(2);
  c1->SetLeftMargin(0.14);
  c1->SetBottomMargin(0.15);
  c1->SetTopMargin(0.02);
  c1->SetRightMargin(0.04);
  c1->Draw();

  double x1 = 0.5;
  double x2 = 120.5;
  double y1 = -2.;
  double y2 = 2.;
  TH1D *h0 = new TH1D("h0","",1, x1, x2);
  h0->SetMinimum(y1);
  h0->SetMaximum(y2);
   h0->GetXaxis()->SetNdivisions(208);
   h0->GetXaxis()->CenterTitle();
   h0->GetXaxis()->SetTitle("Tray #");
   h0->GetXaxis()->SetTitleOffset(1.0);
   h0->GetXaxis()->SetTitleSize(0.07);
   h0->GetXaxis()->SetLabelOffset(0.01);
   h0->GetXaxis()->SetLabelSize(0.05);
   h0->GetXaxis()->SetLabelFont(42);
   h0->GetXaxis()->SetTitleFont(42);
   h0->GetYaxis()->SetNdivisions(210);
   h0->GetYaxis()->SetTitle("yLocal Offset");
   h0->GetYaxis()->SetTitleOffset(1.0);
   h0->GetYaxis()->SetTitleSize(0.07);
   h0->GetYaxis()->SetLabelOffset(0.01);
   h0->GetYaxis()->SetLabelSize(0.045);
   h0->GetYaxis()->SetLabelFont(42);
   h0->GetYaxis()->SetTitleFont(42);
   h0->Draw();

   TLine *l1 = new TLine(x1,y1,x2,y1);
   l1->SetLineWidth(3);
   l1->Draw("same");
   TLine *l2 = new TLine(x1,y2,x2,y2);
   l2->SetLineWidth(3);
   l2->Draw("same");
   TLine *l3 = new TLine(x1,y1,x1,y2);
   l3->SetLineWidth(3);
   l3->Draw("same");
   TLine *l4 = new TLine(x2,y1,x2,y2);
   l4->SetLineWidth(3);
   l4->Draw("same");

   TGraphErrors *gr = new TGraphErrors(N, t, y0, 0, ye);
   gr->SetMarkerStyle(20);
   gr->SetMarkerSize(1.5);
   gr->SetLineWidth(2);
   gr->Draw("p");

   c1->Update();

  ps->Close();

  ofstream outData;
  outData.open("yOffset.dat");
  for(int i=0;i<N;i++) {
    outData << setw(15) << y0[i] << setw(15) << ye[i] << endl;
  }
  outData.close();

  TFile  *fout = new TFile("yFitPar.root","recreate");
  hPar2->Write();
  hPar3->Write();
  fout->Close();
}
