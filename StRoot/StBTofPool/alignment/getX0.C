#include <stdio>
#include "iomanip.h"

void getX0(TString align_file)
{
  gROOT->Reset();
  gROOT->LoadMacro("fitfun.C");   // fit function for localY, local Z

  const Int_t N = 120;
  const Int_t M = 7; // number of module angle groups

  //histograms to check the par2 and par3 distributions
  TH1D *hPar2 = new TH1D("Par2","",500,0., 5.);
  TH1D *hPar3 = new TH1D("Par3","",500,0., 1.);

  Double_t a[M] = {0., 16., 20., 22., 26., 30., 32.};

  TH2D *zLocal2D[M];
  TH1D *zLocal[N][M];

  TFile *fin = new TFile(align_file.Data());
  for(int j=0;j<M;j++) {
    char inhis[100];
    sprintf(inhis,"zLocal_%d",j);
    zLocal2D[j] = (TH2D *)fin->Get(inhis);
    zLocal2D[j]->RebinY(5);
    for(int i=0;i<N;i++) {
      char hisname[100];
      sprintf(hisname,"Tray_%d_A_%d",i+1,j);
      zLocal[i][j] = zLocal2D[j]->ProjectionY(hisname,i+1,i+1);
      zLocal[i][j]->Sumw2();
    }
  }

  TF1 *fitfun = new TF1("fitfun",fitfun,-6.,6.,5);

  double t[N];
  double z0[N][M], ze[N][M];

  gStyle->SetOptStat(101);
  gStyle->SetOptFit(100);
  gStyle->SetTextSize(0.06);
  gStyle->SetTextFont(42);
  TCanvas *c1 = new TCanvas("c1","c1",0,0,800,600);
  c1->SetFillColor(10);
  c1->SetBorderMode(0);
  c1->SetBorderSize(2);
  c1->Divide(4,3);

  TPostScript *ps = new TPostScript("fig/xLocalFit.ps",112);

  for(int j=0;j<M;j++) {
    for(int i=0;i<N;i++) {
      t[i] = i+1;  // tray number

      if(i%12==0) ps->NewPage();   // 12 trays per page
      c1->cd(i%12+1);

      double par[5], err[5];
      fitfun->SetParameters(0., zLocal[i][j]->GetBinContent(50), 0.1, 1., 0.);
      fitfun->SetLineWidth(2);
      zLocal[i][j]->Draw("e");
      double entries = zLocal[i][j]->Integral(1,100);
      if(entries>10) {
	fitfun->SetParLimits(0, -3., 3.);
	fitfun->SetParLimits(2, 2.9, 3.1);
	fitfun->SetParLimits(3, 0.01, 0.16);
	zLocal[i][j]->Fit("fitfun","R");
	fitfun->GetParameters(&par[0]);
	err[0] = fitfun->GetParError(0);
      } else {
	par[0] = 0.;
	err[0] = 0.;
      }

      hPar2->Fill(par[2]);
      hPar3->Fill(par[3]);
      
      char text[100];
      sprintf(text, "Z0 = %5.3f #pm %5.3f", par[0], err[0]);
      
      TLatex *tex = new TLatex(-3, par[1]/3., text);
      tex->SetTextSize(0.07);
      tex->SetTextFont(12);
      tex->Draw("same");
      
      z0[i][j] = par[0];
      ze[i][j] = err[0];
      
      c1->Update();
    }
  }

  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);
  gStyle->SetEndErrorSize(0.01);
 
  TF1 *fitangle = new TF1("fitangle","[0]*sin(x*3.14159/180.)+[1]",-10.,40.);
  
  double z00[N], ze0[N];  // angle=0 modules zOffset
  double z0F[N], zeF[N];  // angle=0 modules zOffset after fitangle fit
  double x0[N], xe[N];
  for(int i=0;i<N;i++) {

    if(i%12==0) ps->NewPage();   // 12 trays per page
    c1->cd(i%12+1);

    z00[i] = z0[i][0];
    ze0[i] = ze[i][0];

    double x1 = -10.;
    double x2 = 50.;
    double y1 = -2.5;
    double y2 = 1.5;
    char title[100];
    sprintf(title,"Tray_%d",i+1);
    TH1D *h0 = new TH1D("h0",title,1, x1, x2);
    h0->SetMinimum(y1);
    h0->SetMaximum(y2);
    h0->GetXaxis()->SetNdivisions(208);
    h0->GetXaxis()->CenterTitle();
    h0->GetXaxis()->SetTitle("Module Angle (deg)");
    h0->GetXaxis()->SetTitleOffset(1.0);
    h0->GetXaxis()->SetTitleSize(0.07);
    h0->GetXaxis()->SetLabelOffset(0.01);
    h0->GetXaxis()->SetLabelSize(0.045);
    h0->GetXaxis()->SetLabelFont(42);
    h0->GetXaxis()->SetTitleFont(42);
    h0->GetYaxis()->SetNdivisions(210);
    h0->GetYaxis()->SetTitle("zLocal Offset");
    h0->GetYaxis()->SetTitleOffset(1.0);
    h0->GetYaxis()->SetTitleSize(0.07);
    h0->GetYaxis()->SetLabelOffset(0.01);
    h0->GetYaxis()->SetLabelSize(0.045);
    h0->GetYaxis()->SetLabelFont(42);
    h0->GetYaxis()->SetTitleFont(42);
    h0->Draw();
    
    TGraphErrors *gr = new TGraphErrors(M, a, z0[i], 0, ze[i]);
    gr->SetMarkerStyle(24);
    gr->SetMarkerSize(1.5);
    gr->SetLineWidth(2);
    gr->Draw("p");
    
    fitangle->SetParameters(-0.5, 0.0);
    gr->Fit("fitangle","R");

    x0[i] = fitangle->GetParameter(0);
    z0F[i] = fitangle->GetParameter(1);
    xe[i] = fitangle->GetParError(0);
    zeF[i] = fitangle->GetParError(1);

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
  double y1 = -3.;
  double y2 = 1.;
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
   h0->GetYaxis()->SetTitle("zLocal Offset");
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

   TGraphErrors *gr = new TGraphErrors(N, t, z00, 0, ze0);
   gr->SetMarkerStyle(20);
   gr->SetMarkerSize(1.5);
   gr->SetLineWidth(2);
   gr->Draw("p");

   TGraphErrors *gr = new TGraphErrors(N, t, z0F, 0, zeF);
   gr->SetMarkerStyle(24);
   gr->SetMarkerSize(1.5);
   gr->SetLineWidth(2);
   gr->Draw("p");

   c1->Update();
  
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
  double y2 = 3.;
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
   h0->GetYaxis()->SetTitle("xLocal Offset");
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

   TGraphErrors *gr = new TGraphErrors(N, t, x0, 0, xe);
   gr->SetMarkerStyle(20);
   gr->SetMarkerSize(1.5);
   gr->SetLineWidth(2);
   gr->Draw("p");

   c1->Update();

  ps->Close();
    
  ofstream outData;
  outData.open("xOffset.dat");
  for(int i=0;i<N;i++) {
    outData << setw(15) << z00[i] << setw(15) << ze0[i] << setw(15) << x0[i] << setw(15) << xe[i] << endl;
  }
  outData.close();

  TFile  *fout = new TFile("xFitPar.root","recreate");
  hPar2->Write();
  hPar3->Write();
  fout->Close();
}
