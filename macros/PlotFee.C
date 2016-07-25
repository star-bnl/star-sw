void PlotFee()
{
  gROOT->Reset();
  gStyle->SetOptStat(0);
  gStyle->SetPalette(1);
  //  gStyle->SetCanvasColor(33);
  TFile *fee_dEdxCal_0_6_0_0 = TFile::Open("fee_dEdxCal_0_6_0_0.root");// no correction
  TH2* mean0 = (TH2 *)  fee_dEdxCal_0_6_0_0->Get("mean");
  mean0->SetXTitle("Sector no.");
  mean0->SetYTitle("Half Fee no. (Fee +0/1 for even/odd padrow");
  mean0->SetTitle("Before Pulser Correction");
  mean0->SetStats(0);
  mean0->SetMaximum(0.5);
  mean0->SetMinimum(-.5);
  TFile *fee_dEdxCal_6_2_0_0 = TFile::Open("fee_dEdxCal_6_2_0_0.root");// after pulser correction
  TH2* mean6 = (TH2 *)  fee_dEdxCal_6_2_0_0->Get("mean");
  mean6->SetXTitle("Sector no.");
  mean6->SetYTitle("Half Fee no. (Fee +0/1 for even/odd padrow");
  mean6->SetTitle("After Pulser Correction");
  mean6->SetStats(0);
  mean6->SetMinimum(0.5);
  mean6->SetMinimum(-.5);

  c1 = new TCanvas("Fee",
                   "Comparison average ln(dE/dx/I_{#pion}) before and after Pulser correctron");
  //               10,10,800,600););
  c1->Divide(2,1);
  c1->cd(1);
  mean0->Draw("colz");
  c1->cd(2);
  mean6->Draw("colz");
}
void PlotFee1D()
{
  //  gROOT->Reset();
  //  gStyle->SetOptStat(0);
  //  gStyle->SetPalette(1);
  //  gStyle->SetCanvasColor(33);
  TFile *fee_dEdxCal_0_6_0_0 = TFile::Open("fee_dEdxCal_0_6_0_0.root");// no correction
  TNtuple* FitP0 = (TNtuple *)  fee_dEdxCal_0_6_0_0->Get("FitP");
  TFile *fee_dEdxCal_6_2_0_0 = TFile::Open("fee_dEdxCal_6_2_0_0.root");// after pulser correction
  TNtuple* FitP6 = (TNtuple *)  fee_dEdxCal_6_2_0_0->Get("FitP");
  c1 = new TCanvas("Fee1D",
                   "Comparison average ln(dE/dx/I_{#pion}) before and after Pulser correctron");
  //               10,10,800,600););
  c1->Divide(2,1);
  c1->cd(1);
  TH1D *h6 = new TH1D("h6","mean corrected",50,-0.5,0.5);
  TH1D *h0 = new TH1D("h0","mean uncorrected",50,-0.5,0.5);
  FitP6->Draw("mean>>h6","rms>0");
  FitP0->Draw("mean>>h0","rms>0","same");
  h0->SetXTitle("<ln((dE/dx)_{meas}/dE/dx_{#pion}> over Half Fee");
  h6->SetXTitle("<ln((dE/dx)_{meas}/dE/dx_{#pion}> over Half Fee");
  h6->SetLineColor(2);
  h6->SetLineWidth(4);
  //  h6->SetNormFactor(1);
  h6->SetStats(0);
  h6->Draw();
  h0->SetLineColor(3); 
  h0->SetLineWidth(4);
  //  h0->SetNormFactor(1);
  h0->Draw("same");
  //______________
  c1->cd(2);
  TH1D *h6O = new TH1D("h6O","rms Outer corrected",50,0.2,1.0);
  TH1D *h6I = new TH1D("h6I","rms Inner corrected",50,0.2,1.0);
  TH1D *h0O = new TH1D("h0O","rms Outer uncorrected",50,0.2,1.0);
  TH1D *h0I = new TH1D("h0I","rms Inner uncorrected",50,0.2,1.0);
  FitP6->Draw("rms>>h6O","rms>0&&j<=250","none");
  FitP6->Draw("rms>>h6I","rms>0&&j>250","none");
  FitP0->Draw("rms>>h0O","rms>0&&j<=250");
  FitP0->Draw("rms>>h0I","rms>0&&j>250");
  h6O->SetXTitle("RMS<ln((dE/dx)_{meas}/dE/dx_{#pion}> over Half Fee");
  h6I->SetXTitle("RMS<ln((dE/dx)_{meas}/dE/dx_{#pion}> over Half Fee");
  h6O->SetLineColor(2);
  h6I->SetLineColor(2);
  h6O->SetLineWidth(4);
  //  h6I->SetNormFactor(1);
  //  h6O->SetNormFactor(1);
  h6I->SetStats(0);
  h6O->SetStats(0);
  h6O->Draw();
  h6I->Draw("same");
  h0O->SetXTitle("RMS<ln((dE/dx)_{meas}/dE/dx_{#pion}> over Half Fee");
  h0I->SetXTitle("RMS<ln((dE/dx)_{meas}/dE/dx_{#pion}> over Half Fee");
  h0O->SetLineColor(3);
  h0I->SetLineColor(3);
  h0O->SetLineWidth(4);
  //  h0I->SetNormFactor(1);
  //  h0O->SetNormFactor(1);
  h0I->SetStats(0);
  h0O->SetStats(0);
  h0O->Draw("same");
  h0I->Draw("same");
}
