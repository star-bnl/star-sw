#include <string>

void photosPlots(std::string fileName = "Upsilon4S_PHOTOS.root") {

  TFile* theFile = new TFile(fileName.c_str(), "read");
  TTree* theTree = dynamic_cast<TTree*>(theFile->Get("Data"));
  TTree* nDaugTree = dynamic_cast<TTree*>(theFile->Get("nDaugTree"));

  TH1F* eMtmHist = new TH1F("eMtmHist", "", 100, 0.0, 5.5);
  eMtmHist->SetXTitle("Electron momentum (GeV/c)");
  eMtmHist->SetYTitle("Frequency/55 (MeV/c)");
  eMtmHist->SetTitleOffset(1.1, "Y");
  TAxis* exAxis = eMtmHist->GetXaxis();
  exAxis->SetLabelSize(0.045);
  exAxis->SetTitleSize(0.045);
  TAxis* eyAxis = eMtmHist->GetYaxis();
  eyAxis->SetLabelSize(0.045);
  eyAxis->SetTitleSize(0.045);
  
  TH1F* pMtmHist = new TH1F("pMtmHist", "", 100, 0.0, 5.5);
  pMtmHist->SetXTitle("Positron momentum (GeV/c)");
  pMtmHist->SetYTitle("Frequency/55 (MeV/c)");
  pMtmHist->SetTitleOffset(1.1, "Y");
  TAxis* pxAxis = pMtmHist->GetXaxis();
  pxAxis->SetLabelSize(0.045);
  pxAxis->SetTitleSize(0.045);
  TAxis* pyAxis = pMtmHist->GetYaxis();
  pyAxis->SetLabelSize(0.045);
  pyAxis->SetTitleSize(0.045);

  TH1F* gMtmHist = new TH1F("gMtmHist", "", 100, 0.0, 5.5);
  gMtmHist->SetXTitle("Photon momentum (GeV/c)");
  gMtmHist->SetYTitle("Frequency/55 (MeV/c)");
  gMtmHist->SetTitleOffset(1.1, "Y");
  TAxis* gxAxis = gMtmHist->GetXaxis();
  gxAxis->SetLabelSize(0.045);
  gxAxis->SetTitleSize(0.045);
  TAxis* gyAxis = gMtmHist->GetYaxis();
  gyAxis->SetLabelSize(0.045);
  gyAxis->SetTitleSize(0.045);

  TH1F* nDaugHist = new TH1F("nDaugHist", "", 10, 0, 10);
  nDaugHist->SetXTitle("Number of daughters");
  nDaugHist->SetYTitle("Frequency");
  nDaugHist->SetTitleOffset(1.15, "Y");
  TAxis* nxAxis = nDaugHist->GetXaxis();
  nxAxis->SetLabelSize(0.045);
  nxAxis->SetTitleSize(0.045);
  TAxis* nyAxis = nDaugHist->GetYaxis();
  nyAxis->SetLabelSize(0.045);
  nyAxis->SetTitleSize(0.045);

  theTree->Draw("p>>eMtmHist", "id==11");
  theTree->Draw("p>>pMtmHist", "id==-11");
  theTree->Draw("p>>gMtmHist", "id==22"); 
  nDaugTree->Draw("nDaug>>nDaugHist");

  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  TCanvas* theCanvas = new TCanvas("theCanvas", "", 900, 700);
  theCanvas->UseCurrentStyle();

  theCanvas->Divide(2,2);

  theCanvas->cd(1);
  gPad->SetLogy();
  double scale = 1.0/eMtmHist->Integral(); // same normalisation number for all plots

  eMtmHist->Scale(scale);
  eMtmHist->SetMaximum(1.0);
  eMtmHist->Draw();

  theCanvas->cd(2);
  gPad->SetLogy();
  pMtmHist->Scale(scale);
  pMtmHist->SetMaximum(1.0);
  pMtmHist->Draw();

  theCanvas->cd(3);
  gPad->SetLogy();
  gMtmHist->Scale(scale);
  gMtmHist->SetMaximum(1.0);
  gMtmHist->Draw();

  theCanvas->cd(4);
  gPad->SetLogy(0);
  nDaugHist->Scale(scale);
  nDaugHist->SetMaximum(0.5);
  nDaugHist->Draw();

  theCanvas->cd(1);
  TLatex latex;
  latex.SetNDC();
  latex.SetTextSize(0.045);
  latex.DrawLatex(0.1, 0.95, "#Upsilon(4S) #rightarrow e^{-} e^{+} decay with PHOTOS");

  theCanvas->Print("photosPlots.png");

}
