/* 
 * mip_histogram_fitter.C
 * Original Author: Matt Walker
 * Update Author: J. Kevin Adkins, University of Kentucky
 * Updates: Remove old and obsolete functions and code, 
 * make code more modular so it's easier to make changes
 * to fits independent of drawing tower fits, etc.
 * Fitting algorithm completely overhauled to now calculate
 * the mean of the fit over the range [0,100]. This is the value
 * we calculate the MIP relative gains with. Also, now using fit
 * to calculate RMS, and subsequently the error on the mean
 * Most recent update: Feb. 25, 2015
 */

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

void drawTower(TH1D*, Int_t, Int_t, Float_t, Float_t);
Bool_t isBadTower(Int_t);

void mip_histogram_fitter(const Char_t* mipRootfile = "combinedMips.root", const Char_t* psName = "mip.ps", const Char_t* rootFilename = "towerMipFits.root", const Char_t* mipOutputName = "mip.gains")
{
  // Load StEmcGeom for tower eta & phi
  gROOT->Macro("loadMuDst.C");
  StEmcGeom *mEmcGeom = StEmcGeom::instance("bemc");

  const Int_t nTowers = 4800;
  Int_t towerStatus[nTowers];
  Double_t fitMean[nTowers], fitMeanError[nTowers];

  cout << "Input Filename: " << mipRootfile << endl;
  cout << "Plot Filename: " << psName << endl;
  cout << "Gain Filename: " << mipOutputName << endl;

  gStyle->SetCanvasColor(10);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetStatColor(10);
  gStyle->SetOptTitle(0);
  gStyle->SetOptDate(0);
  gStyle->SetOptFit(111);
  gStyle->SetOptStat("e");
  gStyle->SetPalette(1);

  /* Declare pointers for histograms before output file. Instantiate after, 
     this allows us to use outfile->Write() to write everything at once */
  TH1D *towerHisto[nTowers];
  TH2F *etaPhiMean, *etaPhiStatus, *etaPhiRatio;

  TFile* outfile = new TFile(rootFilename,"RECREATE");
  Float_t pi = TMath::Pi();

  // Instantiate histograms
  etaPhiMean = new TH2F("etaPhiMean","Fit Means", 40, -1., 1., 120, -pi, pi);
  etaPhiStatus = new TH2F("etaPhiStatus","Status Codes", 40, -1., 1., 120, -pi, pi);
  etaPhiRatio = new TH2F("etaPhiMeanRatio","Mean Ratios", 40, -1., 1., 120, -pi, pi);
  for (Int_t i = 0; i < nTowers; ++i)
    towerHisto[i] = new TH1D(Form("towerHisto_%i",i),Form("Tower %i",i),250,-50.5,199.5);

  TFile *rootfile = new TFile(mipRootfile);
  for(Int_t y = 0; y < nTowers; ++y)
    towerHisto[y]->Add((TH1D*)rootfile->Get(Form("tower_histo_%i",y+1)));

  rootfile->Close();
  if(rootfile) delete rootfile;

  /****************** Begin MIP Fitting for each Tower ******************/
  TF1 *towerFit[nTowers];
  Char_t fitName[100];

  for(Int_t iTow = 0; iTow < nTowers; ++iTow){		
    Int_t softId = iTow+1;
    Float_t mTowerEta = 0., mTowerPhi = 0.;
    mEmcGeom->getEta(softId,mTowerEta);
    mEmcGeom->getPhi(softId,mTowerPhi);

    towerStatus[iTow] = 0;
    fitMean[iTow] = 0.;
    fitMeanError[iTow] = 0.;
    towerHisto[iTow]->GetXaxis()->SetRangeUser(6.,100.);
		
    if(softId%200 == 0) cout << "Now fitting softId: " << softId << endl;

    sprintf(fitName,"towerFit_%i",softId);    
    Double_t fitLow = towerHisto[iTow]->GetMean() - towerHisto[iTow]->GetRMS();
    Double_t fitHigh = towerHisto[iTow]->GetMean() + towerHisto[iTow]->GetRMS()/2.;
    towerFit[iTow] = new TF1(fitName,"[0]*Gaus(x,[1],[2])*Landau(x,[3],[4])",fitLow,fitHigh);

    // Fit Parameters
    towerFit[iTow]->SetParameter(0,towerHisto[iTow]->GetBinContent(towerHisto[iTow]->GetMaximumBin()));
    towerFit[iTow]->SetParameter(1,towerHisto[iTow]->GetMean());
    towerFit[iTow]->SetParameter(2,towerHisto[iTow]->GetRMS());
    towerFit[iTow]->SetParameter(3,towerHisto[iTow]->GetBinCenter(towerHisto[iTow]->GetMaximumBin()));
    towerFit[iTow]->SetParameter(4,towerHisto[iTow]->GetRMS());
    towerFit[iTow]->SetLineColor(kBlue);
    towerFit[iTow]->SetLineWidth(1.5);
    
    // If entries less than 25 we can't fit the tower, so we skip it
    if(towerHisto[iTow]->Integral(56,156) < 25){
      towerStatus[iTow] = 13;
      etaPhiStatus->Fill(mTowerEta,mTowerPhi,towerStatus[iTow]);
      continue;
    }
    
    towerHisto[iTow]->Fit(towerFit[iTow],"RQ");
    towerStatus[iTow]+=1;
    fitMean[iTow] = towerFit[iTow]->Mean(0.,100.);

    // Get the fit parameters in an array
    Double_t fitParams[5] = {0.,0.,0.,0.,0.};
    fitParams[0] = towerFit[iTow]->GetParameter(0);
    fitParams[1] = towerFit[iTow]->GetParameter(1);
    fitParams[2] = towerFit[iTow]->GetParameter(2);
    fitParams[3] = towerFit[iTow]->GetParameter(3);
    fitParams[4] = towerFit[iTow]->GetParameter(4);

    //Calculate the variance of the fit
    Double_t fitVariance = towerFit[iTow]->Variance(0., 100., fitParams);
    Double_t fitSigma = sqrt(fitVariance);
    Double_t entriesInFit = towerHisto[iTow]->Integral(51,151); // Bin 51 centered over zero

    //Set the status codes of the towers
    if(fitMean[iTow] < 6) towerStatus[iTow] += 10; //Bad mean
    if((softId)%20==0){
      if (abs(fitMean[iTow]-towerHisto[iTow]->GetMean()) > 15)
	towerStatus[iTow]+=8;
    }
    else{
      if(abs(fitMean[iTow]-towerHisto[iTow]->GetMean()) > 10)
	towerStatus[iTow]+=8;
    }

    //Check for hard coded bad towers
    if(isBadTower(softId))
      towerStatus[iTow] = 7; 
      
    //Calculate error on mean for good statuses
    if (towerStatus[iTow] == 1)
      fitMeanError[iTow] = fitSigma/sqrt(entriesInFit);

    // Fill eta/phi for good statuses
    if (towerStatus[iTow] == 1){
      etaPhiMean->Fill(mTowerEta,mTowerPhi,fitMean[iTow]);
      etaPhiRatio->Fill(mTowerEta,mTowerPhi,fitMean[iTow]/towerHisto[iTow]->GetMean());
    }
    etaPhiStatus->Fill(mTowerEta,mTowerPhi,towerStatus[iTow]);
  }// Towers loop
  /******************** End MIP Fitting for each Tower ********************/

  /******************* Begin Drawing the Fitted Spectra *******************/
  TPostScript *ps = new TPostScript(psName);
  TCanvas *canvas = new TCanvas("can","can1",100,100,600.,800.);
  Int_t pad;

  cout << endl << "Begin Tower Drawing Loop" << endl;
  for (Int_t iTow = 0; iTow < nTowers; ++iTow){
    Int_t softId = iTow + 1;
    Float_t mTowerEta = 0., mTowerPhi = 0.;
    mEmcGeom->getEta(softId,mTowerEta);
    mEmcGeom->getPhi(softId,mTowerPhi);

    if (softId%400 == 0) cout << "Drawing tower " << softId << endl;
    if(iTow%20 == 0){
      canvas->Update();
      ps->NewPage();
      canvas->Clear();
      canvas->Divide(4,5);
      pad = 1;
    }

    canvas->cd(pad);
    if(towerStatus[iTow] > 1)
      towerHisto[iTow]->SetLineColor(kRed);

    // Draw the tower onto the pad
    drawTower(towerHisto[iTow],softId,towerStatus[iTow],mTowerEta,mTowerPhi);

    Double_t lineMax = towerHisto[iTow]->GetBinContent(towerHisto[iTow]->GetMaximumBin()) + 30;
    if(towerStatus[iTow] == 1){
      TLine *avgValueLine = new TLine(fitMean[iTow],0.,fitMean[iTow],lineMax);
      avgValueLine->SetLineColor(kBlue);
      avgValueLine->SetLineWidth(1.25);
      avgValueLine->Draw("same");
    }
    pad++;
  }//End tower draw loop

  canvas->Update();
  ps->NewPage();
  canvas->Clear();
  canvas->Divide(1,2);
  canvas->cd(1);
  etaPhiMean->GetXaxis()->SetTitle("#eta"); 
  etaPhiMean->GetXaxis()->CenterTitle();
  etaPhiMean->GetYaxis()->SetTitle("#phi");
  etaPhiMean->GetYaxis()->CenterTitle();
  etaPhiMean->Draw("colz");
  canvas->cd(2);
  etaPhiStatus->GetZaxis()->SetRangeUser(0,20);
  etaPhiStatus->GetXaxis()->SetTitle("#eta");
  etaPhiStatus->GetXaxis()->CenterTitle();
  etaPhiStatus->GetYaxis()->SetTitle("#phi");
  etaPhiStatus->GetYaxis()->CenterTitle();
  etaPhiStatus->Draw("colz");

  canvas->Update();
  ps->NewPage();
  canvas->Clear();
  canvas->Divide(1,2);
  canvas->cd(1);
  etaPhiRatio->GetXaxis()->SetTitle("#eta");
  etaPhiRatio->GetXaxis()->CenterTitle();
  etaPhiRatio->GetYaxis()->SetTitle("#phi");
  etaPhiRatio->GetYaxis()->CenterTitle();
  etaPhiRatio->Draw("colz");
  canvas->cd(2);
  TH2F *ratioClone = (TH2F*)etaPhiRatio->Clone();
  ratioClone->GetZaxis()->SetRangeUser(0.7,1.);
  ratioClone->GetXaxis()->SetTitle("#eta");
  ratioClone->GetXaxis()->CenterTitle();
  ratioClone->GetYaxis()->SetTitle("#phi");
  ratioClone->GetYaxis()->CenterTitle();
  ratioClone->Draw("colz");
  ps->Close();
  
 
  cout << "Tower Drawing Loop Complete" << endl;
  /******************** End Drawing the Fitted Spectra ********************/

  /******************** Begin Output of MIP Fit Data ********************/
  ofstream mipOutput(mipOutputName);

  Int_t nGood = 0;
  Int_t nZero = 0;

  for(Int_t iTow = 0; iTow < nTowers; ++iTow){
    Int_t softId = iTow + +1;
    Double_t mipAdc = fitMean[iTow];
    Double_t mipError = fitMeanError[iTow];
    mipOutput << softId << " " << mipAdc << " " << mipError << " " << towerStatus[iTow] << endl;

    if(towerStatus[iTow]==1)nGood++;
    if(towerStatus[iTow]==0)nZero++;
  }
  mipOutput.close();
  /********************* End Output of MIP Fit Data *********************/

  outfile->Write();
  outfile->Close();

  cout << "Finished tower fits" << endl << endl;
  cout << "Number of Good Statuses = " << nGood << endl;
  cout << "Number of Bad Statuses = " << 4800-nGood << endl;
  cout << "Number of Empty Towers = " << nZero << endl;

  // Delete geometry pointer
  if (mEmcGeom) delete mEmcGeom;
} // End of main routine
			
void drawTower(TH1D* histo, Int_t id, Int_t status, Float_t towerEta, Float_t towerPhi)
{
  // Set histogram options
  histo->GetXaxis()->SetTitle("ADC");
  histo->GetXaxis()->CenterTitle();
  histo->Draw(); // Used Sumw2() in histogram maker to calculate errors
 
  // Write softId and status code on plot
  Char_t towerTitle[100];
  Char_t etaTitle[100];
  Char_t phiTitle[100];
  sprintf(etaTitle,"e:%1.2f",towerEta);
  sprintf(phiTitle,"p:%1.2f",towerPhi);
  TLatex etaLatex;
  TLatex phiLatex;
  etaLatex.SetTextSize(0.1);
  phiLatex.SetTextSize(0.1);
  sprintf(towerTitle,"%i",id);
  TLatex titleLatex;
  titleLatex.SetTextSize(0.15);
  if(status!=1) titleLatex.SetTextColor(kRed);
  phiLatex.DrawTextNDC(0.55,0.2,phiTitle);
  etaLatex.DrawTextNDC(0.55,0.33,etaTitle);
  titleLatex.DrawTextNDC(0.55,0.45,towerTitle);
  if(status!=1){
    Char_t towerCode[100];
    sprintf(towerCode,"%i",status);
    TLatex statusLatex;
    statusLatex.SetTextSize(0.15);
    statusLatex.SetTextColor(kRed);
    statusLatex.DrawTextNDC(0.47,0.78,towerCode);
  }
}

Bool_t isBadTower(Int_t id)
{
  // First pass
  if (id == 34   || id == 106  || id == 266  || id == 267  || id == 282  || id == 286  || id == 287  || id == 410  || id == 504  || id == 533  ||
      id == 561  || id == 615  || id == 616  || id == 633  || id == 637  || id == 638  || id == 650  || id == 653  || id == 657  || id == 673  ||
      id == 789  || id == 806  || id == 809  || id == 812  || id == 813  || id == 814  || id == 821  || id == 822  || id == 823  || id == 824  ||
      id == 829  || id == 830  || id == 831  || id == 832  || id == 837  || id == 841  || id == 842  || id == 843  || id == 844  || id == 849  || 
      id == 850  || id == 851  || id == 852  || id == 857  || id == 875  || id == 899  || id == 939  || id == 953  || id == 954  || id == 993  || 
      id == 1026 || id == 1046 || id == 1048 || id == 1080 || id == 1100 || id == 1199 || id == 1200 || id == 1207 || id == 1218 || id == 1219 || 
      id == 1222 || id == 1223 || id == 1224 || id == 1198 || id == 1237 || id == 1241 || id == 1242 || id == 1243 || id == 1244 || id == 1257 || 
      id == 1260 || id == 1312 || id == 1348 || id == 1353 || id == 1354 || id == 1388 || id == 1407 || id == 1409 || id == 1434 || id == 1448 || 
      id == 1574 || id == 1597 || id == 1612 || id == 1654 || id == 1713 || id == 1765 || id == 1766 || id == 1877 || id == 1878 || id == 2073 || 
      id == 2077 || id == 2092 || id == 2093 || id == 2097 || id == 2409 || id == 2589 || id == 2590 || id == 2969 || id == 3070 || id == 3071 || 
      id == 3494 || id == 3495 || id == 3588 || id == 3668 || id == 3678 || id == 3679 || id == 4018 || id == 4019 || id == 4059 || id == 4331 || 
      id == 4357 || id == 4500 || id == 4677 || id == 4678 || id == 4684)
    return true;

  return false;
}
