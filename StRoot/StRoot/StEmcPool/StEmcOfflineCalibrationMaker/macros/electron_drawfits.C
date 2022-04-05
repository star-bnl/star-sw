/*
 * electron_drawfits.C
 * Original author: Matt Walker
 * Update Author: J. Kevin Adkins, University of Kentucky
 * Updates: Removed a lot of code. Much of it was commented
 * out by Alice Ohlson when she made previous changes to the
 * algorithm. Also, removed a lot of code which was unnecessary,
 * and updated naming conventions for clearer reading.
 *
 * Most recent update: Feb 25, 2015
 */

#include <iostream>
#include <fstream>
using namespace std;

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"
#include "TMath.h"
#include "TPostScript.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TLatex.h"
#include "TTree.h"
#include "TLine.h"

void electron_drawfits(const char* infile="CombinedElectrons.root", const char* mipGainFilename="mip.gains", const char* absoluteGainFilename="electron.gains", 
		       const char* psName="electron.ps", const char* ffile="electron.fits")
{
  cout << "Input File: " << infile << endl;
  cout << "Plots File: " << psName << endl;
  
  // Load library and set up decoder & geometry
  gROOT->Macro("loadMuDst.C");
  StEmcDecoder *mDecoder = new StEmcDecoder();
  StEmcGeom *mEmcGeom = StEmcGeom::instance("bemc");

  //**********************************************//
  //Initialization                                //
  //**********************************************//
  const Int_t nTowers = 4800;
  const Int_t nRings = 40;
  const Int_t nCrates = 30;
  const Int_t nCrateSlices = nCrates*20;
  const Double_t pi = TMath::Pi();
  Double_t mipGains[nTowers];
  Int_t mipTowerStatus[nTowers];
  Double_t mipAdc[nTowers];
  Double_t mipAdcErr[nTowers];
  Double_t ringPars[5], crateslicePars[5];

  ifstream mipGainFile(mipGainFilename);
  while(1){
    Int_t softId,towerStat;
    Double_t mipAdcVal , mipAdcErrVal, mipGain;
    mipGainFile >> softId >> mipAdcVal >> mipAdcErrVal >> towerStat;
    if(!mipGainFile.good())break;
    Float_t towerEta, towerTheta;
    mEmcGeom->getEta(softId,towerEta);
    mEmcGeom->getTheta(softId,towerTheta);
    mipGain = 0.264*(1+0.056*towerEta*towerEta)/(sin(towerTheta)*mipAdcVal);
    mipAdc[softId-1] = mipAdcVal;
    mipGains[softId-1] = mipGain;
    mipTowerStatus[softId-1] = towerStat;
    mipAdcErr[softId-1] = mipAdcErrVal;
  }
  mipGainFile.close();

  TPostScript *ps = new TPostScript(psName);

  TFile *rootfile = new TFile(infile,"read");

  TH1 *crateslice_histo[nCrates][nRings/2];
  TH1 *ring_histo[nRings];

  TF1 *ringFit[nRings], *ringExpoFit[nRings], *ringGausFit[nRings];
  TF1 *cratesliceFit[nCrates][nRings/2], *cratesliceExpoFit[nCrates][nRings/2], *cratesliceGausFit[nCrates][nRings/2];

  Int_t ringIndex, sliceEtaIndex;

  // compare fit parameters
  TH2F *hChi2EtaPhi = new TH2F("hChi2EtaPhi","Chi2/NDF",40,-1,1,120,-pi,pi);
  TH2F *hNewGains = new TH2F("hNewGains","New Gains",40,-1.,1.,120,-pi,pi); 

  char name[100], title[100];
  for(Int_t iRing = 0; iRing < nRings; ++iRing){
    Int_t ringId = iRing + 1;
    sprintf(name,"ringHisto_%i",iRing+1);
    ring_histo[iRing] = (TH1F*)rootfile->Get(name);
    ring_histo[iRing]->SetTitle("");
  }
  for(Int_t iCrate = 0; iCrate < nCrates; ++iCrate){
    Int_t crateId = iCrate + 1;
    for(Int_t iRing = 0; iRing < nRings/2; ++iRing){
      Int_t ringId = iRing + 1;
      sprintf(name,"cratesliceHisto_%i_%i",crateId,ringId);
      crateslice_histo[iCrate][iRing] = (TH1F*)rootfile->Get(name);
      crateslice_histo[iCrate][iRing]->SetTitle("");
    }    
  }

  //global graphics functions
  gStyle->SetOptFit(111);
  gStyle->SetCanvasColor(10);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetPalette(1);
  gStyle->SetStatColor(0);
  gStyle->SetOptDate(0);

  //**********************************************//
  //Fit Ring Histograms                           //
  //**********************************************//

  TCanvas *c = new TCanvas("c","",100,100,600.,800.);
  ofstream fitfile(ffile);
  for(Int_t iRing = 0; iRing < nRings; iRing++){
    Int_t ringId = iRing + 1;
    if(iRing%2==0){
      c->Update();
      ps->NewPage();
      c->Clear();
      c->Divide(1,2);
    }
    c->cd((iRing%2)+1);

    sprintf(name,"ringExpoFit_%i",ringId);
    ringExpoFit[iRing] = new TF1(name,"expo",0.25,1.7);
    sprintf(name,"ringGausFit_%i",ringId);
    ringGausFit[iRing] = new TF1(name,"gaus",0.65,1.15);
    sprintf(name,"ringFit_%i",ringId);
    ringFit[iRing] = new TF1(name,"expo(0)+gaus(2)",0.25,1.7);

    ring_histo[iRing]->Fit(ringExpoFit[iRing],"RQ0");
    ring_histo[iRing]->Fit(ringGausFit[iRing],"RQ0");
    ringExpoFit[iRing]->GetParameters(&ringPars[0]);
    ringGausFit[iRing]->GetParameters(&ringPars[2]);
    ringFit[iRing]->SetParameters(ringPars);
    ringFit[iRing]->SetParNames("ExpConst","ExpSlope","GausConst","GausMean","GausSigma");  
    ring_histo[iRing]->Fit(ringFit[iRing],"RQ0");

    ring_histo[iRing]->DrawCopy();
    ringFit[iRing]->DrawCopy("same");

    Double_t ringMean = ringFit[iRing]->GetParameter(3);
    Double_t ringErr = ringFit[iRing]->GetParError(3);

    fitfile << "Ring " << ringId << " " << ringMean << " " << ringErr << endl;
    cout << "Ring " << ringId << " Fit: " << ringMean << " " << ringErr/ringMean << " " << ring_histo[iRing]->GetEntries() << endl;
  }
  
  //**********************************************//
  //Fit Crate Slice Histograms                    //
  //**********************************************//
  for(Int_t iCrate = 0; iCrate < nCrates; iCrate++){
    Int_t crateId = iCrate + 1;
    for(Int_t iRing = 0; iRing < nRings/2; iRing++){
      Int_t ringId = iRing + 1;
      if(iRing%10 == 0){
	c->Update();
	ps->NewPage();
	c->Clear();
	c->Divide(2,5);
      }
      c->cd((iRing%10)+1);

      sprintf(name,"cratesliceExpoFit_%i_%i",crateId,ringId);
      cratesliceExpoFit[iCrate][iRing] = new TF1(name,"expo",0.25,1.7);
      sprintf(name,"cratesliceGausFit_%i_%i",crateId,ringId);
      cratesliceGausFit[iCrate][iRing] = new TF1(name,"gaus",0.65,1.15);
      sprintf(name,"cratesliceFit_%i_%i",crateId,ringId);
      cratesliceFit[iCrate][iRing] = new TF1(name,"expo(0)+gaus(2)",0.25,1.7);
      
      crateslice_histo[iCrate][iRing]->Fit(cratesliceExpoFit[iCrate][iRing],"RQ0");
      crateslice_histo[iCrate][iRing]->Fit(cratesliceGausFit[iCrate][iRing],"RQ0");
      cratesliceExpoFit[iCrate][iRing]->GetParameters(&crateslicePars[0]);
      cratesliceGausFit[iCrate][iRing]->GetParameters(&crateslicePars[2]);
      cratesliceFit[iCrate][iRing]->SetParameters(crateslicePars);
      cratesliceFit[iCrate][iRing]->SetParNames("ExpConst","ExpSlope","GausConst","GausMean","GausSigma");  
      crateslice_histo[iCrate][iRing]->Fit(cratesliceFit[iCrate][iRing],"RQ0");
      
      crateslice_histo[iCrate][iRing]->DrawCopy();
      cratesliceFit[iCrate][iRing]->DrawCopy("same");
      
      Double_t cratesliceMean = cratesliceFit[iCrate][iRing]->GetParameter(3);
      Double_t cratesliceErr = cratesliceFit[iCrate][iRing]->GetParError(3);

      fitfile << "CrateSlice " << iCrate*20+iRing+1 << " " << cratesliceMean << " " << cratesliceErr << endl;
      cout << "CrateSlice " << iCrate*20+iRing+1 << " Fit: " << cratesliceMean << " +/- " << cratesliceErr << endl;      
    }
  }

  ofstream newGainFile(absoluteGainFilename);  
  Double_t absoluteGain, absoluteGainErr;
  Double_t adjustVal, adjustErr;
  Double_t mipGain, mipGainErr;
  Int_t softId, crateId, sequence;
  Float_t mTowerEta, mTowerPhi;
  // Loop through all towers and calculate absoluate gains
  for(Int_t iTow = 0; iTow < nTowers; iTow++){
    absoluteGain = absoluteGainErr = 0.;
    adjustVal = adjustErr = 0.;
    mipGain = 0.;
    crateId = sequence = -1;
    mTowerEta = mTowerPhi = -1.;
    softId = iTow + 1;
    mDecoder->GetCrateFromTowerId(softId,crateId,sequence);
    mEmcGeom->getEta(softId,mTowerEta);
    mEmcGeom->getPhi(softId,mTowerPhi);
    if (fabs(mTowerEta) > 0.965) // Outer tower Eta is 0.967, bump this up to 0.975 for calculating sliceEtaIndex correctly
      mTowerEta += 0.008*fabs(mTowerEta)/mTowerEta;
    sliceEtaIndex = ((TMath::Nint(fabs(mTowerEta) * 1000.0) + 25)/50 - 1);
    ringIndex = ((TMath::Nint(mTowerEta * 1000.0) + 25)/50 + 19);
    if(mipTowerStatus[iTow] == 1){ // Don't calculate if bad MIP status
      mipGain = mipGains[iTow];
      if (sliceEtaIndex <= 12){
	adjustVal = cratesliceFit[crateId-1][sliceEtaIndex]->GetParameter(3);
	adjustErr = cratesliceFit[crateId-1][sliceEtaIndex]->GetParError(3);
	hChi2EtaPhi->Fill(mTowerEta,mTowerPhi,cratesliceFit[crateId-1][sliceEtaIndex]->GetChisquare()/(Double_t)cratesliceFit[crateId-1][sliceEtaIndex]->GetNDF());
      }
      else{ //sliceEtaIndex >= 13 - Use ring values on outer eta for combined statstics
	adjustVal = ringFit[ringIndex]->GetParameter(3);
	adjustErr = ringFit[ringIndex]->GetParError(3);
	hChi2EtaPhi->Fill(mTowerEta,mTowerPhi,ringFit[ringIndex]->GetChisquare()/(Double_t)ringFit[ringIndex]->GetNDF());
      }
      absoluteGain = mipGain/adjustVal;
      mipGainErr = mipAdcErr[iTow]*mipGains[iTow]/mipAdc[iTow];
      absoluteGainErr = sqrt(pow(mipGain*adjustErr/(adjustVal*adjustVal),2) + pow(mipGainErr/adjustVal,2));
    }
    newGainFile << softId << " " << absoluteGain << " " << absoluteGainErr << " " << mipTowerStatus[iTow] << endl;
    cout << "Relative Adjustment Value: " << adjustVal << " +/- " << adjustErr << endl;
    cout << softId << " " << absoluteGain << " " << absoluteGainErr << " " << mipTowerStatus[iTow] << endl;
    if (mipTowerStatus[iTow] == 1)
      hNewGains->Fill(mTowerEta,mTowerPhi,absoluteGain);
  }

  newGainFile.close();

  c->Update();
  ps->NewPage();
  c->Clear();
  c->Divide(1,2);
  c->cd(1);
  hNewGains->SetTitle("Absolute Gains");
  hNewGains->GetXaxis()->SetTitle("#eta");
  hNewGains->GetXaxis()->CenterTitle();
  hNewGains->GetYaxis()->SetTitle("#phi");
  hNewGains->GetYaxis()->CenterTitle();
  hNewGains->SetStats(kFALSE);
  hNewGains->Draw("colz");

  c->cd(2);
  hChi2EtaPhi->SetTitle("#chi^{2}/dof");
  hChi2EtaPhi->GetXaxis()->SetTitle("#eta");
  hChi2EtaPhi->GetXaxis()->CenterTitle();
  hChi2EtaPhi->GetYaxis()->SetTitle("#phi");
  hChi2EtaPhi->GetYaxis()->CenterTitle();
  hChi2EtaPhi->SetStats(kFALSE);
  hChi2EtaPhi->Draw("colz");

  ps->Close();

  // Delete pointers to decoder & geometry
  if (mDecoder) delete mDecoder;
  if (mEmcGeom) delete mEmcGeom;
}
