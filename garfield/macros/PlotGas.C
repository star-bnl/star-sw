/* 
  root.exe lGarfield.C PlotGas.C+
 */
#define __DONT_AVALANCHE__
//#define __USE_AvalancheMicroscopic
#include <iostream>
#include <fstream>
#include "TSystem.h"
#include "TCanvas.h"
#include "TROOT.h"
#include "TApplication.h"
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "TLegend.h"
#include "TMath.h"
#include "GarfieldConstants.hh"
#include "ViewField.hh"
#include "ViewDrift.hh"
#include "ViewCell.hh"
#include "MediumMagboltz.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "ComponentAnalyticField.hh"
#include "Sensor.hh"
#include "DriftLineRKF.hh"
#ifndef __DONT_AVALANCHE__
#include "AvalancheMicroscopic.hh"
#include "AvalancheMC.hh"
#endif /* __DONT_AVALANCHE__ */
#include "FundamentalConstants.hh"
#include "Random.hh"
#include "Plotting.hh"
#include "TPolyMarker.h"
#define __DriftVelocity__
using namespace Garfield;

TFile *PlotGas1(const Char_t *gasFile = "P10.B5kGT297P7590.87Ar0.1CH40.03CF4.gas") {
  if (gSystem->AccessPathName(gasFile)) return 0;
  TString out(gasFile);
  out.ReplaceAll(".gas",".root");
  TFile *fOut = new TFile(out,"recreate");
  // Create a gas medium.  
  MediumMagboltz* gas = new MediumMagboltz();
  gas->LoadGasFile(gasFile);
  gas->PrintGas();
  const Char_t *path = gSystem->ExpandPathName("$GARFIELD_HOME/Data/IonMobility_Ar+_Ar.txt");
  gas->LoadIonMobility(path);
  delete path;
  gas->Initialise();
  // Drift velocities and diffusion
  Int_t nEF =     90;
  Int_t EFmin =   50;
  Int_t EFmax =  500;
  TH1F *vElectron = new TH1F("vElectron","Electron drift velocity [cm/musec]",nEF,EFmin,EFmax);
  TH1F *dTElectron = new TH1F("dTElectron","Electron transferse diffusion [mkm/sqrt(cm)] ",nEF,EFmin,EFmax);
  TH1F *dLElectron = new TH1F("dLElectron","Electron longitudinal diffusion [mkm/sqrt(cm)] ",nEF,EFmin,EFmax);
  
  Double_t vx, vy, vz;
  Double_t dl, dt;
  Double_t xmax = -1;
  Double_t ymax = -1;
  for (Int_t i = 1; i <= nEF; i++) {
    Double_t ez = vElectron->GetBinCenter(i);
    gas->ElectronVelocity(0,0,-ez,0,0,0.5,vx,vy,vz);
    if (1e3*vz > ymax) {
      ymax = 1e3*vz;
      xmax = ez;
    }
    vElectron->SetBinContent(i,1e3*vz);
    gas->ElectronDiffusion(0,0,-ez,0,0,0.5,dl,dt);
    dTElectron->SetBinContent(i,1e4*dt);
    dLElectron->SetBinContent(i,1e4*dl);
  }
  TPolyMarker *pm = new TPolyMarker(1, &xmax, &ymax);
  pm->SetMarkerStyle(23);
  static Int_t color = 1;
  pm->SetMarkerColor(color);
  color++;
  pm->SetMarkerSize(1.3);
  vElectron->GetListOfFunctions()->Add(pm);
  delete gas;
  fOut->Write();
  return fOut;
}
//________________________________________________________________________________
void PlotGas() {
#if 0
  enum {NoGases = 4};
  const Char_t *gasFiles[NoGases] = 
    {"P10/P10.B5kGT297P759P10.gas",
     "CF4.01/B5kGT297P759_0.89Ar_0.1CH4_0.01CF4.gas",
     "CF4.03/P10.B5kGT297P7590.87Ar0.1CH40.03CF4.gas",
     "CF4C/P10.B5kGT297P7590.8Ar0.1CH40.1CF4.gas"};
  const Char_t *gasNames[NoGases] = {"90% Ar + 10% CH_{4} (P10)", 
			       "89% Ar + 10% CH_{4} + 1% CF_{4}", 
			       "87% Ar + 10% CH_{4} + 3% CF_{4}", 
			       "80% Ar + 10% CH_{4} + 10% CF_{4}"};
#else
  enum {NoGases = 10};
  const Char_t *gasFiles[NoGases] = {
    "P10/P10.B5kGT297P759P10.gas",
    "CF4.0025/B5kGT297P759_0.898Ar_0.100CH4_0.003CF4.gas",
    "CF4.0050/B5kGT297P759_0.895Ar_0.100CH4_0.005CF4.gas",
    "CF4.0075/B5kGT297P759_0.893Ar_0.100CH4_0.007CF4.gas",
    "CF4.0100/B5kGT297P759_0.890Ar_0.100CH4_0.010CF4.gas",
    "CF4.0125/B5kGT297P759_0.888Ar_0.100CH4_0.013CF4.gas",
    "CF4.0150/B5kGT297P759_0.885Ar_0.100CH4_0.015CF4.gas",
    "CF4.0200/B5kGT297P759_0.880Ar_0.100CH4_0.020CF4.gas",
    "CF4.0250/B5kGT297P759_0.875Ar_0.100CH4_0.025CF4.gas",
    "CF4.0300/B5kGT297P759_0.870Ar_0.100CH4_0.030CF4.gas"
  };
  const Char_t *gasNames[NoGases] = {
    "90% Ar + 10% CH_{4} (P10)", 
    "89.75% Ar + 10% CH_{4} + 0.25% CF_{4}", 
    "89.50% Ar + 10% CH_{4} + 0.50% CF_{4}", 
    "89.25% Ar + 10% CH_{4} + 0.75% CF_{4}", 
    "89.00% Ar + 10% CH_{4} + 1.00% CF_{4}", 
    "88.75% Ar + 10% CH_{4} + 1.25% CF_{4}", 
    "88.50% Ar + 10% CH_{4} + 1.50% CF_{4}", 
    "88.00% Ar + 10% CH_{4} + 2.00% CF_{4}", 
    "87.50% Ar + 10% CH_{4} + 2.50% CF_{4}", 
    "87.00% Ar + 10% CH_{4} + 3.00% CF_{4}"
  };
#endif
  TFile *files[NoGases] = {0};
  for (Int_t i = 0; i < NoGases; i++) {
    files[i] = PlotGas1(gasFiles[i]);
  }
  TCanvas *c1 = new TCanvas("c1","c1",800,1200);
  c1->Divide(1,3);
  const Char_t *histN[3] = {"vElectron","dTElectron","dLElectron"};
  TLegend *l[3] = {0};
  for (Int_t j = 0; j < 3; j++) {
    TString same("samel");
    c1->cd(j+1);
    TH1F *frame = 0;
    if (j == 0) {//Transverse and longitudinal 
      frame = gPad->DrawFrame(100,3,300,10);
      frame->SetTitle("Electron drift velocity [cm/#musec] versus electric field intensity");
    } else if (j == 1) {
      frame = gPad->DrawFrame(100,100,300,500);
      frame->SetTitle("Electron transverse diffusion [#mum/#sqrt{cm}]");
    } else {
      frame = gPad->DrawFrame(100,150,300,450);
      frame->SetTitle("Electron longitudinal diffusion [#mum/#sqrt{cm}]");
    }
    frame->SetXTitle("Electric field intensity [V/cm]");
    for (Int_t i = 0; i < NoGases; i++) {
      if (! files[i]) continue;
      files[i]->cd();
      TH1 *hist = (TH1 *) gDirectory->Get(histN[j]);
      if (hist) {
	hist->SetLineColor(i+1);
	hist->SetMarkerColor(i+1);
	hist->SetLineWidth(4);
	hist->Draw(same);
	if (! l[j]) {
	  l[j] = new TLegend(0.5,0.5,0.7,0.7);
	  l[j]->Draw();
	  same = "samel";
	}
	//	TString tag = gSystem->DirName(gDirectory->GetName());
	l[j]->AddEntry(hist,gasNames[i]);
      }
    }
  }
}
