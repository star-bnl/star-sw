/* Heinrich Schindler 04/22/13
  root.exe lGarfield.C tpc.C+
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
#include "TSystem.h"
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
#define __DriftVelocity__
using namespace Garfield;

void PlotDrift(const Char_t *gasFile = "P10.B5kGT297P7590.87Ar0.1CH40.03CF4.gas") {
  if (! gSystem->AccessPathName(gasFile)) return;
  const Double_t BarPressure         = 1010.8; // [mbar], TPC-PTB, barometricPressure
  const Double_t inputTPCGasPressure = 1.93;   // [mbar], TPC-PT8, difference between barometer pressure and pressure in TPC
  //  const Double_t pressure = (1011. / 1000.) * 750.; // 1 bar = 750 torr 
  const Double_t pressure = ((BarPressure + inputTPCGasPressure)/ 1000.) * 750.; // [torr], 1 bar = 750 torr 
  const Double_t temperature = 297.839; //273.15 + 24.7; // inputGasTemperature (degree K)
  Double_t BField = 0.5; // Tesla
  Double_t Angle  = 0.0; // rad
  Int_t b = 10*BField;
  Int_t t = temperature;
  Int_t p = pressure;
  TFile *fOut = new TFile("DriftVelocity1V.root","recreate");
  // Create a gas medium.  
  MediumMagboltz* gas = new MediumMagboltz();
  // Use Magboltz to generate the gas table and grid.
  // Specfify te number of collission [1e7] over which the electron is traced in Magboltz
  const Int_t ncoll = 10; //[1e7]
  const Bool_t verbose = true;
  gas->LoadGasFile(gasFile.Data());
  gas->PrintGas();
  const Char_t *path = gSystem->ExpandPathName("$GARFIELD_HOME/Data/IonMobility_Ar+_Ar.txt");
  gas->LoadIonMobility(path);
  delete path;
  gas->Initialise();
  // Drift velocities and diffusion
  Int_t nEF = 1;
  Int_t EFmin = 100;
  Int_t EFmax = 200;
  TH1F *vElectron = new TH1F("vElectron","Electron drift velocity [cm/musec]",nEF,EFmin,EFmax);
  TH1F *dTElectron = new TH1F("dTElectron","Electron transferse diffusion [mkm/sqrt(cm)] ",nEF,EFmin,EFmax);
  TH1F *dLElectron = new TH1F("dLElectron","Electron longitudinal diffusion [mkm/sqrt(cm)] ",nEF,EFmin,EFmax);
  
  Double_t vx, vy, vz;
  Double_t dl, dt;
  for (Int_t i = 1; i <= nEF; i++) {
    Double_t ez = vElectron->GetBinCenter(i);
    gas->ElectronVelocity(0,0,-ez,0,0,0.5,vx,vy,vz);
    vElectron->SetBinContent(i,1e3*vz);
    gas->ElectronDiffusion(0,0,-ez,0,0,0.5,dl,dt);
    dTElectron->SetBinContent(i,1e4*dt);
    dLElectron->SetBinContent(i,1e4*dl);
  }
  fOut->Write();
  return;
}
