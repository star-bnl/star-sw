#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TVector2.h"
#include "TVector3.h"
#include "TCanvas.h"
#endif
void Membrane(Bool_t AccountLaserDelays = kTRUE) {
  Double_t Rdiffuser = 197;
  Double_t RminIFC =       46.6;
  Double_t RadInner[13] = {    60,  64.8,  69.6,  74.4,  79.2,    84,  88.8,  93.6,  98.8,   104, 109.2, 114.4, 119.6};
  Double_t outerRowRadii[32];
  Double_t firstOuterSectorPadRow  =    127.195; // ;
  Double_t outerSectorRowPitch	   =          2;
  Double_t R[45];
  for (Int_t i = 0; i < 45; i++) {
    if (i < 13) R[i] = RadInner[i];
    else        R[i] = firstOuterSectorPadRow + outerSectorRowPitch*(i-14);
  }
/*
  A.Lebedev 02/11/08
  Delays for laser rafts.
  T-zero is a moment, when a laser arrived to TPC wheel surface. 
  All other numbers corresponded to time for laser light to propagate to particular raft (TPC sector). 
  Estimated error in table   0.1ns
  West:     sector             2             4         6        8             12
  Time(ns)                 10.33          3.34      6.14    13.11          17.31
  East:     sector            14            16        18       20      22     24
  Time(ns)                 19.88         12.95      5.97     3.18   10.17  17.14
  For diffusers West <=> East

  in sector 10 there is no laser at all.
A.Lebedev 05/20/15
Reflectors installed: 4 on each side
West reflectors-> sectors    2,  4,  8, 10 illuminated from East ( lasers
enters sectors             -22, 20,-16, 14 accordingly, coming through membrane to
illuminate reflectors)

East reflectors-> sectors 16, 18, 20, 22  illuminated 
from West                  8, -6,  4,  2  accordingly)
2015:  Alive lasers in sectors : 2, 4, 8 and 14, 20
*/  
  //             WE  D
  Int_t difSector[2][4] = {
    { -2,   4,  -8, 10},
    { 22,  20, -18, 16}
  };
  Double_t T0WE[2][4] = { // delays for the above diffusers from corresponding sector
    // -22,   20,  -16,     14 : from sector (<0 means no lasers seen in the sector in 2015
    {10.17, 3.18, 12.95, 19.88},
    //   2,    4,   -6,      8
    {10.33, 3.34,  6.14, 13.11}
  };
  Double_t zMem = 210;
  Int_t nphi = 12; // *5
  TString Title("Distance from Membrane to diffuser [cm]");
  TString Name("DiffuseD");
  if (AccountLaserDelays) {
    Title += " with accounting delays in rafts";
    Name  += "D";
  }
  TH2F *DiffuseD = new TH2F(Name,Title,nphi*2,0.5,24.5,45,0.5,45.5);
  DiffuseD->SetXTitle("sector");
  DiffuseD->SetYTitle("row");
  DiffuseD->SetStats(0);
  Int_t iphi, iphiD;
  for (Int_t sector = 1; sector <= 24; sector++) {
    if (sector <= 12) {iphi = (360 + 90 - 30* sector      )%360;}
    else              {iphi = (      90 + 30*(sector - 12))%360;}
    Int_t we = 0;
    if (sector > 12) we = 1;
    Double_t Phi = TMath::DegToRad()*iphi;
    for (Int_t row = 1; row <= 45; row++) {
      Double_t r = R[row-1];
      Double_t y2 = r*TMath::Sin(Phi);
      Double_t x2 = r*TMath::Cos(Phi);
      TVector3 R2(x2, y2, 0);
      Double_t distMin = 100000;
      Int_t    secM = -1;
      for (Int_t dif = 0; dif < 4; dif++) {
	//      for (Int_t dif = 1; dif <= 1; dif++) {
	Int_t difSec = difSector[we][dif];
	if (difSec < 0) continue;
	if (difSec <= 12) {iphiD = (360 + 90 - 30* difSec      )%360;}
	else              {iphiD = (      90 + 30*(difSec - 12))%360;}
	Double_t Ydiffuser = 2.5;
	if (difSec <= 12) Ydiffuser = -2.5;
 	Double_t PhiDif = TMath::DegToRad()*iphiD;
	Double_t y1 = Rdiffuser*TMath::Sin(PhiDif) + Ydiffuser*TMath::Cos(PhiDif);
	Double_t x1 = Rdiffuser*TMath::Cos(PhiDif) - Ydiffuser*TMath::Sin(PhiDif);
	TVector3 R1(x1,y1,zMem);
	TVector3 dR = R2 - R1;
	Double_t distance = dR.Mag();
	if (AccountLaserDelays) distance += 30*T0WE[we][dif];
	Double_t r2 = dR.XYvector().Mod2();
	TVector2 n = dR.XYvector().Unit();
	Double_t s = - (R1.XYvector()*n);
	TVector2 R = R1.XYvector() + n*s;
	if (R.Mod() <= RminIFC) {
	  TVector2 dRM = R2.XYvector() - R;
	  Double_t x1n = R1.XYvector()*n;
	  Double_t s1 = - x1n - TMath::Sqrt(x1n*x1n + Rdiffuser*Rdiffuser - R1.XYvector().Mod2());
	  if (s1 > s) continue;
	}
#if 0
	cout << Form("%2i => %2i",sector,difSec)
	     << Form(" xyS %8.3f %8.3f",x2,y2)  << Form("\tdif %8.3f %8.3f",x1,y1)
	     << Form(" distance %8.3f s %8.3f",distance,s) << endl;
#endif
	if (distance < distMin) {
	  distMin = distance; 
	  secM = difSec;
	}
      }
      if(secM < 0) continue;  
      DiffuseD->SetBinContent(sector,row,distMin);
      DiffuseD->SetBinError(sector,row,0.1e-9*TMath::Ccgs());

#if 0
      cout << Form("%2i => %2i distMin = %8.3f",sector,secM,distMin) << endl;
#endif
    }
  }
  DiffuseD->SetMinimum(zMem);
  TCanvas *c1 = new TCanvas("c1","c1");
  DiffuseD->Draw("colz");
  TF1::InitStandardFunctions();
  TCanvas *c2 = new TCanvas("c2","c2");
  TF1 *pol0 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol0");
  TF1 *pI = new TF1(*pol0); pI->SetRange(0.5,13.5);
  TF1 *pO = new TF1(*pol0); pO->SetRange(13.5,45.5);
  TH2F *DiffuseDI = new TH2F(*DiffuseD);
  DiffuseDI->SetName("DiffuseDI");
  DiffuseDI->FitSlicesY(pI);
  TH2F *DiffuseDO = new TH2F(*DiffuseD);
  DiffuseDO->SetName("DiffuseDO");
  DiffuseDO->FitSlicesY(pO);
  TH1 *DiffuseDO_0 = (TH1 *) gDirectory->Get("DiffuseDO_0");
  DiffuseDO_0->SetMarkerColor(2);
  DiffuseDO_0->Draw();
  TH1 *DiffuseDI_0 = (TH1 *) gDirectory->Get("DiffuseDI_0");
  DiffuseDI_0->Draw("same");
  for (Int_t i = 1; i <= 24; i++) cout << DiffuseDI_0->GetBinContent(i) << ","; cout << endl;
  for (Int_t i = 1; i <= 24; i++) cout << DiffuseDO_0->GetBinContent(i) << ","; cout << endl;
}
