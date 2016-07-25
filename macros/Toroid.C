#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TF1.h"
#include "TH1.h"
#include "TLegend.h"
#include "TCanvas.h"
#endif

Double_t SolenoidI(Double_t *x, Double_t *par) {
  static Double_t Bz = 0.5; // T
  static Double_t RO = 1.80; // m outer radius
  static Double_t ZO = 1.90; // 
  static Double_t Theta0 = TMath::ATan2(RO,ZO);
  Double_t Theta = TMath::Abs(TMath::DegToRad()*x[0]); // angle wrt Z axis
  Double_t L = 0;
  if (TMath::Abs(Theta) < Theta0) L = ZO/TMath::Cos(Theta);
  else                            L = RO/TMath::Sin(Theta);
  Double_t BT = Bz*TMath::Sin(Theta);
  return BT*L;
}
//________________________________________________________________________________
Double_t SolenoidI2(Double_t *x, Double_t *par) {
  static Double_t Bz = 0.5; // T
  static Double_t RO = 1.80; // m outer radius
  static Double_t ZO = 1.90; // 
  static Double_t Theta0 = TMath::ATan2(RO,ZO);
  Double_t Theta = TMath::Abs(TMath::DegToRad()*x[0]); // angle wrt Z axis
  Double_t L = 0;
  if (TMath::Abs(Theta) < Theta0) L = ZO/TMath::Cos(Theta);
  else                            L = RO/TMath::Sin(Theta);
  Double_t BT = Bz*TMath::Sin(Theta);
  return BT*L*L/2;
}
//________________________________________________________________________________
Double_t ToroidI(Double_t *x, Double_t *par) {
  Double_t BphiR = par[0];
  Double_t RI = par[1]; // [m] inner radius
  Double_t RO = par[2]; // [m] outer radius
  Double_t ZI = par[3]; // star of toroid [m]
  Double_t ZO = par[4]; // end of toroid [m]
  Double_t Theta1 = TMath::ATan2(RI,ZO);
  Double_t Theta2 = TMath::ATan2(RO,ZO);
  Double_t Theta3 = TMath::ATan2(RI,ZI);
  Double_t Theta4 = TMath::ATan2(RO,ZI);
  Double_t Theta = TMath::Abs(TMath::DegToRad()*x[0]); // angle wrt Z axis
  Double_t TanT  = TMath::Tan(Theta);
  if (Theta > Theta4) return 0; // Outside
  if (Theta < Theta1) return 0; // Inside
  Double_t zI = ZI, zO = ZO;
  Double_t ri = ZI *TanT;
  Double_t rO = ZO *TanT;
  if (ri < RI) {
    zI = RI/TanT;
  }
  if (rO > RO) {
    zO = RO/TanT;
  }
  Double_t BL = BphiR/TanT*TMath::Log(zO/zI);
  return BL;
}
//________________________________________________________________________________
Double_t ToroidI2(Double_t *x, Double_t *par) {
  Double_t BphiR = par[0];
  Double_t RI = par[1]; // [m] inner radius
  Double_t RO = par[2]; // [m] outer radius
  Double_t ZI = par[3]; // star of toroid [m]
  Double_t ZO = par[4]; // end of toroid [m]
  Double_t Theta1 = TMath::ATan2(RI,ZO);
  Double_t Theta2 = TMath::ATan2(RO,ZO);
  Double_t Theta3 = TMath::ATan2(RI,ZI);
  Double_t Theta4 = TMath::ATan2(RO,ZI);
  Double_t Theta = TMath::Abs(TMath::DegToRad()*x[0]); // angle wrt Z axis
  Double_t TanT  = TMath::Tan(Theta);
  if (Theta > Theta4) return 0; // Outside
  if (Theta < Theta1) return 0; // Inside
  Double_t zI = ZI, zO = ZO;
  Double_t ri = ZI *TanT;
  Double_t rO = ZO *TanT;
  if (ri < RI) {
    zI = RI/TanT;
  }
  if (rO > RO) {
    zO = RO/TanT;
  }
  Double_t BL2 = BphiR*(zO-zI)*2/TMath::Sin(2*Theta);
  return BL2;
}
//________________________________________________________________________________
void Toroid(Double_t scale = 1) {
  /* Solenoid: B = mu * N / L  * I = mu * n * I;
     L - lenght of solenoid,
     mu = k * mu_0; k - relative permeability (k ~ 100 for iron); mu_0 = 4 * pi * 1e-7 [T*m/A];
     N - no. of turns (=52) , L - length of solenoid; n = N / L - no. of turn per unit length, "turns density".
     n = 52/4.0 = 13; 
     Magent = 10 Main Coils (*4 pancakes) + 2 Space Trim coils (*2 pancakes)
     Two layer pancales wound two-in-hand (bifilaf) fashion in 13 turns (of wire pairs)    
     I = 4.5 kA
     10*4 *13 + 2*2 *13 = 572 turns on 6.2 m => n = 572/6.2 = 92 => B = mu_0 * n * I = 0.52 T
     

     Toroid:   B = mu * N * I/(2 * pi * r)
     STAR solenoid: 
     Main Coil: conductor : 53.9 mm * 47.5 mm with water hole 18.6 mm diameter, I = 4.5 kA
     Pole tip :   -"-     : 22.2 mm * 22.2 mm  -"-            12.3 mm         , I = 1.33 kA

     Todoid started at R = 5 cm => N = 14
     B = mu * N * I / (2 *pi) / r[m] = 4 * pi * 1e-7 [T/A/m] * 14 * 1.33e3 [A] / (2 * pi) / r = 
     2 * 1e-7 * 14 * 1.33e3 / r [T] = 3.72e-3 /r [T] = 7.5e-2 T @ [r = 5 cm] = 750 Gaus = 0.75 kG r R = 5 cm

     Al conductivity rho = 2.82e-8; // Om * cm
   */
  Double_t mu_0 = 4 * TMath::Pi() * 1e-7;// [T*m/A]
  Double_t RI   = 10; // inner radius of Toroid [cm] 
  Double_t RO   = 40; // outer -"-
  Double_t ZI   = 60; // start of toroid
  Double_t ZO   = 260;// end -"-
  Double_t W    = 2.2; // [cm] conductor width
  Int_t N =  2 *TMath::Pi()*RI/2.2; // no. of turns
  Double_t I = 1.33e3*scale; // Current [A]
  Double_t RphiR = mu_0 * N * I / (2 * TMath::Pi()); // Bphi * R [T*m]
  cout << "RI = " << RI << "cm, RO = " << RO << "cm, ZI = " << ZI << "cm, ZO = " << ZO << " cm, No. of Turns = " << N << ", I = " << 1e-3*I << "kA" << endl;
  TString title(Form(":R_{I} = %3.0f cm, R_{O} = %3.0fcm, Z_{I} = %3.0fcm, Z_{O} = %4.0fcm, No. of Turns = %i, I = %5.2fkA",RI,RO,ZI,ZO,N,1e-3*I));
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1)  c1 = new TCanvas("c1","c1");
  c1->cd();
  TH1F *frame1 = c1->DrawFrame(0,0,90.,1.0);
  TString Title("BL versus #Theta");
  Title += title;
  frame1->SetTitle(Title);
  frame1->SetXTitle("#Theta^{o}");
  frame1->SetYTitle("B L (T m)");
  TLegend *l1 = new TLegend(0.6,0.5,0.8,0.7);
  TF1 *Sol1 = new TF1("SolenoidBL",SolenoidI,0,90,0);
  Sol1->Draw("same");
  l1->AddEntry(Sol1,"Solenoid");
  TF1 *Tor1 = new TF1("ToroidBL",ToroidI2,0,90,5);
  Tor1->SetParameter(0, RphiR);
  Tor1->SetParameter(1, 1e-2*RI);
  Tor1->SetParameter(2, 1e-2*RO);
  Tor1->SetParameter(3, 1e-2*ZI);
  Tor1->SetParameter(4, 1e-2*ZO);
  Tor1->SetLineColor(2);
  Tor1->Draw("same");
  l1->AddEntry(Tor1,"Toroid");
  l1->Draw();
  TCanvas *c2 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c2");
  if (! c2)  c2 = new TCanvas("c2","c2");
  c2->cd();
  TH1F *frame2 = c2->DrawFrame(0,0.,90.,1.2);
  Title = "BL^{2} versus #Theta";
  Title += title;
  frame2->SetTitle(Title);
  frame2->SetXTitle("#Theta^{o}");
  frame2->SetYTitle("B L^{2} (T m^{2}");
  TLegend *l2 = new TLegend(0.6,0.4,0.8,0.6);
  TF1 *Sol2 = new TF1("SolenoidBL2",SolenoidI2,0,90,0);
  Sol2->Draw("same");
  l2->AddEntry(Sol2,"Solenoid");
  TF1 *Tor2 = new TF1("ToroidBL2",ToroidI2,0,90,12);
  Tor2->SetParameter(0, RphiR);
  Tor2->SetParameter(1, 1e-2*RI);
  Tor2->SetParameter(2, 1e-2*RO);
  Tor2->SetParameter(3, 1e-2*ZI);
  Tor2->SetParameter(4, 1e-2*ZO);
  Tor2->SetLineColor(2);
  Tor2->Draw("same");
  l2->AddEntry(Tor2,"Toroid");
  l2->Draw();
}
