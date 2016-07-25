#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TF2.h"
#include "TCanvas.h"
#endif
//const Double_t TCUT = 10e-6; // 10 keV
const Double_t TCUT = 10e-9; // 10 eV
const Double_t m = 0.51099907e-3; // electron mass
const Double_t r0 = 2.817940285e-1; // clasical electron radius (sqrt(barn = 1e-28 m*m))
const Double_t Z_media = 18; // Ar
const Double_t sigma0 = 2*TMath::Pi()*Z_media*r0*r0*m;
TCanvas *c1 = 0;
TCanvas *c2 = 0;
TCanvas *c3 = 0;
//________________________________________________________________________________
Double_t AllOther(Double_t *x, Double_t *p) { /* p e- */
  Double_t M = p[0];
  Double_t T = TMath::Exp(x[1]*TMath::Ln10());     // T - kinetic energy of scattered electron
  Double_t bgL10 = x[0]; // TMath::Log10(beta*gamma);
  Double_t bg2 = TMath::Exp(2*bgL10*TMath::Ln10());
  Double_t gamma2 = bg2 + 1;
  Double_t b2  = bg2/gamma2;
  //  Double_t beta = TMath::Sqrt(b2);
  Double_t gamma= TMath::Sqrt(gamma2);
  Double_t E = m*gamma;
  Double_t Tmax = 2*m*(gamma2 - 1)/(1 + 2*gamma*(m/M) + TMath::Power(m/M,2));
  Double_t fT = 1./(b2*T*T);// (1/TCUT - 1/Tmax)/T;
  Double_t gT = 1 + b2*T/Tmax;
  gT += 0.5*(T/E*T/E); // TMath::Power(T/E,2); // only for spin 1/2 particles
  return sigma0*fT*gT*T*TMath::Sqrt(bg2);
}
//________________________________________________________________________________
Double_t Moller(Double_t *x, Double_t * p) { /* e-e- */
  Double_t RF = AllOther(x,p);
  if (RF <= 0) return 0;
  Double_t T = TMath::Exp(x[1]*TMath::Ln10());     // T - kinetic energy of scattered electron
  Double_t bgL10 = x[0]; // TMath::Log10(beta*gamma);
  Double_t bg2 = TMath::Exp(2*bgL10*TMath::Ln10());
  Double_t gamma2 = bg2 + 1;
  Double_t b2  = bg2/gamma2;
  //  Double_t b2  = bg2/gamma2;
  //  Double_t beta = TMath::Sqrt(b2);
  Double_t gamma= TMath::Sqrt(gamma2);
  Double_t E = m*gamma;
  Double_t epsilon = T / (E - m);
  Double_t epsilon_0 = TCUT/(E - m);
  if (epsilon > 0.5 ||
      epsilon < epsilon_0) return 0;
  Double_t fe =1./(b2*(E-m));// 1./(epsilon*epsilon)*epsilon_0/(1 - 2*epsilon_0);
#if 0
  Double_t ge = 4./(9*gamma2 - 10*gamma + 5)*
    (TMath::Power((gamma - 1)*epsilon,2) - 
    (2 *gamma2 + 2*gamma - 1) *epsilon/(1 - epsilon) + gamma2/TMath::Power(1 - epsilon,2));
#else
  Double_t ge = (gamma - 1.)*(gamma - 1.)/(gamma*gamma) 
    +           1./epsilon*(1./epsilon - (2*gamma - 1.)/(gamma*gamma))
    +           1./(1. - epsilon)*(1./(1. - epsilon)*(2*gamma - 1.)/(gamma*gamma));
#endif
  return sigma0*fe*ge*T*TMath::Sqrt(bg2)/RF;
}
//________________________________________________________________________________
Double_t BaBar(Double_t *x, Double_t *p) { /* e-e- */
  Double_t RF = AllOther(x,p);
  if (RF <= 0) return 0;
  Double_t T = TMath::Exp(x[1]*TMath::Ln10());     // T - kinetic energy of scattered electron
  Double_t bgL10 = x[0]; // TMath::Log10(beta*gamma);
  Double_t bg2 = TMath::Exp(2*bgL10*TMath::Ln10());
  Double_t gamma2 = bg2 + 1;
  Double_t b2  = bg2/gamma2;
  //  Double_t beta = TMath::Sqrt(b2);
  Double_t gamma= TMath::Sqrt(gamma2);
  Double_t E = m*gamma;
  Double_t epsilon = T / (E - m);
  Double_t epsilon_0 = TCUT/(E - m);
  if (epsilon > 1 ||
      epsilon < epsilon_0) return 0;
#if 0
  Double_t fe = 1./(epsilon*epsilon)*epsilon_0/(1 - epsilon_0);
#else
  Double_t fe = 1./(E -m);
#endif
  Double_t y = 1./(gamma + 1);
  //  Double_t B0 = gamma2/(gamma2 - 1);
  Double_t B1 = 2 - y*y;
  Double_t B2 = (1 - 2*y)*(3 + y*y);
  Double_t B3 = (1 - 2*y)*(1 - 2*y)*(1 + (1 - 2*y));
  Double_t B4 = (1 - 2*y)*(1 - 2*y)*(1 - 2*y);
#if 0
  Double_t ge = 
    (B0 + epsilon  *(-B1 + epsilon  *(B2 + epsilon  *(- B3 + B4*epsilon  ))))/
    (B0 + epsilon_0*(-B1 + epsilon_0*(B2 + epsilon_0*(- B3 + B4*epsilon_0))));
#else
  Double_t ge = 1./(b2*epsilon*epsilon) - B1/epsilon + B2 - B3*epsilon + B4*epsilon*epsilon;
#endif
  return sigma0*fe*ge*T*TMath::Sqrt(bg2)/RF;
}
//________________________________________________________________________________
void DeltaRays() {
  /* GEANT PHYS331
         Sigma_0 = 2*pi*Z*r0**2*m
	 Z - atomic number of medium
	 M - rest mass of the incident particle
	 E - energy      -"-
	 gamma = E/M
	 T - kinetic energy of scattered electron (of the lower energy in the case e-e+ scattering)
	 beta**2 = 1 - 1/gamma**2
	 y = 1/(gamma + 1)
	 B1 = 2 - y**2
	 B2 = (1 - 2*y)(3 + y**2)
	 B3 = (1 - 2*y)**2 + (1 - 2*y)**3
	 B4 = (1 - 2*y)**2
	 epsilon  T/(E - m)
         epsilon_0 = TCUT/(E - m) <= epsilon <= 1/2 for e-e-
	 epsilon_0 = TCUT/(E - m) <= epsilon <= 1/2 for e+e-
  */
  TF2 *AleN = new TF2("AleN",BaBar,-1,4,TMath::Log10(TCUT),-3,1);
  AleN->SetParameter(0,0.93827231);
  c3 = new TCanvas("pe-","Rutherford");
  c3->SetLogz(1);
  AleN->Draw("colz");
  TF2 *eNeN = new TF2("eNeN",Moller,-1,4,TMath::Log10(TCUT),-3,1);
  eNeN->SetParameter(0,0.93827231);
  c1 = new TCanvas("e-e-","Moller");
  eNeN->Draw("colz");
  TF2 *ePeN = new TF2("ePeN",BaBar,-1,4,TMath::Log10(TCUT),-3,1);
  ePeN->SetParameter(0,0.93827231);
  c2 = new TCanvas("e+e-","Babar");
  ePeN->Draw("colz");
  
}
