//
// $Id BetheBloch.cc $
//
// Description
// Using values of the dedx vs. kinetic energy curve.
// taken from GEANT.
//
// curve was generated using geometry from geant.
// need to transform to dedx vs. beta gamma
// this is done by
// 1) reading the kinetic energy and dedx value
// 2) obtain energy by energy = kinetic energy + mass
// 3) calculate beta*gamma = p/m where p = sqrt(e^2 - m^2)
// 4) insert into the map of beta gamma - dedx values
//
// The function returns a linear interpolation between
// the 2 closest bins.

// If betagamma < 2.5, the function returns a value proportional
// to 1/ beta^2
// There is an overall normalization factor obtained from a fit
// and there is a relative normalization factor between the simple
// 1/beta^2 region and the values from the table.

#include <vector>
#include "TMath.h"
#include "BetheBloch.h"

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

ClassImp(BetheBloch)

//________________________________________________________________________________
Double_t BetheBloch::Sirrf(Double_t Poverm, Double_t Length, Int_t k) {
  Double_t par[7] = {
    7.18739e-01, // Scale
    1.58587e-05, // I
    1.15874e+01, // Delta
   -3.30560e-01, // a0
    8.62137e-02, // a1
   -7.21336e-03, // a2
    1.06436e+01  // Delta_e
  };
  Double_t poverm = Poverm;
  if (poverm > 527.5) poverm = 527.5;
  Double_t beta2inv = 1. + 1./(poverm*poverm);
  Double_t gamma  = TMath::Sqrt(poverm*poverm + 1);
  Double_t Lpoverm = TMath::Log(poverm);
  Double_t K      = 0.307075e+3;// keV/g cm^2 
  Double_t A      = 38.691;
  Double_t Z      = 17.436;
  Double_t rho    = 1.5607e-03;//  0.9*0.00166+0.1*0.000667
  Double_t I      = par[1]; //15.8e-6; //15.8; // MeV for Ar, 13.1e-6 MeV for CH4
  Double_t m      =   0.510998902;// MeV electron Mass
  Double_t pim    = 139.570180;// MeV pion Mass
  Double_t Delta;
  Double_t M = pim;
  if (k) {M = m;   Delta = par[6];}
  else   {M = pim; Delta = par[2];}
  Double_t r = m/M;
  Double_t Tmax = 2*m*poverm*poverm/(1. + r*(2*gamma + r));
  Double_t Tupper = Tmax;
  Double_t si = K*Z/A*rho/2*beta2inv*
    (TMath::Log(2*m*poverm*poverm*Tupper/(I*I)) 
     - (1 + Tupper/Tmax)/beta2inv - Delta);
  if (si <= 0) si = 1.e-12;
  Double_t value = par[0] + TMath::Log(si) + 
    Lpoverm*(par[3] + Lpoverm*(par[4] + Lpoverm*par[5]));
  Double_t sirrf =  TMath::Exp(value);
  // track length correction
  Double_t coeff[9] = { 
    4.84187e-01, -3.87216e-02,  1.63463e-03, -3.91121e-05,
    5.52376e-07, -4.49661e-09,  1.94136e-11, -3.43526e-14
  };
  Double_t X = Length;
  if (X <  20.5) X =  20.5;
  if (X > 120  ) X = 120;
  Double_t FPARAM = 0;
  for (int i = 8; i >= 0; i--) FPARAM = coeff[i] + X*FPARAM;
  FPARAM += -7.01522e-02;
  sirrf *= TMath::Exp(FPARAM);
  return sirrf;
}
    
