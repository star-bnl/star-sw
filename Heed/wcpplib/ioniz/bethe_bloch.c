#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include "wcpplib/ioniz/bethe_bloch.h"
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/stream/prstream.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
/*
2002, I. Smirnov
*/


double Bethe_Bloch_energy_loss(double ratio_Z_to_A, double I_eff, 
			 double betta, double z)
{
  //mcout<<"Bethe_Bloch_energy_loss:\n";
  //Iprintn(mcout, ratio_Z_to_A/(1.0/(gram/mole)));
  //Iprintn(mcout, ratio_Z_to_A);
  //Iprintn(mcout, I_eff/eV);
  //Iprintn(mcout, betta);
  //Iprintn(mcout, z);
  double betta2=betta*betta;
  double gamma2 = pow( lorgamma_1(betta) + 1.0 , 2.0 ); 
  double coef1 = 4 * M_PI * pow( classic_electr_radius, 2.0) * 
    electron_mass_c2 * Avogadro;  // should be 0.3071 according to PDG
  //mcout<<"classic_electr_radius/cm="<<classic_electr_radius/cm<<'\n';
  //mcout<<"electron_mass_c2/MeV="<<electron_mass_c2/MeV<<'\n';
  //mcout<<"Avogadro="<<Avogadro<<'\n';
  //mcout<<"Avogadro/mole="<<Avogadro/mole<<'\n';
  //mcout<<"coef1/(MeV*cm2/mole)="<<coef1/(MeV*cm2/mole)<<'\n';
  double coef2=pow( z , 2.0) * ratio_Z_to_A / betta2 ;
  //mcout<<"coef2/(1.0/gram)="<<coef2/(1.0/gram)<<'\n';
  double sum = log( 2.0 * electron_mass_c2 * betta2 * gamma2 / I_eff ) -
    betta2;
  //mcout<<"sum="<<sum<<'\n';
  //mcout<<"result/(keV*cm2/gram)="<<coef1 * coef2 * sum /(keV*cm2/gram)<<'\n';
  return coef1 * coef2 * sum;
}

double Bethe_Bloch_energy_loss_gamma_1(double ratio_Z_to_A, double I_eff, 
			 double gamma_1, double z)
{
  //mcout<<"Bethe_Bloch_energy_loss:\n";
  //Iprintn(mcout, ratio_Z_to_A/(1.0/(gram/mole)));
  //Iprintn(mcout, ratio_Z_to_A);
  //Iprintn(mcout, I_eff/eV);
  //Iprintn(mcout, betta);
  //Iprintn(mcout, z);
  double betta = lorbetta(gamma_1);
  double betta2 = betta * betta;
  double gamma  = gamma_1 + 1.0;
  double gamma2 = gamma * gamma;
  double coef1 = 4 * M_PI * pow( classic_electr_radius, 2.0) * 
    electron_mass_c2 * Avogadro;  // should be 0.3071 according to PDG
  //mcout<<"classic_electr_radius/cm="<<classic_electr_radius/cm<<'\n';
  //mcout<<"electron_mass_c2/MeV="<<electron_mass_c2/MeV<<'\n';
  //mcout<<"Avogadro="<<Avogadro<<'\n';
  //mcout<<"Avogadro/mole="<<Avogadro/mole<<'\n';
  //mcout<<"coef1/(MeV*cm2/mole)="<<coef1/(MeV*cm2/mole)<<'\n';
  double coef2=pow( z , 2.0) * ratio_Z_to_A / betta2 ;
  //mcout<<"coef2/(1.0/gram)="<<coef2/(1.0/gram)<<'\n';
  double sum = log( 2.0 * electron_mass_c2 * betta2 * gamma2 / I_eff ) -
    betta2;
  //mcout<<"sum="<<sum<<'\n';
  //mcout<<"result/(keV*cm2/gram)="<<coef1 * coef2 * sum /(keV*cm2/gram)<<'\n';
  return coef1 * coef2 * sum;
}
    
double Bethe_Bloch_restricted_energy_loss_gamma_1
(double ratio_Z_to_A, double I_eff, 
 double M,
 double gamma_1, 
 double Ecut, // in internal units
 double z)
{
  //mcout<<"Bethe_Bloch_energy_loss:\n";
  //Iprintn(mcout, ratio_Z_to_A/(1.0/(gram/mole)));
  //Iprintn(mcout, ratio_Z_to_A);
  //Iprintn(mcout, I_eff/eV);
  //Iprintn(mcout, betta);
  //Iprintn(mcout, z);
  double betta = lorbetta(gamma_1);
  double betta2 = betta * betta;
  double gamma  = gamma_1 + 1.0;
  double gamma2 = gamma * gamma;
  double coef1 = 2 * M_PI * pow( classic_electr_radius, 2.0) * 
    electron_mass_c2 * Avogadro;  // should be 0.3071 according to PDG
  //mcout<<"classic_electr_radius/cm="<<classic_electr_radius/cm<<'\n';
  //mcout<<"electron_mass_c2/MeV="<<electron_mass_c2/MeV<<'\n';
  //mcout<<"Avogadro="<<Avogadro<<'\n';
  //mcout<<"Avogadro/mole="<<Avogadro/mole<<'\n';
  //mcout<<"coef1/(MeV*cm2/mole)="<<coef1/(MeV*cm2/mole)<<'\n';
  double coef2=pow( z , 2.0) * ratio_Z_to_A / betta2 ;
  //mcout<<"coef2/(1.0/gram)="<<coef2/(1.0/gram)<<'\n';
  double Mrat = electron_mass_c2 / (M * c_squared);
  double Emax = 2.0 * electron_mass_c2 * betta2 * gamma2 /
    (1.0 + 2.0 * gamma * Mrat + Mrat * Mrat);
  double sum;
  if(Ecut >= Emax)
  {
    sum = log( 2.0 * electron_mass_c2 * betta2 * gamma2 * Emax / 
	       pow(I_eff, 2.0) ) - 2.0 * betta2;
  }
  else
  {
    sum = log( 2.0 * electron_mass_c2 * betta2 * gamma2 * Ecut / 
	       pow(I_eff, 2.0) ) - betta2 * (1.0 + Ecut / Emax);
  }
  //mcout<<"sum="<<sum<<'\n';
  //mcout<<"result/(keV*cm2/gram)="<<coef1 * coef2 * sum /(keV*cm2/gram)<<'\n';
  return coef1 * coef2 * sum;
}
    
