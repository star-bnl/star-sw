#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include "wcpplib/ioniz/e_cont_enloss.h"
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/stream/prstream.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/util/FunNameStack.h"
/*
Continious energy loss of electron similar to GDRELE from GEANT 3.21

2003,  I. Smirnov
*/

double e_cont_enloss(double ratio_Z_to_A, // do not forget: 
			                  // 1.0/(gram/mole) 
		     double I_eff, 
		     double density, 
		     double Ekin, // in internal units
		     double Ecut, // in internal units
		     double z)
{
  mfunname("double e_cont_enloss(...)");
  double gamma = Ekin / electron_mass_c2 + 1.0;
  double gamma2 = gamma * gamma;
  double gamma_1 = Ekin / electron_mass_c2;
  //mcout<<"gamma="<<gamma<<" gamma_1="<<gamma_1<<" gamma2="<<gamma2<<'\n';
  double dedx = 0.0;
  if(gamma_1 <= 0.0) return dedx;
  double Tcme = Ecut / electron_mass_c2;
  double betta = lorbetta(gamma_1);
  double betta2 = betta * betta;
  // calculation of F^+-
  double F;
  if(z > 0)  // positron
  {
    double y = 1.0/(1.0 + gamma);
    double D = Tcme;
    if(gamma_1 < Tcme) D = gamma_1;
    double D2 = D * D/2.0;
    double D3 = 2.0 * D2 * D/3.0;
    double D4 = D2 * D2;
    F = log(gamma_1 * D) - 
      betta2 * (gamma_1 + 2.0 * D - 
		y * (3.0 * D2 + 
		     y * (D - D3 + 
			  y * (D2 - gamma_1 * D3 + D4 ))))/gamma_1;
  }
  else
  {
    double D = Tcme;
    if(D > gamma_1/2.0) D = gamma_1/2.0;
    F = -1.0 - betta2 + log((gamma_1 - D) * D) + gamma_1/( gamma_1 - D)
      + (0.5 * D * D + (1.0 + 2.0 * gamma_1) * log(1.0 - D/gamma_1)) / gamma2;
  }
  double logI = log(I_eff / electron_mass_c2);
  double eldens =    // in 1 / [legnth^3]
    ratio_Z_to_A *   // 1 / [weight]/mole 
    Avogadro *       // from CLHEP Avogadro = 6.0221367e+23/mole 
    density;         // [weight]/[length]^3
  //double con2 = density;
  double C =  // dimensionless 
    1.0 + 2.0 * 
    log((I_eff/GeV)/
	(28.8e-9 * sqrt(density/(g/cm3) * ratio_Z_to_A * gram/mole )));
  //Iprintn(mcout, density/(g/cm3));
  //Iprintn(mcout, ratio_Z_to_A * gram/mole);
  //Iprintn(mcout, C);
  double x0, x1;
  if(density > 0.05 * g/cm3)
  {
    //mcout<<"density > 0.05 * g/cm3\n";
    if(I_eff < 1.0e-7 * GeV)
    {
      if(C < 3.681)
      {
	x0 = 1.0;
      }
      else
      {
	x0 = 0.326 * C - 1.0;
      }
      x1 = 2.0;
    }
    else
    {
      //mcout<<"I_eff >= 1.0e-7 * GeV\n";
      if(C < 5.215)
      {
	//mcout<<"C < 5.215\n";
	x0 = 0.2;
      }
      else
      {
	x0 = 0.326 * C - 1.5;
      }
      x1 = 3.0;
    }
  }
  else
  {
    //mcout<<"density <= 0.05 * g/cm3\n";
    if(C <= 12.25)
    {
      //mcout<<"C <= 12.25\n";
      double ip = long( (C - 10.0)/0.5 ) + 1;
      if(ip < 0) ip = 0;
      if(ip > 4) ip = 4;
      x0 = 1.6 + 0.1 * double(ip);
      x1 = 4.0;
    }
    else
    {
      if(C <= 13.804)
      {
	x0 = 2.0;
	x1 = 5.0;
      }
      else
      {
	x0 = 0.326 * C - 2.5;
	x1 = 5.0;
      }
    }
  }
  double xa = C / 4.606;
  double aa = 4.606 * (xa - x0)/pow(x1 - x0,  3.0);
  double x = log(gamma_1*(gamma + 1.0))/4.606;
		 // gamma2 - 1.0
  double del = 0.0;
  if(x > x0)
  {
    //mcout<<"x > x0\n";
    //mcout<<"x="<<x<<" x0="<<x0<<" x1="<<x1<<'\n';
    del = 4.606  *  x - C;
    if(x <= x1) del = del + aa * pow(x1 - x, 3.0);
  }
  double cons = 0.153536e-3  // this is 2*pi*r0^2* avogadro * Emass(GeV)
    * GeV * cm2  // translate to internal units
    / Avogadro ;  // already included in eldens
  dedx = cons * eldens * (log( 2.0 * gamma_1 + 4.0) - 2.0 * logI + F - del)/
    betta2;
  if(dedx < 0.0) dedx = 0.0;
  //Iprintn(mcout, dedx /(keV/cm));
  return dedx;
}
