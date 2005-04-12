#include <stdexcept>
#include <math.h>
#include "StiElossCalculator.h"
#if 1
#include "Stiostream.h"
#include "StarCallf77.h" 
#define    gdrelx	 F77_NAME(gdrelx,GDRELX)
extern "C"   void type_of_call gdrelx(float &A, float &Z, float &Dens, float &T, float &m, float &dEdx);

#endif
using namespace std;
/// Bethe-Bloch constant in GeVg^-1cm^2
const double StiElossCalculator::_k   = 0.307e-3;

/// Electron mass in GeV/c^2
const double StiElossCalculator::_mec = 0.510998e-3;

/// Energy Loss Calculator Constructor
///\param zOverA Ratio of Z to A of the scattering material,
///\param  ionization2 square of the ionization potential.
StiElossCalculator::~StiElossCalculator()
{}

/*! Calculate and return the average energy loss for an incoming 
  particle of atomic number "z", mass "m", and velocity "beta"
according to the Bethe-Bloch specific energy loss equation.
<p>
The calculation of the maximum kinetic energy done here is exact.
The density effects are neglected. 
\param z2 square of atomic number "z" of incoming particle,
\param zOverA Ratio of Z to A of the scattering material,
\param m  mass (in GeV/c2) of the incoming particle,
\param beta2 square of the relative velocity (beta=v/c) of the incoming particle.
\param  ionization2 square of the ionization potential.
\throw runtime_error whenever beta2==1
\return energy loss in GeV*cm^2/g.
*/
double StiElossCalculator::calculate(double z2, double zOverA, double m, double beta2, double ionization2 ) const
{
  if (beta2>=1. || beta2<0)
    throw runtime_error("StiElossCalculator::calculate() -E- beta2==1");
  double gamma2 = 1./(1-beta2);
  double gamma  = ::sqrt(gamma2);
  double massRatio = _mec/m;
  double tMax = 2.*_mec*beta2*gamma2/(1.+2*gamma*massRatio+massRatio*massRatio);

  return _k*z2*zOverA*(0.5*::log(_mec*beta2*gamma2*tMax/ionization2)-beta2)/beta2;
}

///Calculate and return the average energy loss for an incoming 
///particle of atomic number "z", mass "m", and velocity "beta"
///according to the Bethe-Bloch specific energy loss equation.
/// This function uses the material parameters stored within this object.
///<p>
///The calculation of the maximum kinetic energy done here is exact.
///The density effects are neglected. 
///\param z2 square of atomic number "z" of incoming particle,
///\param _zOverA Ratio of Z to A of the scattering material,
///\param m  mass (in GeV/c2) of the incoming particle,
///\param beta2 square of the relative velocity (beta=v/c) of the incoming particle.
///\param  _ionization2 square of the ionization potential.
///\throw runtime_error whenever beta2==1
///\return energy loss in GeV*cm^2/g.
double StiElossCalculator::calculate(double z2, double m, double beta2) const
{
#if 1
  float dEdx;
  float A = _A;
  float Z = _Z;
  float Dens = _Dens;
  float Mass = m;
  double beta21 = 1 - beta2;
  if (beta21 < 1.e-10) beta21 = 1.e-10; 
  float T = Mass*(1./::sqrt(beta21) - 1);
  gdrelx(A,Z,Dens,T,Mass,dEdx);
#endif
#if 0
  double fdEdx = calculate(z2,_zOverA, m, beta2,_ionization2);
  cout << "StiElossCalculator::calculate(" << z2 << "," << m << "," << beta2 
       << ") = " << fdEdx << "\tGEANT " << dEdx << "\t" << 100*(fdEdx/dEdx - 1) << endl;
#endif
  return dEdx;
}
