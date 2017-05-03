
#ifndef StiElossCalculator_H_INCLUDED
#define StiElossCalculator_H_INCLUDED
#include "Stiostream.h"
/*! Energy Loss Calculator
  <p>
  Service class used to calculate the specific energy loss (dEdx) of 
  heavy particles according to the Bethe-Bloch equation - See Particle
  Data Booklet in European Physical Journal 15, (2000). The energy loss
  is calculated in GeV.
  <p>
  This implementation neglects the density effect correction which becomes
  really important only above 10 GeV/c.
  <p>
  Instances of this class to be created for all relevant scattering materials.
  The creation of an instance requires one specifies the effective Z/A ratio
  as well as the average ionization potential of the material. The effective
  quantities can be calculated by weighing the elemental values by the fractional
  weight of the elements of the mixture. This provides an accurate description
  of the effective zOverA but underestimates the effective ionization potential.
  Hopefully, this is good enough for track reconstruction...
  
  \author Claude A Pruneau, Wayne State University
 */
#include <string>
using namespace std;

class StiElossCalculator
{
 public:
  StiElossCalculator(); 
  StiElossCalculator(double zOverA, double ionization, double A, double Z, double Dens); 
  void set(double zOverA, double ionization, double A, double Z, double Dens); 

  virtual ~StiElossCalculator();
  double calculate(double charge2, double m, double beta2) const;
  double calcError(double charge2, double m, double beta2) const;
  double getzOverA() const 	{return _zOverA;}
  double getionization2() const {return _ionization2;}
  double getA() const 		{return _A;} 
  double getZ() const 		{return _Z;}
  double getDens() const 	{return _Dens;}
 protected:  
  static const double _k;

  /// Ratio of Z to A of the scattering material
  double _zOverA;
  /// square of the ionization potential.
  double _ionization2;
  double _A;
  double _Z;
  double _Dens;
  int    mId;
};
ostream& operator<<(ostream& os, const StiElossCalculator& m);
typedef StiElossCalculator StiELossCalculator;
#endif

