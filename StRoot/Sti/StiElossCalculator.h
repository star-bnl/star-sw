
#ifndef StiElossCalculator_H_INCLUDED
#define StiElossCalculator_H_INCLUDED
#include "Stiostream.h"
#include "Rtypes.h"
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
  StiElossCalculator(Double_t zOverA, Double_t ionization, Double_t A, Double_t Z, Double_t Dens); 

  virtual ~StiElossCalculator();
  Double_t calculate(Double_t charge2, Double_t m, Double_t beta2) const;
  Double_t calcError(Double_t charge2, Double_t m, Double_t beta2) const;
  Double_t getzOverA() const 	{return _zOverA;}
  Double_t getionization2() const {return _ionization2;}
  Double_t getA() const 		{return _A;} 
  Double_t getZ() const 		{return _Z;}
  Double_t getDens() const 	{return _Dens;}
 protected:  
  static const Double_t _k;

  /// Ratio of Z to A of the scattering material
  Double_t _zOverA;
  /// square of the ionization potential.
  Double_t _ionization2;
  Double_t _A;
  Double_t _Z;
  Double_t _Dens;
  Int_t    mId;
};
ostream& operator<<(ostream& os, const StiElossCalculator& m);
typedef StiElossCalculator StiELossCalculator;
#endif

