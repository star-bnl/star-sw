#ifndef StiElossCalculator_H_INCLUDED
#define StiElossCalculator_H_INCLUDED

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
class StiElossCalculator
{
 public:

  StiElossCalculator(double zOverA, double ionization);
  virtual ~StiElossCalculator();
  double StiElossCalculator::calculate(double z2, double m, double beta2) const;
  
 protected:
  
  static const double _k;
  static const double _mec;
  double _zOverA;
  double _ionization2;
  
};

#endif

